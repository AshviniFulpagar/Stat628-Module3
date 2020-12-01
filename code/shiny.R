rm(list=ls())

#The working directory is "Stat628-Module3/code", please running the shiny in the right dictionrctory on just
#running its on web.
## ----------------------------------
#Using package in shiny

library(leaflet)
library(shiny)
library(dplyr)
library(tidytext)
library(Matrix)
library(pluralize)
library(wordcloud)
## ----------------------------------
#Loading required data set and do data processing
#Loading dataset
business_chinese<-read.csv("../clean_data/chinese/business_filter.csv")
review = read.csv("../clean_data/Chinese/review_Chinese_new.csv")
wordlist = read.csv(file ="../wordList/wordList_V2.csv")

business_chinese<-business_chinese[business_chinese$city=="Madison",]
id<-business_chinese$business_id
name<-business_chinese$name
state<-business_chinese$state
lat<-business_chinese$latitude
lng<-business_chinese$longitude
star<-business_chinese$stars
categories<-business_chinese$categories
count<-business_chinese$review_count
level<-c()
for(i in 1:length(business_chinese$business_id)){
  if(star[i]>=4){level[i]="Very Good"}
  else if(star[i]>=3){level[i]="Medium"}
  else {level[i]="Bad"}
}
business_chinese<-mutate(business_chinese,level)


getDistr <- function(word, dataset){
  dataset$text = tolower(dataset$text)
  word = paste(word,"|",singularize(word),"|",pluralize(word),sep="")
  index = grep(word, dataset$text,ignore.case = TRUE)
  if(length(index)==0)
  {
    print("word not detected.")
    return(1)
  }
  stars = dataset$stars[index]
  elements = rep(0,5)
  for(i in 1:5)
  {
    elements[i] = sum(stars==i)/length(stars)
  }
  # barplot(elements,names.arg = c("1","2","3","4","5"))
  return(elements*length(stars))
}


# To get the most frequently used words in review data
mostFreq <- function(dataset, maxrownum)
{
  text_df <- tibble(index = 1:nrow(dataset), review = dataset$text)
  tt  = text_df %>% unnest_tokens(word, review)
  A = cast_sparse(tt, index, word)
  wordlist = colSums(A)
  wordlist = sort(wordlist,decreasing = TRUE)
  wordlist = as.data.frame(wordlist)
  wordlist$word = rownames(wordlist)
  colnames(wordlist) <- c("count","word")
  cleaned_wordlist <- wordlist %>%
    anti_join(get_stopwords())
  return(cleaned_wordlist[1:maxrownum,])
}

## ----------------------------------
##Shiny User interface 

ui <- navbarPage("Yelp data analysis",id="main",
                 tabPanel("Yelp Map",leafletOutput("mymap",height=900,width="100%")),
                 tabPanel("Yelp Star in the City",
                          p(h3("Yelp customer Reviews")),
                          helpText("The barplots and the wordcloud show what the customers focus more in the restaurants, and the rate on each keyword aspect."),
                          br(),
                          p(h4("The key word cloud of this restaurant is:")),
                          plotOutput("wordcloud"),
                          br(),
                          p(h4("The barplot comparing the rate on keywords between this restaurant and the average:")),
                          br(),
                          plotOutput("bar1"),
                          plotOutput("bar2"),
                          br(),
                          p(h3("Business Suggestions")),
                          textOutput("Click_text")),
                 
                 tabPanel("GUIDE",includeMarkdown("../clean_data/chinese/README2.md"))
)

## ----------------------------------
##Shiny Server
server<-shinyServer(function(input, output) {
  
  
  # new column for the popup label
  
  business_chinese <- mutate(business_chinese, cntnt=paste0('<strong>Name: </strong>',name,
                                                            '<br><strong>Category:</strong> ',categories,                 
                                                            '<br><strong>City:</strong> ', city,
                                                            '<br><strong>Star:</strong> ', stars,
                                                            '<br><strong>Review Count:</strong> ',review_count)) 
  
  # create a color paletter for category type in the data file
  
  pal <- colorFactor(pal = c("#1b9e75", "#d95f02", "#7570b3"), domain = c("Very Good", "Medium", "Bad"))
  
  # create the leaflet map  
  output$mymap <- renderLeaflet({
    leaflet(business_chinese) %>% 
      clearShapes() %>%
      addTiles() %>%
      addCircleMarkers(data = business_chinese, lat =  ~latitude, lng =~longitude, 
                       radius = 7, popup = ~as.character(cntnt), 
                       color = ~pal(level), layerId<- ~name,
                       stroke = TRUE, fillOpacity = 1)%>%
      addLegend(title="Yelp Star Level",pal=pal, values=business_chinese$level,opacity=1,position="topleft")%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  observeEvent(input$mymap_marker_click, 
               { # update the location selectInput on map clicks
    p1 <- input$mymap_marker_click
    business_id = business_chinese$business_id[business_chinese$name==p1$id]
    Reviews  = review[which(review$business_id==business_id),]
    frequentword = mostFreq(Reviews,100)
    word = c()
    i=1;count=0
    while(i<100){
      if(frequentword$word[i] %in% wordlist$word)
      {
        word = rbind(word, frequentword[i,])
        count = count+1
      }
      if(count==20){
        break}
      i=i+1}
    #Wilcoxon Rank test Part    
    pvalue<-c()
    pvalue1<-c()
    pvalue2<-c()
    for(i in 1:20)
    {
      keyword<-word$word[i]
      freq1 = getDistr(keyword,Reviews)
      sample1 = rep(1:5, freq1)
      freq2 = getDistr(keyword, review)
      sample2 = rep(1:5,freq2)
      if(sum(freq1)==0){
        print("This keyword is not detected in the reviews")
      }else{
      pvalue1[i] = wilcox.test(sample1,sample2,alternative = "less")$p.value
      pvalue2[i] = wilcox.test(sample1,sample2,alternative = "greater")$p.value
      pvalue [i]= wilcox.test(sample1,sample2)$p.value}}
#Output part
    output$wordcloud<-renderPlot(wordcloud(word$word,word$count))
    
    output$Click_text<-renderText({business_id})
    
    keyword<-word$word[1]
    freq1 = getDistr(keyword,Reviews)
    sample1 = rep(1:5, freq1)
    freq2 = getDistr(keyword, review)
    sample2 = rep(1:5,freq2)
    elements = rep(0,10)
    elements[c(1,3,5,7,9)]=freq1/sum(freq1)
    elements[c(2,4,6,8,10)]=freq2/sum(freq2)
    output$bar1<-renderPlot( barplot(elements,
                                       names.arg = c("1","1","2","2","3","3","4","4","5","5"),
                                       col = c(1,2,1,2,1,2,1,2),
                                       xlab = "stars",
                                       ylab = "frequency",
                                       space = c(0.3,0,0.3,0,0.3,0,0.3,0,0.3,0),
                                       main=keyword,
                                       ylim = c(0,min(max(elements)*1.5,1))))
    keyword2<-word$word[2]
    freq1.2 = getDistr(keyword2,Reviews)
    sample1.2 = rep(1:5, freq1.2)
    freq2.2 = getDistr(keyword2, review)
    sample2.2 = rep(1:5,freq2.2)
    elements.2 = rep(0,10)
    elements.2[c(1,3,5,7,9)]=freq1.2/sum(freq1.2)
    elements.2[c(2,4,6,8,10)]=freq2.2/sum(freq2.2)
    
    output$bar2<-renderPlot(barplot(elements.2,
                                     names.arg = c("1","1","2","2","3","3","4","4","5","5"),
                                     col = c(1,2,1,2,1,2,1,2),
                                     xlab = "stars",
                                     ylab = "frequency",
                                     space = c(0.3,0,0.3,0,0.3,0,0.3,0,0.3,0),
                                     main= keyword2,
                                     ylim = c(0,min(max(elements)*1.5,1))))
    
    })
})

shinyApp(ui=ui, server=server)

