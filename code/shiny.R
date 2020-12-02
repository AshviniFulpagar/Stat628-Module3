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
business_chinese<-business_chinese[-(business_chinese$business_id=="SWDEdiv_v1QppobT1YnEPA"),]
business_chinese<-business_chinese[-(business_chinese$business_id== "t4rBeSFDfwvaIbLftLlhig"),]
business_chinese<-business_chinese[-(business_chinese$business_id=="POJH4cPxgKCWWGVYJ_2i0w"),]
business_chinese<-business_chinese[-(business_chinese$business_id=="IXP6VfjiUFIRiRbZjeUulw"),]
review = read.csv("../clean_data/Chinese/review_Chinese_new.csv")
wordlist = read.csv(file ="../wordList/wordList_V2.csv")


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
                 tabPanel("Yelp Map",leafletOutput("mymap",height = "800px",width="100%"),
                          ),
                 tabPanel("Yelp Star in the City",
                          p(h3("Yelp customer Reviews")),
                          br(),
                          p(h4(textOutput("hhh1"))),
                          br(),
                          helpText("The barplots and the wordcloud show what the customers focus more in the restaurants, and the rate on each keyword aspect."),
                          br(),
                          helpText("In order to show the best description of reviews, it may take about one minute to generate plot result. Thank you very much for your patience."),
                          br(),
                          p(h4("The key word cloud of this restaurant is:")),
                          br(),
                          plotOutput("wordcloud"),
                          br(),
                          p(h4("The barplot comparing the rate on top 12 keywords between this restaurant and the average:")),
                          helpText("It shows the frequency of stars from the review about this restaurant and the average of all the restaurants, when customer mentioned this keyword."),
                          br(),
                          plotOutput("bar1"),
                          plotOutput("bar2"),
                          plotOutput("bar3"),
                          plotOutput("bar4"),
                          plotOutput("bar5"),
                          plotOutput("bar6"),
                          plotOutput("bar7"),
                          plotOutput("bar8"),
                          plotOutput("bar9"),
                          plotOutput("bar10"),
                          plotOutput("bar11"),
                          plotOutput("bar12")),
                 tabPanel("Business Suggestions",
                          p(h3("The tips on business suggestions")),
                          helpText("It may need about one minute to generate results. Please wait for a while"),
                          br(),
                          p(h5("1.We use scores based on Wilcoxon rank test results and giving each key aspects of this restaurant a socre.")),
                          p(h5("2.If the socre is in 90-100, it means this key aspect in this restaurent is pretty good. We suggest the business owner should keep this advantage.")),
                          p(h5("3.If the socre is in 10-90, it shows this aspect is in the average level, we suggest the owner should plan some methods to improve this aspect.")),
                          p(h5("4.If the socre is in 0-10, it indicates this aspect is worse than the average level, we suggest the business owner should solve this problem immediately.")),
                          br(),
                          p(h3("Suggestion and Score")),
                          p(h4(textOutput("hhh2"))),
                          br(),
                          p(h4(textOutput("ot1"))),
                          p(h4(textOutput("ot2"))),
                          p(h4(textOutput("ot3"))),
                          p(h4(textOutput("ot4"))),
                          p(h4(textOutput("ot5"))),
                          p(h4(textOutput("ot6"))),
                          p(h4(textOutput("ot7"))),
                          p(h4(textOutput("ot8"))),
                          p(h4(textOutput("ot9"))),
                          p(h4(textOutput("ot10"))),
                          p(h4(textOutput("ot11"))),
                          p(h4(textOutput("ot12"))),
                          ),
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
    RC<-business_chinese$review_count[business_chinese$name==p1$id]
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
      if(count==12){
        break}
      i=i+1}
    #Wilcoxon Rank test Part    
    pvalue<-c()
    pvalue1<-c()
    pvalue2<-c()
    for(i in 1:12)
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
    output$hhh1<-renderText(paste0("There are ",RC," effective reviews about ",p1$id, " on Yelp"))
    
    output$wordcloud<-renderPlot(wordcloud(word$word,word$count))
  
    
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
                                       legend.text = (c("This Restaurant/Shop", "Average")),
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
                                     legend.text = (c("This Restaurant/Shop", "Average")),
                                     ylim = c(0,min(max(elements)*1.5,1))))
    
    
    # 3
    keyword3 = word$word[3]
    freq1.3 = getDistr(keyword3,Reviews)
    sample1.3 = rep(1:5, freq1.3)
    freq2.3 = getDistr(keyword3, review)
    sample2.3 = rep(1:5,freq2.3)
    elements.3 = rep(0,10)
    elements.3[c(1,3,5,7,9)] = freq1.3/sum(freq1.3)
    elements.3[c(2,4,6,8,10)] = freq2.3/sum(freq2.3)
    
    output$bar3 = renderPlot(barplot(elements.3,
                                     names.arg = c("1","1","2","2","3","3","4","4","5","5"),
                                     col = c(1,2,1,2,1,2,1,2),
                                     xlab = "stars",
                                     ylab = "frequency",
                                     space = c(0.3,0,0.3,0,0.3,0,0.3,0,0.3,0),
                                     main= keyword3,
                                     legend.text = (c("This Restaurant/Shop", "Average")),
                                     ylim = c(0,min(max(elements.3)*1.5,1))))
    # 4
    keyword4 = word$word[4]
    freq1.4 = getDistr(keyword4,Reviews)
    sample1.4 = rep(1:5, freq1.4)
    freq2.4 = getDistr(keyword4, review)
    sample2.4 = rep(1:5,freq2.4)
    elements.4 = rep(0,10)
    elements.4[c(1,3,5,7,9)] = freq1.4/sum(freq1.4)
    elements.4[c(2,4,6,8,10)] = freq2.4/sum(freq2.4)
    
    output$bar4 = renderPlot(barplot(elements.4,
                                     names.arg = c("1","1","2","2","3","3","4","4","5","5"),
                                     col = c(1,2,1,2,1,2,1,2),
                                     xlab = "stars",
                                     ylab = "frequency",
                                     space = c(0.3,0,0.3,0,0.3,0,0.3,0,0.3,0),
                                     main= keyword4,
                                     legend.text = (c("This Restaurant/Shop", "Average")),
                                     ylim = c(0,min(max(elements.4)*1.5,1))))
    # 1
    keyword5 = word$word[5]
    freq1.5 = getDistr(keyword5,Reviews)
    sample1.5 = rep(1:5, freq1.5)
    freq2.5 = getDistr(keyword5, review)
    sample2.5 = rep(1:5,freq2.5)
    elements.5 = rep(0,10)
    elements.5[c(1,3,5,7,9)] = freq1.5/sum(freq1.5)
    elements.5[c(2,4,6,8,10)] = freq2.5/sum(freq2.5)
    
    output$bar5 = renderPlot(barplot(elements.5,
                                     names.arg = c("1","1","2","2","3","3","4","4","5","5"),
                                     col = c(1,2,1,2,1,2,1,2),
                                     xlab = "stars",
                                     ylab = "frequency",
                                     space = c(0.3,0,0.3,0,0.3,0,0.3,0,0.3,0),
                                     main= keyword5,
                                     legend.text = (c("This Restaurant/Shop", "Average")),
                                     ylim = c(0,min(max(elements.5)*1.5,1))))
    # 6
    keyword6 = word$word[6]
    freq1.6 = getDistr(keyword6,Reviews)
    sample1.6 = rep(1:5, freq1.6)
    freq2.6 = getDistr(keyword6, review)
    sample2.6 = rep(1:5,freq2.6)
    elements.6 = rep(0,10)
    elements.6[c(1,3,5,7,9)] = freq1.6/sum(freq1.6)
    elements.6[c(2,4,6,8,10)] = freq2.6/sum(freq2.6)
    
    output$bar6 = renderPlot(barplot(elements.6,
                                     names.arg = c("1","1","2","2","3","3","4","4","5","5"),
                                     col = c(1,2,1,2,1,2,1,2),
                                     xlab = "stars",
                                     ylab = "frequency",
                                     space = c(0.3,0,0.3,0,0.3,0,0.3,0,0.3,0),
                                     main= keyword6,
                                     legend.text = (c("This Restaurant/Shop", "Average")),
                                     ylim = c(0,min(max(elements.6)*1.5,1))))
    # 7
    keyword7 = word$word[7]
    freq1.7 = getDistr(keyword7,Reviews)
    sample1.7 = rep(1:5, freq1.7)
    freq2.7 = getDistr(keyword7, review)
    sample2.7 = rep(1:5,freq2.7)
    elements.7 = rep(0,10)
    elements.7[c(1,3,5,7,9)] = freq1.7/sum(freq1.7)
    elements.7[c(2,4,6,8,10)] = freq2.7/sum(freq2.7)
    
    output$bar7 = renderPlot(barplot(elements.7,
                                     names.arg = c("1","1","2","2","3","3","4","4","5","5"),
                                     col = c(1,2,1,2,1,2,1,2),
                                     xlab = "stars",
                                     ylab = "frequency",
                                     space = c(0.3,0,0.3,0,0.3,0,0.3,0,0.3,0),
                                     main= keyword7,
                                     legend.text = (c("This Restaurant/Shop", "Average")),
                                     ylim = c(0,min(max(elements.7)*1.5,1))))
    # 8
    keyword8 = word$word[8]
    freq1.8 = getDistr(keyword8,Reviews)
    sample1.8 = rep(1:5, freq1.8)
    freq2.8 = getDistr(keyword8, review)
    sample2.8 = rep(1:5,freq2.8)
    elements.8 = rep(0,10)
    elements.8[c(1,3,5,7,9)] = freq1.8/sum(freq1.8)
    elements.8[c(2,4,6,8,10)] = freq2.8/sum(freq2.8)
    
    output$bar8 = renderPlot(barplot(elements.8,
                                     names.arg = c("1","1","2","2","3","3","4","4","5","5"),
                                     col = c(1,2,1,2,1,2,1,2),
                                     xlab = "stars",
                                     ylab = "frequency",
                                     space = c(0.3,0,0.3,0,0.3,0,0.3,0,0.3,0),
                                     main= keyword8,
                                     legend.text = (c("This Restaurant/Shop", "Average")),
                                     ylim = c(0,min(max(elements.8)*1.5,1))))
    # 9
    keyword9 = word$word[9]
    freq1.9 = getDistr(keyword9,Reviews)
    sample1.9 = rep(1:5, freq1.9)
    freq2.9 = getDistr(keyword9, review)
    sample2.9 = rep(1:5,freq2.9)
    elements.9 = rep(0,10)
    elements.9[c(1,3,5,7,9)] = freq1.9/sum(freq1.9)
    elements.9[c(2,4,6,8,10)] = freq2.9/sum(freq2.9)
    
    output$bar9 = renderPlot(barplot(elements.9,
                                     names.arg = c("1","1","2","2","3","3","4","4","5","5"),
                                     col = c(1,2,1,2,1,2,1,2),
                                     xlab = "stars",
                                     ylab = "frequency",
                                     space = c(0.3,0,0.3,0,0.3,0,0.3,0,0.3,0),
                                     main= keyword9,
                                     legend.text = (c("This Restaurant/Shop", "Average")),
                                     ylim = c(0,min(max(elements.9)*1.5,1))))
    # 10
    keyword10 = word$word[10]
    freq1.10 = getDistr(keyword10,Reviews)
    sample1.10 = rep(1:5, freq1.10)
    freq2.10 = getDistr(keyword10, review)
    sample2.10 = rep(1:5,freq2.10)
    elements.10 = rep(0,10)
    elements.10[c(1,3,5,7,9)] = freq1.10/sum(freq1.10)
    elements.10[c(2,4,6,8,10)] = freq2.10/sum(freq2.10)
    
    output$bar10 = renderPlot(barplot(elements.10,
                                     names.arg = c("1","1","2","2","3","3","4","4","5","5"),
                                     col = c(1,2,1,2,1,2,1,2),
                                     xlab = "stars",
                                     ylab = "frequency",
                                     space = c(0.3,0,0.3,0,0.3,0,0.3,0,0.3,0),
                                     main= keyword10,
                                     legend.text = (c("This Restaurant/Shop", "Average")),
                                     ylim = c(0,min(max(elements.10)*1.5,1))))
    # 11
    keyword11 = word$word[11]
    freq1.11 = getDistr(keyword11,Reviews)
    sample1.11 = rep(1:5, freq1.11)
    freq2.11 = getDistr(keyword11, review)
    sample2.11 = rep(1:5,freq2.11)
    elements.11 = rep(0,10)
    elements.11[c(1,3,5,7,9)] = freq1.11/sum(freq1.11)
    elements.11[c(2,4,6,8,10)] = freq2.11/sum(freq2.11)
    
    output$bar11 = renderPlot(barplot(elements.11,
                                     names.arg = c("1","1","2","2","3","3","4","4","5","5"),
                                     col = c(1,2,1,2,1,2,1,2),
                                     xlab = "stars",
                                     ylab = "frequency",
                                     space = c(0.3,0,0.3,0,0.3,0,0.3,0,0.3,0),
                                     main= keyword11,
                                     legend.text = (c("This Restaurant/Shop", "Average")),
                                     ylim = c(0,min(max(elements.11)*1.5,1))))
    # 12
    keyword12 = word$word[12]
    freq1.12 = getDistr(keyword12,Reviews)
    sample1.12 = rep(1:5, freq1.12)
    freq2.12 = getDistr(keyword12, review)
    sample2.12 = rep(1:5,freq2.12)
    elements.12 = rep(0,10)
    elements.12[c(1,3,5,7,9)] = freq1.12/sum(freq1.12)
    elements.12[c(2,4,6,8,10)] = freq2.2/sum(freq2.12)
    
    output$bar12 = renderPlot(barplot(elements.12,
                                     names.arg = c("1","1","2","2","3","3","4","4","5","5"),
                                     col = c(1,2,1,2,1,2,1,2),
                                     xlab = "stars",
                                     ylab = "frequency",
                                     space = c(0.3,0,0.3,0,0.3,0,0.3,0,0.3,0),
                                     main= keyword12,
                                     legend.text = (c("This Restaurant/Shop", "Average")),
                                     ylim = c(0,min(max(elements.12)*1.5,1))))

    #Suggestion score
    score<-pvalue1*100
    output$hhh2<-renderText(paste0("The Score is based on Wilcoxon Rank Test from ",RC," effective reviews about ",p1$id, " on Yelp."))
    output$ot1<-renderText(paste0("The ",word$word[1]," aspect score is ", score[1]))
    output$ot2<-renderText(paste0("The ",word$word[2]," aspect score is ", score[2]))
    output$ot3<-renderText(paste0("The ",word$word[3]," aspect score is ", score[3]))
    output$ot4<-renderText(paste0("The ",word$word[4]," aspect score is ", score[4]))
    output$ot5<-renderText(paste0("The ",word$word[5]," aspect score is ", score[5]))
    output$ot6<-renderText(paste0("The ",word$word[6]," aspect score is ", score[6]))
    output$ot7<-renderText(paste0("The ",word$word[7]," aspect score is ", score[7]))
    output$ot8<-renderText(paste0("The ",word$word[8]," aspect score is ", score[8]))
    output$ot9<-renderText(paste0("The ",word$word[9]," aspect score is ", score[9]))
    output$ot10<-renderText(paste0("The ",word$word[10]," aspect score is ", score[10]))
    output$ot11<-renderText(paste0("The ",word$word[11]," aspect score is ", score[11]))
    output$ot12<-renderText(paste0("The ",word$word[11]," aspect score is ", score[12]))
    })
})

shinyApp(ui=ui, server=server)

