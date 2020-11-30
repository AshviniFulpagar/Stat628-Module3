rm(list=ls())

#The working directory is "Stat628-Module3/code", please running the shiny in the right dictionrctory on just
#running its on web.

#Using package in shiny
library(leaflet)
library(shiny)
library(dplyr)
#Loading required information
business_chinese<-read.csv("../clean_data/chinese/business_filter.csv")
review = read.csv("../clean_data/Chinese/review_Chinese.csv")
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
  if(star[i]>=4){level[i]="High Stars"}
  else if(star[i]>=3){level[i]="Medium"}
  else {level[i]="Bad"}
}
level
business_chinese<-mutate(business_chinese,level)
business_chinese$level
plot(density(star),main="Chinese food Yelp Stars Distribution in Madison",xlab="Yelp Stars")


##User interface 

ui <- navbarPage("Yelp data analysis",id="main",
                 tabPanel("Yelp Map",leafletOutput("mymap",height=900,width="100%")),
                 tabPanel("Yelp Star in the City",textOutput("Click_text")),
                 
                 tabPanel("GUIDE",includeMarkdown("./clean_data/chinese/README2.md"))
)

server<-shinyServer(function(input, output) {
  
  
  # new column for the popup label
  
  business_chinese <- mutate(business_chinese, cntnt=paste0('<strong>Name: </strong>',name,
                                                            '<br><strong>Category:</strong> ',categories,                 
                                                            '<br><strong>City:</strong> ', city,
                                                            '<br><strong>Star:</strong> ', stars,
                                                            '<br><strong>Review Count:</strong> ',review_count)) 
  
  # create a color paletter for category type in the data file
  
  pal <- colorFactor(pal = c("#1b9e75", "#d95f02", "#7570b3"), domain = c("High Stars", "Medium", "Bad"))
  
  # create the leaflet map  
  output$mymap <- renderLeaflet({
    leaflet(business_chinese) %>% 
      clearShapes() %>%
      addTiles() %>%
      addCircleMarkers(data = business_chinese, lat =  ~latitude, lng =~longitude, 
                       radius = 7, popup = ~as.character(cntnt), 
                       color = ~pal(level), layerId<- ~name,
                       stroke = FALSE, fillOpacity = 1)%>%
      addLegend(title="Yelp Star Level",pal=pal, values=business_chinese$level,opacity=1,position="topleft")%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  observeEvent(input$mymap_marker_click, { # update the location selectInput on map clicks
    p <- input$mymap_marker_click
    output$Click_text<-renderText({
      
      business_chinese$stars[business_chinese$name==p$id]
      
    })
  })
})

shinyApp(ui=ui, server=server)



