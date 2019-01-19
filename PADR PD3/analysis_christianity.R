source("libraries.R")

setwd("Prepared Data")

data <- read.csv("christianity_barplot_data.csv", stringsAsFactors = FALSE)
data_map <- read.csv("christianity_map_data.csv", stringsAsFactors = FALSE)

setwd('..')

###########################################

bp <- barplot(t(as.matrix(data[2:3])),
              beside = FALSE, 
              space = 0.5, 
              names.arg=data$Year,         
              col=c("deepskyblue","red"), 
              las=1,
              legend=c("Questions", "Answers"),
              main=sprintf('Number of posts in Christianity from %s to %s', 
                           data$mindate[[1]], 
                           data$maxdate[[1]]))
text(bp, data$Count.question/2, labels = data$Count.question, xpd=TRUE)
text(bp, data$Count.question + data$Count.answer/2, labels = data$Count.answer, xpd=TRUE)
box(bty="l")

###########################################

shinyApp(
  ui = fluidPage(
    leafletOutput("mymap", height = 1000)
  ),
  
  server = function(input, output){
    
    data <- reactive({
      df <- data_map 
      df
    })
    max_count <- max(data_map$Count)

    output$mymap <- renderLeaflet({
      m <- leaflet(data = data() ) %>% 
        addTiles() %>%
        setView(lng=0, lat=0, zoom=2) %>%  
        addCircleMarkers(lng = ~Long,
                         lat = ~Lat,
                         radius = ~Count/max_count*100,
                         popup = paste("<b>", data()$Country, "</b>",
                                       ", number of posts: ",
                                       "<b>", data()$Count, "</b>",
                                       sep=""))
      m
    })
  }
)
