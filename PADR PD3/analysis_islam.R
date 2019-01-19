source("libraries.R")

setwd("Prepared Data")

islam_data <- read.csv("islam_barplot_data.csv", stringsAsFactors = FALSE)
mydata <- read.csv("islam_data.csv", stringsAsFactors = FALSE)

setwd('..')


################################################

bp <- barplot(t(as.matrix(islam_data[2:3])),
              beside = FALSE, 
              space = 0.5, 
              names.arg=islam_data$Year,         
              col=c("deepskyblue","red"), 
              las=1,
              legend=c("Questions", "Answers"),
              args.legend = list(x="topleft"),
              main=sprintf('Number of posts in Christianity from %s to %s', 
                           islam_data$mindate[[1]], 
                           islam_data$maxdate[[1]]))
text(bp, islam_data$Count.question/2, labels = islam_data$Count.question, xpd=TRUE)
text(bp, islam_data$Count.question + islam_data$Count.answer/2, labels = islam_data$Count.answer, xpd=TRUE)
box(bty="l")

################################################

shinyApp(
  ui = fluidPage(
    titlePanel("Number of posts about Islam until"),

    sliderInput(inputId = "year",
                label="",
                # label = "Choose a year",
                value = 2012, min = 2011, max = 2018,
                round = TRUE),

    leafletOutput("mymap", height = 1000)
  ),
  
  server = function(input, output){
    
    
    data <- reactive({
      df <- mydata %>% 
              filter(Year <= input$year) %>%
              group_by(Country, Long, Lat) %>% 
              summarise(Count=n()) 
      df
    })
    
    max_count <- as.numeric(
                    mydata %>% group_by(Country) %>% 
                    summarise(Count=n()) %>%
                    filter(Count==max(Count)) %>%
                    select(Count))

    output$mymap <- renderLeaflet({
      m <- leaflet(data = data() ) %>% # creates map widget
        addTiles() %>% # adds the default OpenStreet map tiles
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
