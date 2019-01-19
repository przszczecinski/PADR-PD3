source("libraries.R")

setwd("Prepared Data")

Posts3 <- read.csv("travel_1_data.csv", stringsAsFactors = FALSE)

setwd('..')

Changes <- function(x){
  Posts3 %>%
    filter(Country != -1) %>%
    group_by(Country, Year) %>%
    mutate(count=n()) %>%
    filter(Country==x) %>%
    distinct(Country, Year, count)
}

Poland <- Changes("Poland")
Peru <- Changes("Peru")
Russia <- Changes("Russia")

matplot(Poland$Year, 
        cbind(Poland$count, Peru$count, Russia$count), 
        type="l", 
        las=1, 
        col=c(1,2,3), 
        lty=1,
        ylab = "Number of posts",
        xlab = "Year",
        main = "Amount of posts relative to countries - changes in time")
legend("topleft", legend=c("Poland", "Peru", "Russia"),
       col=c(1, 2, 3), lty=1)

###############################################

setwd("Prepared Data")

top <- read.csv("travel_most_searched_data.csv", stringsAsFactors = FALSE)
top2 <- top %>% slice(1:40)

setwd('..')

# map of the most searched countries
spdf <- invisible(joinCountryData2Map(top2, joinCode="NAME", nameJoinColumn="Country"))

mapCountryData(spdf, 
               nameColumnToPlot="Value", 
               catMethod ="fixedWidth", 
               mapTitle = "Heatmap of the most searched countries")

###############################################

if (interactive()) {
  ui <- fluidPage(
    
    theme = shinytheme("lumen"),
    titlePanel("The most searched countries"),
    
    selectInput(inputId = "type_year", label = strong("Year"),
                choices = unique(Posts3$Year),
                selected = "2018"),
    sliderInput(inputId = "Amount",
                label = "Number of countries",
                min = 1,
                max = 20,
                value = 10),
    
    plotOutput("distplot")
  )
  
  server <- function(input, output) {
    
    Posts4 <- reactive({
      d <- input$type_year
      l <- input$Amount
      s <- Posts3 %>%
            filter(Country != -1) %>%
            group_by(Country, Year) %>%
            mutate(count=n()) %>%
            filter(Year==d) %>%
            distinct(Country, count) %>%
            arrange(desc(count)) 
      z <- head(s, l)
    })
    
    output$distplot <- renderPlot({
      
      bp <- barplot(Posts4()$count, 
                    #names.arg=Posts4()$Country,
                    col = "#75AADB", 
                    border = "white",
                    xlab = "Amount of occurences",
                    main = "Comparison of countries occurences in Titles",
                    las=1,
                    xpd = TRUE)
      # text(bp, Posts4()$count*(1.05), Posts4()$count)
      text(bp, par("usr")[3]-2, srt=45, xpd=TRUE, adj=1, labels=Posts4()$Country)
    })
  }
  shinyApp(ui, server)
}
