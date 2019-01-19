
source("libraries.R")

setwd("Prepared Data")

sent <- read.csv("politics_sentiment_data.csv", stringsAsFactors = FALSE)
sent1 <- read.csv("politics_sentiment_2_data.csv", stringsAsFactors = FALSE)

setwd('..')



shinyApp(
  ui = fluidPage(
    
    theme = shinytheme("lumen"),
    titlePanel("Correlation between Score and average sentiment"),
    
    selectInput(inputId = "type_post", label = strong("Type of posts"),
                choices = unique(c("Questions", "Answers", "All")),
                selected = "Questions"),
    
    plotOutput("distplot", height = "700")
  ),
  
  server = function(input, output) {
    
    q <- reactive({
      d<-input$type_post
      if(d=="Questions") k=c(1)
      if(d=="Answers") k=c(2)
      if(d=="All") k=c(1, 2)
      positive <- sent %>%
        arrange(desc(ave_sentiment))%>%
        filter(PostTypeId %in% k) 
      positive
    })
    output$distplot <- renderPlot({
      
      plot(q()$ave_sentiment, q()$Score,
           col = "#FF007050",
           xlab = "Sentiment",
           ylab = "Score",
           main = "Correlation")
    })
  },
  options=list(heigth=500)
)

##########################################################

shinyApp(
  ui = fluidPage(
    
    theme = shinytheme("lumen"),
    titlePanel("Sentiment words"),
    
    selectInput(inputId = "order", label = strong("Score Order"),
                choices = c("decreasing", "increasing"),
                selected = "decreasing"),
    sliderInput(inputId = "Amount",
                label = "Count of top obervations",
                min = 1,
                max = 20,
                value = 10),
    
    tableOutput("view")
  ),
  
  server = function(input, output) {
    
    words<-reactive({
      n<-input$Amount
      if(input$order=="decreasing"){
        positive1 <- sent1 %>% arrange(desc(Score))
      }
      if(input$order=="increasing"){
        positive1 <- sent1 %>% arrange(Score)
      }
      
      n <- extract_sentiment_terms(get_sentences(positive1$Title))$negative
      n1 <- lapply(n, function(txt){if(identical(txt, character(0))) 
        return(" ")
        else return(txt)})
      Negative <- stri_paste_list(n1, sep=", ") 
      
      p <- extract_sentiment_terms(get_sentences(positive1$Title))$positive
      p1 <- lapply(p, function(txt){if(identical(txt, character(0))) 
        return(" ")
        else return(txt)})
      Positive <- stri_paste_list(p1, sep=", ") 
      x<-cbind(Negative, Positive, Score=positive1$Score, Sentiment=positive1$ave_sentiment)
      
    })
    
    output$view <- renderTable({
      head(words(), input$Amount, spacing = c("l"))
    },
    striped = TRUE,
    bordered = TRUE,
    width = '100%' )
  },
  options = list(heigth=500)
)
