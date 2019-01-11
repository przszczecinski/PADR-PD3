install.packages("XML")
install.packages("stringi")
install.packages("countrycode")
install.packages("rworldmap")
install.packages("shiny")
install.packages("shinythemes")
install.packages("sentimentr")
install.packages("Rcpp")


library(XML)
library(stringi)
library(dplyr)
library(countrycode)
library(maptools)
library(maps)
library(rworldmap)
library(shiny)
library(shinythemes)
library(sentimentr)
library(Rcpp)
library(microbenchmark)


options(stringsAsFactors = FALSE)

# pomysły na risercz
# https://meta.stackexchange.com/questions/134495/academic-papers-using-stack-exchange-data
#


# DATA 

# Christianity
setwd("2. Prepared Data/Christianity")

Posts.christianity.XML <- xmlParse("Posts.xml")
Posts.christianity <- XML:::xmlAttrsToDataFrame(getNodeSet(Posts.christianity.XML, path='//row'))
Badges.christianity.XML <- xmlParse("Badges.xml")
Badges.christianity <- XML:::xmlAttrsToDataFrame(getNodeSet(Badges.christianity.XML, path='//row'))
Comments.christianity.XML <- xmlParse("Comments.xml")
Comments.christianity <- XML:::xmlAttrsToDataFrame(getNodeSet(Comments.christianity.XML, path='//row'))
PostLinks.christianity.XML <- xmlParse("PostLinks.xml")
PostLinks.christianity <- XML:::xmlAttrsToDataFrame(getNodeSet(PostLinks.christianity.XML, path='//row'))
Tags.christianity.XML <- xmlParse("Tags.xml")
Tags.christianity <- XML:::xmlAttrsToDataFrame(getNodeSet(Tags.christianity.XML, path='//row'))
Users.christianity.XML <- xmlParse("Users.xml")
Users.christianity <- XML:::xmlAttrsToDataFrame(getNodeSet(Users.christianity.XML, path='//row'))
Votes.christianity.XML <- xmlParse("Votes.xml")
Votes.christianity <- XML:::xmlAttrsToDataFrame(getNodeSet(Votes.christianity.XML, path='//row'))



#########################################
##### Amount of posts through years #####
#########################################

p <- Posts.christianity %>% 
  mutate(Year=substr(CreationDate, 1, 4)) %>% 
  group_by(Year)

p.question <- p %>% 
  filter(PostTypeId==1) %>% 
  summarise(Count=n())

p.answer <- p %>% 
  filter(PostTypeId==2) %>%
  summarise(Count=n())


dates <- Posts.christianity %>% arrange(CreationDate) %>% select(CreationDate)
# first post date
mindate <- substr(head(dates, 1)[[1]], 1, 10) 
# last post date
maxdate <- substr(tail(dates, 1)[[1]], 1, 10)  

mydata <- merge(p.question, p.answer, by = 'Year', suffixes = c(".question", ".answer"))
bp <- barplot(t(as.matrix(mydata[2:3])),
              beside = FALSE, 
              space = 0.5, 
              names.arg=mydata$Year,         
              col=c("deepskyblue","red"), 
              las=1,
              legend=c("Question", "Answer"),
              main=sprintf('Liczba postów od %s do %s', mindate, maxdate))
text(bp, p.question$Count/2, labels=p.question$Count, xpd=TRUE)
text(bp, p.question$Count+p.answer$Count/2, labels=p.answer$Count, xpd=TRUE)
box(bty="l")# to poprawić


########################################################
##### Countries where the most of posts come from  #####
########################################################

users <- Users.christianity
posts <- Posts.christianity

tmp <- users %>% 
          mutate(Country = lapply(Location, what_country_are_you_from)) %>% 
          mutate(Country  = lapply(Country, remove_mutliple_countries))

data <- posts %>% 
          left_join(tmp, by=c("OwnerUserId"="Id")) %>% 
          select(ViewCount, Id, Location, Country)


#####################################################
##### 
#####################################################

setwd("2. Prepared Data/Travel")

Posts.travel.XML <- xmlParse("Posts.xml")
Posts.travel <- XML:::xmlAttrsToDataFrame(getNodeSet(Posts.travel.XML, path='//row'))
Users.travel.XML <- xmlParse("Users.xml")
Users.travel <- XML:::xmlAttrsToDataFrame(getNodeSet(Users.travel.XML, path='//row'))

# list of all countries
countries <- codelist$country.name.en

# only questions
Questions <- Posts.travel %>%
              filter(PostTypeId==1)

# Function visit returns matrix containing number of search results for each country and 
# vector Title2: <country_name>,if country_name appears in Title.
visit <- function(){
  k <- 1
  l <- length(countries)
  X <- matrix(0, nrow=l, ncol=2)
  Title2 <- rep(-1, length(Questions$Title))
  for(i in countries)
  {
    is.visited <- stri_detect_fixed(as.factor(Questions$Title), i, case_insensitive=TRUE)
    Title2[is.visited==TRUE] <- i
    n<-length(na.omit(is.visited[is.visited==TRUE]))
    X[k, 1] <- i
    X[k, 2] <- n
    k <- (k+1)
  }
  list(X, Title2)
}

# tdata.frame [country, amount of posts related to it]
Visit <- visit()

Visit2 <- visit()

n.visit <- as.data.frame(Visit[1])
Country <- as.data.frame(Visit[2])

names(n.visit)[c(1, 2)] <- c("Country", "Number")
names(Country) <- "Country"

# changes in time
Questions2 <- Questions %>%
              mutate(Year2=format(as.Date(CreationDate, format="%Y"), "%Y")) %>%
              select(Title, Year2)

Posts3 <- cbind(Questions2, Country)

Changes <- function(x){
    Posts3 %>%
    filter(Country != -1) %>%
    group_by(Country, Year2) %>%
    mutate(count=n()) %>%
    filter(Country==x)%>%
    distinct(Country, Year2, count)
}

Poland <- Changes("Poland")
France <- Changes("France")
Russia <- Changes("Russia")

matplot(Poland$Year2, cbind(Poland$count, France$count, Russia$count), 
        type="l", 
        las=1, 
        col=c(1,2,3), 
        lty=1)

# Do exist searches related to a country, sent from the same country?
places <- Questions %>%
          left_join(Posts3, by=c("Title"="Title")) %>%
          left_join(Users.travel, by=c("OwnerUserId"="Id"))%>%
          filter(Country!= -1) %>%
          mutate(same_place=stri_detect_fixed(Location, Country))%>%
          filter(same_place==TRUE)%>%
          select(Title, Location, Country, same_place)

sprintf("Procent zapytań o tym samym kraju utworzenia i temacie: %f.", nrow(places)/nrow(Questions)*100)


#############################################################
###### Map of the most searched countries in Travel  ########
#############################################################

# the most searched countries
top <- n.visit %>% 
        mutate(Value=as.numeric(Number)) %>% 
        arrange(desc(Value)) %>% 
        slice(1:40)

# here comes the map
mapDevice('x11')
spdf <- joinCountryData2Map(top, joinCode="NAME", nameJoinColumn="Country")

mapCountryData(spdf, nameColumnToPlot="Value", catMethod="fixedWidth")

###
install.packages("leaflet")
library(leaflet)


length(countries)
if (interactive()) {
  ui <- fluidPage(
    
    theme = shinytheme("lumen"),
    titlePanel("The map of..."),
    
    sliderInput(inputId = "Amount",
                label = "Number of countries",
                min = 1,
                max = 280,
                value = 40),
    
    leafletOutput("map")
  )
  
  server <- function(input, output) {
    
    maps <- reactive({
      d<-input$Amount
      top <- n.visit %>% 
        mutate(Value=as.numeric(Number)) %>% 
        arrange(desc(Value)) %>% 
        slice(1:d)
      spdf <- joinCountryData2Map(top, joinCode="NAME", nameJoinColumn="Country")
      
    })
    
    output$map <- renderLeaflet({
      
      mapDevice('x11')
      mapCountryData(spdf, nameColumnToPlot="Value", catMethod="fixedWidth")
      
    })
  }
  shinyApp(ui, server)
}
###

#############################################################
##### Shiny for the most searched countries in Travel #######
#############################################################

the_most_searched <- function(){
  if (interactive()) {
    ui <- fluidPage(
      
      theme = shinytheme("lumen"),
      titlePanel("The most searched countries"),
      
      selectInput(inputId = "type_year", label = strong("Year"),
                  choices = unique(Posts3$Year2),
                  selected = "2018"),
      sliderInput(inputId = "Amount",
                  label = "Number of countries",
                  min = 1,
                  max = 20,
                  value = 10),
      
      plotOutput("distplot")
    )
    
    server <- function(input, output) {
      
      Posts4<-reactive({
        d <- input$type_year
        l <- input$Amount
        s <- Posts3 %>%
              filter(Country != -1) %>%
              group_by(Country, Year2) %>%
              mutate(count=n()) %>%
              filter(Year2==d) %>%
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
}

the_most_searched()

###########################################################################
##### Shiny for the correlation between Score and average sentiment #######
###########################################################################


a <- sentiment_by(Posts.politics$Body, by = NULL)
b <- as.data.frame(a)
sent <- cbind(Posts.politics, b$ave_sentiment)
names(sent)[22]<-"ave_sentiment"

#Shiny

correlation_score_to_avg_sentiment <- function(){
  if (interactive()) {
    ui <- fluidPage(
      
      theme = shinytheme("lumen"),
      titlePanel("Correlation between Score and average sentiment"),
      
      selectInput(inputId = "type_post", label = strong("Type of posts"),
                  choices = unique(c("Questions", "Answers", "All")),
                  selected = "Questions"),
      
      plotOutput("distplot", height = "700")
    )
    
    server <- function(input, output) {
      
      q<-reactive({
        d<-input$type_post
        if(d=="Questions") k=c(1)
        if(d=="Answers") k=c(2)
        if(d=="All") k=c(1, 2)
        positive<-sent %>%
          arrange(desc(ave_sentiment))%>%
          filter(PostTypeId %in% k) %>%
          select(ave_sentiment,OwnerUserId, Id, PostTypeId, Score, ViewCount, AcceptedAnswerId)
        positive
      })
      output$distplot <- renderPlot({
        
        plot(q()$ave_sentiment, q()$Score,
             col = "#FF007050", border = "white",
             xlab = "Sentiment",
             ylab = "Score",
             main = "Correlation")
      })
    }
    shinyApp(ui, server)
  }
}

correlation_score_to_avg_sentiment()
