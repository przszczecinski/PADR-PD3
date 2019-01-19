
source("libraries.R")
source("extract_country.R")

start.time <- Sys.time()

options(stringsAsFactors = FALSE)

setwd("Original Data/Christianity")

message("Getting data for Christianity...")
# getting data for Christianity
Posts.christianity.XML <- xmlParse("Posts.xml")
Posts.christianity <- XML:::xmlAttrsToDataFrame(getNodeSet(Posts.christianity.XML, path='//row'))
Users.christianity.XML <- xmlParse("Users.xml")
Users.christianity <- XML:::xmlAttrsToDataFrame(getNodeSet(Users.christianity.XML, path='//row'))

setwd('..')
setwd('..')

setwd("Original Data/Travel")

message("Getting data for Travel...")
# getting data for Travel
Posts.travel.XML <- xmlParse("Posts.xml")
Posts.travel <- XML:::xmlAttrsToDataFrame(getNodeSet(Posts.travel.XML, path='//row'))
Users.travel.XML <- xmlParse("Users.xml")
Users.travel <- XML:::xmlAttrsToDataFrame(getNodeSet(Users.travel.XML, path='//row'))

setwd('..')
setwd('..')

setwd("Original Data/Islam")

message("Getting data for Islam...")
# getting data for Islam
Users.islam.XML <- xmlParse("Users.xml")
Users.islam <- XML:::xmlAttrsToDataFrame(getNodeSet(Users.islam.XML, path='//row'))
Posts.islam.XML <- xmlParse("Posts.xml")
Posts.islam <- XML:::xmlAttrsToDataFrame(getNodeSet(Posts.islam.XML, path='//row'))

setwd('..')
setwd('..')

setwd("Original Data/Politics")

message("Getting data for Politics...")
# getting data for Politics
Posts.politics.XML <- xmlParse("Posts.xml")
Posts.politics <- XML:::xmlAttrsToDataFrame(getNodeSet(Posts.politics.XML, path='//row'))

setwd('..')

country_centroids <- as.data.frame(read.csv("country_centroids.csv", 
                                            stringsAsFactors = FALSE))
setwd('..')
setwd("Prepared Data")

########################
##### Christianity #####
########################
message("Preprocessing data for Christianity, analysis 1 ...")


p <- Posts.christianity %>% 
  mutate(Year=substr(CreationDate, 1, 4)) %>% 
  group_by(Year) %>% 
  select(PostTypeId, CreationDate, Year)

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
mydata <- cbind(mydata, maxdate, mindate)

write.csv(mydata, "christianity_barplot_data.csv", row.names = FALSE)

###########################################
message("Preprocessing data for Christianity, analysis 2 ...")

users <- Users.christianity
posts <- Posts.christianity

tmp <- users %>% 
  mutate(Country = lapply(Location, extract_country)) %>% 
  mutate(Country = lapply(Country, remove_mutliple_countries)) %>% 
  mutate(Country = as.character(Country))

df <- posts %>% 
  left_join(tmp, by=c("OwnerUserId"="Id")) %>% 
  select(ViewCount, Id, Location, Country) %>%
  group_by(Country) %>%
  summarise(Count=n()) %>% 
  filter(Country!="NA" & !is.na(Country)) %>% # throwing out some trash
  left_join(country_centroids, by=c("Country"="Country"))

write.csv(df, "christianity_map_data.csv", row.names = FALSE)

##################
##### Travel #####
##################
message("Preprocessing data for Travel, analysis 1 ...")

# list of all countries
countries <- codelist$country.name.en

# only questions
Questions <- Posts.travel %>%
              filter(PostTypeId==1) %>% 
              select(Title, CreationDate, OwnerUserId)

# Function visit returns matrix containing number of search results for each country and 
# vector Title2: <country_name>,if country_name appears in Title.
visit <- function(){
  k <- 1
  l <- length(countries)
  X <- matrix(0, nrow=l, ncol=2)
  Title2 <- rep(-1, length(Questions$Title))
  c.rows <- nrow(countries)
  message("Searching for countries, this may take a few minutes...")
  for(i in countries)
  {
    is.visited <- stri_detect_fixed(as.factor(Questions$Title), i, case_insensitive=TRUE)
    Title2[is.visited==TRUE] <- i
    n<-length(na.omit(is.visited[is.visited==TRUE]))
    X[k, 1] <- i
    X[k, 2] <- n
    k <- (k + 1)
    progress(k*0.357) #scaling; 0.357=100/280, where 280 - number of countries
    # if(k %% 2) 
    #   message(cat("Percent of countries searched: ", floor(k/280*100), "%", sep=""))
  }
  message("Search completed.")
  list(X, Title2)
}

# data.frame [country, amount of posts related to it]
Visit <- visit()

n.visit <- as.data.frame(Visit[1])
Country <- as.data.frame(Visit[2])

names(n.visit)[c(1, 2)] <- c("Country", "Number")
names(Country) <- "Country"

# changes in time
Questions2 <- Questions %>%
                mutate(Year = format(as.Date(CreationDate, format="%Y"), "%Y")) %>%
                select(Title, Year)

Posts3 <- cbind(Questions2, Country)

write.csv(Posts3, "travel_1_data.csv", row.names = FALSE)

##############################################################
message("Preprocessing data for Travel, analysis 2 ...")

# the most searched countries
top <- n.visit %>% 
        mutate(Value=as.numeric(Number)) %>% 
        arrange(desc(Value))

write.csv(top, "travel_most_searched_data.csv", row.names = FALSE)


####################
##### Politics #####
####################
message("Preprocessing data for Politics, analysis 1 ...")

# data <- Posts.politics %>% select(Body)
# n <- 100
# nrow(data)
# test.data <- split(data, rep(1:(nrow(data)/n + nrow(data)%%n), each = n))
# m <- length(test.data)
# 
# x <- list()
# k <- 1
# for(i in test.data){
#   x[[k]] <- sentiment_by(get_sentences(test.data[[k]]$Body))
#   progress(k*100/m)
#   k <- (k+1)
# }
message("Starting sentiment analysis on Posts.politics Body, this may take a few minutes...")
a <- sentiment_by(Posts.politics$Body)
message("Sentiment analysis finished")

b <- as.data.frame(a) %>% select(ave_sentiment)

pol <- Posts.politics %>% 
        select(Score, PostTypeId) %>% 
        mutate(Score = as.numeric(as.character(Score))) 

sent <- cbind(pol, b)

write.csv(sent, "politics_sentiment_data.csv", row.names = FALSE)


##################################################################

# Analiza emocji

pol1 <- Posts.politics %>% 
          select(Score, PostTypeId, Title) %>% 
          mutate(Score = as.numeric(as.character(Score)))

message("Starting sentiment analysis on Posts.politics Title, this may take a few minutes...")
a1 <- sentiment_by(pol1$Title)
message("Sentiment analysis finished")

b1 <- as.data.frame(a1)
sent1 <- cbind(pol1, b1$ave_sentiment)
names(sent1)[4] <- "ave_sentiment"

positive1 <- sent1 %>%
              filter(PostTypeId==1) %>%
              select(ave_sentiment, Score, Title)

write.csv(positive1, "politics_sentiment_2_data.csv", row.names = FALSE)

#################
##### Islam #####
#################

message("Preprocessing data for Islam, analysis 1 ...")

p <- Posts.islam %>% 
  mutate(Year=substr(CreationDate, 1, 4)) %>% 
  group_by(Year) %>% 
  select(PostTypeId, CreationDate, Year)

p.question <- p %>% 
  filter(PostTypeId==1) %>% 
  summarise(Count=n())

p.answer <- p %>% 
  filter(PostTypeId==2) %>%
  summarise(Count=n())

dates <- Posts.islam %>% arrange(CreationDate) %>% select(CreationDate)
# first post date
mindate <- substr(head(dates, 1)[[1]], 1, 10) 
# last post date
maxdate <- substr(tail(dates, 1)[[1]], 1, 10)  

mydata <- merge(p.question, p.answer, by = 'Year', suffixes = c(".question", ".answer"))
mydata <- cbind(mydata, maxdate, mindate)

write.csv(mydata, "islam_barplot_data.csv", row.names = FALSE)

#############################################

users <- Users.islam
posts <- Posts.islam

tmp <- users %>% 
  mutate(Country = lapply(Location, extract_country)) %>% 
  mutate(Country = lapply(Country, remove_mutliple_countries)) %>% 
  mutate(Country = as.character(Country)) %>% 
  select(Id, Country, Reputation, Views, UpVotes, DownVotes) %>% 
  mutate(Reputation = as.numeric(Reputation),
         Views = as.numeric(Views),
         UpVotes = as.numeric(UpVotes),
         DownVotes = as.numeric(DownVotes))

df <- posts %>% 
  filter(PostTypeId %in% c(1,2)) %>%  # only questions (1) and answers (2)
  mutate(Year = as.numeric(substr(CreationDate,1,4)),
         ViewCount = as.numeric(ViewCount),
         Score = as.numeric(Score)) %>% 
  select(OwnerUserId, PostTypeId, Year, ViewCount, Score) %>% 
  left_join(tmp, by=c("OwnerUserId"="Id")) %>%
  # group_by(Country, PostTypeId, Year) %>%
  # summarise(Count=n(), 
  #           Avg_Reputation = floor(mean(Reputation, na.rm = TRUE)),
  #           Avg_ViewCount = floor(mean(ViewCount, na.rm = TRUE)),
  #           Avg_UpVotes = floor(mean(UpVotes, na.rm = TRUE)),
  #           Avg_DownVotes = floor(mean(DownVotes, na.rm = TRUE))) %>% 
  filter(Country != "NA" & !is.na(Country)) %>% # throwing out some trash
  left_join(country_centroids, by=c("Country"="Country"))

write.csv(df, "islam_data.csv", row.names = FALSE)

setwd('..')

finish.time <- Sys.time()
(finish.time-start.time)

