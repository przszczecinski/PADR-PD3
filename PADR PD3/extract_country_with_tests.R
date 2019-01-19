
source("extract_country.R")

setwd("2. Prepared Data/Chrisitianity")

Users.christianity.XML <- xmlParse("Users.xml")
Users.christianity <- XML:::xmlAttrsToDataFrame(getNodeSet(Users.christianity.XML, path='//row'))

setwd('..')
setwd('..')

# select the data set
dataset <- Users.christianity

tmp <- dataset %>% select(Location)

###################
#### Main body ####
###################
start.time <- Sys.time()
  extended_check_result <- tmp %>% mutate(Country = lapply(Location, extract_country))
  extended_check_result_fixed <- extended_check_result %>% mutate(Country=lapply(Country, remove_mutliple_countries))
end.time <- Sys.time()
(end.time-start.time)

# percent of recognized Locations
given_Location_count <- dataset %>% filter(!is.na(Location)) %>% summarise(n()) 

result_basic <- tmp %>% mutate(Country=lapply(Location, extract_country_basic))

how_many_recognized_basic <- result_basic %>% filter(!is.na(Location) & !is.na(Country)) %>% summarise(n())
how_many_recognized_extended <- extended_check_result_fixed %>% filter(!is.na(Location) & !is.na(Country)) %>% summarise(n())

####################################################################
#### Checking if extended search is better than basic matching  ####
####################################################################

cat("Countries recognized using extended search to basic search ratio: ", 
    as.numeric(floor(how_many_recognized_extended/how_many_recognized_basic*100)), 
    "%", 
    sep="")

cat("Percent of recognized countries, as Location was given, using only Country matching: ", 
    as.numeric(floor(how_many_recognized_basic/given_Location_count*100)), 
    "%", 
    sep="")
cat("Percent of recognized countries, as Location was given, using extended matching: ",
    as.numeric(floor(how_many_recognized_extended/given_Location_count*100)), 
    "%", 
    sep="")

###################
###################
###################

# checking if there are no vectors in Country column
(doubles <- extended_check_result1_fixed %>% filter(lapply(Country, length)>1))
(doubles2 <- extended_check_result2_fixed  %>% filter(lapply(Country, length)>1))

# comparison between old and new version of extended searching
summary(microbenchmark(
  extended_check_result1 <- tmp %>% mutate(Country = lapply(Location, extract_country_old)), # %>% filter(lapply(Country, length)>1)
  extended_check_result2 <- tmp %>% mutate(Country = lapply(Location, extract_country)), # %>% filter(lapply(Country, length)>1)
  times = 1L
))

extended_check_result1_fixed <- extended_check_result1 %>% mutate(Country=lapply(Country, remove_mutliple_countries))
extended_check_result2_fixed <- extended_check_result2 %>% mutate(Country=lapply(Country, remove_mutliple_countries))

# amount of na's in original data
(original_Location_na_count <- dataset %>% filter(is.na(Location)) %>% summarise(n()))

# amount of not recognized Locations
(extended_check_result1_fixed %>% filter(!is.na(Location) & is.na(Country)) %>% summarise(n()))
(extended_check_result2_fixed %>% filter(!is.na(Location) & is.na(Country)) %>% summarise(n()))


###################
#### Functions ####
###################
 
extract_country_basic <- function(text){
  if(is.na(text))
    return(NA)
  else{
    countries <- codelist$country.name.en
    is.country <- stri_detect_fixed(text, countries, case_insensitive=TRUE)
    if(all(is.country==FALSE))
      return(NA)
    else if(sum(is.country) > 1)
      return(NA)
    return(countries[is.country]) 
  }
}

extract_country <- function(text){
  countries <- codelist$country.name.en
  countries.abb <- codelist$cowc
  
  if(is.character(text)){
    # if text is not empty
    if(is.na(text)==FALSE){
      #is.country <- stri_detect_fixed(text, countries, case_insensitive=FALSE)
      strvec <- trimws(unlist(strsplit(text, split = ",")))
      is.country <- c(strvec %in% countries)
      # if country name was found 
      if(any(is.country==TRUE)){
        # if more than one country name was found
        if(any(is.country) > 1)
          return(NA)
        # else return found country
        else return(strvec[is.country])
      }
      # if no country name was found
      else {
        is.country.abb <- c(strvec %in% countries.abb)
        # if country name abbreviation was found
        if(any(is.country.abb==TRUE)){
          # if more than one country abb was found
          if(sum(is.country) > 1)
            return(NA)
          # else return country with found abb
          else {
            country <- as.character(codelist %>% 
                                    filter(cowc==strvec[is.country.abb==TRUE]) %>% 
                                    select(country.name.en))  
            return(country)
          }
        }
        # if UK was found in text (abb in countries.abb is UKG)
        else if("UK" %in% strvec)
          return("United Kingdom")
        # if no country abb was found
        else {
          is.usa.state <- c(strvec %in% state.name)
          is.usa.state.abb <- c(strvec %in% state.abb)
          # if USA state name was found
          if(any(is.usa.state==TRUE)){
            return("United States")
          }
          # if USA state abb was found
          else if(any(is.usa.state.abb==TRUE)){
            return("United States")
          }
          # if no USA state name or abb was found
          else{
            # checking if text contains city name and can be clearly identified
            is.city <- c(strvec %in% world.cities$name)
            # if city name was found 
            if(any_cpp(is.city, TRUE)){
              suppressWarnings(
              temp <- world.cities %>%
                      filter(name==strvec[is.city]) %>%
                      select(country.etc)
              )
              is.city.in.how.many.countries <- as.integer(count(temp))
              # if there is only one city with specified name
              if(is.city.in.how.many.countries == 1){
                country <- as.character(temp)
                # world.cities has USA and UK, not United States and United Kingdom
                if(country=="USA")
                  country <- "United States"
                else if(country=="UK")
                  country <- "United Kingdom"
                return(country)
              }
            }
          }
        }
      }
    }
  }
  return(NA)
}

remove_mutliple_countries <- function(text){
  if(length(text)>1)
    return(NA)
  return(text)
}

Rcpp::cppFunction('
bool all_cpp(LogicalVector x, bool logicalValue) {
  int n = x.size();
  for (int i=0; i<n; ++i)
      if (x[i]!=logicalValue)
          return FALSE; // early stop, indeks R-owy, nie C-owy
  return TRUE;
}
')

Rcpp::cppFunction('
bool any_cpp(LogicalVector x, bool logicalValue) {
  int n = x.size();
  for (int i=0; i<n; ++i)
    if (x[i]==logicalValue)
      return TRUE; // early stop, indeks R-owy, nie C-owy
  return FALSE;
}
')

# is any_cpp faster than R function

text1 <- "San Francisco, United States"
text2 <- "USA"
text3 <- "San Francisco, Wroclaw, Jerusalem"

is.city1 <- stri_detect_fixed(text1, world.cities$name, case_insensitive=TRUE)
is.city2 <- stri_detect_fixed(text2, world.cities$name, case_insensitive=TRUE)
is.city3 <- stri_detect_fixed(text3, world.cities$name, case_insensitive=TRUE)

summary(microbenchmark(
  any(is.city1==TRUE),
  any_cpp(is.city1, TRUE),
  any(is.city2==TRUE),
  any_cpp(is.city2, TRUE),
  any(is.city3==TRUE),
  any_cpp(is.city3, TRUE)
))

##################
#### Examples ####
##################

extract_country("San Francisco, United States")
extract_country("L'viv, Lviv Oblast Ukraine")
extract_country("Atlanta, GA")
extract_country("South Carolina")
extract_country("Seattle")
extract_country("San Francisco")
extract_country("POL")
extract_country("Jerusalem")
extract_country_new("San Francisco, United States Poland")

# Checking how many countries in world.cities has length <=3
my <- world.cities %>% filter(stri_length(country.etc)<=3 & !(country.etc %in% c("USA", "UK")))
my

