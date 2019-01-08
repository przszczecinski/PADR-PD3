# Shift + Alt + K => keyboard shortcuts

setwd("C:/Users/Przemek/Desktop/Przemek/PW/PADR/PD3/2. Prepared Data/Politics")

Users.politics.XML <- xmlParse("Users.xml")
Users.politics <- XML:::xmlAttrsToDataFrame(getNodeSet(Users.politics.XML, path='//row'))

# select the data set
dataset <- Users.politics

tmp <- dataset %>% select(Location)

###################
#### Main body ####
###################
start.time <- Sys.time()
  extended_check_result <- tmp %>% mutate(Country = lapply(Location, what_country_are_you_from_old))
  extended_check_result_fixed <- extended_check_result %>% mutate(Country=lapply(Country, remove_mutliple_countries))
end.time <- Sys.time()
(end.time-start.time)

# percent of recognized Locations
given_Location_count <- dataset %>% filter(!is.na(Location)) %>% summarise(n()) 

result_basic <- tmp %>% mutate(Country=lapply(Location, what_country_are_you_from_basic))

how_many_recognized_basic <- result_basic %>% filter(!is.na(Location) & !is.na(Country)) %>% summarise(n())
how_many_recognized_extended <- extended_check_result_fixed %>% filter(!is.na(Location) & !is.na(Country)) %>% summarise(n())

####################################################################
#### Checking if extended search is better than basic matching  ####
####################################################################

cat(as.numeric(floor(how_many_recognized_extended/how_many_recognized_basic*100)), "%", sep="")

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
  extended_check_result1 <- tmp %>% mutate(Country = lapply(Location, what_country_are_you_from_old)), # %>% filter(lapply(Country, length)>1)
  extended_check_result2 <- tmp %>% mutate(Country = lapply(Location, what_country_are_you_from)), # %>% filter(lapply(Country, length)>1)
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
 
what_country_are_you_from_basic <- function(text){
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

what_country_are_you_from_old <- function(text){
  if(is.character(text)){
    if(is.na(text)==FALSE){
      strvec <- trimws(unlist(strsplit(text, split=",")))
      # if country name or country abbreviation exists in text
      is.country <- c(strvec %in% codelist$country.name.en)
      is.country.abb <- c(strvec %in% codelist$cowc)
      if(all(is.country==FALSE)){
        if(all(is.country.abb==FALSE)){
          # in case comma was not used to separate country
          strvec2 <- trimws(unlist(strsplit(text, split=" ")))
          is.country2 <- c(strvec2 %in% codelist$country.name.en)
          is.country.abb2 <- c(strvec2 %in% codelist$cowc)
          if(all(is.country2==FALSE)){ 
            if(all(is.country.abb2==FALSE)){
              # if country was not found
              # checking if text contains names or abbreviations of USA states (quite common)
              is.usa.state <- c(strvec %in% state.name)
              is.usa.state.abb <- c(strvec %in% state.abb)
              if(all(is.usa.state==FALSE) & all(is.usa.state.abb==FALSE)){
                # checking if text contains city name and can be clearly identified
                is.city <- c(strvec %in% world.cities$name)
                if(all_cpp(is.city, FALSE))
                  return(NA)
                else {
                  temp <- world.cities %>% 
                          filter(name==strvec[is.city]) %>%
                          select(country.etc)
                  is.city.in.how.many.countries <- as.integer(count(temp))
                  if(is.city.in.how.many.countries <= 1){
                    country <- as.character(temp)
                    # because only USA and UK have abbreviations in world.cities
                    if(country=="USA")
                      country <- "United States"
                    else if(country=="UK")
                      country <- "United Kingdom"
                    return(country)
                  }
                  # if found city cannot be clearly identified,
                  # i.e. occurs in more than one country
                  else return(NA)
                }
              }
              else return("United States")
            }
            else {
              country <- as.character(codelist %>%
                                        filter(cowc==strvec2[is.country.abb2==TRUE]) %>%
                                        select(country.name.en))
              return(country)
            }
          }
          else return(strvec2[is.country2==TRUE])
        }
        else {
          country <- as.character(codelist %>%
                                  filter(cowc==strvec[is.country.abb==TRUE]) %>%
                                  select(country.name.en))
          return(country)
        }
      }
      else return(strvec[is.country==TRUE])
    }
    else return(NA)
  }
  else return(NA)
}

what_country_are_you_from <- function(text){
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
              temp <- world.cities %>% 
                      filter(name==strvec[is.city]) %>%
                      select(country.etc)
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

remove_mutliple_countries_old <- function(list_){
  ### work-around function ###
  # remove_mutliple_countries should be used on output from what_country_are_you_from
  # to deal with cases, when mutliple countries were put into one column
  lapply(list_, function(x){if(length(x)>1) return(NA) else return(x)})
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

what_country_are_you_from("San Francisco, United States")
what_country_are_you_from("L'viv, Lviv Oblast Ukraine")
what_country_are_you_from("Atlanta, GA")
what_country_are_you_from("South Carolina")
what_country_are_you_from("Seattle")
what_country_are_you_from("San Francisco")
what_country_are_you_from("POL")
what_country_are_you_from("Jerusalem")
what_country_are_you_from_new("San Francisco, United States Poland")

# Checking how many countries in world.cities has length <=3
my <- world.cities %>% filter(stri_length(country.etc)<=3 & !(country.etc %in% c("USA", "UK")))
my

