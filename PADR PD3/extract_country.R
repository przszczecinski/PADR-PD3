
source("libraries.R")

Rcpp::cppFunction('
  bool any_cpp(LogicalVector x, bool logicalValue) {
    int n = x.size();
    for (int i=0; i<n; ++i)
    if (x[i]==logicalValue)
    return TRUE; // early stop, indeks R-owy, nie C-owy
    return FALSE;
  }
')


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
  # if text consists of more than one string (is a vector of strings)
  if(length(text)>1)
    return(NA)
  return(text)
}
