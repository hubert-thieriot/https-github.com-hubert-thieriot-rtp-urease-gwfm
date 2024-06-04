clean_country <- function(country){
  countrycode::countrycode(country, origin = "country.name", destination = "iso3c") %>%
    countrycode::countrycode(origin = "iso3c", destination = "country.name")
}
