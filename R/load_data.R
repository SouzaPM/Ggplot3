#' Load plastic waste and supplementary datasets
#'
#' Downloads plastic waste audit data from the TidyTuesday repository and
#' joins it with locally stored GDP, population, and CO2 datasets. Country
#' ISO-2 codes are appended using \pkg{countrycode}.
#'
#' @return A list is not explicitly returned; the function loads
#'   \code{plastics}, \code{gdp_data}, \code{population_data}, and
#'   \code{co2} into the calling environment as side-effects. Consider
#'   returning a named list for a more functional interface.
#'
#' @importFrom readr read_csv 
#' @importFrom dplyr filter mutate rename
#' @importFrom countrycode countrycode
#' @export

load_data <- function(){

  plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-01-26/plastics.csv')
  
  
  plastics <- plastics |>
    filter(country != "EMPTY") |>
    mutate(iso2 = countrycode(country, origin = "country.name", destination = "iso2c"))
  
  gdp_data <- readr::read_csv(here::here("gdp_data.csv")) |>
    rename("iso2" = "iso2c") 
  population_data <- readr::read_csv(here::here("population_data.csv")) |>
    rename("iso2" = iso2c,
           "population" = value)
  co2 <- readr::read_csv(here::here("owid-co2-data.csv")) |>
    dplyr::mutate(
      iso2 = countrycode::countrycode(iso_code,origin = "iso3c",destination = "iso2c"))
  
  list(
    plastics = plastics,
    gdp_data = gdp_data,
    population_data = population_data,
    co2 = co2
  )
}
