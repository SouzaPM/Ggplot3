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
#' @importFrom dplyr filter mutate
#' @importFrom countrycode countrycode
#' @export
load_data <- function(){
# Load plastic waste data
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-01-26/plastics.csv')

# Add ISO-2 codes
plastics <- plastics %>%
  filter(country != "EMPTY") %>%
  mutate(iso2 = countrycode(country, origin = "country.name", destination = "iso2c"))

iso2_list <- unique(plastics$iso2)
iso2_list <- iso2_list[iso2_list != "TW"]

# Load pre-fetched GDP and population data (from World Bank API)
gdp_data <- readr::read_csv("gdp_data.csv")
population_data <- readr::read_csv("population_data.csv")

# Load CO2 data from local file
co2 <- readr::read_csv("owid-co2-data.csv")
}
