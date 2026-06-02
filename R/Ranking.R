#' Create a table ranking countries based on variable of interest
#' 
#' Compute the numeric values, `rank`, needed to organize table
#' 
#' @param variable Character string giving the variable of interest for ranking
#' @param year_select Year to filter to. Default is 2019
#' 
#' @return a DT table
#' 
#' @importFrom dplyr filter mutate summarise arrange rename min_rank desc group_by
#' @importFrom DT datatable formatRound formatStyle styleColorBar
#' 
#' @export

rank_table <- function(variable, year_select) {
  valid_variables <- c(
    "co2_per_capita",
    "plastic_waste_per_capita",
    "gdp_per_capita",
    "population",
    "grand_total",
    "gdp"
  )
  
  if (!variable %in% valid_variables) {
    stop(
      paste0(
        "'",
        variable,
        "' is not a valid variable. Choose one of: ",
        paste(valid_variables, collapse = ", ")
      )
    )
  }
  
  data <- load_data()
  
  co2_year <- data$co2 |>
    dplyr::filter(year == year_select) |>
    dplyr::select(iso2, co2_per_capita)
  
  meta_set <- data$plastics |>
    dplyr::filter(year == year_select) |>
    dplyr::left_join(data$gdp_data, by = "iso2") |>
    dplyr::left_join(data$population_data, by = "iso2") |>
    dplyr::left_join(co2_year, by = "iso2") 
  
  
  meta_set <- meta_set |>
    mutate(
      gdp_per_capita = gdp_billions * 1e9 / population,
      plastic_waste_per_capita = grand_total / population
    )
  
  ranked_table <- meta_set |> 
    filter(
      year == year_select,
      !is.na(country),
      country != "Taiwan"
    ) |>
    group_by(country) |>
    summarise(
      value = mean(.data[[variable]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(!is.na(value)) |>
    mutate(
      rank = min_rank(desc(value))
    ) |> 
    arrange(rank) |>
    rename(
      Country = country,
      Value = value,
      Rank = rank
    )
  
  ranked_table
}