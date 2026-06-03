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
  
  data <- data |>
    dplyr::filter(year.x == year_select)
  
  ranked_table <- data |> 
    filter(
      !is.na(country.x),
      country.x != "Taiwan"
    ) |>
    group_by(country.x) |>
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
      Country = country.x,
      Value = value,
      Rank = rank
    )
  
  ranked_table
}