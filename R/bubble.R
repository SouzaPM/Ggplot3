#' Create a bubble plot of environmental indicators
#'
#' Generates a bubble plot comparing two selected variables with optional transformation.
#'
#' @param data Output from load_data() (list of datasets)
#' @param x Character string for x variable
#' @param y Character string for y variable
#' @param size Character string for bubble size variable (default population)
#' @param transform Transformation to apply: "none", "squared", "log", "reciprocal"
#'
#' @return A ggplot object
#'
#' @importFrom dplyr mutate filter select group_by slice_max left_join
#' @importFrom ggplot2 ggplot aes geom_point labs
#' @export

bubble <- function(data, x, y, size = "population", transform = "none") {
  
  plastics <- data$plastics
  gdp <- data$gdp_data
  pop <- data$population_data
  co2 <- data$co2
  
  co2_year <- co2 |>
    dplyr::filter(year == 2019) |>
    dplyr::select(iso2, co2_per_capita)
  
  gdp_latest <- gdp |>
    dplyr::group_by(iso2) |>
    dplyr::slice_max(date, n = 1)
  
  pop_latest <- pop |>
    dplyr::group_by(iso2) |>
    dplyr::slice_max(date, n = 1)
  
  full_data <- plastics |>
    dplyr::left_join(gdp_latest, by = "iso2") |>
    dplyr::left_join(pop_latest, by = "iso2") |>
    dplyr::left_join(co2_year, by = "iso2") |>
    dplyr::mutate(
      gdp_per_capita = gdp_billions * 1e9 / population,
      plastic_waste_per_capita = grand_total / population
    )
  
  apply_transform <- function(v) {
    if (transform == "squared") return(v^2)
    if (transform == "log") return(log(v))
    if (transform == "reciprocal") return(1 / v)
    v
  }
  
  plot_data <- full_data |>
    dplyr::mutate(
      x_plot = apply_transform(.data[[x]]),
      y_plot = apply_transform(.data[[y]])
    )
  
  ggplot2::ggplot(plot_data, ggplot2::aes(
    x = x_plot,
    y = y_plot,
    size = .data[[size]]
  )) +
    ggplot2::geom_point(alpha = 0.65) +
    ggplot2::labs(
      x = x,
      y = y,
      title = "Bubble Plot of Country Indicators"
    )
}