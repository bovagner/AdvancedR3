#' Summary statistics for metabolites
#'
#' @param df A data.frame/tibble.
#'
#' @return A data.frame/tibble.
descriptive_stats <- function(df) {
  out <- df |>
    dplyr::group_by(metabolite) |>
    dplyr::summarise(across(
      value,
      list(
        mean = mean,
        sd = sd,
        median = median,
        iqr = IQR
      )
    )) |>
    dplyr::mutate(across(
      tidyselect::where(is.numeric),
      ~ round(.x, digits = 1)
    ))
  return(out)
}

#' Plot distribution of each metabolite
#'
#' @param df A data.frame/tibble.
#'
#' @return A ggplot2 plot object
plot_distributions <- function(df) {
  p <- ggplot2::ggplot(df, aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(vars(metabolite), scales = "free")
  return(p)
}
