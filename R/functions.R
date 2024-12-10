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
                sd = sd
            )
        )) |>
        dplyr::mutate(across(
            tidyselect::where(is.numeric),
            ~ round(.x, digits = 1)
        ))
    return(out)
}
