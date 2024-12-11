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
  p <- ggplot2::ggplot(df, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
  return(p)
}


#' Convert column characters to snake case
#'
#' @param data A data.frame/tibble.
#' @param columns Column(s) to convert to snake case
#'
#' @return A data.frame/tibble.
column_values_to_snake_case <- function(data, columns) {
  data |>
    dplyr::mutate(dplyr::across({{ columns }}, snakecase::to_snake_case))
}

#' Function to change lipidomics dataframe from long to wide format
#'
#' @param data A data.frame/tibble containing column `metabolite`
#'
#' @return A data.frame/tibble.
metabolites_to_wider <- function(data) {
  data |> tidyr::pivot_wider(
    names_from = metabolite,
    values_from = value,
    values_fn = mean,
    names_prefix = "metabolite_"
  )
}


#' A transformation recipe for pre-processing
#'
#' @param data lipidomics dataframe
#' @param metabolite_var the column to model
#'
#' @return the recipe
create_recipe_spec <- function(data, metabolite_var) {
  recipes::recipe(data) |>
    recipes::update_role(
      {{ metabolite_var }},
      age,
      gender,
      new_role = "predictor"
    ) |>
    recipes::update_role(
      class,
      new_role = "outcome"
    ) |>
    recipes::step_normalize(
      tidyselect::starts_with("metabolite_")
    )
}

#' Create a workflow object of the model and transformations.
#'
#' @param model_specs The model specs
#' @param recipe_specs The recipe specs
#'
#' @return A workflow object
create_model_workflow <- function(model_specs, recipe_specs) {
  workflows::workflow() |>
    workflows::add_model(model_specs) |>
    workflows::add_recipe(recipe_specs)
}

#' Get model parameters as tibble
#'
#' @param workflow_fitted_model fitted workflow model
#'
#' @return a tibble
tidy_model_output <- function(workflow_fitted_model) {
  workflow_fitted_model |>
    workflows::extract_fit_parsnip() |>
    broom::tidy(exponentiate = TRUE)
}
