#' @title Helper Function for Summary Statistics as a LaTeX Table
#'
#' @description Generate a column of means and standard deviations
#'
#' @param data the data
#' @param var_labels named vector of variable labels.
#'     This is required
#'     For example, to get summaries of the `educ` variable `Education` and the `expr` variable `Experience`
#'     make this `c("Education" = "educ", "Experience" = "expr")`
#'     Note: this also sets the order of variables in the table
#' @param group_var the variable (if any) to group on
#' @param group_condition the filter on the group_var
#'
#' @import magrittr
#' @importFrom dplyr filter select summarize_all mutate group_by ungroup arrange
#' @importFrom tidyr gather spread
#' @importFrom purrr map map_chr transpose
#' @importFrom stringr str_detect str_remove_all
#' @importFrom rlang sym
#' @importFrom broom tidy
#'
#' @examples
#'
#' mtcars_var_labels <- c("MPG" = "mpg", "Displacement" = "disp", "Horse Power (?)" = "hp")
#' textablr_mean_col(mtcars, var_labels = mtcars_var_labels, group_var = "cyl", group_condition = 4)
#'
#' @export textablr_mean_col

textablr_mean_col <- function(data, var_labels, group_var = NULL, group_condition = NULL){

  vars1 <- var_labels

  if (is.null(group_var)) {
    data_filtered <- data
  } else {
    data_filtered <- data %>% filter(!!rlang::sym(group_var) %in% group_condition)
  }

  means <-
    data_filtered %>%
    select(vars1) %>%
    summarize_all(c("mean", "sd")) %>%
    gather() %>%
    mutate(mean = key %>% str_detect("_mean$"),
           key = key %>% str_remove_all("_mean$") %>% str_remove_all("_sd$")) %>%
    group_by(mean) %>%
    mutate(order = row_number()) %>%
    arrange(order) %>%
    # surround SDs with ()s
    mutate(value = case_when(mean == TRUE ~ sprintf("%.2f", value),
                             mean == FALSE ~ sprintf("(%.2f)", value))) %>%
    # get rid of labels for SDs
    mutate(key = case_when(mean == TRUE ~ key,
                           mean == FALSE ~ "")) %>%
    ungroup() %>%
    select(label = key, value)

  n <-
    data_filtered %>%
    nrow() %>%
    sprintf("%d", .) %>%
    as_tibble() %>%
    mutate(label = "N")

  means %>% bind_rows(n) %>% return()

}
