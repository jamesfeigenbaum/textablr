#' @title Helper Function for Summary Statistics as a LaTeX Table
#'
#' @description Generate a column of differences and standard errors
#'
#' @param data the data
#' @param var_labels named vector of variable labels.
#'     This is required
#'     For example, to get summaries of the `educ` variable `Education` and the `expr` variable `Experience`
#'     make this `c("Education" = "educ", "Experience" = "expr")`
#'     Note: this also sets the order of variables in the table
#' @param group_var the variable (if any) to group on
#' @param group1 the first group, the difference will be group1 - group2
#' @param group2 the second group, the difference will be group1 - group2
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
#' textablr_diff_col(mtcars, var_labels = mtcars_var_labels, "gear", 3, 5)
#'
#' @export textablr_diff_col

textablr_diff_col <- function(data, var_labels, group_var, group1, group2) {

  # prep data
  data_1 <-
    data %>%
    filter(!!rlang::sym(group_var) %in% group1 |
             !!rlang::sym(group_var) %in% group2) %>%
    mutate(dummy = (!!rlang::sym(group_var) %in% group1)) %>%
    select(dummy, var_labels)

  # TODO check that there are two groups

  diffs <-
    data_1 %>%
    map(~lm(.x ~ dummy, data = data_1)) %>%
    map_dfr(tidy, .id = "reg") %>%
    filter(reg != "dummy" & term != "(Intercept)") %>%
    select(reg, estimate, se = std.error) %>%
    gather(key = "key", value = "value", -reg) %>%
    group_by(key) %>%
    mutate(order = row_number()) %>%
    arrange(order) %>%
    # TODO stars?
    # surround SEs with []s
    mutate(value = case_when(key == "estimate" ~ sprintf("%.2f", value),
                             key == "se" ~ sprintf("[%.2f]", value))) %>%
    # get rid of labels for SDs
    mutate(reg = case_when(key == "estimate" ~ reg,
                           key == "se" ~ "")) %>%
    ungroup() %>%
    select(label = reg, value)

  n <-
    data_1 %>%
    nrow() %>%
    sprintf("%d", .) %>%
    as_tibble() %>%
    mutate(label = "N")

  diffs %>% bind_rows(n) %>% return()

}
