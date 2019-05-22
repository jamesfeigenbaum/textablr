#' @title Output and Compute Summary Statistics as a LaTeX Table
#'
#' @description Output summary stats results nicely.
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
#'
#' @export textablr_mean_col
#' @export textablr_diff_col
#' @export textablr_means

# means

textablr_mean_col <- function(data, vars, group_var = NULL, group_condition = NULL){

  vars1 <- vars

  if (is.null(group_var)) {
    data_filtered <- data
  } else {
    data_filtered <- filter(!!rlang::sym(group_var) %in% group_condition)
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

textablr_mean_col(mtcars, vars = c("MPG" = "mpg", "Displacement" = "disp", "Horsies (?)" = "hp"),
                  group_var = "cyl", 4)

# difference in means

textablr_diff_col <- function(data, vars, group_var, group1, group2) {

  # prep data
  data_1 <-
    data %>%
    filter(!!rlang::sym(group_var) %in% group1 |
             !!rlang::sym(group_var) %in% group2) %>%
    mutate(dummy = (!!rlang::sym(group_var) %in% group1)) %>%
    select(dummy, vars)

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

textablr_diff_col(mtcars, vars = c("MPG" = "mpg", "Displacement" = "disp", "Horsies (?)" = "hp"),
                  "gear", 3, 5)


# simple table
# means of mtcars for gear 3 and 4 and the difference

col1 <-
  textablr_mean_col(mtcars, vars = c("MPG" = "mpg", "Displacement" = "disp", "Horsies (?)" = "hp"), group_var = "gear", group_condition = "3")

col2 <-
  textablr_mean_col(mtcars, vars = c("MPG" = "mpg", "Displacement" = "disp", "Horsies (?)" = "hp"), group_var = "gear", group_condition = "4")

col3 <-
  textablr_diff_col(mtcars, vars = c("MPG" = "mpg", "Displacement" = "disp", "Horsies (?)" = "hp"), group_var = "gear", group1 = "3", group2 = "4")

textablr_means <- function(..., file = "") {

  temp <-
    bind_cols(...) %>%
    select(label, starts_with("value")) %>%
    gt()

  temp %>%
    tab_header("Means and Differences") %>%
    cols_align("center") %>%
    print()

  out_table <-
    temp %>%
    as_latex() %>%
    str_split("\n") %>%
    unlist() %>%
    .[(str_which(., "midrule")+1):(str_which(., "bottomrule")-1)]

  # if file is empty don't cat to a file at all
  if (file != "") {

    out_table %>%
      # if the following row has a midrule, remove the previous addlinespace
      paste0(collapse = "\n") %>%
      # str_replace_all("\\\\addlinespace(\n.+midrule)", "\\1") %>%
      cat(file = file)

  }


}

textablr_means(col1, col2, col3, file = "latex_testing/means_table.tex")
