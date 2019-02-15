#' @title Output and Compute Summary Statistics as a LaTeX Table
#'
#' @description Output summary stats results nicely.
#'
#' @param data the data...
#'
#' @import magrittr
#' @importFrom purrr map map_chr transpose
#' @importFrom stringr str_c str_replace_all
#'
#' @examples
#'
#'
#' @export textablr_means

# play with mtcars

# two tasks:
# taking the means of a group
# comparing means across groups

# means

means_column <- function(data, vars, group_var, group_condition){

  vars1 <- vars

  means <-
    data %>%
    filter(!!rlang::sym(group_var) %in% group_condition) %>%
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
    data %>%
    filter(!!rlang::sym(group_var) %in% group_condition) %>%
    nrow()

  means %>% gt()

}

means_column(mtcars, vars = c("MPG" = "mpg", "Displacement" = "disp", "Horsies (?)" = "hp"),
             group_var = "cyl", 4)

# difference in means

diffs_column <- function(data, vars, group_var, group1, group2) {

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

  diffs %>% gt()

}

diffs_column(mtcars, vars, "gear", 3, 6)
