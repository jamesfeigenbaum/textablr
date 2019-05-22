#' @title Output and Compute Summary Statistics as a LaTeX Table
#'
#' @description Output summary stats results nicely.
#'
#' @param ... columns of summary statistics (means from `textablr_mean_col` and differences from `textablr_diff_col`)
#' @param file output location, if blank only prints to viewer with `gt()`
#'
#' @import magrittr
#' @importFrom dplyr filter select summarize_all mutate group_by ungroup arrange
#' @importFrom tidyr gather spread
#' @importFrom purrr map map_chr transpose
#' @importFrom stringr str_detect str_remove_all
#' @importFrom rlang sym
#' @importFrom broom tidy
#' @importFrom gt gt tab_header cols_align as_latex
#' @import knitr
#'
#' @examples
#'
#' # simple table
#' # means of mtcars for gear 3 and 4 and the difference
#'
#' mtcars_var_labels <- c("MPG" = "mpg", "Displacement" = "disp", "Horse Power (?)" = "hp")
#'
#' col1 <- textablr_mean_col(mtcars, var_labels = mtcars_var_labels,
#'   group_var = "gear", group_condition = "3")
#'
#' col2 <- textablr_mean_col(mtcars, var_labels = mtcars_var_labels,
#'   group_var = "gear", group_condition = "4")
#'
#' col3 <- textablr_diff_col(mtcars, var_labels = mtcars_var_labels,
#'   group_var = "gear", group1 = "3", group2 = "4")
#'
#' textablr_means(col1, col2, col3)
#'
#' @export textablr_means

textablr_means <- function(..., file = "") {

  out_table <-
    bind_cols(...) %>%
    select(label, starts_with("value")) %>%
    gt()

  out_table %>%
    tab_header("Means and Differences") %>%
    cols_align("center") %>%
    print()

  # if file is empty don't cat to a file at all
  if (file != "") {

    out_table %>%
      as_latex() %>%
      str_split("\n") %>%
      unlist() %>%
      .[(str_which(., "midrule") + 1):(str_which(., "bottomrule") - 1)] %>%
      paste0(collapse = "\n") %>%
      cat(file = file)

  }

  return("See the summary table in the viewer")

}
