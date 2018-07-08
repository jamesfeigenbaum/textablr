#' @title Tex Table Column Toppers
#'
#' @description Helper function to construct tex table column names
#'
#' @param column_titles names of the regression columns
#' @param column_widths how many columns in each name category?
#' @param copy copy the output to clipboard? makes it easy to paste into tex document
#'
#' @importFrom purrr map2_chr
#' @importFrom magrittr add
#' @importFrom utils head
#' @importFrom clipr write_clip
#'
#' @examples
#'
#' # Suppose you have 6 regressions,
#' # 3 on the male subsample and 3 on the female subsample
#'
#' c_title <- c("Male", "Female")
#' c_widths <- c(3, 3)
#'
#' column_names_tex(column_titles = c_title, column_widths = c_widths)
#'
#' @export column_names_tex


# column names

column_names_tex <- function(column_titles, column_widths, copy = TRUE){

  column_underline_start <- column_widths %>% c(1, .) %>% cumsum() %>% add(1) %>% head(-1)
  column_underline_end <- column_widths %>% cumsum() %>% add(1)

  # titles
  out1 <-
    map2_chr(column_titles, column_widths, ~ sprintf("&\\multicolumn{%d}{c}{%s}", .y, .x)) %>%
    paste0(collapse = "  ") %>%
    sprintf("%s \\\\", .)

  # underlines
  out2 <-
    map2_chr(column_underline_start, column_underline_end, ~ sprintf("\\cmidrule(lr){%d-%d}", .x, .y)) %>%
    paste0(collapse = "  ")

  # might be nice to have in the clipboard to paste into a tex document
  if (copy == TRUE) {

    c(out1, out2) %>%
      write_clip()

  }

  c(out1, out2) %>%
    return()

}
