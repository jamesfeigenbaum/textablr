#' @title Tex Table Get Tibbles in Clipboard
#'
#' @name textablr_get_parameters
#' @rdname textablr_get_parameters
#'
#' @description Helper function to load the tibbles for defining summary statistics, variable labels, etc
#'     into the clipboard, so they are easy to change.
#'
#' @importFrom clipr write_clip
#'
#' @export textablr_get_sumstat_names
#' @export textablr_get_var_labels
#' @export textablr_get_var_indicates
#'
#' @rdname textablr_get_parameters

# summary statistic names and formats
textablr_get_sumstat_names <- function() {

  'sumstat_names <-
    data.frame(code = c("N", "Ymean", "R2", "aR2", "df", "F", "Ysd", "APF"),
               proper_name = c("Observations", "Y Mean",
                               "R$^{2}$", "Adjusted R$^{2}$",
                               "Degrees of Freedom", "F-Statistic",
                               "Y Std Dev",
                               "Angrist-Pischke F-Stat"),
               format = c("%.0f", "%.2f", "%.2f", "%.2f", "%.0f", "%.2f", "%.2f", "%.2f")) %>%
    dplyr::mutate(format = format %>% paste0("\\multicolumn{1}{c}{", ., "}"))' %>% write_clip()

  print("The sumstat_names definition is now in your clipboard")
}

#' @rdname textablr_get_parameters

# summary statistic names and formats
textablr_get_var_labels <- function() {

  'var_labels <- tibble::tibble(term = c("wt", "hp"), label = c("Weight", "Horsepower"))' %>% write_clip()

  print("A tibble for variable labelling is now in your clipboard")
}

#' @rdname textablr_get_parameters

# summary statistic names and formats
textablr_get_var_indicates <- function() {

  'var_indicates <- tibble::tibble(term = c("am", "cyl"), indicator = c("Transmission FE", "Cylinders FE"))' %>% write_clip()

  print("A tibble for variable indicating is now in your clipboard")
}
