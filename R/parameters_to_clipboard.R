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
#' @export textablr_get_sumstat_format
#' @export textablr_get_var_indicates
#' @export textablr_get_cluster_labels
#'
#' @rdname textablr_get_parameters

# summary statistic names and formats
textablr_get_sumstat_format <- function() {

  'sumstat_format <-
    data.frame(code = c("nobs", "Ymean", "r.squared", "adj.r.squared", "df", "statistic", "Ysd", "APF"),
               proper_name = c("Observations", "Y Mean",
                               "R$^{2}$", "Adjusted R$^{2}$",
                               "Degrees of Freedom", "F-Statistic",
                               "Y Std Dev",
                               "Angrist-Pischke F-Stat"),
               format = c("%.0f", "%.2f", "%.2f", "%.2f", "%.0f", "%.2f", "%.2f", "%.2f")) %>%
    dplyr::mutate(format = format %>% paste0("\\\\multicolumn{1}{c}{", ., "}"))' %>% write_clip(allow_non_interactive = TRUE)

  print("The sumstat_format definition is now in your clipboard. Don't change the code variable!")
}

#' @rdname textablr_get_parameters

# summary statistic names and formats
textablr_get_var_indicates <- function() {

  'var_indicates <- tibble::tibble(term = c("am", "cyl"), indicator = c("Transmission FE", "Cylinders FE"))' %>% write_clip(allow_non_interactive = TRUE)

  print("A tibble for variable indicating is now in your clipboard")
}

#' @rdname textablr_get_parameters

# cluster SE names
textablr_get_cluster_labels <- function() {

  'cluster_labels <- tibble::tibble(term = c("state", "year"),
                          proper_name = c("Clusters(State)", "Clusters(Year)")) %>%
    dplyr::mutate(term = paste0("cluster_name_", term))' %>% write_clip(allow_non_interactive = TRUE)

  print("A tibble for cluster labelling is now in your clipboard")

}
