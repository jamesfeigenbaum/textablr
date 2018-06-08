#' @title estout lookups
#'
#' @description Helper function to define various lookups
#'
#' @param NULL
#'
#' @importFrom dplyr mutate
#' @import magrittr
#'
#' @keywords internal
#'

# summary stat lookup
# TODO enable custom proper names for summary statistics
sumstat_names <- data.frame(code = c("N", "Ymean", "R2", "aR2", "df", "F"),
                        proper_name = c("Observations", "Y Mean",
                                        "R$^{2}$", "Adjusted R$^{2}$",
                                        "Degrees of Freedom", "F-Statistic"),
                        format = c("%.0f", "%.2f", "%.2f", "%.2f", "%.0f", "%.2f")) %>%
  dplyr::mutate(format = format %>% paste0("\\multicolumn{1}{c}{", ., "}"))

# summary stat lookup for rdrobust
sumstat_names_rdrobust <-
  data.frame(code = c("N", "h_l", "h_r", "b_l", "b_r", "N_h_l", "N_h_r", "c", "p", "q", "kernel", "vce", "bwselect"),
         proper_name = c("Observations", "BW est (h) left", "BW est (h) right",
                         "BW bias (b) left", "BW bias (b) right",
                         "Effective Number of Observations (left)",
                         "Effective Number of Observations (right)",
                         "Cutoff", "Order Estimate (p)", "Order Bias (q)",
                         "Kernel", "VCE", "Bandwidth Selection"),
         format = c("%.0f", "%.3f", "%.3f", "%.3f", "%.3f", "%.0f", "%.0f", "%.2f", "%.0f", "%.0f", "%s", "%s", "%s")) %>%
  dplyr::mutate(format = format %>% paste0("\\multicolumn{1}{c}{", ., "}"))

# indicator levels
indicator_levels <- c("Yes", "No")
# indicator_levels <- c("YES", "NO")
# indicator_levels <- c("Y", "N")
# indicator_levels <- c("Included", "Not Included")

# star levels
star_levels <- c(0.10, 0.05, 0.01)
