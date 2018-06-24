#' @title A few simple lookup vectors and tibbles to help with regression output
#' @rdname textablr_lookups
#'
#' @description Helper function to define various lookups. If you are curious what is in these lookups or defaults, just use
#'     `textablr:::LOOKUP` to see. Eventually, I'll learn how to put the output in this help file...
#'
#' @param NULL
#'
#' @importFrom dplyr mutate
#' @import magrittr
#'
#' @keywords internal

## LOOKUPS

#' @rdname textablr_lookups

# summary stat lookup
# TODO enable custom proper names for summary statistics
sumstat_names <- data.frame(code = c("N", "Ymean", "R2", "aR2", "df", "F", "Ysd", "APF"),
                        proper_name = c("Observations", "Y Mean",
                                        "R$^{2}$", "Adjusted R$^{2}$",
                                        "Degrees of Freedom", "F-Statistic",
                                        "Y Std Dev",
                                        "Angrist-Pischke F-Stat"),
                        format = c("%.0f", "%.2f", "%.2f", "%.2f", "%.0f", "%.2f", "%.2f", "%.2f")) %>%
  dplyr::mutate(format = format %>% paste0("\\multicolumn{1}{c}{", ., "}"))

#' @rdname textablr_lookups

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

## DEFAULTS

#' @rdname textablr_lookups

# which summary stats to include if none are specified
sumstat_include_default <- c("N", "aR2", "Ymean")

#' @rdname textablr_lookups

# indicator levels
indicator_levels <- c("Yes", "No")
# indicator_levels <- c("YES", "NO")
# indicator_levels <- c("Y", "N")
# indicator_levels <- c("Included", "Not Included")

#' @rdname textablr_lookups

# star levels
star_level_default <- c(0.10, 0.05, 0.01)

## UNDEFINED GLOBAL VARIABLES

# this isn't pretty but it should deal with that annoying note
# idea from https://github.com/Rdatatable/data.table/issues/850
. <- NULL
adj.r.squared <- NULL
beta_se  <- NULL
code  <- NULL
estimate  <- NULL
estimate_star  <- NULL
estimate_string <- NULL
felm_fe  <- NULL
indicator <- NULL
label <- NULL
omit <- NULL
order1 <- NULL
order2 <- NULL
p.value <- NULL
proper_name <- NULL
r.squared <- NULL
reg_number <- NULL
sd <- NULL
se <- NULL
sig_stars <- NULL
std.error <- NULL
term  <- NULL
value <- NULL
var <- NULL
