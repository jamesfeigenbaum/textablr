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
sumstat_format_default <-
    data.frame(code = c("nobs", "Ymean", "r.squared", "adj.r.squared", "df", "statistic", "Ysd", "APF", "N_clusters"),
             format = c("%.0f", "%.2f", "%.2f", "%.2f", "%.0f", "%.2f", "%.2f", "%.2f", "%.0f"),
             proper_name = c("Observations", "Y Mean",
                             "R$^{2}$", "Adjusted R$^{2}$",
                             "Degrees of Freedom", "F-Statistic",
                             "Y Std Dev",
                             "Angrist-Pischke F-Stat",
                             "Number of Clusters")) %>%
  dplyr::mutate(format = format %>% paste0("\\multicolumn{1}{c}{", ., "}"))

## DEFAULTS

#' @rdname textablr_lookups

# which summary stats to include if none are specified
sumstat_include_default <- c("nobs", "adj.r.squared", "Ymean")

#' @rdname textablr_lookups

# indicator levels
indicator_levels <- c("Yes", "No")
# indicator_levels <- c("YES", "NO")
# indicator_levels <- c("Y", "N")
# indicator_levels <- c("Included", "Not Included")

#' @rdname textablr_lookups

# star levels
star_level_default <- c(0.10, 0.05, 0.01)
