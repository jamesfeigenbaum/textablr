# lookups

# summary stat lookup
# TODO enable custom proper names for summary statistics
sumstat_names <- tibble(code = c("N", "Ymean", "R2", "aR2", "df", "F"),
                        proper_name = c("Observations", "Y Mean",
                                        "R$^{2}$", "Adjusted R$^{2}$",
                                        "Degrees of Freedom", "F-Statistic"),
                        format = c("%.0f", "%.2f", "%.2f", "%.2f", "%.0f", "%.2f")) %>%
  mutate(format = format %>% paste0("\\multicolumn{1}{c}{", ., "}"))

# indicator levels
indicator_levels <- c("Yes", "No")
# indicator_levels <- c("YES", "NO")
# indicator_levels <- c("Y", "N")
# indicator_levels <- c("Included", "Not Included")

# star levels
star_levels <- c(0.10, 0.05, 0.01)
