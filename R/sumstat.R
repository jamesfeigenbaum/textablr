#' @title Gather Summary Statistics from Regression Results for Output
#'
#' @description Helper function to construct summary statistics
#'
#' @param regs stored regression output in a list
#' @param sumstat_include vector of summary statistics to include
#' @param sumstat_names tibble from the lookup with summary stats and proper names
#'
#' @importFrom purrr map_dfr map_int map_dbl map2
#' @importFrom broom glance
#' @importFrom dplyr pull filter arrange
#' @keywords internal

sumstat_master <- function(regs, sumstat_include = "N", sumstat_names) {

  # summary statistics
  # all with the prefix sumstat_
  # N, R2, aR2, df, Fstat, Ymean

  # put each of these inside an if statement
  # only calculate sumstat_XXX if we want XXX
  # this is good for speed
  # and because some summary stats don't always exist for all regression types

  # save speed by summarizing now
  regs_summary <- regs %>% map(summary)
  regs_glance <- regs %>% map_dfr(glance)

  y_vector <- regs %>%
    map(lfe:::model.frame.felm) %>%
    map(extract2, 1)

  sumstat_storage <- data.frame(reg_number = 1:dim(regs_glance)[1])

  if ("N" %in% sumstat_include) {
    # nobs doesn't work for felm
    # sumstat_N <- map_int(regs, nobs)
    sumstat_storage <- y_vector %>%
      map_int(length) %>%
      tibble("N" = .) %>%
      bind_cols(sumstat_storage, .)
  }

  if ("R2" %in% sumstat_include) {
    sumstat_storage <- regs_glance %>%
      pull(r.squared) %>%
      tibble("R2" = .) %>%
      bind_cols(sumstat_storage, .)
  }

  if ("aR2" %in% sumstat_include) {
    sumstat_storage <- regs_glance %>%
      pull(adj.r.squared) %>%
      tibble("aR2" = .) %>%
      bind_cols(sumstat_storage, .)
  }

  if ("df" %in% sumstat_include) {
    sumstat_storage <- regs_glance %>%
      pull("df.residual") %>%
      tibble("df" = .) %>%
      bind_cols(sumstat_storage, .)
  }

  if ("F" %in% sumstat_include) {
    sumstat_storage <- regs_glance %>%
      pull("statistic") %>%
      tibble("F" = .) %>%
      bind_cols(sumstat_storage, .)
  }

  if ("Ymean" %in% sumstat_include) {
    # this doesn't work for felm
    # sumstat_Ymean <- map(regs, extract2, "model") %>%
    #   map(extract2, 1) %>%
    #   map_dbl(mean)

    # but this felm function works for lm and glm
    sumstat_storage <- y_vector %>%
      map_dbl(mean) %>%
      tibble("Ymean" = .) %>%
      bind_cols(sumstat_storage, .)
  }

  out_sumstats <- sumstat_storage %>%
    select(-1) %>%
    map(1:(length(sumstat_include)), pull, .data = .) %>%
    # surround with \multicolumn{1}{c}{XXX}
    map2(sumstat_names %>%
           filter(code %in% sumstat_include) %>%
           arrange(match(code, sumstat_include)) %>%
           pull(format),
         ~ sprintf(fmt = .y, .x)) %>%
    # add on name
    map2(sumstat_names %>%
           filter(code %in% sumstat_include) %>%
           arrange(match(code, sumstat_include)) %>%
           pull(proper_name) %>%
           as.character(), ., c) %>%
    map_chr(paste0, collapse = " & ") %>%
    str_c(" \\\\")

  return(out_sumstats)

}
