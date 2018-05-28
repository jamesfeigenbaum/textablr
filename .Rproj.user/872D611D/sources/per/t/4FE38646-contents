# summary statistics
# all with the prefix sumstat_
# N, R2, aR2, df, Fstat, Ymean

# put each of these inside an if statement
# only calculate sumstat_XXX if we want XXX
# this is good for speed
# and because some summary stats don't always exist for all regression types

sumstat_master <- function(regs, sumstat_include = "N", sumstat_names) {

  # save speed by summarizing now
  regs_summary <- regs %>% map(summary)
  regs_glance <- regs %>% map_dfr(glance)

  y_vector <- regs %>%
    map(lfe:::model.frame.felm) %>%
    map(extract2, 1)

  if ("N" %in% sumstat_include) {
    # nobs doesn't work for felm
    # sumstat_N <- map_int(regs, nobs)
    sumstat_N <- y_vector %>% map_int(length)
  }

  if ("R2" %in% sumstat_include) {
    sumstat_R2 <- regs_glance %>%
      pull(r.squared)
  }

  if ("aR2" %in% sumstat_include) {
    sumstat_aR2 <- regs_glance %>%
      pull(adj.r.squared)
  }

  if ("df" %in% sumstat_include) {
    sumstat_df <- regs_glance %>%
      pull("df.residual")
  }

  if ("F" %in% sumstat_include) {
    sumstat_F <- regs_glance %>%
      pull(statistic)
  }

  if ("Ymean" %in% sumstat_include) {
    # this doesn't work for felm
    # sumstat_Ymean <- map(regs, extract2, "model") %>%
    #   map(extract2, 1) %>%
    #   map_dbl(mean)

    # but this felm function works for lm
    sumstat_Ymean <- y_vector %>% map_dbl(mean)
  }

  out_sumstats <- sumstat_include %>%
    str_c("sumstat_", .) %>%
    map(dynGet) %>%
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
           pull(proper_name), ., c) %>%
    map_chr(paste0, collapse = " & ") %>%
    str_c(" \\\\")

  return(out_sumstats)

}
