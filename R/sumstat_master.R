#' @title Gather Summary Statistics from Regression Results for Output
#'
#' @description Helper function to construct summary statistics
#'
#' @param regs stored regression output in a list
#' @param sumstat_include vector of summary statistics to include
#' @param sumstat_format tibble from the lookup with summary stats and proper names
#' @param cluster_names named vector of cluster SE variables
#'
#' @importFrom purrr map_dfr map_int map_dbl map2 map_chr map2_dfr
#' @importFrom broom glance
#' @importFrom dplyr pull filter arrange row_number bind_cols everything
#' @importFrom tidyr spread gather
#' @importFrom stats sd nobs model.frame
#' @keywords internal

sumstat_master <- function(regs, sumstat_include = c("nobs", "adj.r.squared", "Ymean"),
                           sumstat_format, cluster_names) {

  # summary statistics
  # all with the prefix sumstat_
  # N, R2, aR2, df, Fstat, Ymean

  # put each of these inside an if statement
  # only calculate sumstat_XXX if we want XXX
  # this is good for speed
  # and because some summary stats don't always exist for all regression types
  # but not needed for the summary stats in broom::glance

  # if any of the summary stats are unnamed, used the defaults for all
  # TODO fix this at some point
  if (sumstat_include %>% names() %>% map_lgl(magrittr::equals, "") %>% sum() > 0 |
      sumstat_include %>% names() %>% is.null()) {

    sumstat_include <-
      set_names(sumstat_include,
                sumstat_format %>%
                  mutate(order = match(code, sumstat_include)) %>%
                  filter(!is.na(order)) %>%
                  arrange(order) %>%
                  pull(proper_name) %>%
                  as.character())
  }

  # save speed by summarizing now
  # regs_summary <- regs %>% map(summary)
  # actually this is never used...
  regs_glance <- regs %>% map_dfr(glance)

  y_vector <- regs %>%
    map(model.frame) %>%
    map(pull, var = 1)

  # below only works if the models are all linear regressions...
  # y_vector <- regs %>%
  #   map(~ tibble(resid = .x$residuals %>% as.vector(),
  #                fitted = .x$fitted.values %>% as.vector(),
  #                y = resid + fitted)) %>%
  #   map(pull)

  # anything in regs_glance is EASY!
  sumstat_storage <-
    regs_glance %>%
    mutate(reg_number = row_number()) %>%
    select(reg_number, everything())

  if ("N" %in% sumstat_include | "nobs" %in% sumstat_include) {
    # nobs doesn't work for felm...
    # but it does with fallback (wtf?)
    sumstat_N <- map_int(regs, nobs, use.fallback = TRUE)
    sumstat_storage <- sumstat_N %>%
      tibble("nobs" = .) %>%
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

  if ("Ysd" %in% sumstat_include) {
    sumstat_storage <- y_vector %>%
      map_dbl(stats::sd) %>%
      tibble("Ysd" = .) %>%
      bind_cols(sumstat_storage, .)
  }

  # could add other functions of Y pretty easily...

  if ("APF" %in% sumstat_include) {

    sumstat_storage <-
      regs %>%
      map(magrittr::extract("stage1")) %>%
      map(magrittr::extract("iv1fstat")) %>%
      map(magrittr::extract(1)) %>%
      map(magrittr::extract("F")) %>%
      tibble(reg_number = 1:dim(regs_glance)[1], term = .) %>%
      # if a regression is not an IV
      # term will be NULL
      filter(!map_lgl(term, is.null)) %>%
      tidyr::unnest(term) %>%
      right_join(tibble::tibble(reg_number = 1:dim(regs_glance)[1]),
                 by = "reg_number") %>%
      select(APF = term) %>%
      bind_cols(sumstat_storage, .)

  }

  if ("clusters" %in% sumstat_include) {

    sumstat_storage <- sumstat_storage %>%
      mutate(clusters = TRUE)

  }

  out_sumstats_tbl <- sumstat_storage %>%
    select(sumstat_include) %>%
    map2_dfr(sumstat_format %>%
           filter(code %in% sumstat_include) %>%
           arrange(match(code, sumstat_include)) %>%
           pull(format),
         ~ sprintf(fmt = .y, .x))

  # this is the point to pop in the cluster list, if one exists
  if ("clusters" %in% sumstat_include) {

    # get the cluster list
    out_clusters <-
      cluster_master(regs, cluster_names)

    cluster_index <- match("clusters", sumstat_include)

    out_sumstats_tbl <- bind_cols(
      out_sumstats_tbl[1:(cluster_index - 1)],
      out_clusters,
      out_sumstats_tbl[(cluster_index + 1):length(out_sumstats_tbl)])

  }

  col_names <- sprintf("reg_number_%d", 1:length(regs)) %>%
    c("label", .)

  out_sumstats <- out_sumstats_tbl %>%
    t() %>%
    as_tibble(rownames = "label") %>%
    set_names(col_names)

  return(out_sumstats)

}
