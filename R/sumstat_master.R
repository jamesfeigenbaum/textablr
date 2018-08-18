#' @title Gather Summary Statistics from Regression Results for Output
#'
#' @description Helper function to construct summary statistics
#'
#' @param regs stored regression output in a list
#' @param sumstat_include vector of summary statistics to include
#' @param sumstat_names tibble from the lookup with summary stats and proper names
#'
#' @importFrom purrr map_dfr map_int map_dbl map2 map_chr
#' @importFrom broom glance
#' @importFrom dplyr pull filter arrange
#' @importFrom tidyr spread gather
#' @importFrom stats sd
#' @keywords internal

sumstat_master <- function(regs, sumstat_include = "N", sumstat_names, cluster_labels) {

  # summary statistics
  # all with the prefix sumstat_
  # N, R2, aR2, df, Fstat, Ymean

  # put each of these inside an if statement
  # only calculate sumstat_XXX if we want XXX
  # this is good for speed
  # and because some summary stats don't always exist for all regression types

  # save speed by summarizing now
  # regs_summary <- regs %>% map(summary)
  # actually this is never used...
  regs_glance <- regs %>% map_dfr(glance)

  y_vector <- regs %>%
    map(~ tibble(resid = .x$residuals %>% as.vector(), fitted = .x$fitted.values %>% as.vector(), y = resid + fitted)) %>%
    map(pull)

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

    if ("r.squared" %in% names(regs_glance)) {

      sumstat_storage <- regs_glance %>%
        pull(r.squared) %>%
        tibble("R2" = .) %>%
        bind_cols(sumstat_storage, .)

    } else {

      sumstat_storage <- bind_cols(sumstat_storage, "R2" = rep(NA, nrow(sumstat_storage)))

    }

  }

  if ("aR2" %in% sumstat_include) {

    if ("adj.r.squared" %in% names(regs_glance)) {

      sumstat_storage <- regs_glance %>%
        pull(adj.r.squared) %>%
        tibble("aR2" = .) %>%
        bind_cols(sumstat_storage, .)

    } else {

      sumstat_storage <- bind_cols(sumstat_storage, "aR2" = rep(NA, nrow(sumstat_storage)))

    }

  }

  if ("df" %in% sumstat_include) {

    if ("df.residual" %in% names(regs_glance)) {

      sumstat_storage <- regs_glance %>%
        pull(df.residual) %>%
        tibble("df" = .) %>%
        bind_cols(sumstat_storage, .)

    } else {

      sumstat_storage <- bind_cols(sumstat_storage, "df" = rep(NA, nrow(sumstat_storage)))

    }

  }

  if ("F" %in% sumstat_include) {

    if ("statistic" %in% names(regs_glance)) {

      sumstat_storage <- regs_glance %>%
        pull("statistic") %>%
        tibble("F" = .) %>%
        bind_cols(sumstat_storage, .)

    } else {

      sumstat_storage <- bind_cols(sumstat_storage, "F" = rep(NA, nrow(sumstat_storage)))

    }

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

  # clusters
  if ("N_clusters" %in% sumstat_include) {

    # number of clusters
    clusters <-
      regs %>%
      map(magrittr::extract2, "clustervar") %>%
      tibble(reg_number = 1:dim(regs_glance)[1], clustervar = .) %>%
      # if a regression has no clusters
      # clustervar will be NULL
      filter(!map_lgl(clustervar, is.null)) %>%
      mutate(cluster_name = clustervar %>% map(names)) %>%
      tidyr::unnest(clustervar, cluster_name) %>%
      mutate(cluster_n = map(clustervar, levels) %>% map_int(length)) %>%
      # how many ways?
      group_by(reg_number) %>%
      mutate(cluster_k = n()) %>%
      select(reg_number, starts_with("cluster_")) %>%
      # deal with regressions without clustered SEs
      right_join(tibble(reg_number = 1:dim(regs_glance)[1]), by = "reg_number") %>%
      # if cluster_k is NA, make that 0
      mutate(cluster_k = ifelse(is.na(cluster_k), 0, cluster_k)) %>%
      ungroup()

    sumstat_kway_clusters <- clusters %>%
      select(-cluster_k) %>%
      spread(key = "cluster_name", value = "cluster_n", sep = "_") %>%
      select(-cluster_name_NA) %>%
      t() %>%
      as.tibble(rownames = "term") %>%
      filter(term != "reg_number") %>%
      left_join(cluster_labels %>% rownames_to_column(var = "order"), by = "term") %>%
      select(-term) %>%
      gather(key = "reg_number", value = "value", -proper_name, -order) %>%
      mutate(reg_number = reg_number %>% str_extract("[0-9]+") %>% as.numeric()) %>%
      arrange(reg_number, order) %>%
      # surround with \multicolumn{1}{c}{XXX}
      mutate(value = ifelse(is.na(value), "", value %>% sprintf("\\multicolumn{1}{c}{%s}", .))) %>%
      spread(key = reg_number, value = value, sep = "_", fill = "") %>%
      arrange(order) %>%
      select(-order) %>%
      map(magrittr::extract) %>%
      purrr::transpose() %>%
      map_chr(paste0, collapse = " & ") %>%
      str_c(" \\\\")

  }

  # clustering works differently
  sumstat_include0 <- sumstat_include[sumstat_include != "N_clusters"]

  out_sumstats <- sumstat_storage %>%
    select(sumstat_include0) %>%
    map(1:(length(sumstat_include0)), pull, .data = .) %>%
    # surround with \multicolumn{1}{c}{XXX}
    map2(sumstat_names %>%
           filter(code %in% sumstat_include0) %>%
           arrange(match(code, sumstat_include0)) %>%
           pull(format),
         ~ sprintf(fmt = .y, .x)) %>%
    # add on name
    map2(sumstat_names %>%
           filter(code %in% sumstat_include0) %>%
           arrange(match(code, sumstat_include0)) %>%
           pull(proper_name) %>%
           as.character(), ., c) %>%
    map_chr(paste0, collapse = " & ") %>%
    str_c(" \\\\")

  # if we want cluster SEs, include them
  if ("N_clusters" %in% sumstat_include) {
    out_sumstats <- c(out_sumstats, sumstat_kway_clusters)
  }

  return(out_sumstats)

}
