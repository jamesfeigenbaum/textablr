# working with clustering

# 2 way clusters in stata
# http://tomzimmermann.net/2018/08/22/two-way-clustering-in-stata/

library(tidyverse)
library(haven)
library(lfe)
library(textablr)
library(broom)

nls <- "http://www.stata-press.com/data/r14/nlswork.dta" %>%
  read_dta() %>%
  as_tibble()

nls_reg0 <-
  nls %>%
  felm(data = ., ln_wage ~ age + as.factor(race))

nls_reg1 <-
  nls %>%
  felm(data = ., ln_wage ~ age + as.factor(race) | 1 | 0 | idcode)

nls_reg2 <-
  nls %>%
  felm(data = ., ln_wage ~ age + as.factor(race) | 1 | 0 | idcode + year)

# get the names of the cluster variables
regs <- list(nls_reg0, nls_reg1, nls_reg1)

regs_glance <- regs %>% map_dfr(glance)

regs %>%
  map(magrittr::extract2, "clustervar") %>%
  tibble(clustervar = .) %>%
  mutate(reg_number = row_number()) %>%
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

# if only one way clustering


# if any 2+ clustering

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
