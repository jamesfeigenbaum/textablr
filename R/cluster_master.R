#' @title Gather SE Cluster from Regression Results for Output
#'
#' @description Helper function to construct clustering counts
#'
#' @param regs stored regression output in a list
#' @param cluster_names vector of cluster variable names
#'
#' @importFrom purrr map_dfr map_int map_lgl
#' @importFrom broom glance
#' @importFrom dplyr pull filter arrange row_number bind_cols everything
#' @importFrom tidyr spread gather expand
#' @importFrom stats sd nobs
#' @keywords internal

cluster_master <- function(regs, cluster_names) {

  # first check if there are any regressions with clustered SEs
  # for now, this only works with `felm` regressions from the `lfe` package
  regs_with_clusters <-
    regs %>%
    map(magrittr::extract2, "clustervar") %>%
    tibble(clustervar = .) %>%
    mutate(reg_number = row_number()) %>%
    # if a regression has no clusters
    # clustervar will be NULL
    filter(!map_lgl(clustervar, is.null)) %>%
    nrow()

  if (regs_with_clusters == 0) {
    return(NULL)
  }

  regs_glance <- regs %>% map_dfr(glance)

  cluster_tibble <-
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

  # produce a list that can be slotted into the summary stats code
  cluster_out <-
    cluster_tibble %>%
    # create dummy tibble, expanded to number of regressions x different clusters
    tidyr::expand(reg_number, cluster_name) %>%
    # merge on the cluster information
    left_join(cluster_tibble, by = c("reg_number", "cluster_name")) %>%
    # keep just the clusters
    filter(!is.na(cluster_name)) %>%
    select(-cluster_k) %>%
    # if cluster_names vector has names, merge on
    left_join(cluster_names %>% tibble(cluster_name = ., label = names(.)), by = "cluster_name") %>%
    # if there is a label use that, otherwise the cluster_name
    mutate(cluster_name_proper = if_else(!is.na(label), label, cluster_name)) %>%
    select(-label, -cluster_name) %>%
    spread(key = "cluster_name_proper", value = "cluster_n", fill = "") %>%
    group_by(reg_number) %>%
    mutate_all(funs(sprintf("\\multicolumn{1}{c}{%s}", .))) %>%
    ungroup() %>%
    select(-reg_number)

  return(cluster_out)

}
