# coefficients

x_fe_master <- function(regs, var_labels = NULL){

  # if var_lables is null build dummy version
  if (is.null(var_labels)) {
    var_labels <- "Running Variable"
  }

  # pull out running variables from regressions
  running_vars <-
    regs %>%
    map(extract2, "call") %>%
    as.character() %>%
    str_extract("x = .*") %>%
    str_replace(".*\\$", "") %>%
    str_replace(",.*", "") %>%
    str_replace("\\)", "") %>%
    tibble(var_name = .)

  # extract with tidy
  # merge on labels or omit codes and order
  reg_table <-
    map_dfr(regs, extract, c("coef", "se", "z", "pv"), .id = "reg_number") %>%
    mutate(version = rep(c("Conventional", "Bias-Corrected", "Robust"), max(.$reg_number))) %>%
    mutate(reg_number = reg_number %>% as.numeric()) %>%
    rowwise() %>%
    mutate(stars = sum(pv < star_levels)) %>%
    ungroup() %>%
    mutate(estimate_string = coef %>%
           sprintf("%.2f", .)) %>%
    mutate(estimate_star = if_else(stars == 0, estimate_string,
                                   paste0(estimate_string, "\\sym{", strrep("*", stars), "}")
    )) %>%
    # add parentheses to SE
    # TODO use options for SE or t or pvalue
    mutate(se = if_else(is.na(se), "", se %>% sprintf("(%.2f)", .))) %>%
    select(reg_number, estimate_star, se, label = version) %>%
    gather(key = "beta_se", value = "value", -reg_number, -label) %>%
    spread(key = reg_number, value = value, sep = "_", fill = "") %>%
    mutate(label = if_else(beta_se == "estimate_star", label, "")) %>%
    mutate(line_ender = if_else(beta_se == "estimate_star", "\\\\", "\\\\ \\addlinespace")) %>%
    select(-beta_se) %>%
    map(extract) %>%
    purrr::transpose() %>%
    map_chr(paste0, collapse = " & ") %>%
    str_replace("& \\\\", "\\\\")

  return(reg_table)

}
