# # summary statistics
# # for rdrobust
# # all with the prefix sumstat_
# # c("N", "h_l", "h_r", "b_l", "b_r", "N_h_l", "N_h_r", "c", "p", "q", "kernel", "vce", "bwselect")
#
# sumstat_master <- function(regs, sumstat_include = "N", sumstat_names_rdrobust) {
#
#   temp <- regs %>%
#     map_dfr(extract, c("h_l", "h_r", "b_l", "b_r", "N_h_l", "N_h_r", "c", "p", "q", "kernel", "vce", "bwselect")) %>%
#     # add N
#     mutate(N = regs %>% map_dfc(extract, "N") %>%
#              map_dfc(sum) %>%
#              t() %>%
#              as.vector())
#
#   temp_str <- temp %>%
#     select(sumstat_include) %>%
#     rownames_to_column() %>%
#     gather(key = "code", value = "value", -rowname) %>%
#     # surround with \multicolumn{1}{c}{XXX}
#     left_join(sumstat_names_rdrobust, by = "code") %>%
#     filter(str_detect(format, "%s")) %>%
#     mutate(value = value %>% sprintf(format, .))
#
#   temp_num <- temp %>%
#     select(sumstat_include) %>%
#     rownames_to_column() %>%
#     gather(key = "code", value = "value", -rowname) %>%
#     # surround with \multicolumn{1}{c}{XXX}
#     left_join(sumstat_names_rdrobust, by = "code") %>%
#     filter(str_detect(format, "%.*f")) %>%
#     mutate(value = value %>% as.numeric() %>% sprintf(format, .))
#
#   temp_both <- bind_rows(temp_num, temp_str) %>%
#     spread(key = "rowname", value = "value", sep = "_") %>%
#     select(proper_name, starts_with("rowname")) %>%
#     purrr::transpose() %>%
#     map_chr(paste0, collapse = " & ") %>%
#     str_c(" \\\\")
#
#   return(temp_both)
#
# }
