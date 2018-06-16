#' @title Compile Regression Coefficients for Output
#'
#' @description Helper function to construct coefficients
#'
#' @param regs stored regression output in a list
#' @param var_labels tibble of variable labels
#' @param var_indicates tibble of variables to indicate
#' @param var_omits vector of variables to omit
#' @param star_levels statistical significance stars
#'
#' @import tibble
#' @importFrom purrr map_dfr map_chr map_lgl transpose
#' @import stringr
#' @importFrom tidyr gather spread unnest
#' @import dplyr
#' @importFrom broom tidy
#'
#' @keywords internal

# coefficients

x_fe_master <- function(regs, var_labels = NULL, var_indicates = NULL, var_omits = NULL, star_levels = star_level_default){

    reg_columns <- length(regs)

    # if var_lables is null build dummy version
    if (is.null(var_labels)) {
      var_labels <- tibble(term = c(""), label = c(""))
    }

    # if var_indicates is empty, put a long random string in to prevent bad catches"
    if (is.null(var_indicates)) {
      var_indicates <- tibble(term = c("asfkjhafdkjahsdfuashfajshgfaskjhgfaskjhdfgaksjgfaskdjfg"),
                              indicator = c("Missing FE"))
    }

    # if var_omits is null, build dummy version
    if (is.null(var_omits)) {
      var_omits <- c("")
    }

    # fes
    fe_terms <-
      regs %>%
      map(magrittr::extract2, "fe") %>%
      map(names) %>%
      tibble(reg_number = 1:reg_columns, term = .) %>%
      # if a regression is not an felm call
      # term will be NULL
      filter(!map_lgl(term, is.null)) %>%
      tidyr::unnest(term) %>%
      mutate(felm_fe = TRUE)

    # extract with tidy
    # merge on labels or omit codes and order
    reg_table <-
      map_dfr(regs, tidy, .id = "reg_number", fe = FALSE, fe.error = FALSE) %>%
      as_tibble() %>%
      mutate(reg_number = reg_number %>% as.numeric()) %>%
      left_join(var_labels, by = "term") %>%
      bind_rows(fe_terms) %>%
      # check against indicator regexs
      mutate(indicator_term = var_indicates %>%
               pull(term) %>%
               paste0(collapse = "|") %>%
               str_extract(string = term, pattern = .)) %>%
      left_join(var_indicates, by = c("indicator_term" = "term")) %>%
      left_join(var_omits %>% as_tibble() %>% rename(term = value) %>% mutate(omit = 1),
                by = "term") %>%
      # drop the rows we are omitting
      filter(is.na(omit)) %>%
      # give term as the label for rows without a label given
      mutate(label = if_else(is.na(label), term, label))

    # split the reg_table into coefficients and indicator rows
    reg_table_x <-
      reg_table %>%
      filter(is.na(indicator)) %>%
      rowwise() %>%
      mutate(sig_stars = sum(p.value < star_levels)) %>%
      ungroup()

    reg_table_fe <-
      reg_table %>%
      filter(!is.na(indicator))

    reg_table_x_varlist <- reg_table_x %>%
      select(label, term) %>%
      mutate(label = if_else(is.na(label), term, label)) %>%
      select(label) %>%
      distinct() %>%
      pull()

    reg_table_fe_varlist <- reg_table_fe %>%
      select(indicator) %>%
      distinct() %>%
      pull()

    reg_table_varlist <- c(reg_table_x_varlist, reg_table_fe_varlist)

    out_x <- expand.grid(label = reg_table_x_varlist, reg_number = 1:reg_columns, stringsAsFactors = FALSE) %>%
      as_tibble() %>%
      left_join(reg_table_x, by = c("label", "reg_number")) %>%
      # TODO add customization for digits and decimals in beta and se etc
      # add stars
      mutate(estimate_string = estimate %>%
               sprintf("%.2f", .)) %>%
      mutate(estimate_star = if_else(sig_stars == 0, estimate_string,
                                     paste0(estimate_string, "\\sym{", strrep("*", sig_stars), "}")
      )) %>%
      # add parentheses to SE
      # TODO use options for SE or t or pvalue
      mutate(se = if_else(is.na(std.error), "", std.error %>% sprintf("(%.2f)", .))) %>%
      select(reg_number, estimate_star, se, label) %>%
      gather(key = "beta_se", value = "value", -reg_number, -label) %>%
      spread(key = reg_number, value = value, sep = "_", fill = "") %>%
      # order everything based on labels
      mutate(order2 = row_number()) %>%
      left_join(var_labels %>% select(label) %>% mutate(order1 = row_number()), by = "label") %>%
      arrange(order1, order2) %>%
      select(-order1, -order2) %>%
      mutate(label = if_else(beta_se == "estimate_star", label, "")) %>%
      mutate(line_ender = if_else(beta_se == "estimate_star", "\\\\", "\\\\ \\addlinespace")) %>%
      select(-beta_se) %>%
      map(magrittr::extract) %>%
      purrr::transpose() %>%
      map_chr(paste0, collapse = " & ") %>%
      str_replace("& \\\\", "\\\\")

    if (reg_table_fe %>% nrow() != 0) {

      out_fe <- expand.grid(indicator = reg_table_fe_varlist, reg_number = 1:reg_columns, stringsAsFactors = FALSE) %>%
        as_tibble() %>%
        left_join(reg_table_fe, by = c("indicator", "reg_number")) %>%
        group_by(indicator, reg_number) %>%
        summarize(count = sum(!is.na(estimate)) + sum(felm_fe == TRUE, na.rm = TRUE)) %>%
        # yes no
        mutate(value = case_when(count == 0 ~ indicator_levels[2],
                                 count > 0 ~ indicator_levels[1])) %>%
        # surround with \multicolumn{1}{c}{XXX}
        mutate(value = value %>% sprintf("\\multicolumn{1}{c}{%s}", .)) %>%
        select(-count) %>%
        ungroup() %>%
        spread(key = reg_number, value = value, sep = "_", fill = "") %>%
        # order everything based on order of indicators
        left_join(var_indicates %>% select(indicator) %>% mutate(order = row_number()), by = "indicator") %>%
        arrange(order) %>%
        select(-order) %>%
        map(magrittr::extract) %>%
        purrr::transpose() %>%
        map_chr(paste0, collapse = " & ") %>%
        map_chr(paste, "\\\\ \\addlinespace")

    }

    if (reg_table_fe %>% nrow() == 0) {
      out_x %>%
        return()
    } else {
      c(out_x, out_fe) %>%
        return()
    }

}
