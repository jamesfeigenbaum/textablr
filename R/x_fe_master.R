#' @title Compile Regression Coefficients for Output
#'
#' @description Helper function to construct coefficients
#'
#' @param regs stored regression output in a list
#' @param var_labels vector of variable labels
#' @param var_indicates vector of variables to indicate
#' @param var_omits vector of variables to omit
#' @param star_levels statistical significance stars
#'
#' @import tibble
#' @importFrom purrr map_dfr map_chr map_lgl transpose
#' @import stringr
#' @importFrom tidyr gather spread unnest
#' @import dplyr
#' @importFrom broom tidy
#' @importFrom fuzzyjoin regex_full_join
#'
#' @keywords internal

# coefficients

x_fe_master <- function(regs, var_labels = NULL, var_indicates = NULL, var_omits = NULL,
                        star_levels = star_level_default,
                        beta_digits, se_digits){

    reg_columns <- length(regs)

    # if var_labels is null build dummy version
    if (is.null(var_labels)) {
      var_labels <- c("No Labels" = "")
    }

    # if var_indicates is empty, put a long random string in to prevent bad catches"
    if (is.null(var_indicates)) {
      var_indicates <- c("Missing FE" = "asfkjhafdkjahsdfuashfajshgfaskjhgfaskjhdfgaksjgfaskdjfg")
    }

    # make var_labels and var_indicates tibbles for easier use downstream
    # but easier for users to input named vectors
    var_labels <- var_labels %>%
      tibble(term = ., label = names(.))
    var_indicates <- var_indicates %>%
      tibble(term = ., indicator = names(.))

    # if var_omits is null, build dummy version
    if (is.null(var_omits)) {
      var_omits <- c("")
    }

    # fes from felm
    # fes as factors in other regression packages are handled using regex in the var indicator tibble
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

    # extract with tidy from broom package
    # merge on labels or omit codes and order
    reg_table <-
      map_dfr(regs, tidy, .id = "reg_number", fe = FALSE, fe.error = FALSE) %>%
      as_tibble() %>%
      mutate(reg_number = reg_number %>% as.numeric()) %>%
      left_join(var_labels, by = "term") %>%
      bind_rows(fe_terms) %>%
      # omit any variables?
      left_join(var_omits %>% as_tibble() %>% rename(term = value) %>% mutate(omit = 1),
                by = "term") %>%
      # drop the rows we are omitting
      filter(is.na(omit)) %>%
      # check against indicator regexs
      regex_full_join(var_indicates, by = "term") %>%
      filter(!is.na(reg_number)) %>%
      # give term as the label for rows without a label given
      mutate(term = term.x) %>%
      mutate(label = case_when(!is.na(label) ~ label,
                               TRUE ~ term))

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

    beta_fmt <- beta_digits %>% sprintf("%s%df", "%.", .)
    se_fmt <- se_digits %>% sprintf("(%s%df)", "%.", .)

    out_x <- expand.grid(label = reg_table_x_varlist, reg_number = 1:reg_columns, stringsAsFactors = FALSE) %>%
      as_tibble() %>%
      left_join(reg_table_x, by = c("label", "reg_number")) %>%
      # TODO add customization for digits and decimals in beta and se etc
      # add stars
      mutate(estimate_string = estimate %>%
               sprintf(beta_fmt, .)) %>%
      mutate(estimate_star = if_else(sig_stars == 0, estimate_string,
                                     paste0(estimate_string, strrep("*", sig_stars))
      # mutate(estimate_star = if_else(sig_stars == 0, estimate_string,
      #                                paste0(estimate_string, "\\sym{", strrep("*", sig_stars), "}")

      )) %>%
      # add parentheses to SE
      # TODO use options for SE or t or pvalue
      mutate(se = if_else(is.na(std.error), "", std.error %>% sprintf(se_fmt, .))) %>%
      select(reg_number, estimate_star, se, label) %>%
      gather(key = "beta_se", value = "value", -reg_number, -label) %>%
      spread(key = reg_number, value = value, sep = "_", fill = "") %>%
      # order everything based on labels
      # if a variable is not in var_labels... alphabetical
      mutate(order2 = row_number()) %>%
      left_join(var_labels %>%
                  mutate(order1 = row_number()),
                by = "label") %>%
      # if the intercept is around, make it first
      mutate(order0 = if_else(label == "(Intercept)", 1, 2)) %>%
      arrange(order0, order1, order2) %>%
      select(-order0, -order1, -order2, -term) %>%
      mutate(label = if_else(beta_se == "estimate_star", label, "")) %>%
      select(-beta_se)

    if (reg_table_fe %>% nrow() != 0) {

      out_fe <- expand.grid(indicator = reg_table_fe_varlist,
                            reg_number = 1:reg_columns, stringsAsFactors = FALSE) %>%
        as_tibble() %>%
        left_join(reg_table_fe, by = c("indicator", "reg_number")) %>%
        group_by(indicator, reg_number) %>%
        summarize(count = sum(!is.na(estimate)) + sum(felm_fe == TRUE, na.rm = TRUE)) %>%
        # yes no
        mutate(value = case_when(count == 0 ~ indicator_levels[2],
                                 count > 0 ~ indicator_levels[1])) %>%
        # surround with \multicolumn{1}{c}{XXX}
        # actually don't we'll do this later after we gt it
        # mutate(value = value %>% sprintf("\\multicolumn{1}{c}{%s}", .)) %>%
        select(-count) %>%
        ungroup() %>%
        spread(key = reg_number, value = value, sep = "_", fill = "") %>%
        # order everything based on order of indicators
        left_join(var_indicates %>% select(indicator) %>% mutate(order = row_number()), by = "indicator") %>%
        arrange(order) %>%
        select(-order) %>%
        rename(label = indicator)

    }

    if (reg_table_fe %>% nrow() == 0) {
      out_x %>%
        mutate(table_part = "x") %>%
        return()
    } else {
      bind_rows(out_x %>% mutate(table_part = "x"),
                out_fe %>% mutate(table_part = "fe")) %>%
        return()
    }

}
