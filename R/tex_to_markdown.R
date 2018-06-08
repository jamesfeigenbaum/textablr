#' @title Estout to Output Regression Results
#'
#' @description Like stata's `estout`, output regression results nicely.
#'
#' @param out_table the latex output from `estout`
#'
#' @import stringr
#' @importFrom purrr transpose map map2_dfc map_chr
#' @importFrom magrittr add
#' @importFrom tidyr separate
#'
#' @return out_table The LaTeX code for the table
#'
#' @keywords internal

# turn latex table into something we can look at in the console

tex_to_markdown <- function(out_table) {

  columns <- out_table %>%
    str_count("&") %>%
    max() %>%
    add(1)

  temp1 <-
    out_table %>%
    # get rid of some latex code
    str_replace_all("\\\\multicolumn\\{.\\}\\{.\\}", "") %>%
    str_replace_all("\\\\sym", "") %>%
    str_replace_all("\\\\addlinespace", "") %>%
    str_replace_all("[{}]", "") %>%
    str_replace_all("\\\\", "") %>%
    as_tibble() %>%
    separate(value, sep = "& ", into = paste0("c", 1:columns), fill = "right") %>%
    select(starts_with("c"))

  # pad every column to be the max + K
  max_widths <-
    temp1 %>%
    map(1:columns, pull, .data = .) %>%
    map(str_length) %>%
    map(max, na.rm = TRUE) %>%
    as.numeric() %>%
    add(2)

  temp1 %>%
    map(1:columns, pull, .data = .) %>%
    map2_dfc(max_widths, ~ str_pad(.x, width = .y, side = "right")) %>%
    map(extract) %>%
    purrr::transpose() %>%
    map_chr(paste0, collapse = " ") %>%
    str_replace_all("midrule.+$", str_dup("-", max_widths %>% sum() %>% add(columns - 1))) %>%
    cat(sep = "\n") %>%
    return()

}


