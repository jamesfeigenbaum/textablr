#' @title Output Regression Results as a LaTeX Table
#'
#' @description Like stata's `estout`, output regression results nicely.
#'
#' @param ... stored regression output
#' @param file output location, if blank only prints to viewer
#' @param var_labels named vector of variable labels.
#'     For example, to label the `educ` variable `Education` and the `expr` variable `Experience`
#'     make this `c("Education" = "educ", "Experience" = "expr")`
#'     Note: this sets the order of variables in the table
#'     Note: to get multiple line variable labels, just use `\\`
#'     Note: to get the times symbol for interactions, use `XINTERACTION`
#'     Neither of these will look very good in the gt output...
#' @param var_indicates named vector of variables to indicate.
#'     This sets of the order of indicator variables in the table
#' @param var_omits vector of variables to omit
#' @param sumstat_include vector of summary statistics to include.
#'     The defaults are `c("nobs", "adj.r.squared", "Ymean")`
#'     Adjust the names by using names in the vector:
#'     `c("Observations" = "nobs", "Adjusted R-Squared" = "adj.r.squared", "Y Mean Value" = "Ymean")`
#'     The names of these summary stats match the output of `broom::glance` when possible
#' @param star_levels vector of cut offs for statistical significance stars.
#'     The defaults are `c(0.10, 0.05, 0.01)`.
#'     Make this NULL for no stars at all
#' @param sumstat_format tibble of digit formatting for summary statistics
#'     Copy the default tibble to the clipboard with `textablr_get_sumstat_format()` function
#'     and change digits.
#' @param beta_digits 3 or 3.1 or 3.14 or 3.146 or 3.1459... default is 2 digts = 3.14
#' @param se_digits 3 or 3.1 or 3.14 or 3.146 or 3.1459... default is whatever beta_digits is
#' @param cluster_names named vector for cluster SE variables (one or multi-way clustering)
#'
#' @import magrittr
#' @import gt
#' @importFrom purrr map map_chr transpose
#' @importFrom stringr str_c str_replace_all str_replace str_which str_detect str_split
#'
#' @examples
#'
#' # using lm, glm, and felm with `mtcars`
#' reg1 <- mtcars %>% lfe::felm(data = ., mpg ~ wt | cyl)
#' reg2 <- mtcars %>% lm(data = ., mpg ~ hp)
#' reg3 <- mtcars %>% dplyr::filter(gear == 4) %>%
#'     glm(data = ., vs ~ wt + am, family = binomial(link = "logit"))
#' reg4 <- mtcars %>% lm(data = ., mpg ~ wt + hp + am + as.factor(cyl))
#' reg5 <- mtcars %>% lm(data = ., wt ~ hp)
#' reg6 <- mtcars %>% lfe::felm(data = ., wt ~ hp | cyl + am)
#'
#' # and a named vector of variable labels (optional)
#' var_labels <- c("Weight" = "wt", "Horsepower" = "hp")
#' # when labelling an instrumental variable in felm syntax
#' # the term should be "`x(fit)`" including the backticks
#' # if x is instrumented for by some z
#'
#' # which variables to omit? (optional)
#' var_omits <- c("(Intercept)")
#'
#' # which variables to indicate yes no (good for FEs) (optional)
#' var_indicates <- c("Transmission FE" = "am", "Cylinders FE" = "cyl")
#'
#' # which summary stats to include?
#' sumstat_include <- c("nobs", "adj.r.squared", "Ymean")
#'
#' # textablr_estout(reg1, reg2, reg3, reg4, reg5, reg6,
#' #   file = "", var_labels = var_labels, var_omits = var_omits,
#' #   var_indicates = var_indicates, sumstat_include = sumstat_include)
#'
#' @export textablr_estout

textablr_estout <- function(..., file = "",
                            var_labels = NULL, var_omits = NULL, var_indicates = NULL,
                            sumstat_include = sumstat_include_default, sumstat_format = sumstat_format_default,
                            star_levels = star_level_default,
                            beta_digits = 2, se_digits = beta_digits,
                            cluster_names = NULL) {

  regs <- list(...)

  # we're going to extract some things right from regs
  # but other things from the list of summaries
  regs_summary <- map(regs, summary)

  # simple stuff
  # all with the prefix reg_

  # count the number of regressions
  # for numbering columns
  reg_columns <- length(regs)

  # get the functions called to run regressions
  # useful metadata
  # TODO add this to output
  reg_calls <- map(regs, magrittr::extract2, c("call")) %>%
    as.character()

  # summary statistics
  out_sumstats <- sumstat_master(regs, sumstat_include, sumstat_format, cluster_names) %>%
    mutate(table_part = "summary")

  # coefficients and FEs
  out_x_fe <- x_fe_master(regs, var_labels, var_indicates, var_omits, star_levels, beta_digits, se_digits)
  # table part is defined in x_fe_master

  # and put the whole output table together
  # TODO
  # i used to put midrules between column numbers and xfe
  # and between xfe and sumstats
  out_table <- bind_rows(out_x_fe, out_sumstats) %>%
    # convert reg number variable names to proper titles
    set_names(c(" ", sprintf("(%d)", 1:reg_columns), "table_part"))

  out_table %>%
    gt() %>%
    tab_header("Regressions") %>%
    cols_align("center") %>%
    cols_hide("table_part") %>%
    print()

  # if file is empty don't cat to a file at all
  if (file != "") {

    # which cells to put in multicolumn?
    # if the string starts with this then...

    fe_labels <-
      out_table %>%
      as_tibble() %>%
      filter(table_part %in% c("fe")) %>%
      pull(" ") %>%
      paste0(collapse = "|") %>%
      str_c("|incasefeisblank")

    summary_labels <-
      out_table %>%
      as_tibble() %>%
      filter(table_part %in% c("summary")) %>%
      pull(" ") %>%
      paste0(collapse = "|") %>%
      str_c("|incasesummaryisblank")

    out_table %>%
      gt() %>%
      cols_hide("table_part") %>%
      as_latex() %>%
      str_split("\n") %>%
      unlist() %>%
      .[(str_which(., "toprule") + 1):(str_which(., "bottomrule") - 1)] %>%
      tibble(tex = .) %>%
      # put multicolumn around summary and fes
      # and put multicolumn around reg numbers at top
      mutate(tex = if_else(str_detect(tex, summary_labels) |
                             str_detect(tex, fe_labels) |
                             str_detect(tex, "& \\(1\\) &"),
                           str_replace_all(tex, "& ([^&\\\\]+) ", "& \\\\multicolumn{1}{c}{\\1} "),
                           tex)) %>%
      # addlinespace at the end of any SE rows or FE rows
      mutate(tex = if_else(str_detect(tex, "^ &") & str_detect(tex, "\\([0-9]+\\.[0-9]+\\)"),
                          str_c(tex, "\\addlinespace"),
                          tex)) %>%
      mutate(tex = if_else(str_detect(tex, fe_labels),
                    str_c(tex, "\\addlinespace"),
                    tex)) %>%
      pull(tex) %>%
      paste0(collapse = "\n") %>%
      # put stars in \sym{}
      str_replace_all("(\\*+)", "\\\\sym{\\1}") %>%
      # put midrule between FE and sum stats
      # replace not replace all because only do first one
      str_replace(summary_labels, "\\\\midrule\n \\0") %>%
      # if there are any "\textbackslash" it is because
      # long variable labels with \\s got converted
      str_replace_all("textbackslash ", "\\\\") %>%
      # and fix interaction symbols
      str_replace_all("XINTERACTION", "$\\\\times$") %>%
      # and kill leading and trailing white space
      str_trim() %>%
      cat(file = file)

  }

  return("Regression table made. See the viewer")

}
