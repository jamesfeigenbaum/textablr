#' @title Output Regression Results as a LaTeX Table
#'
#' @description Like stata's `estout`, output regression results nicely.
#'
#' @param regs stored regression output in a list
#' @param file output location, if blank prints to screen
#' @param var_labels named vector of variable labels.
#'     For example, to label the `educ` variable `Education` and the `expr` variable `Experience`
#'     make this `c("Education" = "educ", "Experience" = "expr")`
#'     Note: this sets the order of variables in the table
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
#' @param cluster_labels tibble of labels for cluster SE variables (one or multi-way clustering)
#'
#' @import magrittr
#' @importFrom purrr map map_chr transpose
#' @importFrom stringr str_c str_replace_all
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
#' # function will take regressions as a list of list objects
#' regs <- list(reg1, reg2, reg3, reg4, reg5, reg6)
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
#' # textablr_estout(regs, file = "", var_labels, var_omits, var_indicates, sumstat_include)
#'
#' @export textablr_estout

textablr_estout <- function(regs, file = "",
                            var_labels = NULL, var_omits = NULL, var_indicates = NULL,
                            sumstat_include = sumstat_include_default, sumstat_format = sumstat_format_default,
                            star_levels = star_level_default,
                            beta_digits = 2, se_digits = beta_digits,
                            cluster_labels = NULL) {

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
  out_sumstats <- sumstat_master(regs, sumstat_include, sumstat_format, cluster_labels)

  # coefficients and FEs
  out_x_fe <- x_fe_master(regs, var_labels, var_indicates, var_omits, star_levels, beta_digits, se_digits)

  # column numbers for output
  out_colnumbers <- 1:reg_columns %>%
    # surround with \multicolumn{1}{c}{XXX}
    sprintf("\\multicolumn{1}{c}{(%s)}", .) %>%
    map(magrittr::extract) %>%
    purrr::transpose() %>%
    map_chr(paste0, collapse = " & ") %>%
    str_c("& ", ., " \\\\")

  # and put the whole output table together
  out_table <- c(
    out_colnumbers,
    "\\midrule",
    out_x_fe,
    "\\midrule",
    out_sumstats)

  # if file is empty don't cat to a file at all
  if (file != "") {

    out_table %>%
      # if the following row has a midrule, remove the previous addlinespace
      paste0(collapse = "\n") %>%
      str_replace_all("\\\\addlinespace(\n.+midrule)", "\\1") %>%
      cat(file = file)

  }

  markdown_table <- tex_to_markdown(out_table) %>%
    cat(sep = "\n")

  invisible(out_table)

}
