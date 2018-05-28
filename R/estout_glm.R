library(tidyverse)
library(broom)
library(stringr)
library(magrittr)
library(purrr)
library(data.table)

# compare with stargazer
library(stargazer)

# regression packages
library(lfe)
library(rdrobust)

# create dummy data for probit and logit etc
# use mtcars

reg1 <-
  mtcars %>%
  glm(data = ., vs ~ wt, family = binomial(link = "logit"))

reg2 <-
  mtcars %>%
  glm(data = ., vs ~ hp, family = binomial(link = "logit"))

reg3 <-
  mtcars %>%
  filter(gear == 4) %>%
  glm(data = ., vs ~ am, family = binomial(link = "logit"))

reg4 <-
  mtcars %>%
  glm(data = ., vs ~ wt + hp + am + as.factor(cyl), family = binomial(link = "logit"))

reg5 <-
  mtcars %>%
  glm(data = ., vs ~ hp, family = binomial(link = "logit"))

reg6 <-
  mtcars %>%
  glm(data = ., vs ~ hp + as.factor(cyl), family = binomial(link = "logit"))

# input

# function will take regressions as a list of list objects
regs <- list(reg1, reg2, reg3, reg4, reg5, reg6)

# one optional parameter will be a tibble with variables and labels
var_labels <- tibble(term = c("wt", "hp"),
                     label = c("Weight", "Horsepower"))

# which variables to omit?
var_omits <- c("(Intercept)")

# which variables to indicate yes no
# so they aren't omitted but the coefficients aren't there
var_indicates <- tibble(term = c("am", "cyl"), indicator = c("Transmission FE", "Cylinders FE"))

# which summary stats to include?
sumstat_include <- c("N", "Ymean")

# lookups
# TODO put this somewhere else (out of this file?)

# summary stat lookup
# TODO enable custom proper names for summary statistics
sumstat_names <- tibble(code = c("N", "Ymean", "R2", "aR2", "df", "F"),
                        proper_name = c("Observations", "Y Mean",
                                        "R$^{2}$", "Adjusted R$^{2}$",
                                        "Degrees of Freedom", "F-Statistic"),
                        format = c("%.0f", "%.2f", "%.2f", "%.2f", "%.0f", "%.2f")) %>%
  mutate(format = format %>% paste0("\\multicolumn{1}{c}{", ., "}"))

# indicator levels
indicator_levels <- c("Yes", "No")

# star levels
star_levels <- c(0.10, 0.05, 0.01)

# we're going to exract somethings right from regs
# but other things from the list of summaries
regs_summary <- map(regs, summary)

# simple stuff
# all with the prefix regs_

# count the number of regressions
# for numbering columns
reg_columns <- length(regs)

# get the functions called to run regressions
# useful metadata
reg_calls <- map(regs, extract2, c("call")) %>%
  as.character()

# summary statistics
# all with the prefix sumstat_
# N, R2, aR2, df, Fstat, Ymean

sumstat_N <- map_int(regs, nobs)

sumstat_R2 <- regs_summary %>%
  map_dbl(extract2, c("r.squared"))

sumstat_aR2 <- regs_summary %>%
  map_dbl(extract2, c("adj.r.squared"))

sumstat_df <- map_int(regs, extract2, "df.residual")

sumstat_F <- regs_summary %>%
  map(extract2, c("fstatistic")) %>%
  map_dbl(extract, "value")

sumstat_Ymean <- map(regs, extract2, "model") %>%
  map(extract2, 1) %>%
  map_dbl(mean)

# coefficients
# extract with tidy
# merge on labels or omit codes and order
reg_table <-
  map_dfr(regs, tidy, .id = "reg_number") %>%
  as.tibble() %>%
  mutate(reg_number = reg_number %>% as.numeric()) %>%
  rownames_to_column(var = "rownumber") %>%
  mutate(rownumber = rownumber %>% as.numeric()) %>%
  left_join(var_labels, by = "term") %>%
  # check against indicator regexs
  mutate(indicator_term = var_indicates %>%
           pull(term) %>%
           paste0(collapse = "|") %>%
           str_extract(string = term, pattern = .)) %>%
  left_join(var_indicates, by = c("indicator_term" = "term")) %>%
  left_join(var_omits %>% as.tibble() %>% rename(term = value) %>% mutate(omit = 1),
            by = "term") %>%
  # drop the rows we are omitting
  filter(is.na(omit))

# split the reg_table into coefficients and indicator rows
reg_table_x <-
  reg_table %>%
  filter(is.na(indicator)) %>%
  rowwise() %>%
  mutate(stars = sum(p.value < star_levels)) %>%
  ungroup()

reg_table_fe <-
  reg_table %>%
  filter(!is.na(indicator))

reg_table_x_varlist <- reg_table_x %>%
  select(rownumber, label, term) %>%
  mutate(label = if_else(is.na(label), term, label)) %>%
  group_by(label) %>%
  summarize(order = min(rownumber)) %>%
  arrange(order) %>%
  pull(label)

reg_table_fe_varlist <- reg_table_fe %>%
  select(rownumber, indicator) %>%
  group_by(indicator) %>%
  summarize(order = min(rownumber)) %>%
  arrange(order) %>%
  pull(indicator)

reg_table_varlist <- c(reg_table_x_varlist, reg_table_fe_varlist)

out_x <- expand.grid(label = reg_table_x_varlist, reg_number = 1:reg_columns, stringsAsFactors = FALSE) %>%
  as.tibble() %>%
  left_join(reg_table_x, by = c("label", "reg_number")) %>%
  # TODO add customization for digits and decimals in beta and se etc
  # add stars
  mutate(estimate_string = estimate %>%
           sprintf("%.2f", .)) %>%
  mutate(estimate_star = if_else(stars == 0, estimate_string,
                                 paste0(estimate_string, "\\sym{", strrep("*", stars), "}")
  )) %>%
  # add parentheses to SE
  # TODO use options for SE or t or pvalue
  mutate(se = if_else(is.na(std.error), "", std.error %>% sprintf("(%.2f)", .))) %>%
  select(reg_number, estimate_star, se, label) %>%
  gather(key = "beta_se", value = "value", -reg_number, -label) %>%
  spread(key = reg_number, value = value, sep = "_", fill = "") %>%
  mutate(label = if_else(beta_se == "estimate_star", label, "")) %>%
  mutate(line_ender = if_else(beta_se == "estimate_star", "\\\\", "\\\\ \\addlinespace")) %>%
  select(-beta_se) %>%
  map(extract) %>%
  purrr::transpose() %>%
  map_chr(paste0, collapse = " & ") %>%
  str_replace("& \\\\", "\\\\")

out_fe <- expand.grid(indicator = reg_table_fe_varlist, reg_number = 1:reg_columns, stringsAsFactors = FALSE) %>%
  as.tibble() %>%
  left_join(reg_table_fe, by = c("indicator", "reg_number")) %>%
  group_by(indicator, reg_number) %>%
  summarize(count = sum(!is.na(estimate))) %>%
  # yes no
  mutate(value = case_when(count == 0 ~ indicator_levels[2],
                           count > 0 ~ indicator_levels[1])) %>%
  # surround with \multicolumn{1}{c}{XXX}
  mutate(value = value %>% sprintf("\\multicolumn{1}{c}{%s}", .)) %>%
  select(-count) %>%
  ungroup() %>%
  spread(key = reg_number, value = value, sep = "_", fill = "") %>%
  map(extract) %>%
  purrr::transpose() %>%
  map_chr(paste0, collapse = " & ") %>%
  map_chr(paste, "\\\\ \\addlinespace")

out_colnumbers <- 1:reg_columns %>%
  # surround with \multicolumn{1}{c}{XXX}
  sprintf("\\multicolumn{1}{c}{(%s)}", .) %>%
  map(extract) %>%
  purrr::transpose() %>%
  map_chr(paste0, collapse = " & ") %>%
  str_c("& ", ., " \\\\")

out_sumstats <- sumstat_include %>%
  str_c("sumstat_", .) %>%
  map(get) %>%
  # surround with \multicolumn{1}{c}{XXX}
  map2(sumstat_names %>%
         filter(code %in% sumstat_include) %>%
         arrange(match(code, sumstat_include)) %>%
         pull(format),
       ~ sprintf(fmt = .y, .x)) %>%
  # add on name
  map2(sumstat_names %>%
         filter(code %in% sumstat_include) %>%
         arrange(match(code, sumstat_include)) %>%
         pull(proper_name), ., c) %>%
  map_chr(paste0, collapse = " & ") %>%
  str_c(" \\\\")

out_table <- c(
  out_colnumbers,
  "\\midrule",
  out_x,
  out_fe,
  "\\midrule",
  out_sumstats)

# TODO
# if the following row has a midrule, remove the previous addlinespace

cat(out_table, file = "latex_testing/table_logit.tex", sep = "\n")
