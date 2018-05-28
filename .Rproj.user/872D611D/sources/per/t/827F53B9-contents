library(tidyverse)
library(broom)
library(stringr)
library(magrittr)
library(purrr)
library(data.table)
library(jjfPkg)

# compare with stargazer
library(stargazer)

# regression packages
library(lfe)
library(rdrobust)

rm(list = ls())

x <- runif(1000, -5, 5)
y <- 5 + 2*x + 3*(x >= 0) + rnorm(1000)
dt <- tibble(x = x, y = y)

ggplot(dt, aes(x = x, y = y, group = (x>0))) +
  geom_point(alpha = .1) +
  geom_smooth(se = FALSE) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  theme_jjf()

# create dummy data for RD

reg1 <- rdrobust(dt$y, dt$x, )
reg2 <- rdrobust(dt$y, dt$x, p = 2)
reg3 <- rdrobust(dt$y, dt$x, kernel = "epanechnikov")
reg4 <- rdrobust(dt$y, dt$x, kernel = "uniform")

# input

# function will take regressions as a list of list objects
regs <- list(reg1, reg2, reg3, reg4)

# one optional parameter will be the name of the running variable
var_labels <- "Running Variable X"

# which summary stats to include?
sumstat_include <- c("N", "h_l", "h_r", "b_l", "b_r", "N_h_l", "N_h_r", "c", "p", "q",
                     "kernel", "vce", "bwselect")

source("R/lookups.R")

# we're going to extract some things right from regs
# but other things from the list of summaries
# regs_summary <- map(regs, summary)
# doesn't work for rdrobust

# simple stuff
# all with the prefix regs_

# count the number of regressions
# for numbering columns
reg_columns <- length(regs)

# get the functions called to run regressions
# useful metadata
# TODO add this to output
reg_calls <- map(regs, extract2, c("call")) %>%
  as.character()

# summary statistics
source("R/sumstat_rdrobust.R")
out_sumstats <- sumstat_master(regs, sumstat_include, sumstat_names_rdrobust)

source("R/coeffs.R")
out_x_fe <- x_fe_master(regs, var_labels, var_indicates, var_omits)

out_colnumbers <- 1:reg_columns %>%
  # surround with \multicolumn{1}{c}{XXX}
  sprintf("\\multicolumn{1}{c}{(%s)}", .) %>%
  map(extract) %>%
  purrr::transpose() %>%
  map_chr(paste0, collapse = " & ") %>%
  str_c("& ", ., " \\\\")

out_table <- c(
  out_colnumbers,
  "\\midrule",
  out_x_fe,
  "\\midrule",
  out_sumstats)

# TODO
# if the following row has a midrule, remove the previous addlinespace

cat(out_table, file = "latex_testing/table1.tex", sep = "\n")
