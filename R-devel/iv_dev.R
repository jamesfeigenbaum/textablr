library(lfe)
library(magrittr)
library(tibble)
library(purrr)
library(stringr)
library(tidyr)
library(dplyr)
library(broom)
library(haven)

# using lm, glm, and felm with `card 1995`
card1995 <- "https://storage.googleapis.com/causal-inference-mixtape.appspot.com/card.dta" %>%
  read_dta()

# Regressions
ols <- card1995 %>%
  lm(data = ., lwage ~ educ + exper + black + south + married + smsa)

iv1 <- card1995 %>%
  felm(data = ., lwage ~ exper + black + south + married + smsa | 0 | (educ ~ nearc4))
fs1 <- card1995 %>%
  lm(data = . , educ ~ nearc4 + exper + black + south + married + smsa)

iv2 <- card1995 %>%
  felm(data = ., lwage ~ exper + black + south + married + smsa | 0 | (educ ~ nearc2))
fs2 <- card1995 %>%
  lm(data = . , educ ~ nearc2 + exper + black + south + married + smsa)

iv3 <- card1995 %>%
  felm(data = ., lwage ~ exper + black + south + married + smsa | 0 | (educ ~ nearc4 + nearc2))
fs3 <- card1995 %>%
  lm(data = . , educ ~ nearc4 + nearc2 + exper + black + south + married + smsa)

# Regression Table

regs <- list(ols, iv1, iv2, iv3, fs1, fs2, fs3)

var_labels <- tibble::tibble(term = c("educ", "`educ(fit)`", "exper", "black", "south", "married", "smsa", "nearc4", "nearc2"),
                             label = c("Education", "Education IV", "Experience", "Black", "South", "Married", "SMSA", "Near 4Year College", "Near 2Year College"))

sumstat_include <- c("N", "Ymean", "APF", "Ysd", "F")

estout(regs, var_labels = var_labels, var_omits = var_omits, sumstat_include = sumstat_include)

# F-Stat

# Cunningham reports that the F statistic for IV in first stage is 15.767
# This is just the t-stat on `nearc4` in the first stage squared
# How can I extract that?

iv1$stage1 %>%
  broom::tidy() %>%
  filter(term %in% iv1$stage1$instruments) %>%
  magrittr::extract("statistic") %>%
  magrittr::raise_to_power(2) %>%
  pull()

# F-Stat

# Can we get this working with multiple regressions?

iv_list <- list(iv1, iv2, iv3)

reg_columns <- length(iv_list)

instrument_list <-
  iv_list %>%
  map(magrittr::extract("stage1")) %>%
  map(magrittr::extract("instruments")) %>%
  tibble(reg_number = 1:reg_columns, term = .) %>%
  filter(!map_lgl(term, is.null)) %>%
  tidyr::unnest(term)

iv_list %>%
  map(magrittr::extract("stage1")) %>%
  map_dfr(broom::tidy, .id = "reg_number", fe = FALSE, fe.error = FALSE) %>%
  as_tibble() %>%
  mutate(reg_number = reg_number %>% as.numeric()) %>%
  right_join(instrument_list, by = c("reg_number", "term")) %>%
  magrittr::extract("statistic") %>%
  magrittr::raise_to_power(2) %>%
  pull()

#' @title IV First Stage Diagnostics for felm
#'
#' @description Helper function to construct IV test stats
#'
#' @param regs stored regression output in a list
#'
#' @import AER
#' @importFrom purrr map_dfr map_chr map_lgl transpose
#'
#' @keywords internal

# solution
# send the regressions to the ivreg function in the AER package and glance the diagnostics

reg_columns <- length(regs)

# are there any IVs?
iv_columns <-
  regs %>%
  map(magrittr::extract("stage1")) %>%
  map_lgl(is.null) %>%
  not()

iv_regs <- regs[iv_columns]

# extract data
data_in <-
  iv_regs %>%
  map(model.frame) %>%
  map(as_tibble)

# generate formulas
iv1$call$formula



names <- iv_regs %>%
  map(magrittr::extract("names"))

y_names <- iv_regs %>%
  map(magrittr::extract("lhs"))

z_names <- iv_regs %>%
  map(magrittr::extract("stage1")) %>%
  map(magrittr::extract("instruments"))

endo_names <- iv_regs %>%
  map(magrittr::extract("endovars"))

y <- data_in %>%
  map2(y_name, select)

z <- data_in %>%
  map2(y_name, udselect)
select(-y_name, -endo_name)
x <- data_in %>% select(-y_name, -z_names)

## endogenous/instrument variables
endo <- which(!(colnames(x) %in% colnames(z)))
inst <- which(!(colnames(z) %in% colnames(x)))
if((length(endo) <= 0L) | (length(inst) <= 0L))
  stop("no endogenous/instrument variables")

## convenience functions
lmfit <- function(x, y) {
  rval <- lm.fit(x, y)
  rval$x <- x
  rval$y <- y
  return(rval)
}

rss <- iv_regs %>%
  map(magrittr::extract("residuals")) %>%
  map(raise_to_power, 2) %>%
  map(sum)

wald <- function(obj0, obj1, vcov. = NULL) {
  df <- c(obj1$rank - obj0$rank, obj1$df.residual)
  if (!is.function(vcov.)) {
    w <- ((rss(obj0) - rss(obj1)) / df[1L]) / (rss(obj1)/df[2L])
  } else {
    if (NCOL(obj0$coefficients) > 1L) {
      cf0 <- structure(as.vector(obj0$coefficients),
  .Names = c(outer(rownames(obj0$coefficients), colnames(obj0$coefficients), paste, sep = ":")))
      cf1 <- structure(as.vector(obj1$coefficients),
  .Names = c(outer(rownames(obj1$coefficients), colnames(obj1$coefficients), paste, sep = ":")))
    } else {
      cf0 <- obj0$coefficients
      cf1 <- obj1$coefficients
    }
    cf0 <- na.omit(cf0)
    cf1 <- na.omit(cf1)
    ovar <- which(!(names(cf1) %in% names(cf0)))
    vc <- vcov.(lm(obj1$y ~ 0 + obj1$x))
    w <- t(cf1[ovar]) %*% solve(vc[ovar,ovar]) %*% cf1[ovar]
    w <- w / df[1L]
  }
  pval <- pf(w, df[1L], df[2L], lower.tail = FALSE)
  c(df, w, pval)
}

# Test for weak instruments
for(i in seq_along(endo)) {
aux0 <- lmfit(z[, -inst, drop = FALSE], x[, endo[i]])
aux1 <- lmfit(z,                        x[, endo[i]])
rval[i, ] <- wald(aux0, aux1, vcov. = vcov.)
}

## Wu-Hausman test for endogeneity
if(length(endo) > 1L) aux1 <- lmfit(z, x[, endo])
xfit <- as.matrix(aux1$fitted.values)
colnames(xfit) <- paste("fit", colnames(xfit), sep = "_")
auxo <- lmfit(      x,        y)
auxe <- lmfit(cbind(x, xfit), y)
rval[nrow(rval) - 1L, ] <- wald(auxo, auxe, vcov. = vcov.)

## Sargan test of overidentifying restrictions
r <- residuals(obj)
auxs <- lmfit(z, r)
rval[nrow(rval), 1L] <- length(inst) - length(endo)
if(rval[nrow(rval), 1L] > 0L) {
  rval[nrow(rval), 3L] <- length(r) * (1 - rss(auxs)/sum((r - mean(r))^2))
  rval[nrow(rval), 4L] <- pchisq(rval[nrow(rval), 3L], rval[nrow(rval), 1L], lower.tail = FALSE)
}

