library(tidyverse)
library(textablr)
library(haven)
library(lfe)

## IV

# using lm, glm, and felm with `card 1995`
card1995 <- "https://storage.googleapis.com/causal-inference-mixtape.appspot.com/card.dta" %>%
  read_dta() %>%
  # convert the region indicator dummies to a factor
  gather(key = "region", value = "dummy", starts_with("reg")) %>%
  filter(dummy == 1) %>%
  mutate(region = region %>% str_remove("reg") %>% as.numeric()) %>%
  select(-dummy)

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

iv4 <- card1995 %>%
  felm(data = ., lwage ~ exper + black + south + married + smsa | region | (educ ~ nearc4))
fs4 <- card1995 %>%
  felm(data = . , educ ~ nearc4 + exper + black + south + married + smsa | region)

# Regression Table

# regs <- list(ols, iv1, iv2, iv3, fs1, fs2, fs3)
regs <- list(ols, iv1, iv2, iv3, iv4)

var_labels <- c("Education" = "educ", "Education IV" = "`educ(fit)`",
                "Near 4Year College" = "nearc4", "Near 2Year College" = "nearc2")

sumstat_include <- c("nobs", "Ymean", "APF", "Ysd", "statistic")

var_indicates <- c("Region FE" = "region", "Race and South Controls" = "black|south")

textablr_estout(regs, var_labels = var_labels, var_omits = "(Intercept)", var_indicates = var_indicates,
                sumstat_include = sumstat_include)

## Clustering

nls <- "http://www.stata-press.com/data/r14/nlswork.dta" %>%
  read_dta() %>%
  as_tibble()

nls_reg0 <-
  nls %>%
  felm(data = ., ln_wage ~ age + as.factor(race))

nls_reg1 <-
  nls %>%
  felm(data = ., ln_wage ~ age + as.factor(race) | 0 | 0 | idcode)

nls_reg2 <-
  nls %>%
  felm(data = ., ln_wage ~ age + as.factor(race) | 0 | 0 | idcode + year)

# get the names of the cluster variables
regs <- list(nls_reg0, nls_reg1, nls_reg2)

textablr_estout(regs, sumstat_include = c("N" = "nobs", "Clusters" = "clusters", "Ymean" = "Ymean", "Ysd" = "Ysd"),
                cluster_names = c("ID Code Clusters" = "idcode", "Years" = "year"))

textablr_estout(regs, sumstat_include = c("People" = "nobs", "clusters", "Ymean", "Ysd"),
                cluster_names = c("ID Code Clusters" = "idcode", "Years" = "year"))
