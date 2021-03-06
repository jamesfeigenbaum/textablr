library(tidyverse)
library(textablr)

reg1 <- mtcars %>% lfe::felm(data = ., mpg ~ wt | cyl)
reg2 <- mtcars %>% lm(data = ., mpg ~ hp)
reg3 <- mtcars %>% dplyr::filter(gear == 4) %>%
    glm(data = ., vs ~ wt + am, family = binomial(link = "logit"))
reg4 <- mtcars %>% lm(data = ., mpg ~ wt + hp + am + as.factor(cyl))
reg5 <- mtcars %>% lm(data = ., wt ~ hp)
reg6 <- mtcars %>% lfe::felm(data = ., wt ~ hp | cyl + am)

# and a named vector of variable labels (optional)
var_labels <- c("A long variable \\ name needs a new line" = "hp", "Weight XINTERACTION Blah" = "wt")
# when labelling an instrumental variable in felm syntax
# the term should be "`x(fit)`" including the backticks
# if x is instrumented for by some z

# which variables to omit? (optional)
var_omits <- c("(Intercept)")

# which variables to indicate yes no (good for FEs) (optional)
var_indicates <- c("Cylinders FE" = "cyl", "Transmission FE" = "am")

# which summary stats to include?
sumstat_include <- c("nobs", "adj.r.squared", "Ymean")

textablr_estout(reg1, reg2, reg3, reg4, reg5, reg6,
                file = "latex_testing/table1_NEW.tex",
                var_labels = var_labels,
                var_omits = var_omits,
                var_indicates = var_indicates,
                sumstat_include = sumstat_include)

####

# y mean not working with probit and logit etc

gtcars %>%
  mutate(euro = ctry_origin %in% c("Germany", "Italy", "United Kingdom")) %>%
  count(euro)

reg1 <- gtcars %>%
  mutate(euro = ctry_origin %in% c("Germany", "Italy", "United Kingdom")) %>%
  lm(data = ., euro ~ hp + msrp)

reg2 <- gtcars %>%
  mutate(euro = ctry_origin %in% c("Germany", "Italy", "United Kingdom")) %>%
  glm(data = ., euro ~ hp + msrp, family = binomial(link = "logit"))

reg3 <- gtcars %>%
  mutate(euro = ctry_origin %in% c("Germany", "Italy", "United Kingdom")) %>%
  glm(data = ., euro ~ hp + msrp, family = binomial(link = "probit"))

regs <- list(reg1, reg2, reg3)

textablr_estout(regs, sumstat_include = sumstat_include)

