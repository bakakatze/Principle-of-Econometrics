# https://bookdown.org/ccolonescu/RPoE4

library(devtools)
install_github("ccolonescu/PoEdata")

load("mroz.rda")

require(tidyverse)
require(stargazer)
require(AER)

#
## 10.1 Instrumental Variables (IV) metods ####

# The IV method is often called two-stage least squares / 2SLS

# let's use wage-experience-education data
mroz1 = mroz[mroz$lfp==1, ]

educ.ols = lm(educ ~ exper + I(exper^2) + mothereduc, data = mroz1)
summary(educ.ols)

# then compute the fitted value from this model and used it as a regressor for the next model
educHat = fitted(educ.ols)
wage.2sls = lm(log(wage) ~ educHat + exper + I(exper^2), data = mroz1)
summary(wage.2sls) # the SEs are incorrect, should use ivreg()

# Let's compare different models
mroz1.ols = lm(log(wage) ~ educ + exper + I(exper^2), data = mroz1)
mroz1.iv = ivreg(log(wage) ~ educ + exper + I(exper^2) | exper + I(exper^2) + mothereduc, data = mroz1)
mroz1.iv1 = ivreg(log(wage) ~ educ + exper + I(exper^2) | exper + I(exper^2) + mothereduc + fathereduc, data = mroz1)

stargazer(mroz1.ols, wage.2sls, mroz1.iv, mroz1.iv1,
          title = "Wage equation: OLS, 2SLS, and IV models compared",
          header = FALSE,
          keep.stat = "n",
          omit.table.layout = "n",
          star.cutoffs = NA,
          digits = 4,
          intercept.bottom=FALSE, #moves the intercept coef to top
          column.labels=c("OLS","explicit 2SLS", "IV mothereduc", 
                          "IV mothereduc and fathereduc"),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          dep.var.caption="Dependent variable: wage",
          model.names=FALSE,
          star.char=NULL)

# 2SLS and first IV model are the same
# but the variance estimation is incorrect in the 2SLS

## 11. Simultaneous Equations Models ####






