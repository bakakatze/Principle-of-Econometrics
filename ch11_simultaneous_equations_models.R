
# get the data from https://github.com/ccolonescu/PoEdata/blob/master/data
load("truffles.rda")

require(tidyverse)
require(systemfit)

#
# D - demand, S - supply, q - quantity, p - price, ps - price of substitute, di - income, pf - cost of production

# Structural models
D = q ~ p + ps + di
S = q ~ p + pf

sys = list(D,S)

# Define the instrumental variables (those that do not exist in another structural model)
instr = ~ ps + di + pf

# fit the model using 2 stage least squares method
truff.sys = systemfit(sys, inst = instr, method = '2SLS', data = truffles)
summary(truff.sys)
# this can estimate the quantity supplied and demanded simultaneously

## Analysing the reduced form can be useful for prediction but not very useful for exploratory analysis
Q.red = lm(q ~ ps+di+pf, data = truffles)
P.red = lm(p ~ ps+di+pf, data = truffles)

summary(Q.red)
summary(P.red)

#
## SOME CAVEATS

# Simultaneous Equation Modelling will not work if there is no significant/reliable instrumental variables (IVs).
# The estimate for the structural model without the appropriate IV will be biased and inconsistent.

# Finding the right IVs is the biggest challenge in this model.




