
require(tseries) # for ADF unit root tests
require(dynlm)
require(tidyverse)
require(car) # for calculating robust standard errors
require(sandwich)
require(forecast)

#
#### LOAD DATA ####

load("usa.rda")

usa = usa %>%
  mutate(Dgdp = gdp - lag(gdp, 1),
         Dinf = inf - lag(inf, 1),
         Df = f - lag(f, 1),
         Db = b - lag(b, 1))
  
# convert data frame into time series
usa.ts = ts(usa, start = c(1984, 1), end = c(2009, 4), frequency = 4)

plot(usa.ts)

#
#### First-Order Autoregressive Model AR(1) and Differencing ####

# Need to make sure the time series is stationary
plot(usa.ts[,"f"])
Acf(usa.ts[,"f"]) # lag of 10?

# Can test using Dickey-Fuller test
adf.test(usa.ts[,"f"], k = 10)
# it is non-stationary (borderline)


## Let's look at 'b' variable
plot(usa.ts[,"b"])
Acf(usa.ts[,"b"])

adf.test(usa$b, k = 10)

# This is running the model using the non-stationary time series actual data
f = usa.ts[,"f"]
f.dyn = dynlm(d(f) ~ L(f) + L(d(f)))
summary(f.dyn)


b = usa.ts[,"b"]

# NOTE
# A series is l(0) if it is stationary when integrated of order 0 (i.e. it is stationary in levels, not in differences)
# A series is l(1) if it is stationary in its first differences

df = diff(usa.ts[,"f"])
plot(df)
Acf(df)
adf.test(df, k = 2)

db = diff(usa.ts[,"b"])
plot(db)
Acf(db)
adf.test(df, k = 1)

# Both the f and b series are stationary in first differences.

## Now, let's run the ARIMA(1,1,0)
df.dyn = dynlm(d(df) ~ L(df)-1) # -1 just remove the intercept
db.dyn = dynlm(d(db) ~ L(db)-1)

summary(df.dyn)
summary(db.dyn)


# package forecast can determine the order of integration of a series quickly.
ndiffs(f)
ndiffs(usa.ts[,"b"])

# But it's better to look at the correlogram first. Also, ndiffs max at 2nd order of integration, so becareful.
# If need more, need to specify d.max = n

#### Cointegration ####

# Two time series are cointegrated when their trends are not too far apart.
# A cointegration test is in fact a Dickey-Fuller test on residuals.

# Let's see the state of cointegration between f and b

fb.dyn = dynlm(b ~ f)
ehat.fb = resid(fb.dyn)
ndiffs(ehat.fb)

plot(ehat.fb)

output = dynlm(d(ehat.fb) ~ L(ehat.fb) + L(d(ehat.fb)) - 1)
summary(output)

# The relevant t-statistics are -4.196 which is less than -3.37 (Dickey Fuller statistics)
# So, we reject the null hypothesis that the residuals have unit roots, therefore the series are cointegrated

## Or more simply, we can run the Phillips-Ouliaris test using the tseries package
po.test(cbind(b, f))

# The PO test marginally rejects the null of no cointegration at the 5% level

#### The Error Correction Model ####

# We use it for short run and long run dynamics of Yt ~ Xt

# Data: b = bonds, f = funds

## Error correction notation:
# delta(b_t) = - alpha (b_t-1 - B1 - B2(f_t-1)) + D0 delta(f_t) + D1 delta(f_t-1) + v_t

# regress bonds over funds (OLS), and use the intercept and beta coefficient of the model for the long run estimates
b.ols = dynlm(L(b) ~ L(f))

b1ini = coef(b.ols)[[1]]
b2ini = coef(b.ols)[[2]]

# regress bonds over lag bonds, funds, and lag funds (OLS)
d.ols = dynlm(b ~ L(b) + f + L(f))

aini = 1-coef(d.ols)[[2]]
d0ini = coef(d.ols)[[3]]
d1ini = coef(d.ols)[[4]]

# construct the variables for the error correction model
Db = diff(b)
Df = diff(f)
Lb = stats::lag(b, -1)
Lf = stats::lag(f, -1)

LDf = stats::lag(diff(f), -1)

# construct the data frame
bfset = data.frame(ts.union(cbind(b, f, Lb, Lf, Db, Df, LDf)))

# run the model
bf.nls = nls(Db ~ -a*(Lb - b1 - b2*Lf) + d0*Df + d1*LDf,
             na.action = na.omit,
             data = bfset,
             start = list(a = aini, b1 = b1ini, b2 = b2ini, d0 = d0ini, d1 = d1ini))

summary(bf.nls)






