

library(mfx)
library(tidyverse)

library(hrbrthemes)
library(estimatr)
library(ivreg)

library(fixest)
library(sandwich)



library(lmtest)
library(margins)

library(vtable)
library(broom)

library(modelsummary)



theme_set(hrbrthemes::theme_ipsum())




View(starwars)



# lm function: simple linear regression.


ols1 = lm(mass ~ height, data = starwars)

View(ols1)


summary(ols1)

summary(ols1)$coefficients


tidy(ols1, conf.int = TRUE)


glance(ols1)

# Robust Standard Error:

ols1_robust = lm_robust(mass ~ height, data = starwars)

ols1_robust




NeweyWest(ols1) ## Print the HAC VCOV

sqrt(diag(NeweyWest(ols1))) ## Print the HAC SEs





msummary(list(ols1, ols1_robust))








































