

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



# Regression Table Output:

msummary(list(ols1, ols1_robust))



ols1_hac = lmtest::coeftest(ols1, vcov = NeweyWest)
ols1_hac


# clustered standard errors:


lm_robust(mass ~ height, data = starwars, clusters = homeworld)


# Interaction Effect:

ols_ie = lm(mass ~ gender * height, data = starwars)
summary(ols_ie)


# Fixed Effets:

ols_fe = feols(mass ~ height | species, data = starwars) 
## Fixed effect(s) go after the "|"
ols_fe


# Specifying Standard Errors:

#feols(mass ~ height | species,
  #    data = starwars, se = 'standard')


## We now have two fixed effects: species and homeworld
ols_hdfe = feols(mass ~ height | species + homeworld, data = starwars)
ols_hdfe




ols_hdfe = summary(ols_hdfe, cluster = ~ species + homeworld) ## I'll go with this one
ols_hdfe

# Etable:

etable(ols_fe, ols_hdfe)


# plotting estimation coefficients:


coefplot(list(ols_fe, ols_hdfe))

## Add legend (optional)
legend("bottomleft", col = 1:2, lwd = 1, pch = c(20, 17),
       legend = c("FE and no clustering", "HDFE and twoway clustering"))





msummary(list(ols1, ols_fe, ols_hdfe), output = "latex")

datasummary_balance(~ gender,
                    data = starwars %>% select(height:mass, birth_year, eye_color, gender))






#https://modelsummary.com/articles/modelsummary.html


otype = ifelse(knitr::is_latex_output(), 'return', 'kable')

## st() is an alias for sumtable()



humans =
  starwars %>%
  filter(species=="Human") %>%
  select(where(Negate(is.list))) ## Drop list columns (optional)

humans



st(humans %>% select(height:mass, birth_year, eye_color, gender),
   group = 'gender',
   out = otype)





ggplot(humans, aes(x = height, y = mass)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm")





























































































