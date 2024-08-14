

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

library(stargazer)



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





# Newey West VCOV:

ols1_hac = lmtest::coeftest(ols1, vcov = NeweyWest)
ols1_hac

# NeweyWest Standard Errors:

NeweyWest(ols1) ## Print the HAC VCOV

sqrt(diag(NeweyWest(ols1))) ## Print the HAC SEs




# clustered standard errors:


lm_robust(mass ~ height, data = starwars, clusters = homeworld)



# Interaction Effect:

ols_ie = lm(mass ~ gender * height, data = starwars)
summary(ols_ie)


# Fixed Effets:

ols_fe = feols(mass ~ height | species, data = starwars) 
## Fixed effect(s) go after the "|"
ols_fe





## We now have two fixed effects: species and homeworld

ols_2fe = feols(mass ~ height | species + homeworld, data = starwars)
ols_2fe




ols_2fe = summary(ols_2fe, cluster = ~ species + homeworld) ## I'll go with this one
ols_2fe

# Etable:

etable(ols_fe, ols_2fe)


# plotting estimation coefficients:


coefplot(list(ols_fe, ols_2fe))

## Add legend (optional)
legend("bottomleft", col = 1:2, lwd = 1, pch = c(20, 17),
       legend = c("FE and no clustering", "2 FE and twoway clustering"))



# Regression Tables:



msummary(list(ols1, ols_fe, ols_2fe), output = "latex")

## or



stargazer(ols1, ols_ie, 
          header=FALSE,
          type= "latex",
          out = "model_output1.tex",
          title="Model With and Without Interaction Term",
          keep.stat="n",digits=2,
          column.labels = c("Model 1", "Model 2"),
          intercept.bottom=FALSE)


stargazer(ols1, ols_ie,
          header=FALSE,
          title="Baseline Vs Baseline With Interaction Term",
          type= "latex",
          out = "model_output2.tex",
          keep.stat="n", # what statistics to print
          omit.table.layout="n",
          star.cutoffs=NA,
          digits=3,
          intercept.bottom=FALSE, #moves the intercept coef to top
          column.labels = c("Model 1", "Model 2"),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          dep.var.caption="Mass of Animal",
          model.names=FALSE,
          star.char=NULL) #supresses the stars


## texreg: better suited for handling multiple types of regression: for e.g. with
## or without fixed effects.

library(texreg)


texreg(list(ols1, ols_fe, ols_2fe),
       caption = "Regression Results OLS, OLS-FE and OLS-HDFE",
       label = "tab:models")
          

# Data Summary Table:
          
datasummary_balance(~ gender,
                    data = starwars %>% select(height:mass, birth_year, eye_color, gender))







# Summary Stats Table:

otype = ifelse(knitr::is_latex_output(), 'return', 'kable')




st(humans %>% select(height:mass, birth_year, eye_color, gender),
   group = 'gender',
   out = otype)




# Correlation Plot:

ggplot(humans, aes(x = height, y = mass)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm")



# References:

#https://modelsummary.com/articles/modelsummary.html


#https://socviz.co/modeling.html

























































































