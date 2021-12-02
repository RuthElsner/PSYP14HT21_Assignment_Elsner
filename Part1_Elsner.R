##PART 1

#load the data set
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")
dp1 = data_sample_1
View(dp1)


#load packages

library(tidyverse)
library(gridExtra)	
library(psych)
library(gsheet)	
library(magrittr)	
library(titanic)
library(dplyr)
library(car)
library(ggplot2)
library(lm.beta) 



#data and variable diagnostic (age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva, pain)
  #checking for NA's
  #mean, descriptives

summary(dp1)
describe(dp1)

  #get rid of NA's
    #pain 0-10 -- error
    #STAI_trait 20-80 -- error
    #pain_cat 0-52 -- ok
    #mindfulness 1-6 -- ok

    #exclude NA's (pain and STAI_trait) and check again

filter(dp1, pain > 9)


dp1 = dp1 %>%
  slice(-c(88))

summary(dp1)
summary(dp1$pain)

filter(dp1, STAI_trait < 20)
dp1 = dp1 %>%
  slice(-c(34))

summary(dp1$STAI_trait)

dp1 = dp1 %>%
  mutate(sex = factor(sex))

str(dp1$sex)

summary(dp1)

#check for influential outliers (ggplot and Cook's distance >1 or 4/N)
  # extreme values on the y axis
  # extreme values on the x axis

windows()
dp1 %>%
  ggplot() + 
  aes(x = age, y = pain) + 
  geom_point() +
  geom_smooth(method = "lm")

plot1 <- lm(pain ~ age, data = dp1)
plot1 %>%
  plot(which = 4)
#8,23,46

windows()
dp1 %>%
  ggplot() + 
  aes(x = sex, y = pain) + 
  geom_point() +
  geom_smooth(method = "lm")

plot2 <- lm(pain ~ sex, data = dp1)
plot2 %>%
  plot(which = 4)
#8,12,85

windows()
dp1 %>%
  ggplot() + 
  aes(x = STAI_trait, y = pain) + 
  geom_point() +
  geom_smooth(method = "lm")

plot3 <- lm(pain ~ STAI_trait, data = dp1)
plot3 %>%
  plot(which = 4)
#20,64,85

windows()
dp1 %>%
  ggplot() + 
  aes(x = pain_cat, y = pain) + 
  geom_point() +
  geom_smooth(method = "lm")

plot4 <- lm(pain ~ pain_cat, data = dp1)
plot4 %>%
  plot(which = 4)
#12,115,149

windows()
dp1 %>%
  ggplot() + 
  aes(x = mindfulness, y = pain) + 
  geom_point() +
  geom_smooth(method = "lm")

plot5 <- lm(pain ~ mindfulness, data = dp1)
plot5 %>%
  plot(which = 4)
#8,33,42

windows()
dp1 %>%
  ggplot() + 
  aes(x = cortisol_serum, y = pain) + 
  geom_point() +
  geom_smooth(method = "lm")

plot6 <- lm(pain ~ cortisol_serum, data = dp1)
plot6 %>%
  plot(which = 4)
#8,60,85

windows()
dp1 %>%
  ggplot() + 
  aes(x = cortisol_saliva, y = pain) + 
  geom_point() +
  geom_smooth(method = "lm")

plot7 <- lm(pain ~ cortisol_saliva, data = dp1)
plot7 %>%
  plot(which = 4)
#8,60,85

# no outliers > 1 but a lot higher than 4/N = 0.02! first, keep them and check the assumptions 



#hierarchical regression
  # model 1: age and sex as predictors of pain

mod1 = lm(pain ~ age + sex, data = dp1)
mod1

sm1 = summary(mod1)
sm1

  # model 2: age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva as predictors of pain 

mod2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = dp1)
mod2

sm2 = summary(mod2)
sm2



#check assumptions of linear regression
  #Normality
    #QQ plot, Histogramm, Skew & Kurtosis

describe(residuals(mod1))

mod2 %>%
  plot(which = 2)

residuals_mod2 = enframe(residuals(mod2))
residuals_mod2 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(mod2))
    #skew and kurtosis between -1-1 --> normality

  #Linearity

mod1 %>%
  residualPlots()

mod2 %>%
  residualPlots()
    #no significant -->linear! 


  #Homoscedasticity

mod1 %>%
  ncvTest()

mod2 %>%
  plot(which = 3)

mod2 %>%
  ncvTest()
    #no significant --> homoscedasticity


  #No Multicollinearity

mod1 %>%
  vif()

mod2 %>%
  vif()
    #VIF > 3 
      # mod1: all good
      # mod2: not good --> cortisol_serum and cortisol_saliva are higher --> treatment is necessary
           #Data multicollinearity

windows()
dp1 %>%
  select(pain, age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva) %>%
  pairs.panels(col = "red", lm = T)

  #high correlation between the cortisol measures --> no reliable test results
  # due to the theoretical background --> exclude the cortisol_saliva
#new mod2 --> mod2_2
  #re-run everything! 
mod2_2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = dp1)
mod2_2

sm2_2 = summary(mod2_2)
sm2_2

#check assumptions of linear regression again for the new model
  #Normality
  #QQ plot, Histogramm, Skew & Kurtosis

mod2_2 %>%
  plot(which = 2)

residuals_mod2_2 = enframe(residuals(mod2_2))
residuals_mod2_2 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(mod2_2))
    #skew and kurtosis between -1 and 1 --> normally distributed


  #Linearity

mod2_2 %>%
  residualPlots()
    #no significant -->linear! 


  #Homoscedasticity

mod2_2 %>%
  plot(which = 3)

mod2_2 %>%
  ncvTest()
    #no significant --> homoscedasticity


  #No Multicollinearity

mod2_2 %>%
  vif()

    #VIF < 3 --> all good


#model comparison

summary(mod1)$adj.r.squared
summary(mod2_2)$adj.r.squared

    #variance increased, after adding more predictors to the model

  #AIC and anova 

AIC(mod1)
AIC(mod2_2)

    #difference is bigger than 2 which means they are significantly different in model fit, smaller AIC means better model fit and less errors
    #--> Mod2_2 is better and doing better predictions

anova(mod1, mod2_2)

    #highly significant! comparing the amount of variance explained by the two models




#regression equation of model 2_2 (Exercise 6)
    #Y = b0 + b1*X1
      #pain
      #predictors: age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum
      #Y= ????0 + ????1???X1 + ????2*X2 +.+ bn*Xn
      #b0 = intercept
      #b1...bn regression coefficients for predictor
      #Xn = value of predictors

#pain = 1.47 + (-0.04 * age) + 0.16 * sex + (-0.01 * STAI_trait) + 0.11 * pain_cat + (-0.28 * mindfulness) + 0.57 * cortisol_serum
summary(mod2_2)

##report statistics for predictors

confint(mod1)
confint(mod2_2)

lm.beta(mod1)
lm.beta(mod2_2)

summary(mod2_2)
sm1
sm2
