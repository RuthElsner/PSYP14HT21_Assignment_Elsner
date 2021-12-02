##PART 3

#Exercise 12

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
library(lme4) 	
library(lmerTest)  
library(cAIC4)	
library(r2glmm)	
library(MuMIn) 



#load the data set
data_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_3.csv")
dp3 = data_sample_3
View(dp3)

data_sample_4 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_4.csv")
dp4 = data_sample_4
View(dp4)

#check the data sets for NA's, errors and variable types
#basic descriptives: summary, describe
#outliers

summary(dp3)
describe(dp3)

summary(dp4)
describe(dp4)


#A LOT TO RECODE
  #change sex into factor
  
dp3 = dp3 %>%
  mutate(sex = factor(sex))

str(dp3$sex)
summary(dp3$sex)
  #wrong variable value: Women --> change it to female

dp3 = dp3 %>%
 mutate(sex = fct_recode(sex, 
                     "female"="female", 
                     "male"="male", 
                     "female"="woman"))


dp4 = dp4 %>%
  mutate(sex = factor(sex))

str(dp4$sex)

summary(dp4$sex)

  #household income dp3 "-" --> exclude

filter(dp3, household_income < 0)


dp3 = dp3 %>%
  slice(-c(2))

summary(dp3)
summary(dp3$household_income)


  #change hospital also into factor

dp3 = dp3 %>%
  mutate(hospital = factor(hospital))

str(dp3$hospital)
summary(dp3$hospital)

dp4 = dp4 %>%
  mutate(hospital = factor(hospital))

str(dp4$hospital)
summary(dp4$hospital)

  #check for outliers (weight, IQ, household income) --> all good!!

Q_hi = quantile(dp3$household_income, probs=c(.25, .75), na.rm = FALSE)
iqr_hi = IQR(dp3$household_income)
up_hi =  Q_hi[2]+1.5*iqr_hi  
low_hi = Q_hi[1]-1.5*iqr_hi

up_hi
low_hi

Q_w = quantile(dp3$weight, probs=c(.25, .75), na.rm = FALSE)
iqr_w = IQR(dp3$weight)
up_w =  Q_w[2]+1.5*iqr_w  
low_w = Q_w[1]-1.5*iqr_w

up_w
low_w


Q_iq = quantile(dp3$IQ, probs=c(.25, .75), na.rm = FALSE)
iqr_iq = IQR(dp3$IQ)
up_iq =  Q_iq[2]+1.5*iqr_iq  
low_iq = Q_iq[1]-1.5*iqr_iq

up_iq
low_iq

Q_hi = quantile(dp4$household_income, probs=c(.25, .75), na.rm = FALSE)
iqr_hi = IQR(dp4$household_income)
up_hi =  Q_hi[2]+1.5*iqr_hi  
low_hi = Q_hi[1]-1.5*iqr_hi

up_hi
low_hi

Q_w = quantile(dp4$weight, probs=c(.25, .75), na.rm = FALSE)
iqr_w = IQR(dp4$weight)
up_w =  Q_w[2]+1.5*iqr_w  
low_w = Q_w[1]-1.5*iqr_w

up_w
low_w


Q_iq = quantile(dp4$IQ, probs=c(.25, .75), na.rm = FALSE)
iqr_iq = IQR(dp4$IQ)
up_iq =  Q_iq[2]+1.5*iqr_iq  
low_iq = Q_iq[1]-1.5*iqr_iq

up_iq
low_iq



#Cook's distance for all of them 
plot1 <- lm(pain ~ age, data = dp3)
plot1 %>%
  plot(which = 4)
#101, 105, 146

plot2 <- lm(pain ~ sex, data = dp3)
plot2 %>%
  plot(which = 4)
#101, 143, 146

plot3 <- lm(pain ~ STAI_trait, data = dp3)
plot3 %>%
  plot(which = 4)
#125, 143, 162

plot4 <- lm(pain ~ pain_cat, data = dp3)
plot4 %>%
  plot(which = 4)
#101, 116, 143

plot5 <- lm(pain ~ mindfulness, data = dp3)
plot5 %>%
  plot(which = 4)
#30, 164, 194

plot6 <- lm(pain ~ cortisol_serum, data = dp3)
plot6 %>%
  plot(which = 4)
#81, 101, 143

dp3 %>%
  slice(c(30, 81, 101, 105, 116, 125, 143, 146, 162, 164, 194))

  # no extreme outliers, keep them 

#Building the linear mixed model
# create a random intercept model
  #fixed effect predictors: age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum
  #random intercept: hospital

mod_rnd_int_3 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = dp3)
mod_rnd_int_3

confint(mod_rnd_int_3)

#model diagnostic - NOT NECESSARY


#model coefficients and the confidence intervals of the coefficients for all fixed effect predictors
confint(mod_rnd_int_3)
summary(mod_rnd_int_3)

r2beta(mod_rnd_int_3, method = "nsj", data = dp3)

#variance (marginal R² and conditional R²)
r.squaredGLMM(mod_rnd_int_3)

AIC(mod2_2)



#predict pain in the dp4 with regression equation obtained on dp3
prediction = predict(mod_rnd_int_3, dp4, allow.new.levels = TRUE)
prediction


#the variance explained by the model on dp4
  #1-(RSS/TSS)
    #compare to dp3


RSS_2 = sum((dp4[,"pain"] - prediction)^2)
RSS_2

mod_mean_2 = lm(pain ~ 1, data = dp4)
TSS_2= sum((dp4$pain - predict(mod_mean_2))^2)
TSS_2

R2_2 = 1 - (RSS_2/TSS_2)
R2_2


#new linear mixed effects model on dp3 predicting pain
  #include most influential predictor of previous model- mod_rnd_int_3
      

mod_rnd_int_3 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = dp3)
mod_rnd_int_3
summary(mod_rnd_int_3)
coefTable(mod_rnd_int_3)

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

stdCoef.merMod(mod_rnd_int_3)


      #look for the highest standardized beta coefficients (stdcoef) in the previous model --> cortisol serum 

  #random intercept and random slope - only one!!!

mod_rnd_slp_3 = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), data = dp3)
mod_rnd_slp_3
summary(mod_rnd_slp_3)

#visualize the fitted regression lines for each hospital separately

pred_slp = predict(mod_rnd_slp_3)

windows()
dp3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 10) + geom_line(color = "red",
                                                       aes(y = pred_slp, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)


