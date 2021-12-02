##PART 2
#Exercise 6 and 9


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


#data file 1 load
#exclude the data (same as part 1)
  #household income: really low values --> how to handle (exclude and keep is ok)
#run data and model diagnostic (new variables in there: weight, IQ, household income)
  #reasons for data recoding for sex, pain und STAI_trait: Part 1
  

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")
dp1 = data_sample_1
View(dp1)

summary(dp1)

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

#weight, IQ, household_income: outlier test
  #histogramm and quantile testing
dp1 %>%
  ggplot() + 
  aes(x = household_income) + 
  geom_histogram(bins = 40)

dp1 %>%
  ggplot() + 
  aes(x = weight) + 
  geom_histogram(bins = 40)

dp1 %>%
  ggplot() + 
  aes(x = IQ) + 
  geom_histogram(bins = 40)


Q_hi = quantile(dp1$household_income, probs=c(.25, .75), na.rm = FALSE)
iqr_hi = IQR(dp1$household_income)
up_hi =  Q_hi[2]+1.5*iqr_hi  
low_hi = Q_hi[1]-1.5*iqr_hi

up_hi
low_hi

Q_w = quantile(dp1$weight, probs=c(.25, .75), na.rm = FALSE)
iqr_w = IQR(dp1$weight)
up_w =  Q_w[2]+1.5*iqr_w  
low_w = Q_w[1]-1.5*iqr_w

up_w
low_w


Q_iq = quantile(dp1$IQ, probs=c(.25, .75), na.rm = FALSE)
iqr_iq = IQR(dp1$IQ)
up_iq =  Q_iq[2]+1.5*iqr_iq  
low_iq = Q_iq[1]-1.5*iqr_iq

up_iq
low_iq

    #weigh, household_income, IQ: not excluding any because it is actually realistic that these values are true
    #statistical point of view: outliers are not to bad  

windows()
dp1 %>%
  ggplot() + 
  aes(x = household_income, y = pain) + 
  geom_point() +
  geom_smooth(method = "lm")


windows()
dp1 %>%
  ggplot() + 
  aes(x = weight, y = pain) + 
  geom_point() +
  geom_smooth(method = "lm")


windows()
dp1 %>%
  ggplot() + 
  aes(x = IQ, y = pain) + 
  geom_point() +
  geom_smooth(method = "lm")

mod_iq = lm(pain~ IQ, data = dp1)
mod_iq %>%
  plot(which = 4)

mod_hi = lm(pain~ household_income, data = dp1)
mod_hi %>%
  plot(which = 4)

mod_w = lm(pain~ weight, data = dp1)
mod_w %>%
  plot(which = 4)

  #all of the cook's distance values are smaller than 1 but some are higher than 0.02. 

#hi: 20,97,102
#iq:70, 84,85
#w: 12, 20, 34

dp1 %>%
  slice(c(20, 97, 102, 70, 84, 85, 12, 34))
    # no errors or outliers


summary(dp1)

#Backward regression
  #Exercise 9 (Overfitting)
  #predictors of pain: age, sex, STAI, pain catastrophizing, mindfulness, serum cortisol, weight, IQ, household income
  #check all the assumptions and exclude bad influences/ predictors, step by step making the model smaller  


mod_back_all = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = dp1)
mod_back_all

mod_back_short = step(mod_back_all, direction = "backward")

summary(mod_back_short)

#check assumptions of linear regression
  #normality (-1 and 1)

describe(residuals(mod_back_short))

  #linearity (no sign)

mod_back_short %>%
  residualPlots()

  #Homoscedasticity (no sign)

mod_back_short %>%
  ncvTest()

  #no Multicollinearity (VIF > 3)

mod_back_short %>%
  vif()

#ALL GOOD

#Report: R2, F, df, and p value, predictor statistics
summary(mod_back_short)


#report statistics for predictors

confint(mod_back_short)
lm.beta(mod_back_short)


#new regression model with the predictors out of the backward regression
  #save model in a new R object 

backward_model = mod_back_short
backward_model


#run mod2_2 from part 1 again
  #save model in a new R object 

mod2_2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = dp1)
mod2_2

theory_based_model = mod2_2
theory_based_model


#model comparison (AIC and anova)
  #backward model 1 and 2 

summary(mod_back_all)$adj.r.squared
summary(backward_model)$adj.r.squared

    #small difference in the two models. variance increased a tiny bit after removing predictors

AIC(mod_back_all)
AIC(backward_model)

    #difference > 2!! significant difference in model fit, smaller AIC means better model fit and less errors 
    #backward_model is better! 

#no anova testing because the models are not nested (Exercise 8)


  #backward model and theory-based model

summary(theory_based_model)$adj.r.squared
summary(backward_model)$adj.r.squared

    #no difference compared the variance. It is too small to say there is a difference

AIC(theory_based_model)
AIC(backward_model)

anova(theory_based_model, backward_model)

    #difference > 2 --> significant difference in model fit -->backward model is better

  

##DATA FILE 2
#load data file 2
#run data and model diagnostic
#check for NA's and errors: nothing
#basic descriptives: summary


data_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_2.csv")
dp2 = data_sample_2
View(dp2)

summary(dp2)
describe(dp2)

dp2 = dp2 %>%
  mutate(sex = factor(sex))

str(dp2$sex)


#houseold income: check for outliers!

mod_2_hi = lm(pain~ household_income, data = dp2)
mod_2_hi %>%
  plot(which = 4)

    #cook's distance: 73, 91, 126

dp2 %>%
  slice(c(73, 91, 126))
  #no errors --> keep them  

Q_2_hi = quantile(dp2$household_income, probs=c(.25, .75), na.rm = FALSE)
iqr_2_hi = IQR(dp2$household_income)
up_2_hi =  Q_2_hi[2]+1.5*iqr_2_hi  
low_2_hi = Q_2_hi[1]-1.5*iqr_2_hi

up_2_hi
low_2_hi

#no outliers - all good


#make predictions
#backward and theory-based model: use the regression equations/model and predict the pain

prediction_bk = predict(backward_model,newdata = dp2)
prediction_bk

prediction_tbm = predict(theory_based_model, newdata = dp2)
prediction_tbm

dp2$pain


#another possible way but really long...
age = dp2$age
pain_cat = dp2$pain_cat
mindfulness = dp2$mindfulness
cortisol_serum = dp2$cortisol_serum
newdata_to_predict = as.data.frame(cbind(age, pain_cat, mindfulness, cortisol_serum))
predicted_pain = predict(backward_model, newdata = newdata_to_predict)
cbind(newdata_to_predict, predicted_pain)

#Compare the predicted values with the actual pain ratings 
#Which model was able to predict the actual pain ratings in data file 2 better? --> go to report part 
summary(prediction_bk)
summary(dp2$pain)
summary(prediction_tbm)

#Report: prediction performance for both models
  #several ways how to do that
    #calculate the sum of squared differences between the predicted and the actual pain values (or the sum of absolute differences) for each model
##NOT ASKED FOR! 
pred_1_tbm = predict(theory_based_model)
pred_1_bm = predict(backward_model)

pred_1_bm
pred_1_tbm

RSS_1_tbm = sum((dp1[, "pain"] - pred_1_tbm)^2)
RSS_1_bm = sum((dp1[, "pain"] - pred_1_bm)^2)

RSS_1_tbm
RSS_1_bm

    #tbm is better!RSS number of bm is higher 

##IMPORTANT 
pred_2_tbm = predict(theory_based_model, dp2)
pred_2_bm = predict(backward_model, dp2)

pred_2_bm
pred_2_tbm


RSS_2_tbm = sum((dp2[, "pain"] - pred_2_tbm)^2)
RSS_2_bm = sum((dp2[, "pain"] - pred_2_bm)^2)

RSS_2_tbm
RSS_2_bm

    #backward has more errors and is not so good (number is bigger)

#regression equation for backward model 
# pain = 1.28 + (-0.04 * age) + 0.11 * pain_cat + (-0.27 * mindfulness) + 0.53 * cortisol_serum
summary(backward_model)
summary(theory_based_model)
