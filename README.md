---
title: "TLC Data Analysis"
output: ''
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library the packages that you need
```{r}
library(foreign)
library(nnet)
library(ggplot2)
library(prettyR)
library(nlme)
```


First figure out who is randomized to different treatments
Could have a difference model and then use the therapist ID as the multilevel component

Need to figure out if there are any outliers

Age has a goofy one with 451.  Need to fix that.  Assuming it is 45.  

Nothing else seems too goofy
```{r}
#setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/TLC")
#datTLC = read.csv("Target 1_SPSS Analysis File_4.20.18.csv", header = TRUE)
head(datTLC)
#summary(datTLC)
datTLC$Age = ifelse(datTLC$Age == 451, 45, datTLC$Age)
```


Need to collapse some categories for assessing differences in randomization many of them only have one person.  Just making all of them binary would probably be the easiest.

Once you figure out the final data set, you can get rid of the missing data before any analyses.  So make sure you have all the variables into one dataset and then delete missing observations.

So we want to create a seperate data set for the actual analysis and the testing of whether a variable is related to treatment assignment, because we don't want the dicotomous for the descriptives.

After getting rid of the missing data and then creating a new data set for testing random assignment, we want to dichomize the categorical variables to test whether treatment assignment is any different.

Then get rid of the previous demographics, because those are not dichotmoized and substitute for the new one.
```{r}
attach(datTLC)
datAnalysis = data.frame(NumericPackage, Age, Gen, Hisp, Race, SexOrientation, RelStatus, Education, Employment, Pre_RAS_PersonalConfidenceHope, Pre_RAS_WillingSeekHelp, Pre_RAS_GoalSuccessOrientation, Pre_RAS_NoSymptomDomination, Pre_INQ_PB, Pre_SSMI_TotalScore, Pre_SIS_Ideation_TotalScore, Pre_SIS_RPP_TotalScore, Pre_INQ_TB, Post_RAS_PersonalConfidenceHope, Post_RAS_GoalSuccessOrientation, Post_INQ_PB)


dim(datAnalysis)
datAnalysis = na.omit(datAnalysis)
dim(datAnalysis)
sum(is.na(datAnalysis))
datRandom =  datAnalysis


datCat= data.frame(datRandom[,3:9])

datCat = data.frame(apply(datCat, 2, function(x){ifelse(x > 1, 0, 1)}))
head(datCat)

datRandom[,3:9] = NULL

datRandom = data.frame(datRandom, datCat)
head(datRandom)
```
Before you do anything subset the data to the constructs that you want.  It will get to heavy with other measures


Make sure randomization is not different each group.  Need all demographics and pretreatment variables into one data frame.  May need to get rid of pretreatment scores, because model run out of degrees of freedom.

Used this website to check: https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/

How is the model dealing with missing data.
```{r}
test = multinom(NumericPackage ~ Age + factor(Gen) + factor(Hisp)+ factor(Race)+ factor(SexOrientation)+ factor(RelStatus)+ factor(Education)+ factor(Employment)+ Pre_RAS_PersonalConfidenceHope+ Pre_RAS_WillingSeekHelp+ Pre_RAS_GoalSuccessOrientation+ Pre_RAS_NoSymptomDomination+ Pre_INQ_PB+ Pre_SSMI_TotalScore+ Pre_SIS_Ideation_TotalScore+ Pre_SIS_RPP_TotalScore+ Pre_INQ_TB, data = datRandom)
summary(test)
z = summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p
```

First, need to attach the correct data set.

Get descriptives.  Get descriptives for Cat and Binary seperate using functions 
Get descriptives from continous get both mean and sd in one line.

Pre_INQ_TB has an NA and na.rm = TRUE for apply isn't working, but probably won't matter later on, because you will only these analyses with complete data.
```{r}
attach(datAnalysis)
datCatBinary = data.frame(NumericPackage, Gen, Hisp, Race, SexOrientation, RelStatus, Education, Employment)
datCatBinary = apply(datCatBinary, 2, function(x){describe.factor(x)})
datCatBinary

datContinue = data.frame(Age, Pre_RAS_PersonalConfidenceHope, Pre_RAS_WillingSeekHelp, Pre_RAS_GoalSuccessOrientation, Pre_RAS_NoSymptomDomination, Pre_INQ_PB, Pre_SSMI_TotalScore, Pre_SIS_Ideation_TotalScore, Pre_SIS_RPP_TotalScore, Pre_INQ_TB, Post_RAS_PersonalConfidenceHope, Post_RAS_GoalSuccessOrientation, Post_INQ_PB)


datContinue = round((data.frame(apply(datContinue, 2, function(x){c(mean(x), sd(x))}))),2)
datContinue
```
First analysis will be a multilevel model with interaction effect between pre and post and treatement (need to factor treatment) with one outcome.  Random effects will be people nested in time. 

First need to create longitudinal form version of analysis.

There is no post for c("Pre_RAS_WillingSeekHelp", "Post_RAS_WillingSeekHelp"), c("Pre_RAS_NoSymptomDomination", "Post_RAS_NoSymptomDomination")

Need to double check that the prescores are the ones that you want
```{r}

datTLCLong = reshape(datAnalysis, varying = list(c("Pre_RAS_PersonalConfidenceHope", "Post_RAS_PersonalConfidenceHope"), c("Pre_RAS_GoalSuccessOrientation", "Post_RAS_GoalSuccessOrientation"), c("Pre_INQ_PB", "Post_INQ_PB")), idvar = "ï..RID", direction = "long", times = c(0,1))

head(datTLCLong)
attach(datTLCLong)
dim(datTLCLong)
```
Create binary version of the treatments not working with treatments.
```{r}
T1 = ifelse(NumericPackage == 1, 1, 0)
T2 = ifelse(NumericPackage == 2, 1, 0)
T3 = ifelse(NumericPackage == 3, 1, 0)
datTLCLong = data.frame(datTLCLong, T1, T2, T3)
```


So in theory, we want a simple random slopes and intercepts model 
```{r}
NumericPackage = factor(NumericPackage)
relevel(NumericPackage, ref = "3")
perConf = lme(Pre_RAS_PersonalConfidenceHope ~ T1*time + T2*time, random =~ time  | ï..RID, data = datTLCLong)
summary(perConf)

```




