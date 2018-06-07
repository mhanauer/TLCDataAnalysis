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
library(prettyR)
library(descr)
```
Need to figure out if there are any outliers

Age has a goofy one with 451.  Need to fix that.  Assuming it is 45.  

Nothing else seems too goofy
```{r}
#setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/TLC")
#datTLC = read.csv("Target 1_SPSS Analysis File_4.20.18.csv", header = TRUE)
head(datTLC)
summary(datTLC)
datTLC$Age = ifelse(datTLC$Age == 451, 45, datTLC$Age)
```

First thing is to impute the data.  So subset the data that you want. 
```{r}
attach(datTLC)
datAnalysis = data.frame(NumericPackage, Age, Gen, Hisp, Race, SexOrientation, RelStatus, Education, Employment, Pre_RAS_PersonalConfidenceHope, Pre_RAS_WillingSeekHelp, Pre_RAS_GoalSuccessOrientation, Pre_RAS_NoSymptomDomination, Pre_INQ_PB, Pre_SSMI_TotalScore, Pre_SIS_Ideation_TotalScore, Pre_SIS_RPP_TotalScore, Pre_INQ_TB, Post_RAS_PersonalConfidenceHope, Post_RAS_GoalSuccessOrientation, Post_INQ_PB)

library(Amelia)
library(mitools)
m = 5
a.out = amelia(x = datAnalysis, m=m, noms = c("NumericPackage", "Gen", "Hisp", "Race", "SexOrientation",  "RelStatus", "Education", "Employment"))

summary(a.out)

```



Need to collapse some categories for assessing differences in randomization many of them only have one person.  Just making all of them binary would probably be the easiest.

Once you figure out the final data set, you can get rid of the missing data before any analyses.  So make sure you have all the variables into one dataset and then delete missing observations.

So we want to create a seperate data set for the actual analysis and the testing of whether a variable is related to treatment assignment, because we don't want the dicotomous for the descriptives.

After getting rid of the missing data and then creating a new data set for testing random assignment, we want to dichomize the categorical variables to test whether treatment assignment is any different.

Then get rid of the previous demographics, because those are not dichotmoized and substitute for the new one.

To dichomoize the variables, I am saying that we are taking the first category versus all other categories. Should be fine not super interested in the actual meaning.  The first value is likely the most popular so this method should work.
```{r}
attach(datTLC)
datAnalysis = data.frame(NumericPackage, Age, Gen, Hisp, Race, SexOrientation, RelStatus, Education, Employment, Pre_RAS_PersonalConfidenceHope, Pre_RAS_WillingSeekHelp, Pre_RAS_GoalSuccessOrientation, Pre_RAS_NoSymptomDomination, Pre_INQ_PB, Pre_SSMI_TotalScore, Pre_SIS_Ideation_TotalScore, Pre_SIS_RPP_TotalScore, Pre_INQ_TB, Post_RAS_PersonalConfidenceHope, Post_RAS_GoalSuccessOrientation, Post_INQ_PB)


datRandom =  datAnalysis


datCat= data.frame(datRandom[,3:9])
head(datCat)

datCat = data.frame(apply(datCat, 2, function(x){ifelse(x > 1, 0, 1)}))
head(datCat)

datRandom[,3:9] = NULL

datRandom = data.frame(datRandom, datCat)
head(datRandom)
```
Before you do anything subset the data to the constructs that you want.  It will get to heavy with other measures


Make sure randomization is not different each group.  Need all demographics and pretreatment variables into one data frame.

Used this website to check: https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/

Because all categorical variables are dichotmized, the factor variable actually not necessary, but fine because all zero's the reference category.
Using this p-value assumes that beta is normally distributed, which is probably fine, if we took sample data and then recalcuated the betas even for categorical variables, we would get a randomlly distributed distribution of betas for all the variables.
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
```{r}
attach(datAnalysis)
datCatBinary = data.frame(NumericPackage, Gen, Hisp, Race, SexOrientation, RelStatus, Education, Employment)
datCatBinary = apply(datCatBinary, 2, function(x){describe.factor(x)})
datCatBinary

datContinue = data.frame(Age, Pre_RAS_PersonalConfidenceHope, Pre_RAS_WillingSeekHelp, Pre_RAS_GoalSuccessOrientation, Pre_RAS_NoSymptomDomination, Pre_INQ_PB, Pre_SSMI_TotalScore, Pre_SIS_Ideation_TotalScore, Pre_SIS_RPP_TotalScore, Pre_INQ_TB, Post_RAS_PersonalConfidenceHope, Post_RAS_GoalSuccessOrientation, Post_INQ_PB)


datContinue = round((data.frame(apply(datContinue, 2, function(x){c(mean(x), sd(x))}))),2)
datContinue
```
Centering all the continous variables.  Create new data set for datAnalysis, because you want to grab the continious variables more easily.

Not sure why but some missing data are back into the categorical data.  However, above the total data are 104 and here they are also 104.  Shouldn't make a difference, because if the data are missing they will be missing no matter what.
```{r}
datContinue = data.frame(Age, Pre_RAS_PersonalConfidenceHope, Pre_RAS_WillingSeekHelp, Pre_RAS_GoalSuccessOrientation, Pre_RAS_NoSymptomDomination, Pre_INQ_PB, Pre_SSMI_TotalScore, Pre_SIS_Ideation_TotalScore, Pre_SIS_RPP_TotalScore, Pre_INQ_TB, Post_RAS_PersonalConfidenceHope, Post_RAS_GoalSuccessOrientation, Post_INQ_PB)

datCatBinary = data.frame(NumericPackage, Gen, Hisp, Race, SexOrientation, RelStatus, Education, Employment)


datContinue = data.frame(scale(datContinue, center = TRUE, scale = FALSE))
datContinue
datCatBinary = data.frame(NumericPackage, Gen, Hisp, Race, SexOrientation, RelStatus, Education, Employment)
datAnalysis = data.frame(datCatBinary, datContinue)
sum(is.na(datAnalysis))
datAnalysis = na.omit(datAnalysis)
dim(datAnalysis)
summary(datAnalysis)
```


First analysis will be a multilevel model with interaction effect between pre and post and treatement (need to factor treatment) with one outcome.  Random effects will be people nested in time. 

First need to create longitudinal form version of analysis.

There is no post for c("Pre_RAS_WillingSeekHelp", "Post_RAS_WillingSeekHelp"), c("Pre_RAS_NoSymptomDomination", "Post_RAS_NoSymptomDomination")

Need to double check that the prescores are the ones that you want

Checking the means to make sure the long data is going correct.  They are 
```{r}
datTLCLong = reshape(datAnalysis, varying = list(c("Pre_RAS_PersonalConfidenceHope", "Post_RAS_PersonalConfidenceHope"), c("Pre_RAS_GoalSuccessOrientation", "Post_RAS_GoalSuccessOrientation"), c("Pre_INQ_PB", "Post_INQ_PB")), idvar = "ï..RID", direction = "long", times = c(0,1))

head(datTLCLong)
attach(datTLCLong)
dim(datTLCLong)

compmeans(Pre_RAS_PersonalConfidenceHope, time)
mean(datAnalysis$Post_RAS_PersonalConfidenceHope)
```
Create binary version of the treatments not working with treatments.
Double check that this treatment assignment is the same.
Now I need to create z-scores for the continous variables to reduce collinarity 
```{r}
Treat1 = ifelse(NumericPackage == 1, 1, 0)
Treat2 = ifelse(NumericPackage == 2, 1, 0)
Treat3 = ifelse(NumericPackage == 3, 1, 0)
datTLCLong = data.frame(datTLCLong, Treat1, Treat2, Treat3)

```


We first want to attach the long data set that was created above, because that has the long format that we want.  Then we run two models one with random intercepts only and another with random slopes and intercepts.  We do this by creating a function for both models.

Both models are comparing one treatment versus both other treatements.  Treatment three is the most intensive and treatment two is the least intensive. 
```{r}
attach(datTLCLong)
head(datTLCLong)
multi_fun_random_intercept = function(x){
  output = lme(x ~  Treat3*time + Treat2*time, random = ~ 1 | ï..RID)
  summary(output)
}

Conf_random_intercept = multi_fun_random_intercept(Pre_RAS_PersonalConfidenceHope)
Conf_random_intercept

Orien_random_intercept = multi_fun_random_intercept(Pre_RAS_GoalSuccessOrientation)
Orien_random_intercept

INQ_PB_random_intercept = multi_fun_random_intercept(Pre_INQ_PB)
INQ_PB_random_intercept

multi_fun_random_intercept_slope = function(x){
  output = lme(x ~  Treat3*time + Treat2*time, random = ~ time | ï..RID)
  summary(output)
}

Conf_random_intercept_slope = multi_fun_random_intercept_slope(Pre_RAS_PersonalConfidenceHope)
Conf_random_intercept_slope

Orien_random_intercept_slope = multi_fun_random_intercept_slope(Pre_RAS_GoalSuccessOrientation)
Orien_random_intercept_slope

INQ_PB_random_intercept_slope = multi_fun_random_intercept_slope(Pre_INQ_PB)
INQ_PB_random_intercept_slope
```
Now try repeated measures anova.  Need to worry about unbalanced so need to use Anova function from car with type III standard errors

Not really sure how to get repeated measures with unbalanced design into R or SPSS.  This is right because of no balance, but wrong, because no repeated measures component.

Tried with ezANOVA and won't work with unbalanced design recommends mixed design.
```{r}
attach(datTLCLong)

library(car)

aovFun = function(x){
  options(contrasts=c('contr.sum','contr.poly'))
  aovResults=Anova(lm(x ~ Treat3*time + Treat2*time, data = datTLCLong, type   = "III"))
  aovResults
}

outcomes = data.frame(Pre_RAS_PersonalConfidenceHope, Pre_RAS_GoalSuccessOrientation, Pre_INQ_PB)
outcomesAnova = apply(outcomes, 2, aovFun)
outcomesAnova


library(ez)
ezANOVA(data = datTLCLong, dv = .(Pre_RAS_PersonalConfidenceHope), wid = .(ï..RID), within = .(time), between = .(Treat1), type = 3)

```
Try with t-tests with difference scores

Package 3 (C) is the main one 

Need to get the original data first, because we want the difference score, which is easier to gather with the wide format data.  Then transform the package numbers into their own variable.

Again we are comparing the differences between one treatment and the other two treatments.

First looking at the assumptions for a t-test, then conducting the t-tests.
```{r}
attach(datAnalysis)
library(e1071)
Treat1 = ifelse(NumericPackage == 1, 1, 0)
Treat2 = ifelse(NumericPackage == 2, 1, 0)
Treat3 = ifelse(NumericPackage == 3, 1, 0)


diffConfi = Post_RAS_PersonalConfidenceHope-Pre_RAS_PersonalConfidenceHope
diffOrien =  Post_RAS_GoalSuccessOrientation-Pre_RAS_GoalSuccessOrientation 
diffINQ =  Post_INQ_PB-Pre_INQ_PB
diffDat = data.frame(diffConfi, diffOrien, diffINQ)

hist_kurt = function(x){
  hist(x) 
  kurtosis(x)
}
diff_hist_kurt = apply(diffDat, 2, hist_kurt)
diff_hist_kurt


t_testDiff = function(x){
  t_test_treat3 = t.test(x~Treat3)
  t_test_treat2 = t.test(x~Treat2)
  output = list(t_test_treat3, t_test_treat2)
  return(output)
}

diff_t_test = apply(diffDat, 2, t_testDiff)
diff_t_test
```
Try subsetting the data and testing 3 versus 1, 3 versus 2, and 2 versus 1.
```{r}

```



