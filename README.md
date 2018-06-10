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
library(Amelia)
library(mitools)
library(BaylorEdPsych)
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
First thing is to impute the data.  We want the data above, because all of the categorical variables are dicotmized.  So if we used an imputed data set, for the descriptives, we will need to collapse the categories. 
```{r}
attach(datRandom)

littleTest =  LittleMCAR(datRandom)
littleTest$p.value

m = 5
a.out = amelia(x = datRandom, m=m, noms = c("NumericPackage", "Gen", "Hisp", "Race", "SexOrientation",  "RelStatus", "Education", "Employment"))

summary(a.out)

datAnalysis1 = a.out$imputations$imp1
datAnalysis2 = a.out$imputations$imp2
datAnalysis3 = a.out$imputations$imp3
datAnalysis4 = a.out$imputations$imp4
datAnalysis5 = a.out$imputations$imp5
```


Before you do anything subset the data to the constructs that you want.  It will get to heavy with other measures


Make sure randomization is not different each group.  Need all demographics and pretreatment variables into one data frame.

Used this website to check: https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/

Because all categorical variables are dichotmized, the factor variable actually not necessary, but fine because all zero's the reference category.
Using this p-value assumes that beta is normally distributed, which is probably fine, if we took sample data and then recalcuated the betas even for categorical variables, we would get a randomlly distributed distribution of betas for all the variables.

Created a function that can have all five data sets into the model while only changing the data set.

Create a function that grabs the parameter estmiates and se's for each data set then get the right ones and conduct statistical test.

Need to grab the standard errors, put them into their own dataset.  They are row three so should be able to subset them.

Have the coef and se's in their own dataframes inside each of the randomTests datasets.  
Now I need to cbind each of them creating two 

This is wrong.  You have two set of coefficients and two sets of 

I need to grab the second row, because that is a different set of coefficients and ses, because there are two levels for this analysis. 

I need to stack all of the variables 
```{r}
random_test_fun = function(x){
randomTest = multinom(NumericPackage ~ Age + factor(Gen) + factor(Hisp)+ factor(Race)+ factor(SexOrientation)+ factor(RelStatus)+ factor(Education)+ factor(Employment)+ Pre_RAS_PersonalConfidenceHope+ Pre_RAS_WillingSeekHelp+ Pre_RAS_GoalSuccessOrientation+ Pre_RAS_NoSymptomDomination+ Pre_INQ_PB+ Pre_SSMI_TotalScore+ Pre_SIS_Ideation_TotalScore+ Pre_SIS_RPP_TotalScore+ Pre_INQ_TB, data = x)
}

randomTest1  = random_test_fun(datAnalysis1)
randomTest2 = random_test_fun(datAnalysis2)
randomTest3 = random_test_fun(datAnalysis3)
randomTest4 = random_test_fun(datAnalysis4)
randomTest5 = random_test_fun(datAnalysis5)


combineEst = function(x){
  cof1 = data.frame(summary(x)$coefficients)[1,]
  cof2 = data.frame(summary(x)$coefficients)[2,]
  se1 = data.frame(summary(x)$standard.errors)[1,]
  se2 = data.frame(summary(x)$standard.errors)[2,]
  cof_se = list("cof1" = cof1, "cof2" = cof2, "se1" = se1, "se2" = se2)
  return(cof_se)
}

randomTest1 = combineEst(randomTest1)

randomTest2 = combineEst(randomTest2)
randomTest3 = combineEst(randomTest3)
randomTest4 = combineEst(randomTest4)
randomTest5 = combineEst(randomTest5)

randomTest5
# Need five variables for each data set.  Create a dataset for each coefficient and se first. 
# Need to somehow subset the data so that it grabs.  Just do it with coef1's first then try to generalize
combineCoefSes = function(a,b,c,d,e){
  combineCoef15 = data.frame(rbind(a,b,c,d,e))
}

coefs1 = combineCoefSes(randomTest1$cof1, randomTest2$cof1, randomTest3$cof1, randomTest4$cof1, randomTest5$cof1)

coefs2 = combineCoefSes(randomTest1$cof2, randomTest2$cof2, randomTest3$cof2, randomTest4$cof2, randomTest5$cof2)

ses1 = combineCoefSes(randomTest1$se1, randomTest2$se1, randomTest3$se1, randomTest4$se1, randomTest5$se1)

ses2 = combineCoefSes(randomTest1$se2, randomTest2$se2, randomTest3$se2, randomTest4$se2, randomTest5$se2)

meldAll = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = (1-pnorm(abs(z_stat), 0, 1))*2
  return(p)
}

coefs_Ses1 = meldAll(coefs1, ses1)
coefs_Ses1

coefs_Ses2 = meldAll(coefs2, ses2)
coefs_Ses2

```

First, need to attach the correct data set.

Get descriptives.  Get descriptives for Cat and Binary seperate using functions 
Get descriptives from continous get both mean and sd in one line.

Do all of this five times one for each data set. 

Figure out how to use sapply with double brackets for doing what you have below but with five data sets.

So first create a data frame with all five data sets

1. Run a loop over the five data frames.
2. If possible seperate the variables into cat and non cat and run descriptives on them
```{r}
datAnalysisAll = list(datAnalysis1 =datAnalysis1, datAnalysis2 = datAnalysis2, datAnalysis3 = datAnalysis3, datAnalysis4 = datAnalysis4, datAnalysis5 = datAnalysis5)
head(datAnalysisAll)

b.out = NULL
se.out = NULL
mean.sd.out = NULL
a.out$imputations[[1]]
datAnalysisAll[[]]

for(i in 1:m) {
  mean.sd.out = apply(datAnalysisAll[[i]], 2, mean)  
}
mean.sd.out

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



