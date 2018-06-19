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

describe(datRandom)
apply(datCat, 2, function(x){describe.factor(x)})

datRandom = data.frame(datRandom, datCat)
head(datRandom)
```
Now we are transforming into long format first, because we cannot do this later automatically.
```{r}
datRandom = reshape(datRandom, varying = list(c("Pre_RAS_PersonalConfidenceHope", "Post_RAS_PersonalConfidenceHope"), c("Pre_RAS_GoalSuccessOrientation", "Post_RAS_GoalSuccessOrientation"), c("Pre_INQ_PB", "Post_INQ_PB")), idvar = "ï..RID", direction = "long", times = c(0,1))
```



Here we are imputting the data for the randomization test to see if there is any relationship between treatment assignment and any observed variables.

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
Here we need to get the descriptives, because later on I want to transform them into long form, so do the descriptives now
First, need to attach the correct data set.

So we are creating a list for the five data sets.  Then we create an empty data set that we will fill in later.  Then we run the for loop that says for each data set, because in the list the data sets are identitfied through [[]], input into the new empty data set (use the [[]] here as well to input into) run the apply function over each data set and grab the column means.  Then create a data.frame so it is easier to veiw.  Then write a small function that transposes it into the correct format.  Then do this for sd's and use mi.meld to combine them correctly.

This should be fine, because you will likley need to make all cat vars binary so, you should get percentage of those cateogirzed as 1's, which from there you can get the numbers in each category.
```{r}
datAnalysisAll = list(datAnalysis1 =datAnalysis1, datAnalysis2 = datAnalysis2, datAnalysis3 = datAnalysis3, datAnalysis4 = datAnalysis4, datAnalysis5 = datAnalysis5)
head(datAnalysisAll)

datAnalysisAllDes = lapply(1:m, function(x){subset(datAnalysisAll[[x]], time == 0)})

mean.out = NULL
m =5
for(i in 1:m) {
  mean.out[[i]] = apply(datAnalysisAll[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}
mean.out

descFun = function(x){
  x = data.frame(t(x))
}
mean.out = descFun(mean.out)
mean.out

# no get sds
sd.out = NULL
m =5
for(i in 1:m) {
  sd.out[[i]] = apply(datAnalysisAll[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
sd.out
mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out
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
Data Analysis and data generating process for the analysis starts here

First analysis will be a multilevel model with interaction effect between pre and post and treatement (need to factor treatment) with one outcome.  Random effects will be people nested in time. 

First need to create longitudinal form version of analysis.

There is no post for c("Pre_RAS_WillingSeekHelp", "Post_RAS_WillingSeekHelp"), c("Pre_RAS_NoSymptomDomination", "Post_RAS_NoSymptomDomination")

Need to double check that the prescores are the ones that you want

Checking the means to make sure the long data is going correct. 


We first want to attach the long data set that was created above, because that has the long format that we want.  Then we run two models one with random intercepts only and another with random slopes and intercepts.  We do this by creating a function for both models.

Both models are comparing one treatment versus both other treatements.  Treatment three is the most intensive and treatment two is the least intensive. 

Now I need to loop datAnalysis over everything.
For the DF's make sure if your model changes that you change those.

Because the pt function is giving the probability of something greater than or equal, we need to make everything negative, because you the tail not everything besides the tail.  Making everything negative avoids this problem and just times two for two tails.

Personal Confidence Hope
```{r}
meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  options(scipen=999)
  p = round((2*pt(-abs(t_stat), df = 111)),3)
  return(p)
}

m =5
output = NULL
coef_output = NULL
se_output = NULL
for(i in 1:m){
  output[[i]] = lme(Pre_RAS_PersonalConfidenceHope ~  factor(NumericPackage)*time, random = ~ 1 | ï..RID, data =    datAnalysisAll[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$tTable[,c(1)]
  se_output[[i]]= output[[i]]$tTable[,c(2)]
}

coef_output = data.frame(coef_output)
coef_output = t(coef_output)
coef_output = data.frame(coef_output)
coef_output

se_output = data.frame(se_output)
se_output = t(se_output)
se_output = data.frame(se_output)

meldAllT_stat(coef_output, se_output)
```
Now Pre_RAS_GoalSuccessOrientation
```{r}
m =5
output = NULL
coef_output = NULL
se_output = NULL
datAnalysisAll[[1]]
for(i in 1:m){
  output[[i]] = lme(Pre_RAS_GoalSuccessOrientation ~  factor(NumericPackage)*time, random = ~ 1 | ï..RID, data =    datAnalysisAll[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$tTable[,c(1)]
  se_output[[i]]= output[[i]]$tTable[,c(2)]
}

coef_output = data.frame(coef_output)
coef_output = t(coef_output)
coef_output = data.frame(coef_output)
coef_output

se_output = data.frame(se_output)
se_output = t(se_output)
se_output = data.frame(se_output)

meldAllT_stat(coef_output, se_output)
```
Now with Pre_INQ_PB
```{r}
m =5
output = NULL
coef_output = NULL
se_output = NULL
datAnalysisAll[[1]]
for(i in 1:m){
  output[[i]] = lme(Pre_INQ_PB ~  factor(NumericPackage)*time, random = ~ 1 | ï..RID, data =    datAnalysisAll[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$tTable[,c(1)]
  se_output[[i]]= output[[i]]$tTable[,c(2)]
}

coef_output = data.frame(coef_output)
coef_output = t(coef_output)
coef_output = data.frame(coef_output)
coef_output

se_output = data.frame(se_output)
se_output = t(se_output)
se_output = data.frame(se_output)

meldAllT_stat(coef_output, se_output)
```
Trying to get the degrees of freedom and contrats.  R uses treatment versus all others as the default method, so maybe look into dunnetts method for alpha-pvalue correction.
```{r}
datAnalysis1$NumericPackage = as.factor(datAnalysis1$NumericPackage)
test = lme(Pre_RAS_PersonalConfidenceHope ~  NumericPackage*time, random = ~ 1 | ï..RID, data = datAnalysis1)
test
summary(test)
anova(test, L = c("NumericPackage3:time" = 1, "NumericPackage2:time" = -1))

```
