# Lab 4

###########Heteroskedasticity and R###########
library(pacman)

##For this lab, we need the following packages:
p_load(tidyverse, lfe, robustbase)

?education


##to pick up where we left off, I'm just going to reload and (re)rename our variables

df <- education %>% 
  rename(residents = X1,
         per_capita_income = X2,
         young_residents = X3,
         per_capita_exp = Y,
         state = State)

##Recall that last time, we talked through the Goldfeld-Quandt Test. Let's talk through that again:

#1) Order your data according to the variable you are worried about heteroskedasticity in:

data=arrange(df,per_capita_income)

##same data as before, just reorderd

#2) Figure out how many observations are in the 'top xx%" and bottom xx% of data. We've chosen 25% of our dataset to be our sample size for both 
#We need to know what 1/4 of our dataset is, so we ran this line of code:

n_GQ=as.integer(nrow(data)*1/4)

#3) Run regressions, using only the two samples you've picked out

lm_g1=lm(data=head(data,n_GQ), per_capita_exp ~ per_capita_income + residents + young_residents)

lm_g2=lm(data=tail(data,n_GQ), per_capita_exp ~ per_capita_income + residents + young_residents)

y=a+b_1x_1 +b_2x_2....
##4 Record/calculate the sum of squared errors

e_g1=resid(lm_g1)
e_g2=resid(lm_g2)

sse_g1=sum(e_g1^2)
sse_g2=sum(e_g2^2)

##5)Calculate the GQ-statistic, and then test our hypothesis

##recall, this statistic is constructed as follows:

stat_GQ= (sse_g2/sse_g1)

##and, remember, this statistic is 'distributed according to the F-distribution 
##(translation: now we just need to do an F-test)

##R has a ton of info about this distribution, and if we want to figure out P-values from the F-dist., we
##just use the function pf()

##R has in its working memory a whole bunch of information from a whole bunch of statistical distributions,
##If we were doing a test from a normal distribution, we could use the function pnorm()

## *both* of the degrees of freedom  for this test are equal to n-k, wwhere K is the the #of parameters estimated
##and n is the number of observatons

###SMALL mistake: In lab 3, I mistakenly wrote that our degrees of freedom were
##n_GQ-3, not n_GQ-4. Note that we DO include the intercept term in "K"
##So, 3 independent variables plus an intercept means that K=4


p_GQ=pf(q=stat_GQ, df1=n_GQ-4,df2=n_GQ-4, lower.tail = F)
p_GQ

##6) Interpret the results of our test:

##H0: SSE_g1=SSE_g2 (In other words, the test statistic we computed is equal to 1)
##H1: SSE_g1<SSE_g2 (In other words, the test statistic we computed is greater than 1)

##What is the result of our hypothesis test?

##We reject the null hypothesis (at the 5% level)

##Note: if we want to run this test as a two-tailed test, we do it as follows:

p_GQ=pf(q=stat_GQ, df1=n_GQ-4,df2=n_GQ-4, lower.tail = F)*2

##if we run the test this way, then our null and alternative hypotheses are that:
##H0: SSE_g1=SSE_g2
##H1: SSE_g1 is not equal to SSE_g2

##Breusch-Pagan test

##Inuition: Heteroskedasticity means that we're worried about correlation between our x variables
##and our error terms. To test this, we can just run a regression of our (squared) error terms on our independent variables!
##(OLS Regression is 'designed' (really good at) finding correlations between variables)


##1) First, we need to get our residuals:

##We start by running this regression

lm_frst=lm(data=df,  per_capita_exp ~ per_capita_income + residents + young_residents)


##then we get the residuals

e=resid(lm_frst)

##2) Run a regression of our squared errors on our explanatory variables

lm_BP=lm(I(e^2) ~ per_capita_income + residents + young_residents, data=df)

##Note: if you want R to 'do some math' on one of your variables before you put it in a regression
##you can just put that variable inside the function I()
## I() tells R: do this math before you look at this variable

##3) Record the R^2 value from this second regression

r2_BP=summary(lm_BP)$r.squared

##this value says that our explanatory variables can explain 31.2% of the variation in our squared residuals

##4) Compute the BP-statistic:

##From Connor's slides:
##we use the formula n*r2_BP, where n is the number of observations we have

stat_BP=50*r2_BP

##This test statistic is distributed according to ANOTHER distribution, the chi-squared distribution
#So, we use the following function to extract info from this distribution: pchisq()

p_BP=pchisq(q=stat_BP, df=3, lower.tail=F)

#note: the degrees of freedom for the BP test will always be equal to the number of variables
##(NOT INCLUDING THE INTERCEPT)

##Another note: we ALWAYS run the BP test as a 1-tailed test, with lower.tail=F

##Now we just need to state our null and alternative hypotheses, and interpret the test result


#H0: b1=b2=b3=0
#H1: one or more of b1, b2, and/or b3 are NOT equal to 0.

##In this case, we reject the null hypothesis( with p<0.01)



##Finally, we'll look at the White test:
##The intuition for the White Test?
##Let's do the same thing as in the B-P test, 
##but be much more flexible about the form of heteroskedasticity that we might have


##most of the steps are the same as in the BP test!

##1) run the same 'first' regression as in the BP test:

##lm_frst=lm(data=df,  per_capita_exp ~ per_capita_income + residents + young_residents)

#Note: we already did this!

#2) record the residuals (we already did this, too). We saved  them in R as 'e' on line 108

#3) Run a regression of your squared errors on 3 sets of variables:

##a) your x variables
##b) your x variables, but squared
##c) the interactions of all of your pairs of x variables

lm_W=lm(I(e^2) ~ per_capita_income + residents + young_residents +
          I(per_capita_income^2) + I(residents^2) + I(young_residents^2) +
          per_capita_income:residents + per_capita_income:young_residents+
          residents:young_residents, data=df)

##We can include interaction terms by using I() (for example, I(per_capita_income*residents) will interact those two variables)
##alternatively, we can also include interaction terms by putting a ':' between them

##and, we need to extract the R^2 value

r2_W=summary(lm_W)$r.squared

##Now, we're back to the script of the BP test

##4) Compute the White-statistic:

##we use the formula n*r2_W, where n is the number of observations we have

stat_W=50*r2_W

##This statistic is distributed according to the chi-squared distribution. 

##to get p-value info from this distribution in R, we use the function pchisq()

p_W=pchisq(q=stat_BP, df=9, lower.tail=F)

#note: the degrees of freedom for the W test will always be equal to the number of variables
##from the 'White regression'
##(NOT INCLUDING THE INTERCEPT)

##Another note: we ALWAYS run the W test as a 1-tailed test, with lower.tail=F

##Now we just need to state our null and alternative hypotheses, and interpret the test result
#H0: b1=b2=b3=...b9=0
#H1: one or more of our explanatory variable are NOT equal to 0. 
##(intuitively, when we reject the null hypothesis on either the Breusch-Pagan or the White test, 
##we're concluding that one of our variables explains some of our errors,  which implies heteroskedasticity)

##In this case, we reject the null hypothesis (at the 10% level)


###Dealing with Heteroskedasticity##################


##So: you think you have heteroskedasticity in your data, what can you do?

##The most common solution is to run a heteroskedasticity-robust regression 
##(that is, a regression with heteroskedasticity-robust standard errors)

##It turns out, doing this is very easy in R

##we can the function #felm to automatically adjust our standard errors:

reg_robust=felm(data=df, per_capita_exp ~ per_capita_income + residents + young_residents)

##let's compare these results to those from our classical OLS model

reg_standard=lm(data=df, per_capita_exp ~ per_capita_income + residents + young_residents)

summary(reg_standard)

##we specify "robust=T' to see the heteroskedasticity-robust results
summary(reg_robust, robust=T)

##Notice: our coefficients are the same (as they should be), only our standard errors have changed.


