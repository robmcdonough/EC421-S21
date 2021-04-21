# Lab 3 

###########Heteroskedasticity and R###########
library(pacman)

##For this lab, we need the following packages:
p_load(tidyverse, lfe, robustbase)

##we're going to use some educational data from a good econometrics textbook
?education

##Note: this data comes pre-loaded with the robustbase package: as soon as you load the robustbase package,
##you should be able to look at, load, save the dataset

head(education)

df=education

##It's always a good idea when using new data (or, unfamiliar data) to turn the names into something sensible

##Let's talk about 3 quick ways to rename variables

##1) If we want to rename a single variable (ie, a column in a dataframe), we can use the names() function

##recall that the names() function lets us look at variable names:
names(df)

##to use this function to rename 1 variable, we do the following:

names(df)[3]='residents'

names(df)

##2)If we want to rename **every** variable, we can use the names() function with a list of names

names(df)=c("state","region","residents","per_capita_income","young_residents","per_capita_exp")

##3) We can also use a function from the tidyverse

df= education %>%
  rename(resident=X1,
         per_capita_income=X2,
         young_residents=X3,
         per_capita_exp=Y)

##notice that the syntax for rename() is new_name=old_name, and we do not need to use quotes

#####Looking for Heteroskedasticity

##A good first step when considering if heteroskedasticity may imopact your data is to try to visualize
##the heteroskedasticity you're worried about

##Loosely speaking, heteroskedasticity is when: our disturbances are not distributed in the same way
##across al of our data


##(in plain language: our disturbances/ the variance of our y variable changes with one of our variables)

##With this loose interpretation in mind, we might be able to see heteroskedasticity 
##if we plot the disturbances against some other variable

##to do this, we need to run a quick regression first, and extract the residuals

reg=lm(per_capita_exp ~ per_capita_income, data=df)

##to extract the residuals from a regression, we use the resid() function

errs=resid(reg)
errs

##with these residuals, we can make a quick plot to check for heteroskedasticity
ggplot(data=df, aes(x=per_capita_income, y=errs))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_minimal()

##geom_smooth is a very particular kind of plot in ggplot2. It fits a line to your data, and also displays
##the confidence interval around that fitted line. There are many methods to fit a line to data, and we 
##need to tell R which one we want to use. But remember that fitting a line to data is one way of describing 
##what OLS regression does. So, we specify the method="lm". 

##Notice that here, the confidence interval for out data gets larger for the extremely high and low per_capita_income
##values (it looks a bit like an hourglass turned on its side). This definitely makes me worry about heterokedasticity

##This is a good first step, but we probably want to test for heteroskedsticity a bit more formally

##The first test that we'll talk about is the Goldfeld-Quandt test

###Intuition: We want to be sure that our variances (our disturbances) aren't changing with our predictors. 
##If we have concerns about this happening with one specific predictor (like here, with per capita income).
##we can test that concern by taking different subsets of our data and asking the question: 
##do the disturbances from these subsets seem significantly different from one another

##To complete the Goldfeld Quandt test, we complete the following steps:

##1) We order our data by our variable of interest. Let's use income

##to order data by a numeric variable, we can use the arrange() function:

data = df %>% arrange(per_capita_income)

##note: if we want to order data by descending order, we would just write the code
##          data = df %>% arrange(desc(per_capita_income))
##      (IMPORTANT: for the Goldfeld-Quandt test, just arrange the data in ascending order)


##2) Take two subsets of the data, based on the fraction of data you want to use. 
##We're going to use 1/2 of the data (the top 1/4 and the bottom 1/4)

n_GQ= as.integer(nrow(data)*1/4)

##self check: nrow() tells us how many rows are in our data. But why did I need to include the as.integer()

##hint: try running the code without the as.integer() part

##Answer: we can only run regressions on **whole numbers** of observations

##3) Run two regressions, with the same regression formula, where the only difference is the subset of data you use:

lm_g1=lm(data=head(data,n_GQ), per_capita_exp ~ per_capita_income + resident + young_residents)

lm_g2=lm(data=tail(data,n_GQ), per_capita_exp ~ per_capita_income + resident + young_residents)

##4) Now we need to extract the SSR (or SSE) from each regression

##This takes a few lines of code
##4.1) extract residuals
e_g1=resid(lm_g1)
e_g2=resid(lm_g2)
###4.2) square the residuals, then add them together

sse_g1=sum(e_g1^2)
sse_g2=sum(e_g2^2)

##5) calculate the G-Q test statistic, and the associated p-value
##Recall from lecture that the G-Q test statistic is distributed according to the F distribution,
##and has the form:

stat_GQ=sse_g2/sse_g1

##And, *both* of the degrees of freedom  are equal to n-k, where K is the the #of parameters estimated
##and n is the number of observatons



##R has in its working memory a whole bunch of information from a whole bunch of statistical distributions,
##including the F distribution

##for instance, to calculate p-values from an F-distribution, we can just use function pf()
##(if we were doing a test that required a normal distribution, we would use pnorm())

###SMALL mistake: In lab, I mistakenly wrote that our degrees of freedom were
##n_GQ-3, not n_GQ-4. Note that we do include the intercept term in "K"
##So, 3 independent variables plus an intercept means that K=4

p_GQ=pf(q=stat_GQ, df1=n_GQ-4,df2=n_GQ-4, lower.tail = F)

##we can interpret this value as a p-value

p_GQ


##6) State the null and alternative hypothesis, and come to a conclusion

#H0: stat_GQ=1

#H1: stat_GQ > 1

##Note: we wrote this as a one-sided test. We'll start lab $ by talking about
##turning this into a 2-sided test. 

##In this case, we reject the null hypothesis (p<0.05)