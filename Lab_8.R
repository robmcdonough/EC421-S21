# Lab 8: Wrap-up on Time Series
library(pacman)
p_load(lmtest)
p_load(tidyverse)

##To start, I am NOT loading the tidyverse package (there's a reason for this that will come up soon)

##Last week, we modified our super-cool time series function so that it included autocorrelation:
ts_ar_model=function(t,y_init, a_0,a_1, phi, sigma){
  
  y=c()
  u=c()
  y[1]=y_init
  
  epsilon=rnorm(t,mean=0,sd=sigma)
  u[1]=epsilon[1]
  
  for(i in 2:t){
    u[i]=phi*u[i-1] + epsilon[i]
    y[i]=a_0 + a_1*y[i-1] + u[i]
  }
  
  ts_data=data.frame(time=c(1:t),
                     y=y)
  return(ts_data)
}
##Let's save some data from this time series to mess around with

ts_data=ts_ar_model(t=60,y_init=1,a_0=1, a_1=0.95, phi=0.9, sigma=0.2)

##Notice: when phi gets close to 1, we could say that our autocorrelation is getting 'stronger' (informal language here!)
##What do I mean by 'stronger'? I mean that more and more of the error from 'yesterday' is being preserved/ is persisting
#into today

##We also know how to run regressions on time series data:

lm_ts=lm(data=ts_data, formula=y ~ lag(y, 1))

##as a reminder, when we run a regression like this, each 'observation' of y and x's that gets fed into 
##the regression looks like this:

#y_t=y_(t-1)
#y_(t-1)=y_(t-2)
#y_t(t-2)=y_(t-3)

##We can run this regression and get some output. But, say we're worried about autocorrelation.

##We might want to check if we can 'see' the autocorrelation that we're worried about in this data
##and, we might also want to formally test for that autocorrelation

##Visualizing autocorrelation

##A good first step when we're worried about autocorrelation is just to try to visualize it with a graph,
##just like what we did with heteroskedasticity

##in order to make these graphs, we're going to have to save our residuals into our dataframe

###But, right now there is a problem with our residuals

errors=as.data.frame(resid(lm_ts))

##(this is one of the trickiest kinds of coding problems: our code 'runs', and we don't even get 
##a warning message) 

##what's wrong here? 

##Hint: we ran a time series regression with 60 observations and a 1st order lag. 
##How many residuls should we actually have?

##Answer: We should only have 59 residuals, because we have a 1-period lag. So, we can't use our first observation (when t=1)

##so, here is the following warning:


##IMPORTANT WARNING: MAKE SURE THAT YOU HAVE LOADED THE TIDYVERSE PACKAGE BEFORE USING THE LAG FUNCTION

##the lag function in the tidyverse is 'better' when it comes to time-series econometrics than the one that is
##pre-loaded into R. 

p_load(tidyverse)

lm_ts=lm(data=ts_data, formula=y ~ lag(y, 1))

errors=as.data.frame(resid(lm_ts))

##note: when you specify a lag with the lag function, include a space after the comma 

##Okay, with that out of the way, we're ready to start plotting. 

ts_data$errors=c(NA, resid(lm_ts))

##note: in general, when adding residuals in this way, include an "NA" for the first 'n' observations, where
##n is the number of lags that you have

##EX: if we ran the same model, but included y_{t-2} as an independent variable, then we would have 2 lags, and 
##would need to have 2 NA's.

##There are two types of plots that are useful for visualizing potential autocorrelation:

##1) A scatter/line plot with 'time' on the x-axis, and the 'present' residuals on the y-axis

ggplot(data=ts_data, aes(x=time, y=errors))+
  geom_line()+
  geom_point()+
  theme_minimal()

##what we're looking for here is a 'trend' in our residuals. 

##2) A scatter/line plot, but with the lagged residuals on the x axis, and the 'present'
##residuals on the y-axis. 

##The idea here is even more straightforward. The whole idea of autocorrelation is that our
##errors have some kind of pattern over time. In other words, the errors from yesterday inform the errors
##today. 

##In this case, with simple autocorrelation, we should expect to see a linear relationship when we 
##plot this data

ggplot(data=ts_data, aes(x=lag(errors), y=errors))+
  geom_line()+
  geom_point()+
  theme_minimal()

##remember: if you want your randomly generated data to be identical to
##someone else's, you can always set a seed before using the ts_ar_model
##function set.seed(number_goes_here)

##Now, we're going to talk about a formal test for autocorrelation, the Breusch-Godfrey test

###The intuition for this test is pretty similar to the intuition for our second graph:

##if we have autocorrelation, then 'running a regression' of the data in graph #2 should
##lead to a statistically significant coefficient. 

##note: I'll be running the B-G test using the version that COnnor discusses for 'dynamic models
##with lagged outcome variables". The process is similar for static models, but there are some 
##differences. See COnnor's slides for more details. 


#There are a few steps to this test:

#1) Run the regression that you want to run (but that you're worried might suffer from autocorrelation)

lm_ts=lm(data=ts_data, formula=y ~ lag(y, 1))

#2) Calculate the residuals from the regression in step 1, and add them (optionally) to your dataframe

ts_data$errors=c(NA, resid(lm_ts))

##3) regress the residuals from the first regression on:

###--any explanatory variables from the first regression. Here, that's only lag(y, 1)
###-- an intercept (the lm function will do this automatically)
###-- the lagged residuals (up to the # of lags that you want to test for autocorrelation)

###NOte: this B_G test is a test for a specific order of autocorrelation. For instance
##in this case, if we only include the first lag of our residuals, then we'll be 
##specifically testing for 1st order autocorrelation. 

##if you wanted to test for 2nd order autocorrelation, you'd need to include the second lag
##of your residuals

##here though, since we generated the data, we know that we should be worried about 1st order
##autocorrelation, so I'm jsut going to include lag(errors, 1)

lm_BG=lm(data=ts_data, formula = errors ~ lag(y, 1) + lag(errors, 1))

##4) Use an F (or LM) test with the null hypothesis that there is no autocorrelation

##We can do this easily with the waldtest() function from the lmtest package

waldtest(lm_BG, c("lag(errors, 1)"))

#That second argument is always going to be just a list of all of the lagged error terms that you
#are testing for autocorrelation. Make sure to use quotes, like above

##what's the intuition for what we're doing here?

##we're basically trying to test: do our lagged residuals have an impact on OLS's ability to 
##predict our (non-lagged) residuals

##What we're interested in here is the p value on line 2

##5) Use the results of this wald test to derive a conclusion regarding our hypothesis test

##h0: there is no 1st order autocorrelation (in other words, phi=0)

##h1: phi is not equal to 0. there is first order autocorrelation

##in this case, we reject the null hypothesis, with a p value = ~0


##Note: the BG test is always for a specific order of autocorrelation, If we reject the null hypothesis
##of the BG test, it is only telling us that we reject the null hypothesis of "no autocorrelation
##of the order that you tested for"

##in plain language: the results of this test tell us nothing about whether or not there is 2nd 
##order autocorrelation. 
