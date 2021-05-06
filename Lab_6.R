# Lab 6: A bit more on Time Series

library(pacman)
p_load(tidyverse, ggthemes)


##Recap: Last week we thought about the following 'time series' process

##y_t= alpha_0+ alpha_1*y_{t-1}+ u_t ~N(0, sigma)

##In terms of the  Autoregressive distributed-lag (ADL) models Connor has 
##been teaching you about, we normally refer to these models as ADL(p,q) models

####Quick question for you to answer: what are the right numbers for p and q here?
 
###Answer: 
###p is always going to refer to the greatest number of lags in the dependent variable that we see on the RHS of the '='
###q is always going to refer to the greatest number of lags in any *independent variables* that we see of the RHS of '='
###so, this is an ADL(1,0)

##and, we wrote a function in R that simulates this time series
ts_model=function(t,y_init, a_0,a_1, sigma){
  ##first we created some space to save our time series
  y=c()
  ##then, we told R that the 'first' value in our time series 
  y[1]=y_init
  
  ##after that, we generated a set of error terms 
  u=rnorm(t,mean=0,sd=sigma)
  
  ##then, we had R use a for-loop to fill in the rest of the time series
  for( i in 2:t){
    ##the idea with our header here is that R is going to need to fill in
    ##all of the remaining time series values from time period 2 to the final
    ##time period, t. 
    y[i]=a_0 + a_1*y[i-1] + u[i]
    ##now, we can tell R: to find out what y should be in each other time period (each period after t=1), 
    ##look at the last entry in the list (y_{t-1})
  }
  ##the nice thing about for-loops is that they go in order. So, the first thing this for-loop
  ##is going to figure out is y_2 (and, it can do that successfully, because we already filled in y_1)
  
  ##all that was left was to save the data in a nice format
  ts_data=data.frame(time=c(1:t),
                     y=y)
  ##then, return that dataframe
  return(ts_data)
}


ts_model(200,5,.39,.87,0.04)

##it can be very useful to visualize your time series with a line graph(especially because we created this data)

ggplot(aes(x=time,y=y),data=ts_model(200,5,.39,.87,0.04))+
  geom_line(color='red')+
  theme_minimal()

##it looks like our time series function is working as intended


###Now, let's say that we're econometricians who see a dataset like this in the real world, and let's say that we have 
##a strong belief that this time series is coming from a model that looks like the 'true' model

##that is, we really believe that y_t= alpha_0+ alpha_1*y_{t-1}+ u_t (we really believe that if we know y in t-1,
##then we should be able to predict y in t)

##if that's the case, then we would really like to run a regression that can estimate our coefficients 
#(alpha_0 and alpha_1)

##To do that (in R) we need to be able to tell R that we want to regress y on y_{t-1}
##(that is, we need to be able to tell R that the independent variable is just the dependent variable, from one period ago)

##Luckily, we can do this in R very easily, using the same old lm() function, along with a new function: lag()

##The lag function works very much like the I(). The lag function tells R: look 'one row up' for this variable

##WHat do I mean by that?
ts_data=ts_model(30,5,.39,.87,0.04)

##With that in mind, everything else in our regression code stays the same:

lm_ts=lm(y~lag(y), data=ts_data)

summary(lm_ts)

##So, R estimated alpha_0 and alpha_1


###one thing to point out: our estimate for alpha_1 is **close** to the true coefficient, but it isn't perfect

###Remember: when a time-series OLS model includes lagged versions of the dependent variable, then OLS is 
###biased. 

##But, luckily, we can show that OLS is still **consistent** in this case. 


##But, one thing that we do know is that our coefficients in this [regression] are **consistent**

##One intuitive way to think about consistency is: given enough observations, our estimated coefficients eventually converge to
##the 'correct' coefficients

##One nice thing about simulating time-series data is that we can see consistency by making our 't' bigger


ts_data_big=ts_model(t=1000000,5,.39,.87,0.04)

lm_adl_big=lm(y~lag(y), data=ts_data_big)

summary(lm_adl_big)


###NOTES:

##1) IF we wanted to include a lag by more than 1 period, we just use the code 


lag(y,2)

##for instance, this will tell R to use a 2-period lag (y_{t-2})

##2) If we wanted to use lags for any other variables, we can still just use the lag function 
    ###in other words, we can still include other variables in a time-series regression in R, just like we did before. 
