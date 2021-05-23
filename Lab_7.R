# Lab 7: Simulating Data, Parallelizing, and more Time Series
library(pacman)
p_load(tidyverse,future.apply, parallel,data.table)

##Last week we kept working with the function we made:
ts_model=function(t,y_init, a_0,a_1, sigma){
  
  y=c()
  y[1]=y_init
  
  u=rnorm(t,mean=0,sd=sigma)
  
  for( i in 2:t){
    y[i]=a_0 + a_1*y[i-1] + u[i]
  }
  
  ts_data=data.frame(time=c(1:t),
                     y=y)
  return(ts_data)
}

ts_data=ts_model(60,5,.39,.87,0.04)

##and we used it to run a time-series regression
lm_adl_1=lm(y~lag(y), data=ts_data)

summary(lm_adl_1)


###Autocorrelation

##Recall that the time series process that we used to generate our time series looked like this:
y[i]=a_0 + a_1*y[i-1] + u[i]

##But, when we talked about our random noise, we said that u_t was distributed
##~N(0,sigma)

##Specifically, our noise terms satisfied the condition of contemporaneous exogeneity


##Autocorrelation is one way that we can violate this condition. When we have autocorrelation,
##The error term in our model is correlated over time. A good example of this is 
##what is called an autoregressive(AR) model. For instance, this is an AR(1) model:

y[i]=a_0 + a_1*y[i-1] + u[i]
##this looks the same as our model from before

##BUT!

u_t=phi*u_{t-1} + epsilon_t

##Our errors, u, are different. Specifically, our error from last period partially
##determines what our error is today.

##And, we have this new term, epsilon. For now, we're going to say that epsilon is entirely random.
#For instance maybe it's ~N(0,1).


##An example of AR process? Something like the unemployment rate might fit the bill:
##We imagine that unemployment this month is probably based on unemployment last month, plus some good or bad 
#'shock' to that happens each period. For instance, when covid hit, that was a massive, negative shock. 
#But, that negative shock from covid didn't just disappear after 1 month. That shock continued to impact the 
#unemployment rate in the economy. (In other words, the disturbance to the unemployment rate was correlated over time)

##How could we modify our ts_model function so that it simulates a time series process
##with errors like this:

u_t=phi*u_{t-1} + epsilon_t

##hint: you can use all the tools we used last week (look at the stuff that we already put
##in our function)

###We need to make two big changes to our function to accomplish this

##1) most importantly, we need to put u[i] inside of our for-loop, so that it
##can develop the same way that y[i] does

###Note: we want to put u[i] in the for-loop before y[i]

##2) We need to define epsilon 

##This also tips us off for some other tiny things that we need to do

###---We need to save some space for u, just like we did with Y
###---We also need to save the first value of u
###--- We need to define phi as an input

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

ts_ar_model(t=60,y_init=5,a_0=1, a_1=0.5,phi=.9,sigma=0.5)

##Another change that we can make to our function, is to make our function actually 
##return one of the coefficients from a regression

ts_ar_model=function(t,y_init, a_0,a_1, phi, sigma){
##t=30; y_init=5; a_0=1; a_1=.5; phi=.9; sigma=.5  
  ##always make sure to comment out your 'test values'
  ##otherwise, the value you give to your inputs won't matter.
  ##(R will just use the test values, otherwise)
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
  
  lm_ts=lm(data=ts_data, formula = y~lag(y))
  
  data_out=data.frame(alpha_1=(unname(lm_ts$coefficients[2])))
  ##the unname function gets rid of the 'name' on this number, so that only the number comes out
  return(data_out)
}

ts_ar_model(t=60,y_init=5,a_0=1, a_1=0.5,phi=.9,sigma=0.5)

###The function that we just wrote works great. But, if we want to simulate this data and regression
### a whole bunch of times, we're going to need a way to do that efficiently

##Using lapply to simulate data repeatedly

##Enter the apply family

###THere is a set of functions referred to as the 'apply' family. IN basic terms, these 
###functions take a function that you give them, and then apply that function to a set of inputs

###For instance, the lapply function takes a list, 
##and then runs a function over every element of that list

##Example:

example_list=c(1:10)

lapply(example_list, FUN=sqrt)

##THe lapply function uses this basic syntax:

##lapply(list, function you want the list to be run through)

##the nice thing about lapply is that it also works on functions that WE create

###NOTE: We're just going to work with lapply today. FOr more info on the apply family, 
##A good starting point is Dr. Ed Rubin's lecture notes
## https://raw.githack.com/edrubin/EC525S19/master/NotesLab/07RMisc/07RMisc.html#1

##How can we use lapply to simulate our time series process many times?

##It turns out we can do it by making just 1 tiny tweak to our function
##I'm just going to put in one more optional input, called 'iteration'

ts_ar_model=function(iteration=1,t,y_init, a_0,a_1, phi, sigma){
  ##t=30; y_init=5; a_0=1; a_1=.5; phi=.9; sigma=.5  
  ##always make sure to comment out your 'test values'
  ##otherwise, the value you give to your inputs won't matter.
  ##(R will just use the test values, otherwise)
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
  
  lm_ts=lm(data=ts_data, formula = y~lag(y))
  
  data_out=data.frame(iteration=iteration,alpha_1=(unname(lm_ts$coefficients[2])))
  ##the unname function gets rid of the 'name' on this number, so that only the number comes out
  return(data_out)
}
ts_ar_model(t=60,y_init=5,a_0=1, a_1=0.5,phi=.9,sigma=0.5)

##our function works very similarly to how it was working before. I've just got this new'iteration'
##value.

##but, this extra argument is going to let us tell R to run this function a bunch of times with
##lapply

lapply(X=c(1:3),FUN=ts_ar_model, t=60,y_init=5,a_0=1, a_1=0.5,phi=.9,sigma=0.5)

##NOTE: when a function has more than 1 input, you can only provide lapply with a list
##for one of those inputs. (Any additional ones have to be specified)

##Now, what lapply is going to do is run through the ts_ar_model function using the values
##1, 2 and 3 as the 'iteration'

##Basically, this iteration argument is now telling R: here's how many times to run this function
##(using lapply)

##If we want to save our data, we can also make it look just a bit nicer using the rbindlist function
###(from the data.table package)

output=lapply(X=c(1:100),FUN=ts_ar_model, t=60,y_init=5,a_0=1, a_1=0.5,phi=.9,sigma=0.5) %>% rbindlist()

##Parallelizing simulations

##lapply works great for simple simulations. But, it has one downside: when the simulation
##gets complicated, lapply can be very inefficient with your computer's time. 

##We want a way to run our simulation many times, but also quickly. THis is where parallelization
##comes in

##like most other applications on your computer, the thing that actually runs your R code (ie, does
##the math, runs the regression, etc) is your CPU. But, for most of the stuff we do in R,
##your computer's CPU is WILDLY overqualified. 

##One of the ways modern computer rock is that their CPUs have multiple cores. You can think 
##about each computer core as being as separate brain. Each one of them can do a single task by 
##itself. Together, they can do super complicated tasks (Like, say, run a video game)

##Since the tasks we want to do are pretty simple (just some algebra, really), we actually have all
##of the cores work separately. 

##Real quick: make sure that the parallel package is loaded

p_load(parallel)
detectCores()

##the detect cores function will tell you how many cores your computer has. 

##Mine has 12, for instance

##Say we want to run 120 simulation. Parellelization is when I say to my computer:
##have each of those 12 cores run 10 of the simulations

##Basically, parallelization is 'divide and conquer' for your computer

##it turns out that this is quite easy to do in R. 

##To simulate data in R using parallelization, follow 3 steps:

##1) Set a seed
##you can put any whole number in this function
set.seed(135)

##Remember all of those noise variables that we generated? (like epsilon)
###If we 'set a seed' before we generate those variables, then anyone else 
###who runs our code and sets the same seed will get the same noise variables
##Basically, setting a seed is a way to be transparent/honest in your simulations

#2) NExt, use the plan() function to tell R that we want to parallelize

plan(multisession)

##THis tells R to use all of the cores in your computer separately

##Finally, we use the function future_lapply
##This is just a version of the same lapply function, but set up for parallelized computing

parallel_output=future_lapply(X=1:10000,
                              FUN=ts_ar_model, ##don't include the parentheses when writing your function
                              t=60,y_init=5,a_0=1, a_1=0.5,phi=.9,sigma=0.5,
                              future.seed=T ##THis tells the computer to use that seed we set earlier
                              ) %>%rbindlist()
