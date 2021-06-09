# Lab 10: Instrumental Variables
library(pacman)
p_load(tidyverse, AER)

#1) IV, a motivation

##LAst week, we were talking about what we can do with a properly randomized experiment:

##What we can do is **Causal Inference**: we can figure out what causal effect one variable
##has on another

##Sadly, as economists, we don't always get to run experiments for the causal questions we're
##interested in

##Ex: last week, we thought about a super important question to economists: what impact does a college
##degree have on wages. And, we thought about how a bunch of economists could try to answer this 
##question by randomly assigning some people to get a college degree, and randomly telling
##other people that they were not allowed to get a college degree. 

##But, we can never do this experiment in real life (And, that's a good thing! This experiment
##would be super unethical)

##Question: say we stick with our previous example, and say that we can't run a randomized
##experiment. Instead, we just observe people: some of them have college degrees, and some
##dont. Why can't we  just take the difference
##between average wages for these two groups of people, and say that it is the 
##'causal effect of a college degree?'
##'
##Answer: selection bias: we might imagine, for instance, that the only people who go to college
##are those who have naturally higher 'ability' (this is a super vague way that economists will
##talk about this). When I say ability, what I mean is: something about a person that determines
##how well they do in school and also in the workplace. 

##But, we can't really see ability

##So, we have a variable that is correlated with wages, and correlated with getting a degree,
## and we can't 'see' it. 

##this sounds a lot like omitted variable bias!

##And just like OVB, selection bias will... well... bias your estimated causal effect!

##In the land of causal inference, this becomes very important! If your estimated treatment coefficient is biased,
##then it isn't really an estimate of the causal effect you wanted to find!

##Enter the instrumental variable

##IV is a tool that we can use to estimate a causal effect, but in situations where
##you don't have a randomized experiment and you're worried about selection bias.

##In the example above, we could definitely imagine that even though higher ability
##individuals are the ones getting higher wages and college degrees, the college degree
##is still having a causal effect on wages. 

##Our problem is that we have selection bias mixed together with our causal effect

##What we need is a strategy to separate these two factors. 

##we're going to do that by finding a new variable, which we call an instrument

##The variable we're looking for needs two things (two things we'll need to assume)

#1) Our new variable is entirely unrelated to the thing causing selection bias

#2) Our new variable is really, really REALLY correlated with our variable of interest
##(in this case, education)

##if we actually find an instrument that does this, it basically gives us a way to 
##'move' our education variable without 'involving' the thing causing selection bias
##(remember the question we want to answer: how does a change in education change wages)




##One instrument that has been used to try to answer the 'education-wage' question is:

##distance from college when you were 17. 


#Why does this work?

##1) We're assuming that innate ability doesn't relate to how far away you live
##from a college

##Funny example: if I have a son, and he's in high school and he's bad at calculus,
## I can't make him better at calculus by moving across the street from UO's math department

##2) Living near a college might make you more likely to go to college. 

###Funny example: if you're 17 and see all the cool parties college students go to,
##you might want to go to college more. 


##with this in mind, here's what we've got set up:

##if we take all the kids who lived close to a college growing up, and compared them
##to all the kids who lived far from a collge, then we can't say anything about
##which group has higher ability. 

##Ability is random, with respect to how far you are from a college. 

##But, the kids who live closer are just more likely to go to college. 

##In other words, distance is giving us a source of variation in college
##attendence that isn't correlated with ability. 



# 2) Estimating 2SLS in R

##one of the easiest ways to run an IV analysis is using something called 2 stage least squares


##POST-LAB EDIT: I realized post-lab that the sample-data that we used for our 2SLS example 
##               was not really the best example. To avoid letting that mistake get even 'larger'
##               I'm not including that example here, but instead just the generic code for using 
##              2sls

##1) Run a first stage regression: regress the variable
##that you are interested in the causal effect of with your instrument
##as an explanatory variable (plus any controls you want to use)

stage_1=lm(independent_variable_of_interest~instrument +control_1+control_2, data=sample_data)

##One thing that's cool about 2SLS is that we can 'test' our 
##assumption that distance correlates to education. 

summary(stage_1)
##the p-value on your instrument's coefficient will tell you if your instrument actually seems to be correlated with your 
##explanatory variable of interest

##step 1.5) extract our fitted values (the predictions of education from this
##regression)

sample_data$independent_variable_of_interest_predictions=fitted.values(stage_1)

##step 2) Run your second stage regression,

#which is a regression of your outcome variable (wages) on
##all of your controls from the first stage, plus the predictions
##of education. CRITICAL: don't include the actual education values,
##                       also, don't include your instrument

stage_2=lm(dependent_variable~ independent_variable_of_interest_predictions +
             control_1+control_2, data=sample_data)

summary(stage_2)
##The coefficient on education variable in your stage 2 regression can
##be interpreted as the causal effect of your independent variable of interest
##on your dependent variable

##NOTE: THIS INTERPRETATION IS THE MOST BASIC ONE. THIS EXPLANATION IS SUFFICIENT FOR THIS COURSE,
##      BUT IGNORES SOME IMPORTANT DETAILS THAT ARE BEYOND THE SCOPE OF THIS COURSE. FEEL FREE TO CONTACT
##      ME IF YOU WANT TO LEARN MORE.




