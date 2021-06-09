# Lab 9: Introduction to Causal Inference
library(pacman)
p_load(tidyverse)

##Simulating Data, now with randomization!

##I want to set stage: A good example of a *causal* question that
##economists are very concerned with is whether or not more education
##leads to more earnings for individuals. In practice, this is  *very* hard question to answer

##FOr this lab, let's pretend that if you already have a bachelors degree in economics, then your average wage
##is going to be ~$54,000. We want to think about this as our baseline. (This number is the rough average of starting salary for a person with a 
##bachelors degree in economics). 

##Furthermore, we're going to assume that if any of these people earned a masters degree in economics, 
##then their wages would go up by $10,000 exactly. 

##Question: what is the causal effect of a masters degree in economics in this situation?

##Answer: in this situation, we can say that the causal impact of a masters degree is +$10,000. 

##This helps clarify what we mean by 'causal effect'. What is the change in a person's wage if they do get the masters degree,
##compared to if they don't. 

##There are definitely causal effects (causal relationships) in the real world, but we cannot see them. In other words, even if
##this +10,000 were true, no researcher would ever be able to 'see' that perfectly. 

##Question: Why? 

##Answer: Because, for each person, we can only see what they earn with a masters degree, or without.

##If a person has a masters degree, we can't see what they would have earned at that point in their lives
##WITHOUT a masters degree.
##(Unless we find a time-machine)

##THis is called the fundamental problem of causal inference. 


###One of the ways that we can 'get around' this problem is through randomized experiments

###Example: medical trials for COVID vaccines. We figured out how effective these vaccines are by randomly giving some people the
##vaccines, and then randomly giving other people a placebo. And then, we used that random experiment to figure 
##out what the causal effect of covid vaccines was. 

##Returning to our example, I'm going to try to convince you that, if we could run the right randomized experiment,
##we would be able to 'recover' the true causal effect of a masters degree (+10,000), even without a time machine.


##Let's say that a few economic researchers are very interested in running an experiment to find out the causal effect of
##getting a masters degree in economics. SO, they are going to do the following experiment: they are going to take 
##a bunch of undergraduates who are just about to earn their bachelors degree in economics. And, they are going to 
##randomly FORCE some of these individuals to earn masters degrees in economics. 

##This experiment could never happen in real life:

##Reasons why we couldn't do this?
##1) Researchers are not allowed to force people to do things generally
##(And, even if they could, it would be unethical to do this)
##2) MAsters degrees are costly and time-consuming, so it would be very difficult to logistically run this experiment
##3) Even if we tried to force some people to get (or not get) this degree, it would be hard to enforce. 

##But, for now, we're going to assume that all of these problems go away. Specifically, we're going to say that
##these researchers offer to pay for the masters degree for anyone who is randomly selected. And, for those people
##who are not selected, the degree is just too expensive. 

##So, everyone who we randomly pick gets a masters, and everyone else doesn't. 

##Let's think about how to simulate this in R:

##First, we we need to figure out what the salary of everyone would be if they did not get a masters degree.

##we said that $54,000 was the average, but that was just an average. We can use our rnorm() function
##to generate a whole bunch of starting salaries with an average of 54000

##Note: when simulating data like this, it's a good idea to give each 'observation'an ID number.

##here, I'm just going to simulate 10000, and number them
set.seed(10050)
sim_data=data.frame(id=c(1:10000), wages=rnorm(10000,54000,5000))
##(I'm also going to give each of our observations an ID #, which is just a good habit when simulating data like this)
ggplot()+
  geom_density(data=sim_data, aes(x=wages))


##We know that everyone who is randomly selected to get a masters degree will earn $10,000 additional dollars.

##But, we need to randomly select people. 

##we can do this with the rbinom() function. YOu can think about this function as simulating flipping a coin

rbinom(10000,1,.5)

##we can add this into our dataframe to decide who gets treatment:

sim_data = sim_data %>% mutate(treatment_status=rbinom(10000,1,.5))

##Now, the next thing that we need to do is describe what happens when you get the masters degree, and when you don't

##to do this, I'm going to use a cool function in r: case_when()

##basically, this function says: if x is the case, this is the result, 
                                #if y is the case, then this other thing is the result

sim_data = sim_data %>% mutate(wages_post_experiment=case_when(treatment_status==1 ~ (wages+10000),
                                                               treatment_status==0 ~ (wages + 0)))


##In other words, we're telling R that if the person is given a masters degree, their wages afterwards are 10000 higher
##and if they aren't given the masters degree, their wages are 0 higher. 

##The last thing that we're going to do is delete our original wages column.

##Because, we want to be honest/realistic about the fact that we can only ever see one 'outcome'

sim_data = sim_data %>% mutate(wages=NULL)

##remember, NULL is the way we tell R that something is 'not there'


####################################################
##Randomization, average treatment effects, and you! 

###Normally, at this point in our previous labs, we would start running regressions. 

##in this case, we don't even need to run a regression! 

###If you have a (well) randomized experiment, then all you need to do to estimate the causal effect
##of your 'treatment' variable (in this case getting a masters degree) is little bit of averaging

##First, we compute the average wages of everyone who got the masters degree:

##Note: again, we need to use the double equals sign

average_masters=sim_data %>% filter(treatment_status==1) %>% summarise(mean(wages_post_experiment))

##Next, we're going to do the same thing but for the people who only have a bachelors degree

average_no_masters=sim_data %>% filter(treatment_status==0) %>% summarise(mean(wages_post_experiment))

##The final step: just subtract the first average from the second
average_masters-average_no_masters

##WOW! THat is super close to the true causal effect!

##When we have proper randomization, we can determine the average effect of our treatment just
##through subtraction. (in simple experiments like this one)

##The way you would interpret this answer is the following:

#"We estimate the causal effect of getting a masters degree in economics at $10,058.86"

##Now, the one other thing that we can do is run a quick regression:

lm_experiment=lm(data=sim_data, formula = wages_post_experiment ~ treatment_status)

summary(lm_experiment)

##Two important things:

##1) The coefficient on 'treatment status' is EXACTLY equal to the average that we calculated above

#2) When we use a regression here, we can also calculate a p-value. 
####So, in this case, we can say that the estimated coefficient is statistically significant'

