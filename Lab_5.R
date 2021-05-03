# Lab 5: Functions and Time Series

library(pacman)
p_load(tidyverse, ggthemes)

##Function Creation

##In R, a function is an object that takes some inputs (could be one, could be more than one),
##and then gives us an output

##(Remember, everything in R is an object)


##When a lot of people think about functions, they think about math
### EX: f(x)=x^2+10


##Notice, this function is really doing exactly what functions do in R

##The big difference between the 'math' functions you're probably used to and 'R" functions is that functions in R
##can have inputs or outputs that are NOT just numbers

##Example: we have now used the function summary(), which took a 'regression object' (xx=lm(...))
##as an input, and gave us a table as the output

###An example of a function that we could create in R:

###Say that I wanted to know what day of the week it would be X days from today. I could create a function
###in R that took one thing as an input: the number of days into the future that I wanted to 'look'
###and then, as an output, gave me the day of the week it would be as a character vector

##Like this: future_days(7)="Wednesday"


###IN R, not only can you load functions from packages, you can also create your own

#(In fact, every single function that you load from a package is just a function in R that someone else
##made, and then thought 'hey, this is useful/cool. I should put this online')

##Just like any other object, if we want to create a function in R, we need to give it a value,
##and a name

##The 'value' that we put inside a function is R code. (A list of instructions that R can use)
##In order to give those instruction, we use a special function: the 'function()' function. 

##The formula to make a function geoes like this:

##we start by naming our function, and then telling R how many inputs it uses,

##and then , we put our instructions inside of {}
my_new_function= function(input1,input2,...){
  ##instructions go here. (Any kind of code you can imagine)
  ##for instance, 
  input1^2
  
  
##and at the end of our function, we always conclude with the line
  return(output)
  ##This is how we tell R what it should 'spit out' when it finishes our instructions
  
  
  ##Note: the names of our inputs can be anything. They;re just how we refer to stuff in the function instructions
}


##An example will help here: Let's try to make a function in R that takes a number (x), 
##squares it, adds 10 and then returns x^2+10. IE, we make the function f(x)=x^2+10

square_plus_ten=function(x){
  
  x_squared_plus_10=(x^2)+10
  
  return(x_squared_plus_10)
}

##Now, to use our function, we just 'call it' like any other

square_plus_ten(10)

##and, when we try to use our function, it works!

##But, a few important notes here:

##1) R has *NOT* saved anything called x_squared_plus_ten. In general, R does NOT save internal
##  objects from functions

##2) Since we did not 'save' the result of square_plus_ten(10), it only shows up in our console
#(just like the result of any other function)

result=square_plus_ten(10)

##3) You can only ever put 1 thing inside of the return() function. (One object, that is)


##4)The squiggly brackets tell R where the function starts and ends.  will only do the stuff inside of the brackets


##Check in: try to write a function that does the following: takes a vector as an input, 
##squares all the elements of that vector, and then returns the squared vector

##EX: if the input vector is c(1,2,3,4), the output should be c(1,4,9,16)


##One way to do it:

func1=function(x1,x2,x3,x4){
  
  vsq=c(x1^2,x2^2,x3^2,x4^2)
  
  return(vsq)
}

##But, this function has a weakness: it can only make a list of 4 squared numbers!


##But, remember that lists can also be objects

vect=c(1:10)

##so, we can also write
vec_sq=function(vectr){
  vec_sq=vectr^2
  
  return(vec_sq)
}

##The only real difference here is that now we've written our function in a way that will let it accept a vector *or*
##a single number as the input

vec_sq(vect)

##Pro-tip: there are MANY MANY MANY ways to write a function that 'succeeds' at what you want
##         ultimately, with complicated functions, it's worth spending a bit of time
# figuring out an efficient way of writing the function. This just comes with practice



##Functions can let us 'chain together' a few different steps, and can be complicated
##For instance, we can have functions that output graphs

##EX:

vec_sq_graph = function(x){
  x_square = x^2
  
  
  ##to make things straightforward, I'm going to create a dataframe with the following:
  df=data.frame(x=x,y=x_square)
  ##note: this dataframe will not exist once our function finishes
  
  ##now, I'm going to tell R to make a graph
  
  plot=ggplot(data=df,aes(x=x,y=y))+
    geom_line()
  
  return(plot)
}

ex_vectr=c(1:10)

vec_sq_graph(ex_vectr)

##For-loops


##Sometimes in R, we want to do the same thing, a whole bunch of times.
##The most basic way to do that is with a for-loop. 

##The basic idea for a for loop is: we tell R, that for every 'iteration' in a list,
##do XXXXXXX

##A for loop needs exactly two things to work:

#1) A header, which specifies the 'iteration list'
#2) A body, where we tell R the actual stuff we want to be done


##THe simplest for-loop ever:

##Here, I've written the header
##(In this case, I'm telling R to do this 2000 times')
for(x in 1:2000){
  ##Now, I write the body of my for-loop inside {}
  print("GO DUCKS!")
}

##Warning up front: simulations are simple to write, but they are very inefficient when it comes
##to 'big' projects (like simulating a lot of data many times)

##But, the usefulness of for-loops comes from the fact that we can 'involve' our list of iterations
##in our instructions


##Simple example:
for(i in 1:10){
  print(i)
}

##why is this useful? 

##Well, say we want to run a function over a list

##An important note: when we want to save the result from a for-loop, 
##we normally 'set some space aside' before we run the loop

x=c()

for(i in 1:10){
  
  x[i]=square_plus_ten(i)
  
  ##recall: using the square brackets, we can refer to the position inside of lists (or matrices)
  ##For instance, when i is equal to 1, R is going to put the result from square_plus_ten(1)
  ##into the first spot on the list x
}

#Quick note: R starts *indexing* at 1. This is unusual for coding languages and can trip you up if you don't keep it in mind. 

##If you don't know what this note means, you can ignore it, for now. Just know that, in the above for-loop,
##R doesn't understand x[0]. (that is, you can't refer to the 0'th element in a list)


##We can have R iterate over any kind of list ( so long as it makes sense)

##for instance:

for( i in c(2,00,-45,610)){
  
  print(square_plus_ten(i))
}

##For-loops are especially useful when we want to 'simulate' time series data

##Time-series in R 

##Let's think about a time series that looks like this:

##y_t= alpha_0+ alpha_1*y_{t-1} + u_t ~N(0, sigma)

##We can write a function in R that simulates this process. To do so, we need to write a function that:

##1) draws our list of random errors (t of them)
##2) set a starting value for Y,
##3) generate all of our y's after the starting Y using a for-loop


ts_model=function(t,y_init, a_0,a_1, sigma){
  ##first, we need to set some space aside to save our time series
  y=c()
  
  ##next, we want to tell R that the first element of this list is y_init
  
  y[1]=y_init
  
  ##Now, we draw our random errors
  
  u=rnorm(t,mean=0,sd=sigma)
  
  ##recall, R has a bunch of info about the normal distribution. If we use the rnorm() function,
  ##R can simulate draws from a normal distribution
  
  ##now, all that's left is to use a for-loop to finish our time series y
  
  for( i in 2:t){
    y[i]=a_0 + a_1*y[i-1] + u[i]
  }
  
  
  ##NOW, we just need to save our data in a nice form, and tell R to return it:
  
  ts_data=data.frame(time=c(1:t),
                     y=y)
  
  return(ts_data)
}


ts_model(200,5,.39,.87,0.04)



##We didn't have time for this in lab, but we can also graph our new time series really easily:
#Actually, we can pass the `ts_model()` function we made into a ggplot, and generate a graph of the process!
ggplot(aes(x = time, y = model), data = ts_model(200,5,.39, .87, .04)) +  
  geom_line(col = 'purple') +
  theme_minimal()
