# Lab 2 

########### Loading Data and Making Graphs###########

#Our first lines in a new r script should be for loading packages

##recall how I said I always start:
library(pacman)
p_load(tidyverse, ggthemes, viridis, ggridges, lubridate)

###Directories

## we want to be able to have R work with other data from out computer. 
##To do that, we need to be able to tell R where to look to find things on our computer.

##At any given time, R has a 'working directory': the place in the computer's files
##where R is currently looking. We can check our current working directory with the function 

getwd()

##R can only load files from its working directory. So, if we've saved a bunch of data to some folder, 
##we may want to change R's working directory to that folder. We can do this with the function:

setwd("C:/Users/rober/Dropbox/Spring2021/Week_2")
 
##Windows and R can both use directory separator '/'. If you copy a file path from your windows explorer
## (or finder, on a mac), make sure you've got the correct directory separator


###Loading data:
##Let's try downloading some data and pulling it into R. We're going to use a dataset on presidential 
##approval ratings from 2001-2006. You can download the data by copying this link:

##https://r-data.pmagunia.com/system/files/datasets/dataset-34020.csv

##Exercise: Follow the link that I just provided, and download the csv data. Then, put that data into a folder that you are comfortable 
##working from, and then set that folder as your working directory

##Great, now, we're ready to pull this data into R!
## To do this, we can use a function from the tidyverse:

read.csv("dataset-34020.csv")

##notice that our data displayed on our console (Literally, R 'read' the data to us). But
##we need to save this data as an object

bush_approval=read.csv("dataset-34020.csv")

##let's explore the data a bit:

View(bush_approval)

##One important point: when we want to reference a specific column in a dataframe, we do so
##by writing the name of the dataframe, then '$', then the name of the column. 

#EX:
mean(bush_approval$approve)

##self-check: recall what we did with tidyverse last time. Run the following line of code, and tell me what it's doing. 
bush_approval %>% filter(year > 2003) %>% summarize(net_approval=mean(approve-disapprove))

##A quick aside on dates/times: when you grab a dataset with a time element, the way that the 'date' is recorded will 
##frequently be a bit weird. Just look at our dataset, where the month and year are separate variables:

bush_approval %>% select(month, year) %>% head

##a useful R package is the 'lubridate' package, which helps R actually understand date-based objects

##Note: The lubridate package can be finicky (because the way people record 'dates' in dataframes can be weird). 

##we're going to use the function make_datetime:

?make_datetime

#let's take the example from the help window and see what it does
make_datetime(year = 1999, month = 12, day = 22, sec = 10)

##okay, cool, and if we just want day and month?
make_datetime(year = 1999, month = 12)

##Nice, this should work for us. Notice: the function automatically added a day (the first day of the month). 


##Now let's try fixing our dataset:

bush_approval=mutate(bush_approval,year_month=make_datetime(bush_approval$year, bush_approval$month))

head(bush_approval)

##Awesome. Now, we're ready to make cool graphs!

###ggplot2

##R has some in-built function for creating graphs, but they are much less effective
##than the 'ggplot2' package, which has two really nice advantages:

##1: It's designed to work nicely with the tidyverse, and lets you use the same 'structure' to create different plots

##2. Everyone uses ggplot2. So when you get stuck, there is a 99.5% chance (p<0.01) you can find the fix online.

##For most ggplots that you could ever make, the basic template will be the same: 

#To make a plot with ggplot, we need 3 things:
##1: the data for the plot
##2: the aesthetic mapping (what variable goes on what axis, if we want to break our data out by some variable, etc.)
##3: the geom (the type of plot we want to make)

##To actually make the plot, we starting by putting 1 & 2 in the 'ggplot()' function:

ggplot(data = DATA, aes(X_variable, Y_variable, ...))

##This sets up a blank 'template' for a graph. For instance, if we do this with our data:

ggplot(data = bush_approval, aes(year_month, approve))

##we get a plot with the right axes and variables, but no data. 
##To actually get data, we need to add a geom! (We need to tell R what kind of plot we want).

#We do this by starting with the same 'gplot()' function, and then adding a 
##geom_PLOT_TYPE_HERE() function with a '+' symbol.

ggplot(data = bush_approval, aes(year_month, approve))+ 
  GEOM_FUNCTIO(optional_stuff_here)

##Let's try to make a scatterplot of Bush's approval ratings:

ggplot(data = bush_approval, aes(year_month, approve))+
  geom_point()

##Awesome, we got a basic plot working!

##Note: we can also put the 'aes' part of our code into the geom_point line and we get the same thing out:

ggplot(data = bush_approval)+
  geom_point(aes(year_month, approve))

##But, we can do so much more with ggplot!
##We can add colors, based on some categorical variable in our data:

ggplot(data = bush_approval)+
  geom_point(aes(year_month, approve, color=iraq.war))

##...close, but now we've identified another common way R tries to 'help' us
##Note the class of bush_approval$iraq.war

class(bush_approval$iraq.war)

#Aha! R is seeing our dummy variable with 0's and 1's, and doesn't understand that it's a dummy variable.
##We can make a simple change so that R knows that this variable isn't really 'numeric'

ggplot(data = bush_approval)+
  geom_point(aes(year_month, approve, color=as.factor(iraq.war)))

##The 'factor' class is how R handles objects with multiple, discrete categories.
##As another silly example:
ggplot(data = bush_approval)+
  geom_point(aes(year_month, approve, color=as.factor(month)))

##Also, we can change the labels for things to make them a more readable:
ggplot(data = bush_approval)+
  geom_point(aes(year_month, approve, color=as.factor(iraq.war)))+
  labs(x="Time", y="Approval", color="Iraq War Indicator")

##An important note: see how we added the new theme to the plot? We put in another 
##'+', and then another line of code. This is how it all works in ggplot2.
##'We start with the ggplot() function, and then keep adding things to it.


##We can also give our plot a distinct theme:

ggplot(data = bush_approval)+
  geom_point(aes(year_month, approve, color=as.factor(iraq.war)))+
  labs(x="Time", y="Approval", color="Iraq War Indicator")+
  theme_minimal()

##There are a whole bunch of included themes, some of which replicate the
##style of major news sources:

ggplot(data = bush_approval)+
  geom_point(aes(year_month, approve, color=as.factor(iraq.war)))+
  labs(x="Time", y="Approval", color="Iraq War Indicator")+
  theme_fivethirtyeight()


##We can also put more than one 'geom()' on the same plot. 
##Say we want to connect our points with a line. We can do 
##this by just adding a line plot to our existing plot:

ggplot(data = bush_approval)+
  geom_point(aes(year_month, approve, color=as.factor(iraq.war)))+
  geom_line(aes(year_month, approve))+
  theme_fivethirtyeight()
  
##in complicated plots like this, it's a good idea to put the aes() argument
##in each geom_type line, instead of in the ggplot() line

##A helpful link: (a cheatsheet showing you the basics for all the basic plot types)
https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

##Since it's on your homework, I figure I'll mention how to do histograms.
##Histograms are made with the geom_histogram() function

ggplot(data = bush_approval)+
geom_histogram(aes(x=(approve-disapprove)))+
  labs(x="Time", y="Approval", color="Iraq War Indicator")+
  theme_fivethirtyeight()

##we can also break-out our histogram by some category, using the 'fill'
##option in aes()

##let's make a quick variable for "after 9/11:
##We can do this with another quick lubridate function: 'as.Date'

bush_approval=bush_approval %>% mutate (post_911=(year_month>as.Date("2001-09-01")))

ggplot(bush_approval)+
geom_histogram(aes(x=(approve),  fill=as.factor(post_911)))+
  labs(x="Time", y="Approval", fill="After 9/11?")+
  theme_fivethirtyeight()

##note that we got a warning from R: it's telling us it wasn't quite sure how big
##to make the bins in our histogram. Let's check our data out a bit more:
summary(bush_approval$approve)

##We can always change how 'wide' our bins are in histograms. Let's try 4% bins

ggplot(bush_approval)+
  geom_histogram(aes(x=(approve),  fill=as.factor(post_911)), binwidth = 4)+
  labs(x="Time", y="Approval", fill="After 9/11?")+
  theme_fivethirtyeight()


#now that we have some basics down, we can make some plots with cool datasets!


## https://github.com/rfordatascience/tidytuesday

##### If someone has some data published online, we can usually download it straight into R:


ufo_sightings <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")


#check out the dataset
head(ufo_sightings, 10)
names(ufo_sightings)

#look at this date column..
class(ufo_sightings$date_time)
#it's a character, but it would be nice if we could use these dates/times in our plots. So let's convert it using the lubridate package. 
#Parse_date_time() takes things that already look pretty much like dates, and tells R that they're dates
ufo <- ufo_sightings %>% 
  mutate(date_time = parse_date_time(date_time, 'mdy_HM')) %>%
  ##mdy_HM stands for month,day, year-hours minutes
  ##basically, we're telling R what format the dates are stored as
  filter(country != "NA")
#we also want to get rid of the NA values for country. 

#let's look at what time of day these ufo sightings are happening. And let's make it a bit spooky, because why not
ufo %>% 
  ggplot(aes(x = hour(date_time))) + 
  geom_density(color="yellow") +
  labs(x="Hour of the Day (Military Time)", Y='# of Sightings')+
  theme_dark()


#If we wanted to break it down by country, we could use geom_density_ridges()
ufo %>% 
  ggplot(aes(x = hour(date_time), y = country, fill = country)) + 
  geom_density_ridges() +
  labs(x="Hour of the Day (Military Time)", Y='# of Sightings', fill="Country of Sighting")+
  theme_minimal()

# what about the time of the year? We can look at it monthly using the month() function (again from lubridate)
ufo %>% 
  ggplot(aes(x = month(date_time), y = country, fill = country))+ 
  geom_density_ridges() +
  theme_minimal()

#now let's look at total ufo sightings per year
ufo_total <- ufo %>% group_by(year(date_time)) %>% summarize(total = n())


#Something cool: the names() function doesn't just tell us the names from an object, 
##we can also use it to change the names of an object
names(ufo_total) <- c("year", "total")

##One final graph to leave you with

ggplot(aes(x = year, y = total), data = ufo_total)+
  geom_line(color="blue") + 
  labs(x = "Year",
       y = "UFO Sightings",
       title = "Total Recorded UFO Sightings") +
  theme_linedraw()

