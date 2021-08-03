ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)
head(wash)
head(chi)

# install.packages("packagename")
library(lubridate)
library(MASS)
library(ggplot2)
library(dplyr)
library(plyr)
library(tidyverse)
library(grid)


#1 Popular times of travel (i.e., occurs most often in the start time)
#1-1 What is the most common month?

# NEW YORK
#1)
month = 1
for (num in 1:12){
    print(length(which(month(ny$Start.Time) == month)))
    month = month + 1
}

#2)
length(which(format(as.Date(ny$Start.Time), "%m") == '01'))
length(which(format(as.Date(ny$Start.Time), "%m") == '02'))
length(which(format(as.Date(ny$Start.Time), "%m") == '03'))
length(which(format(as.Date(ny$Start.Time), "%m") == '04'))
length(which(format(as.Date(ny$Start.Time), "%m") == '05'))
length(which(format(as.Date(ny$Start.Time), "%m") == '06'))
length(which(format(as.Date(ny$Start.Time), "%m") == '07'))
length(which(format(as.Date(ny$Start.Time), "%m") == '08'))
length(which(format(as.Date(ny$Start.Time), "%m") == '09'))
length(which(format(as.Date(ny$Start.Time), "%m") == '10'))
length(which(format(as.Date(ny$Start.Time), "%m") == '11'))
length(which(format(as.Date(ny$Start.Time), "%m") == '12'))

# June is the most common month in New York.

#visualize
#1)
table(month(ny$Start.Time))
#2)
hist(month(ny$Start.Time), breaks=18, col="grey", border="white", 
     main="Histogram of the most common month of New York", xlab="month", ylab="count")

#WASHINGTON
#1)
month = 1
for (num in 1:12){
    print(length(which(month(as.POSIXct(wash$Start.Time, format="%Y-%m-%d")) == month)))
    month = month + 1
}
#2)
Jan<- length(which(format(as.Date(wash$Start.Time), "%m") == '01'))
Feb<- length(which(format(as.Date(wash$Start.Time), "%m") == '02'))
Mar<- length(which(format(as.Date(wash$Start.Time), "%m") == '03'))
Apl<- length(which(format(as.Date(wash$Start.Time), "%m") == '04'))
May<- length(which(format(as.Date(wash$Start.Time), "%m") == '05'))
Jun<- length(which(format(as.Date(wash$Start.Time), "%m") == '06'))
Jul<- length(which(format(as.Date(wash$Start.Time), "%m") == '07'))
Agu<- length(which(format(as.Date(wash$Start.Time), "%m") == '08'))
Sep<- length(which(format(as.Date(wash$Start.Time), "%m") == '09'))
Oct<- length(which(format(as.Date(wash$Start.Time), "%m") == '10'))
Nov<- length(which(format(as.Date(wash$Start.Time), "%m") == '11'))
Dec<- length(which(format(as.Date(wash$Start.Time), "%m") == '12'))

Washington_Month <- c(Jan, Feb, Mar, Apl, May, Jun, Jul, Agu, Sep, Oct, Nov, Dec) 
Washington_Month
# June is the most common month in Washington.

#visualize
#1-1)
Wash_Month <-as.numeric(format(as.Date(wash$Start.Time), "%m"))
table(Wash_Month)
#1-2)
table(month(as.POSIXct(wash$Start.Time, format="%Y-%m-%d")))
#2)
hist(month(as.POSIXct(wash$Start.Time, format="%Y-%m-%d")), 
     breaks=18, col="grey", border="white", 
     main="Histogram of the most common month of Washington", xlab="month", ylab="count")

# CHICAGO
#1)
month = 1
for (num in 1:12){
    print(length(which(month(chi$Start.Time) == month)))
    month = month + 1
}
#2)
length(which(format(as.Date(chi$Start.Time), "%m") == '01'))
length(which(format(as.Date(chi$Start.Time), "%m") == '02'))
length(which(format(as.Date(chi$Start.Time), "%m") == '03'))
length(which(format(as.Date(chi$Start.Time), "%m") == '04'))
length(which(format(as.Date(chi$Start.Time), "%m") == '05'))
length(which(format(as.Date(chi$Start.Time), "%m") == '06'))
length(which(format(as.Date(chi$Start.Time), "%m") == '07'))
length(which(format(as.Date(chi$Start.Time), "%m") == '08'))
length(which(format(as.Date(chi$Start.Time), "%m") == '09'))
length(which(format(as.Date(chi$Start.Time), "%m") == '10'))
length(which(format(as.Date(chi$Start.Time), "%m") == '11'))
length(which(format(as.Date(chi$Start.Time), "%m") == '12'))

# June is the most common month in Chicago.

#visualize
#1)
table(month(chi$Start.Time))
#2)
hist(month(chi$Start.Time), breaks=18, col="grey", border="white", 
     main="Histogram of the most common month of Chicago", xlab="month", ylab="count")

#1-2 What is the most common day of Week?
# NEW YORK
M <-length(which(weekdays(as.Date(ny$Start.Time)) == 'Monday'))
T <- length(which(weekdays(as.Date(ny$Start.Time)) == 'Tuesday'))
W <- length(which(weekdays(as.Date(ny$Start.Time)) == 'Wednesday'))
TH <- length(which(weekdays(as.Date(ny$Start.Time)) == 'Thursday'))
F <- length(which(weekdays(as.Date(ny$Start.Time)) == 'Friday'))
S <- length(which(weekdays(as.Date(ny$Start.Time)) == 'Saturday'))
SU <- length(which(weekdays(as.Date(ny$Start.Time)) == 'Sunday'))
Week_NY <- c(M, T, W, TH, F, S, SU)
WEEK_NY <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
df <- data.frame(WEEK_NY,Week_NY); df
# Wednesday is the most common day of Week in New York.

#Washington
M <-length(which(weekdays(as.Date(wash$Start.Time)) == 'Monday'))
T <- length(which(weekdays(as.Date(wash$Start.Time)) == 'Tuesday'))
W <- length(which(weekdays(as.Date(wash$Start.Time)) == 'Wednesday'))
TH <- length(which(weekdays(as.Date(wash$Start.Time)) == 'Thursday'))
F <- length(which(weekdays(as.Date(wash$Start.Time)) == 'Friday'))
S <- length(which(weekdays(as.Date(wash$Start.Time)) == 'Saturday'))
SU <- length(which(weekdays(as.Date(wash$Start.Time)) == 'Sunday'))


Week_Wash <- c(M, T, W, TH, F, S, SU)
WEEK_Wash <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
df <- data.frame(WEEK_Wash,Week_Wash) 
# Wednesday is the most common day of Week in Washington.

# Visualize
df

#Chicago
M <-length(which(weekdays(as.Date(chi$Start.Time)) == 'Monday'))
T <- length(which(weekdays(as.Date(chi$Start.Time)) == 'Tuesday'))
W <- length(which(weekdays(as.Date(chi$Start.Time)) == 'Wednesday'))
TH <- length(which(weekdays(as.Date(chi$Start.Time)) == 'Thursday'))
F <- length(which(weekdays(as.Date(chi$Start.Time)) == 'Friday'))
S <- length(which(weekdays(as.Date(chi$Start.Time)) == 'Saturday'))
SU <- length(which(weekdays(as.Date(chi$Start.Time)) == 'Sunday'))


Week_Chi <- c(M, T, W, TH, F, S, SU)
WEEK_Chi <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
df <- data.frame(WEEK_Chi,Week_Chi) 
# Wednesday is the most common day of Week in Chicago.

# Visualize
df

#1-3 What is the most common hour of day?
#New York
Hour <- hour(as.POSIXct(ny$Start.Time, format="%Y-%m-%d %H")) 
ny <- data.frame(ny, Hour)

NY_Time <- function(data){
    time = 1
    each_time = NULL
    for (num in 1:24){
        each_time[time] = length(which(hour(data$Start.Time) == time))
        time = time + 1
    }
    return(each_time)
}
# 17:00 to 18:00 is the most common hour in New York.

# Visualize
NY_Time(ny)
ggplot(aes(x=Hour), data=ny) + geom_histogram(binwidth=0.5) + 
ggtitle('HIstogram of Number of hours')

# Washington
#wash <- wash[-89051,]
Hour <- hour(as.POSIXct(wash$Start.Time, format="%Y-%m-%d %H")) 
wash <- data.frame(wash, Hour)
#head(ny)

W_Time <- function(data){
    time = 1
    each_time = NULL
    for (num in 1:24){
        each_time[time] = length(which(hour(as.POSIXct(wash$Start.Time, format="%Y-%m-%d %H")) == time))
        time = time + 1
    }
    return(each_time)
}

# Visualize
W_Time(wash)
ggplot(aes(x=Hour), data=wash) + geom_histogram(binwidth=0.5) + 
ggtitle('HIstogram of Number of hours in Washington')

# Chicago
Hour <- hour(chi$Start.Time) 
chi <- data.frame(chi, Hour)
#head(chi)

C_Time <- function(data){
    time = 1
    each_time = NULL
    for (num in 1:24){
        each_time[time] = length(which(hour(as.POSIXct(data$Start.Time, format="%Y-%m-%d %H")) == time))
        time = time + 1
    }
    return(each_time)
}

# Visualize
C_Time(chi)
ggplot(aes(x=Hour), data=chi) + geom_histogram(binwidth=0.5) + 
ggtitle('HIstogram of Number of hours in Chicago')

#2. Popular stations and trip
#2-1)What is the most common start station?

# New York
count(table(ny$Start.Station))

plot(ny$Start.Station, main="The Number of the Common Start Station in New York", 
     xlab="Station name", ylab="count")

max(table(ny$Start.Station))
which(table(ny$Start.Station) == max(table(ny$Start.Station)))
#Pershing Square North is the most common Start.Station (592)

# Washington
count(table(wash$Start.Station))

plot(wash$Start.Station, main="The Number of the Common Start Station in Washington", 
     xlab="Station name", ylab="count")

max(table(wash$Start.Station))
which(table(wash$Start.Station) == max(table(wash$Start.Station)) )
# Columbus Circle / Union Station is the most common Start.Station (1700)

# Chicago
count(table(chi$Start.Station))

plot(chi$Start.Station, main="The Number of the Common Start Station in Chicago", 
     xlab="Station name", ylab="count")

max(table(chi$Start.Station))
which(table(chi$Start.Station) == max(table(chi$Start.Station)) )
# Streeter Dr & Grand Ave is the most common Start.Station (210)

#2-2 What is the most common end station?
# New York
count(table(ny$End.Station))

plot(ny$End.Station, main="The Number of the Common End Station in New York", 
     xlab="Station name", ylab="count")

max(table(ny$End.Station))
which(table(ny$End.Station) == max(table(ny$End.Station)))
# Pershing Square North is the most common End Station (556)

# Washington
#count(table(wash$End.Station))
plot(wash$End.Station, main="The Number of the Common End Station in Washington", 
     xlab="Station name", ylab="count")

max(table(wash$End.Station))
which(table(wash$End.Station) == max(table(wash$End.Station)))
# Columbus Circle / Union Station is the most common End Station (1767)

# Chicago
#count(table(chi$End.Station))
plot(chi$End.Station, main="The Number of the Common End Station in Chicago", 
     xlab="Station name", ylab="count")

max(table(chi$End.Station))
which(table(chi$End.Station) == max(table(chi$End.Station)))
# Streeter Dr & Grand Ave is the most common End Station (233)

#4. User info
#4-1) What are the counts of each user type?
# New York
table(ny$User.Type)
count(ny$User.Type)

plot(ny$User.Type, main="The Number of the User Type in New York", 
     xlab="User Type", ylab="count")

# Washington
table(wash$User.Type)
count(wash$User.Type)

plot(wash$User.Type, main="The Number of the User Type in Washington", 
     xlab="User Type", ylab="count")

# Chicago
table(chi$User.Type)
count(chi$User.Type)

plot(chi$User.Type, main="The Number of the User Type in Chicago", 
     xlab="User Type", ylab="count")

#4-2) What are the counts of each gender(only available for NYC and Chicago)?
# New York
count(ny$Gender)
plot(ny$Gender, main="The Number of the Gender in New York", 
     xlab="Gender Type", ylab="count")

# Chicago
count(chi$Gender)
plot(chi$Gender, main="The Number of the Gender in Chicago", 
     xlab="Gender Type", ylab="count")

#4-3) What are the earliest, most recent, most common year of birth(only available for NYC and Chicago)?
# New York
ny$Birth.Year[which.min(ny$Birth.Year)] # the earliest -> 1885
ny$Birth.Year[which.max(ny$Birth.Year)] # the most recent -> 2001
which(table(ny$Birth.Year) == max(table(ny$Birth.Year)))  # the most common year -> 1989

# Chicago
chi$Birth.Year[which.min(chi$Birth.Year)] # the earliest -> 1899
chi$Birth.Year[which.max(chi$Birth.Year)] # the most recent -> 2002
which(table(chi$Birth.Year) == max(table(chi$Birth.Year)))  # the most common year -> 1989

system('python -m nbconvert Explore_bikeshare_data.ipynb')