#data upload
chicago <- read.csv("C:/Users/M85278/Downloads/chicago.csv")
View(chicago)

new.york <- read.csv("C:/Users/M85278/Downloads/new-york-city.csv")

washington <- read.csv("C:/Users/M85278/Downloads/washington.csv")

library(ggplot2)
  
head(chicago)
head(new.york)
head(washington)

summary(new.york)
summary(chicago)
summary(washington)
###########################################################################################################

####What is the most common hour of day to rent bike?

#extraction of relevant data

washington[,8]<-as.integer(substring(washington$Start.Time,12,13))
new.york[,10]<-as.integer(substring(new.york$Start.Time,12,13))
chicago[,10]<-as.integer(substring(chicago$Start.Time,12,13))

##first plots

#over all data
x<-c(washington$V8,new.york$V10,chicago$V10)
qplot(x,bins=24,main = "Histogram of number of rentals by rental time")+
  scale_x_continuous(breaks = seq(0, 23, 1))+
  labs(x = "Hour",y = "Number of rentals")
#Here we can see 2 peaks at 8AM and at 5PM. Let's jump into more detail.


### And here you can find basic numbers
Count_all<- NULL

for (n in 1:24) {
Count_all[n]<-length(subset(x,x==n-1))
}

print(Count_all)

hours_all<- data.frame(Hour=c(0:23),Count=Count_all)

library(dplyr)
arrange(hours_all,desc(Count))

#divided by cities

#####################3 Washington ###########################
qplot(washington$V8, bins=24,main = "Histogram of number of rentals in Washington by rental time")+
  scale_x_continuous(breaks = seq(0, 23, 1))+
  labs(x = "Hour",y = "Number of rentals")

# Washington's peak is at 8AM

### And here you can find basic numbers
Count_washington<- NULL

for (n in 1:24) {
  Count_washington[n]<-length(subset(washington$V8,washington$V8==n-1))
}

print(Count_washington)

hours_washington<- data.frame(Hour=c(0:23),Count=Count_washington)

arrange(hours_washington,desc(Count))

#####################3 New York ##############################

qplot(new.york$V10,bins=24,main = "Histogram of number of rentals in New York by rental time")+
  scale_x_continuous(breaks = seq(0, 23, 1))+
  labs(x = "Hour",y = "Number of rentals")
# New York's peak is at 5PM

### And here you can find basic numbers
Count_new.york<- NULL

for (n in 1:24) {
  Count_new.york[n]<-length(subset(new.york$V10,new.york$V10==n-1))
}

print(Count_new.york)

hours_new.york<- data.frame(Hour=c(0:23),Count=Count_new.york)

arrange(hours_new.york,desc(Count))


#####################3 Chicago ##############################
qplot(chicago$V10,bins=24,main = "Histogram of number of rentals in Chicago by rental time")+
  scale_x_continuous(breaks = seq(0, 23, 1))+
  labs(x = "Hour",y = "Number of rentals")
# Chicago's peak is at 5PM

### And here you can find basic numbers
Count_chicago<- NULL

for (n in 1:24) {
  Count_chicago[n]<-length(subset(chicago$V10,chicago$V10==n-1))
}

print(Count_chicago)

hours_chicago<- data.frame(Hour=c(0:23),Count=Count_chicago)

arrange(hours_chicago,desc(Count))


#####################################################################################################

#### What is the total travel time for users in different cities?

######################## Chicago ##############################
c(sum(chicago$Trip.Duration),"seconds in Chicago")

c(sum(chicago$Trip.Duration)/60,"minutes in Chicago")

c(sum(chicago$Trip.Duration)/60/60,"hours in Chicago")


######################## Washington ##############################
c(sum(washington$Trip.Duration),"seconds in Washington")

c(sum(washington$Trip.Duration)/60,"minutes in Washington")

c(sum(washington$Trip.Duration)/60/60,"hours in Washington")

######################## New York ##############################
c(sum(new.york$Trip.Duration),"seconds in New York")

c(sum(new.york$Trip.Duration)/60,"minutes in New York")

c(sum(new.york$Trip.Duration)/60/60,"hours in New York")

########################################################################################################

#### What are the counts of each gender (only available for NYC and Chicago)?


#####################Chicago##############################
qplot(x=V10,data=chicago,bins=24,
      main = "Histogram of number of rentals in Chicago by rental time")+
  scale_x_continuous(breaks = seq(0, 23, 1))+
  labs(x = "Hour",y = "Number of rentals")+
  facet_grid(Gender~.)
# we can see that men used bikes much more than other Genders. 
# Also we can see 2 peaks before and after work (8AM an 5PM).

# Here we can find absolute numbers of users by Gender and percentage
# total
length(chicago$V10)
#Male
c(length(subset(chicago$V10,chicago$Gender=="Male")),
  length(subset(chicago$V10,chicago$Gender=="Male"))/length(chicago$V10)*100)

#Female
c(length(subset(chicago$V10,chicago$Gender=="Female")),
  length(subset(chicago$V10,chicago$Gender=="Female"))/length(chicago$V10)*100)

#Undefined
c(length(subset(chicago$V10,chicago$Gender=="")),
  length(subset(chicago$V10,chicago$Gender==""))/length(chicago$V10)*100)


#####################New York##############################
qplot(x=V10,data=new.york,bins=24,
      main = "Histogram of number of rentals in new.york by rental time")+
  scale_x_continuous(breaks = seq(0, 23, 1))+
  labs(x = "Hour",y = "Number of rentals")+
  facet_grid(Gender~.)
# we can see that men used bikes much more than other Genders. 
# Also we can see 2 peaks before and after work (8AM an 5PM).

# Here we can find absolute numbers of users by Gender and percentage
# total
length(new.york$V10)
#Male
c(length(subset(new.york$V10,new.york$Gender=="Male")),
  length(subset(new.york$V10,new.york$Gender=="Male"))/length(new.york$V10)*100)

#Female
c(length(subset(new.york$V10,new.york$Gender=="Female")),
  length(subset(new.york$V10,new.york$Gender=="Female"))/length(new.york$V10)*100)

#Undefined
c(length(subset(new.york$V10,new.york$Gender=="")),
  length(subset(new.york$V10,new.york$Gender==""))/length(new.york$V10)*100)

###################### Total #################################
# new variable with all data in 1 table
yz<-data.frame(y=c(new.york$V10,chicago$V10),
           z=c(new.york$Gender,chicago$Gender))

# histogram by Gendre
qplot(x=y,data=yz,bins=24,
      main = "Histogram of number of rentals by rental time")+
  scale_x_continuous(breaks = seq(0, 23, 1))+
  labs(x = "Hour",y = "Number of rentals")+
  facet_grid(z~.)
# we can see that men used bikes much more than other Genders. 
# Also we can see 2 peaks before and after work (8AM an 5PM).

# Here we can find absolute numbers of users by Gender and percentage
hour <- yz$y
# total
length(hour)
#Male
c(length(subset(hour,yz$z=="Male")),
  length(subset(hour,yz$z=="Male"))/length(hour)*100)

#Female
c(length(subset(hour,yz$z=="Female")),
  length(subset(hour,yz$z=="Female"))/length(hour)*100)

#Undefined
c(length(subset(new.york$V10,yz$z=="")),
  length(subset(new.york$V10,yz$z==""))/length(hour)*100)
