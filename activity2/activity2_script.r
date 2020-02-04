#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
(heights_cm)

#look at the first tree height
heights[1]

#look at the 2nd and 3rd tree heights
heights[2:3]

datW <- read.csv("y:\\Students\\atekle\\a02\\2011124.csv")
str(datW)

################
#Question 1

#number of rows
nrow(datW) #157849
#number of columns
ncol(datW) #9

#specify a column with a proper date format
#note the format here dataframe$column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
#google date formatting in r to find more options and learn more

#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))

################
#Question 2
vectors<-c(1,2,3,4,5)
vectors.character<-as.character(vectors)
vectors.numeric<-as.numeric(vectors)
vectors.integer<-as.integer(vectors)
vectors.factor<-as.factor(vectors)

#find out all unique site names
levels(datW$NAME)
#RESULTS
#[1] "ABERDEEN, WA US"                  "LIVERMORE, CA US"                
#[3] "MANDAN EXPERIMENT STATION, ND US" "MORMON FLAT, AZ US"              
#[5] "MORRISVILLE 6 SW, NY US"

#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

#look at the mean maximum temperature for Aberdeen
#with na.rm argument set to true to ingnore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
#Result
#14.68515

#calculate the average daily temperature
#This temperature will be halfway between the minimum and maximum temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change the automatic output of column names to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)


#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

##########################
#Question 3
help(hist)
help(paste)
#answer is written in the word document attached

#############################
#Question 4 
par(mfrow=c(2,2))
#make a histogram for the first site in our levels, Aberdeen
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#histogram for the second site in our level
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="lightblue",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "red",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "red", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "red", 
       lty = 3,
       lwd = 3)

#histogram for the third site in our level
hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="yellow",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "lightblue",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "lightblue", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "lightblue", 
       lty = 3,
       lwd = 3)

#histogram for the 4th site in our level
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="lightblue",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "yellow",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "yellow", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "yellow", 
       lty = 3,
       lwd = 3)

##########################
#Question 5
par(mfrow=c(2,2))
h1 <- hist(datW$TAVE[datW$siteN == 1],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two datasets on the plot. Here both plots share zero as a minimum.
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 3, 
       lty = 1)

h2 <- hist(datW$TAVE[datW$siteN == 2],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[2]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="yellow",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot2 <- seq(0,40, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot2 <-  dnorm(seq(0,40, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two datasets on the plot. Here both plots share zero as a minimum.
y.scaled2 <- (max(h2$density)/max(y.plot2)) * y.plot2

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot2,
       y.scaled2, 
       type = "l", 
       col = "royalblue3",
       lwd = 3, 
       lty = 1)

h3 <- hist(datW$TAVE[datW$siteN == 3],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[3]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="lightblue",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot3 <- seq(-40,40, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot3 <-  dnorm(seq(-40,40, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two datasets on the plot. Here both plots share zero as a minimum.
y.scaled3 <- (max(h3$density)/max(y.plot3)) * y.plot3

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot3,
       y.scaled3, 
       type = "l", 
       col = "royalblue3",
       lwd = 3, 
       lty = 1)

h4 <- hist(datW$TAVE[datW$siteN == 4],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="red",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot4 <- seq(0,40, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot4 <-  dnorm(seq(0,40, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two datasets on the plot. Here both plots share zero as a minimum.
y.scaled4 <- (max(h4$density)/max(y.plot4)) * y.plot4

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot4,
       y.scaled4, 
       type = "l", 
       col = "royalblue3",
       lwd = 3, 
       lty = 1)

##################
#Question 6
current.threshold<-qnorm(0.95,
                         mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                         sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#current.threshold is 18.51026
#increase mean by four and check for probability above the current threshold for extreme temperature
result<-1 - pnorm(current.threshold,
                  mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4,
                  sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#result is 0.2031656

##################
##Question 7
#histogram of daily precipitation for Aberdeen 
par(mfrow=c(1,1))
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily precipitation", 
     ylab="Relative frequency",
     xlim = c(0,100),
     col="grey50",
     border="white")

###################
##Queston 8
#histogram of daily precipitation for Aberdeen 
total.precipitation.bysite <- aggregate(datW$PRCP, by=list(datW$NAME), FUN="sum",na.rm=TRUE)
total.precipitation.bysite
total.prcp.by.siteandyear<-aggregate(datW$PRCP, by=list(datW$NAME,datW$year), FUN="sum",na.rm=TRUE)
colnames(total.prcp.by.siteandyear) <- c("NAME","year","T.PRCP")



###################
##Queston 9
avg.precipitation.bysite <- aggregate(datW$PRCP, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
avg.precipitation.bysite
###################
##Queston 10
#Please refer to the word document.