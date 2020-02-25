#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("y:\\Students\\atekle\\a03\\bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#data without skipping first 3 rows
datWwithoutskip <- read.csv("y:\\Students\\atekle\\a03\\bewkes_weather.csv",
                 na.strings=c("#N/A"), header=FALSE)
#get sensor info from file
# this data table will contain all relevent units
sensorInfo <-   read.csv("y:\\Students\\atekle\\a03\\bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)
#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)

library(lubridate)
#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))
## [1] 0
#wind speed
length(which(is.na(datW$wind.speed)))
## [1] 0
#precipitation
length(which(is.na(datW$precipitation)))
## [1] 0
#soil temperature
length(which(is.na(datW$soil.moisture)))
## [1] 707
#soil moisture
length(which(is.na(datW$soil.temp)))
## [1] 707

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#changing below freezing point values to NA
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,] 

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  


#Question 5
#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#create a lightscale column in datW
datW$lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy

assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}
#check if the values are equal and return error if they are not equal 
assert(isTRUE(all.equal(datW$DD[lightscale > 0],datW$DD[datW$lightscale > 0])),"They are not equal")


#Question 6
#the first argument is a logical statement to be evaluated as true or false on a vector
#the second argument is the value that my air.tempQ1 column will be given if the statement
#is true. The last value is the value that will be given to air.tempQ1 if the statement is false.
#In this case it is just given the air temperature value
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)
#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.
#create a new wind speed column
datW$wind.speed2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed ))
#wind.speed2 NA values  cannot be greater than wind.speed NA values
assert(length(which(is.na(datW$wind.speed)))<=length(which(is.na(datW$wind.speed2))),"The wind speed has more NA values.")

plot(datW$DD,datW$wind.speed2,xlab="Day of year",ylab = "wind speed in ms",type="b")

#Question 7
#make a plot with filled in points (using pch)
#line lines
#look at the soil temperature and moisture data before day 193
plot(datW$DD[datW$DD>189 & datW$DD<193], datW$soil.moisture[datW$DD>189 & datW$DD<193], pch=19, type="b", xlab = "Day of Year",
     ylab="soil moisture in cm3 water per cm3 soil")

plot(datW$DD[datW$DD>189 & datW$DD<193], 
     datW$soil.temp[datW$DD>189 & datW$DD<193], pch=19, type="b", xlab = "Day of Year",
     ylab="Soil temperature (degrees C)")

#check the difference between the soil temperature and air temperature
check.difference<-datW$soil.temp[datW$DD>189 & datW$DD<193]-datW$air.temperature[datW$DD>189 & datW$DD<193]
plot(datW$DD[datW$DD>189 & datW$DD<193], 
     check.difference, pch=19, type="b", xlab = "Day of Year",
     ylab="Difference of soil and air temperature (degrees C)")
#Question 8
Avg.air.temp<-mean(datW$air.temperature)
Avg.wind.speed<-mean(datW$wind.speed)
Avg.soil.moisture<-mean(datW$soil.moisture,na.rm=TRUE)
Avg.soil.temp<-mean(datW$soil.temp,na.rm=TRUE)
total.precipitation<-sum(datW$precipitation)
len.air.temp<-length(datW$air.temperature)
len.wind.speed<-length(datW$wind.speed)
len.soil.moisture<-length(which(!is.na(datW$soil.moisture)))
len.soil.temp<-length(which(!is.na(datW$soil.temp)))
len.total.precipitation<-length(datW$precipitation)
starting.date<-min(datW$doy)
End.date<-max(datW$doy)
#Question 9
plot(datW$DD,datW$soil.moisture,xlab="Day of year",ylab = "soil moisture in cm3 water per cm3 soil",type="b")
plot(datW$DD,datW$air.temperature,xlab="Day of year",ylab = "air temp in degree c",type="b")
plot(datW$DD,datW$soil.temp,xlab="Day of year",ylab = "soil temp in degree c",type="b")
plot(datW$DD,datW$precipitation,xlab="Day of year",ylab = "precipitation in mm",type="b")

