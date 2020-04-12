#install.packages(c("raster","sp","rgdal","rgeos","plyr"))
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)
#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_1966.shp")
g1998 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_1998.shp")
g2005 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_2005.shp")
g2015 <- readOGR("Y:\\Students\\hkropp\\a06\\GNPglaciers\\GNPglaciers_2015.shp")

str(g2015)
#data stores all accompanying info/measurements for each spatial object
head(g2015@data)

#polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]
#Question 1
g1966@proj4string

# spplot(g1966, "GLACNAME")
# #check glacier names
# g1966@data$GLACNAME
# 
# g2015@data$GLACNAME
#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))
#read in rgb imagery from landsat
redL <- raster("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_red.tif")
greenL <- raster("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_green.tif")
blueL <- raster("Y:\\Students\\hkropp\\a06\\glacier_09_05_14\\l08_blue.tif")

#check coordinate system
redL@crs
#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <-raster(paste0("Y:\\Students\\hkropp\\a06\\NDVI\\NDVI_",ndviYear[i],".tif"))
}
#Question 2
str(NDVIraster[[1]])
#get projection
NDVIraster[[1]]@crs
plot(NDVIraster[[1]])

#Question 3

par(mai=c(0.5,0.5,0.5,0.5))
par(mfrow= c(1,2))
plot(NDVIraster[[1]], axes=T)
plot(g1966, col="black", axes=T)

#Question4
par(mfrow= c(1,1))
par(mai=c(0.5,0.5,0.5,0.5))
plot(NDVIraster[[13]], axes=F)
plot(g2015p, add=TRUE, border="black", axes=F)
#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#Question5

#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

gAllp1 <- plyr::join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <-  plyr::join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <-  plyr::join(gAllp2,g2015p@data, by="GLACNAME", type="full")

plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
} 

for(i in 1:39){
  # Calculate percentage change in area 
  gAll$a2015percentage.change[i]<- (abs(area(g2015p)[i]-area(g1966p)[i])/area(g1966p)[i])*100
  
  #add it as a column to the g2015p  data
  g2015p@data$a2015percentage.change[i]<- (abs(area(g2015p)[i]-area(g1966p)[i])/area(g1966p)[i])*100
}

spplot(g2015p, "a2015percentage.change")

#Question6
diffPoly <- gDifference(g1966p, g2015p)
plot(diffPoly)
#plot with NDVI
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
plot(diffPoly,col="black", border=NA,add=TRUE)

maxloss<- gAll$GLACNAME[gAll$a2015percentage.change==max(gAll$a2015percentage.change)] 
max(gAll$a2015percentage.change) #84.72
#########################################
par(mfrow= c(1,1))
par(mar=c(3,3,3,3))
par(oma=c(0,0,2,0))
plotRGB(rgbL, ext=c(271000,279600,5420000,5431000), stretch="lin"
        , axes=TRUE,main="Boulder Glacier area that has experienced 84.72% loss in 1966 ")
plot(subset(g1966, g1966@data$GLACNAME=="Boulder Glacier"), col="blue", border=NA, add=T)

par(mfrow= c(1,1))
par(mar=c(3,3,3,3))
par(oma=c(0,0,2,0))
plotRGB(rgbL, ext=c(271000,279600,5420000,5431000), stretch="lin", axes=TRUE
        ,main="Boulder Glacier area that has experienced 84.72% loss in 1998 ")
plot(subset(g1998, g1998@data$GLACNAME=="Boulder Glacier"), add=TRUE, border=NA, col="blue")

par(mfrow= c(1,1))
par(mar=c(3,3,3,3))
par(oma=c(0,0,2,0))
plotRGB(rgbL, ext=c(271000,279600,5420000,5431000), stretch="lin", axes=TRUE
        ,main="Boulder Glacier area that has experienced 84.72% loss in 2005 ")
plot(subset(g2005, g2005@data$GLACNAME=="Boulder Glacier"), add=TRUE, border=NA, col="blue")

par(mfrow= c(1,1))
par(mar=c(3,3,3,3))
par(oma=c(0,0,2,0))
plotRGB(rgbL, ext=c(271000,279600,5420000,5431000), stretch="lin", axes=TRUE
        ,main="Boulder Glacier area that has experienced 84.72% loss in 2015")
plot(subset(g2015, g2015@data$GLACNAME=="Boulder Glacier"), add=TRUE, border=NA, col="blue")


#Question7

#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}

plot(ndviYear, meanDiff, type="b",
     xlab= "Year",
     ylab="Average NDVI (unitless)",
     pch=19)

#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)


# Question 8
#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units
#convert to a raster
par(mai=c(0.4,0.4,0.4,0.4))
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)

#rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)
#######################################
#Question9
meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)


#remove zone zero from mean change in NDVI per year
meanChange<-meanChange[-1,]

# add mean change in NDVI per year into the 2015 glacier polygons
g2015@data$meanChange<-meanChange[,2]

# map where the mean change in vegetation is color coded within the 2015 glacier polygons
spplot(g2015, "meanChange")

#Question10
range_NDVI<-range(meanChange[,2]) # range of mean change in NDVI values
# Range: [-0.0015, 0.0047]

# proportion of values in meanChange which are positive
prop_pos_NDVI<- length(subset(meanChange[,2], meanChange[,2]>0))/39 # 74%(0.7436)

#Question11

NDVIfit_avg <- calc(NDVIstack,mean) 

meanChange <- zonal(NDVIfit_avg, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply

for(i in 1:39){
  g2015p@data$NDVImean[i]<- meanChange[i,2]
}


plot(NDVIfit_avg, axes=F, legend= T, main="Glacier extent by surrounding\nmaximum NDVI average")
g2015p@data$NDVIcol <- ifelse(g2015p@data$NDVImean<0.2,"red",
                              ifelse(g2015p@data$NDVImean<0.3, "blue",
                                     ifelse(g2015p@data$NDVImean<0.4, "lightblue",
                                            ifelse(g2015p@data$NDVImean<0.5, "lightgreen", 
                                                   ifelse(g2015p@data$NDVImean<0.6, "orange", "yellow")))))
legend("left", c("<0.2","<0.3","<0.4", 
                    "<0.5", "<0.6", ">=0.6"), fill=c("red", "blue", "lightblue", "lightgreen",
                                                     "orange", "yellow" ), horiz=FALSE,cex=1)
plot(g2015p, add=TRUE, col=paste(g2015p@data$NDVIcol),border=FALSE)
