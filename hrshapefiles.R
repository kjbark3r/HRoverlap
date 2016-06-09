######################################################
## Migration - Area Overlap of Seasonal Home Ranges ##
######  NSERP - Kristin Barker - May/June 2016  ######
######################################################

##SET WD
####Work computer, personal laptop, or external hard drive
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\NSERP_AreaOverlap"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\NSERP_AreaOverlap"
wd_external <- "E:\\Kristins\\NSERP_AreaOverlap\\"

if (file.exists(wd_workcomp)) {
  setwd(wd_workcomp)
} else {
  if(file.exists(wd_laptop)) {
    setwd(wd_laptop)
  } else {
    if(file.exists(wd_external)) {
      setwd(wd_external)
    } else {
      cat("Are you SURE you got that file path right?\n")
    }
  }
}
rm(wd_workcomp, wd_laptop, wd_external)

##LOAD PACKAGES
library(sp) #for kernel centroid estimate
library(adehabitatHR) #for kernel centroid estimate
library(raster)
library(rgdal)
library(gsubfn)
library(maptools) #for writeSpatialShape

###########################################################################################
#GENERATE SUMMER AND WINTER KDE AND MCP RANGES FOR EACH ANIMAL
###########################################################################################

gpsdata <- read.csv("collardata-locsonly-equalsampling.csv", as.is = TRUE, header = TRUE)
gpsdata$Date <- as.Date(gpsdata$Date, "%Y-%m-%d")

#CREATE LISTS OF ANIMALS TO RUN
#I did this manually based on transmission end dates (code took too long to run)
elklist.w14 <- read.csv("w14.csv", header = TRUE)
	numelk.w14 <- nrow(elklist.w14)
elklist.w15 <- read.csv("w15.csv", header = TRUE)
	numelk.w15 <- nrow(elklist.w15)
elklist.w16 <- read.csv("w16.csv", header = TRUE)
	numelk.w16 <- nrow(elklist.w16)
elklist.s14 <- read.csv("s14.csv", header = TRUE)
	numelk.s14 <- nrow(elklist.s14)
elklist.s15 <- read.csv("s15.csv", header = TRUE)
	numelk.s15 <- nrow(elklist.s15)

############
#WINTER 2014
for(i in 1:numelk.w14) {
  ######### PER-ANIMAL CODE 
  elk=elklist.w14[i,]
  
  temp_dat_win14 <- subset(gpsdata, gpsdata$AnimalID == elk) #Subset individual animal
  temp_dat_win14 <- subset(temp_dat_win14, temp_dat_win14$Date >= as.Date("2014-01-01") & temp_dat_win14$Date <= as.Date("2014-03-31"))
  
  #Get xy points, write out the points to dataframe, to spatial data frame, to shapefile
  xy <- data.frame("x"=temp_dat_win14$Long,"y"=temp_dat_win14$Lat)
  latlong = CRS("+init=epsg:4326")
  stateplane = CRS("+init=epsg:2818")
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_win14, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  #writeOGR(xy.spdf.sp, ".", paste(elk, "_win14_pts", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeSpatialShape(xy.spdf.sp, paste(elk, "_win14_pts", sep=""))
  
  # WinterKDE
  kud <- kernelUD(xy.spdf.sp, h="href", grid = 5000)
  image(kud)

  #kud.raster <- raster(kud)
  #writeRaster(kud.raster, paste(elk, "win14_kde.tif", sep=""), format="GTiff", overwrite=TRUE)
  
  #Define 95% volume contour and export to shapefile, extract area
  w14.95vol <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km")
  #writeOGR(w14.95vol, ".", paste(elk, "_win14_vol95", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeSpatialShape(w14.95vol, paste(elk, "_win14_vol95", sep=""))
 
  #95% MCP home range  [commenting out for sake of speed in preliminary analysis May2016]
  #xysp=SpatialPoints(xy,proj4string = latlong)
  #wmcp=mcp(xysp, percent=95, unin="m", unout="km")
  #writeOGR(wmcp, ".", paste(elk, "_win14_mcp", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
}    

############
#SUMMER 2014
for(i in 1:numelk.s14) {
  ######### PER-ANIMAL CODE 
  elk=elklist.s14[i,]

  temp_dat_sum14 <- subset(gpsdata, gpsdata$AnimalID == elk)
  temp_dat_sum14 <- subset(temp_dat_sum14, temp_dat_sum14$Date >= as.Date("2014-06-01") & temp_dat_sum14$Date <= as.Date("2014-08-31"))
  #Get xy points, write out the points to dataframe, to spatial data frame, to shapefile
  xy <- data.frame("x"=temp_dat_sum14$Long,"y"=temp_dat_sum14$Lat)
  latlong = CRS("+init=epsg:4326")
  stateplane = CRS("+init=epsg:2818")
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_sum14, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  #writeOGR(xy.spdf.sp, ".", paste(elk, "_sum14_pts", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeSpatialShape(xy.spdf.sp, paste(elk, "_sum14_pts", sep=""))
  
  # KDE
  kud <- kernelUD(xy.spdf.sp, h="href", grid = 5000)
  image(kud)
  #kud.raster <- raster(kud)
  #	writeRaster(kud.raster, paste(elk, "sum14_kde.tif", sep=""), format="GTiff", overwrite=TRUE)
  #Define 95% volume contour and export to shapefile, extract area
  s14.95vol<- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km")
  #writeOGR(s14.95vol, ".", paste(elk, "_sum14_vol95", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeSpatialShape(s14.95vol, paste(elk, "_sum14_vol95", sep=""))
  
  #95% MCP home range  [commenting out for sake of speed in preliminary analysis May2016]
  #xysp=SpatialPoints(xy,proj4string = latlong)
  #smcp=mcp(xysp, percent=95, unin="m", unout="km")
  #writeOGR(smcp, ".", paste(elk, "_sum14_mcp", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
}

############
#WINTER 2015
for(i in 1:numelk.w15) {
  ######### PER-ANIMAL CODE 
  elk=elklist.w15[i,]
  
  temp_dat_win15 <- subset(gpsdata, gpsdata$AnimalID == elk) #Subset individual animal
  temp_dat_win15 <- subset(temp_dat_win15, temp_dat_win15$Date >= as.Date("2015-01-01") & temp_dat_win14$Date <= as.Date("2015-03-31"))
  
  #Get xy points, write out the points to dataframe, to spatial data frame, to shapefile
  xy <- data.frame("x"=temp_dat_win15$Long,"y"=temp_dat_win15$Lat)
  latlong = CRS("+init=epsg:4326")
  stateplane = CRS("+init=epsg:2818")
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_win15, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  #writeOGR(xy.spdf.sp, ".", paste(elk, "_win15_pts", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeSpatialShape(xy.spdf.sp, paste(elk, "_win15pts", sep=""))
  
  # WinterKDE
  kud <- kernelUD(xy.spdf.sp, h="href", grid = 5000)
  image(kud)

  #kud.raster <- raster(kud)
  #writeRaster(kud.raster, paste(elk, "win15_kde.tif", sep=""), format="GTiff", overwrite=TRUE)
  
  #Define 95% volume contour and export to shapefile, extract area
  w15.95vol<- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km")
  #writeOGR(w15.95vol, ".", paste(elk, "_win15_vol95", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeSpatialShape(w15.95vol, paste(elk, "_win15_vol95", sep=""))
  
  #95% MCP home range  [commenting out for sake of speed in preliminary analysis May2016]
  #xysp=SpatialPoints(xy,proj4string = latlong)
  #wmcp=mcp(xysp, percent=95, unin="m", unout="km")
  #writeOGR(wmcp, ".", paste(elk, "_win15_mcp", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
}    

############
#SUMMER 2015
for(i in 1:numelk.s15) {
  ######### PER-ANIMAL CODE 
  elk=elklist.s15[i,]

  temp_dat_sum15 <- subset(gpsdata, gpsdata$AnimalID == elk)
  temp_dat_sum15 <- subset(temp_dat_sum15, temp_dat_sum15$Date >= as.Date("2015-06-01") & temp_dat_sum14$Date <= as.Date("2015-08-31"))
  #Get xy points, write out the points to dataframe, to spatial data frame, to shapefile
  xy <- data.frame("x"=temp_dat_sum15$Long,"y"=temp_dat_sum15$Lat)
  latlong = CRS("+init=epsg:4326")
  stateplane = CRS("+init=epsg:2818")
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_sum15, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  #writeOGR(xy.spdf.sp, ".", paste(elk, "_sum15_pts", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeSpatialShape(xy.spdf.sp, paste(elk, "_sum15pts", sep=""))
  
  # KDE
  kud <- kernelUD(xy.spdf.sp, h="href", grid = 5000)
  image(kud)
  #kud.raster <- raster(kud)
  #	writeRaster(kud.raster, paste(elk, "sum15_kde.tif", sep=""), format="GTiff", overwrite=TRUE)
  #Define 95% volume contour and export to shapefile, extract area
  s15.95vol<- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km")
  #writeOGR(s15.95vol, ".", paste(elk, "_sum15_vol95", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeSpatialShape(s15.95vol, paste(elk, "_sum15_vol95", sep=""))
  
  #95% MCP home range  [commenting out for sake of speed in preliminary analysis May2016]
  #xysp=SpatialPoints(xy,proj4string = latlong)
  #smcp=mcp(xysp, percent=95, unin="m", unout="km")
  #writeOGR(smcp, ".", paste(elk, "_sum15_mcp", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
}

############
#WINTER 2016
for(i in 1:numelk.w16) {
  ######### PER-ANIMAL CODE 
  elk=elklist.w16[i,]
  
  temp_dat_win16 <- subset(gpsdata, gpsdata$AnimalID == elk) #Subset individual animal
  temp_dat_win16 <- subset(temp_dat_win16, temp_dat_win16$Date >= as.Date("2016-01-01") & temp_dat_win14$Date <= as.Date("2016-03-31"))
  
  #Get xy points, write out the points to dataframe, to spatial data frame, to shapefile
  xy <- data.frame("x"=temp_dat_win16$Long,"y"=temp_dat_win16$Lat)
  latlong = CRS("+init=epsg:4326")
  stateplane = CRS("+init=epsg:2818")
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_win16, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  #writeOGR(xy.spdf.sp, ".", paste(elk, "_win15_pts", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeSpatialShape(xy.spdf.sp, paste(elk, "_win16pts", sep=""))
  
  # WinterKDE
  kud <- kernelUD(xy.spdf.sp, h="href", grid = 5000)
  image(kud)
  
  #kud.raster <- raster(kud)
  #writeRaster(kud.raster, paste(elk, "win15_kde.tif", sep=""), format="GTiff", overwrite=TRUE)
  
  #Define 95% volume contour and export to shapefile, extract area
  w16.95vol<- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km")
  #writeOGR(w16.95vol, ".", paste(elk, "_win16_vol95", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeSpatialShape(w16.95vol, paste(elk, "_win16_vol95", sep=""))
  
  #95% MCP home range  [commenting out for sake of speed in preliminary analysis May2016]
  #xysp=SpatialPoints(xy,proj4string = latlong)
  #wmcp=mcp(xysp, percent=95, unin="m", unout="km")
  #writeOGR(wmcp, ".", paste(elk, "_win15_mcp", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
}    

###########################################################################################
##CALCULATE AREA OF OVERLAP BETWEEN SEASONAL RANGES
##For each individual
###########################################################################################

library(rgeos)

#remove previous elk's values to ensure correct numbers are being used
rm(res,sum,sum_area,win,win_area,overlap_area)

#Had to remove last slash from file paths to open shp's (change from Kelly's code)
win<-readOGR(dsn = "C:/Users/kjbark3r/Documents/GitHub/AreaOverlap_Test", layer = "35018_win_vol95")  #Read in shapefile of 95% volume contour
#plot(win)
win_area<-area(win)
sum<-readOGR(dsn = 'C:/Users/kjbark3r/Documents/GitHub/AreaOverlap_Test', layer = "35018_sum_vol95")  #Read in shapefile of 95% volume contour
#plot(sum)
sum_area<-area(sum)
res <- gIntersection(win, sum, byid=FALSE, drop_lower_td=FALSE)
#plot(res)
overlap_area<-area(res)

(percent_overlap<-(overlap_area/sum_area)*100)
