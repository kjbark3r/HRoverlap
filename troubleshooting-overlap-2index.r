######################################################
## Migration - Area and Volume Overlap Calculations ##
########  NSERP - Kristin Barker - June 2016  ########
######################################################

##SET WD
####Work computer, personal laptop, or external hard drive
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\HRoverlap"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\HRoverlap"
wd_external <- "E:\\Kristins\\HRoverlap\\"

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
library(raster) #prob don't need this one here
library(rgdal) #for latlong/stateplane conversions
library(gsubfn)
library(maptools) #for writeSpatialShape
library(dplyr) #for joins

###########################################################################################
#SET UP DATA
###########################################################################################

#GPS DATA FROM COLLARS, PLUS MIGRATION SEASON/YEAR
locs <- read.csv("collardata-locsonly-equalsampling.csv", as.is = TRUE, header = TRUE)
locs$Date <- as.Date(locs$Date, "%Y-%m-%d")
#can remove extraneous columns here#
locs$MigHR <- ifelse(between(locs$Date, as.Date("2014-01-01"), as.Date("2014-03-31")), "Winter 2014", 
                ifelse(between(locs$Date, as.Date("2014-06-01"), as.Date("2014-08-31")), "Summer 2014", 
                  ifelse(between(locs$Date, as.Date("2015-01-01"), as.Date("2015-03-31")), "Winter 2015", 
                    ifelse(between(locs$Date, as.Date("2015-06-01"), as.Date("2015-08-31")), "Summer 2015",
                      ifelse(between(locs$Date, as.Date("2016-01-01"), as.Date("2016-03-31")), "Winter 2016",
                        ifelse(NA))))))

#DEFINE PROJECTIONS
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")

###########################################################################################
# RUN A -- INDEXED [1,2]
###########################################################################################

######################
#SPRING 2014 MIGRATION

list.spr14a <- read.csv("spr14.csv", header = TRUE)
  numelk.spr14a <- nrow(list.spr14a)
  
spr14a <- data.frame(matrix(ncol = 3, nrow = numelk.spr14a)) #create df wo NAs
colnames(spr14a) <- c("AnimalID", "spr14aao", "spr14avi")

for(i in 1:numelk.spr14a) {
  elk <- list.spr14a[i,]
  
  #subset individual and seasonal locations
  temp_dat_spr14a <- subset(locs, AnimalID == elk) 
  temp_dat_spr14a <- subset(temp_dat_spr14a, MigHR == "Winter 2014" | MigHR == "Summer 2014")
  
  #Get xy points, write out the points to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_spr14a$Long,"y"=temp_dat_spr14a$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_spr14a, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection; store results
  ao <- kerneloverlap(xy.spdf.sp[,22], method = "HR", percent = 95)
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  spr14a[[i,1]] <- elk
  spr14a[[i,2]] <- ao[2,1]
  spr14a[[i,3]] <- vol[2,1]
}    

######################
#FALL 2014 MIGRATION

list.fall14a <- read.csv("fall14.csv", header = TRUE)
  numelk.fall14a <- nrow(list.fall14a)
  
fall14a <- data.frame(matrix(ncol = 3, nrow = numelk.fall14a)) #create df wo NAs
colnames(fall14a) <- c("AnimalID", "fall14aao", "fall14avi")

for(i in 1:numelk.fall14a) {
  elk <- list.fall14a[i,]
  
  #subset individual and seasonal locations
  temp_dat_fall14a <- subset(locs, AnimalID == elk) 
  temp_dat_fall14a <- subset(temp_dat_fall14a, MigHR == "Summer 2014" | MigHR == "Winter 2015")
  
  #Get xy points, write out the points to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_fall14a$Long,"y"=temp_dat_fall14a$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_fall14a, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection; store results
  ao <- kerneloverlap(xy.spdf.sp[,22], method = "HR", percent = 95)
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  fall14a[[i,1]] <- elk
  fall14a[[i,2]] <- ao[2,1]
  fall14a[[i,3]] <- vol[2,1]
}    
hra <- full_join(fall14a, spr14a, by = "AnimalID")

######################
#SPRING 2015 MIGRATION

list.spr15a <- read.csv("spr15.csv", header = TRUE)
  numelk.spr15a <- nrow(list.spr15a)

spr15a <- data.frame(matrix(ncol = 3, nrow = numelk.spr15a)) #create df wo NAs
colnames(spr15a) <- c("AnimalID", "spr15aao", "spr15avi")

for(i in 1:numelk.spr15a) {
  elk <- list.spr15a[i,]
  
  #subset individual and seasonal locations
  temp_dat_spr15a <- subset(locs, AnimalID == elk) 
  temp_dat_spr15a <- subset(temp_dat_spr15a, MigHR == "Winter 2015" | MigHR == "Summer 2015")
  
  #Get xy points, write out the points to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_spr15a$Long,"y"=temp_dat_spr15a$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_spr15a, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection; store results
  ao <- kerneloverlap(xy.spdf.sp[,22], method = "HR", percent = 95)
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  spr15a[[i,1]] <- elk
  spr15a[[i,2]] <- ao[2,1]
  spr15a[[i,3]] <- vol[2,1]
}    
hra <- full_join(hra, spr15a, by = "AnimalID")

######################
#FALL 2015 MIGRATION

list.fall15a <- read.csv("fall15.csv", header = TRUE)
  numelk.fall15a <- nrow(list.fall15a)
  
fall15a <- data.frame(matrix(ncol = 3, nrow = numelk.fall15a)) #create df wo NAs
colnames(fall15a) <- c("AnimalID", "fall15aao", "fall15avi")

for(i in 1:numelk.fall15a) {
  elk <- list.fall15a[i,]
  
  #subset individual and seasonal locations
  temp_dat_fall15a <- subset(locs, AnimalID == elk) 
  temp_dat_fall15a <- subset(temp_dat_fall15a, MigHR == "Summer 2015" | MigHR == "Winter 2016")
  
  #Get xy points, write out the points to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_fall15a$Long,"y"=temp_dat_fall15a$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_fall15a, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection; store results
  ao <- kerneloverlap(xy.spdf.sp[,22], method = "HR", percent = 95)
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  fall15a[[i,1]] <- elk
  fall15a[[i,2]] <- ao[2,1]
  fall15a[[i,3]] <- vol[2,1]
} 
hra <- full_join(hra, fall15a, by = "AnimalID")

hra <- hra[,c("AnimalID", "spr14aao", "spr14avi", "fall14aao", "fall14avi",
                         "spr15aao", "spr15avi", "fall15aao", "fall15avi")]

###########################################################################################
# RUN B -- INDEXED [2,1]
###########################################################################################

######################
#SPRING 2014 MIGRATION

list.spr14b <- read.csv("spr14.csv", header = TRUE)
  numelk.spr14b <- nrow(list.spr14b)
  
spr14b <- data.frame(matrix(ncol = 3, nrow = numelk.spr14b)) #create df wo NAs
colnames(spr14b) <- c("AnimalID", "spr14bao", "spr14bvi")

for(i in 1:numelk.spr14b) {
  elk <- list.spr14b[i,]
  
  #subset individual and seasonal locations
  temp_dat_spr14b <- subset(locs, AnimalID == elk) 
  temp_dat_spr14b <- subset(temp_dat_spr14b, MigHR == "Winter 2014" | MigHR == "Summer 2014")
  
  #Get xy points, write out the points to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_spr14b$Long,"y"=temp_dat_spr14b$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_spr14b, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection; store results
  ao <- kerneloverlap(xy.spdf.sp[,22], method = "HR", percent = 95)
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  spr14b[[i,1]] <- elk
  spr14b[[i,2]] <- ao[2,1]
  spr14b[[i,3]] <- vol[2,1]
}    

######################
#FALL 2014 MIGRATION

list.fall14b <- read.csv("fall14.csv", header = TRUE)
  numelk.fall14b <- nrow(list.fall14b)
  
fall14b <- data.frame(matrix(ncol = 3, nrow = numelk.fall14b)) #create df wo NAs
colnames(fall14b) <- c("AnimalID", "fall14bao", "fall14bvi")

for(i in 1:numelk.fall14b) {
  elk <- list.fall14b[i,]
  
  #subset individual and seasonal locations
  temp_dat_fall14b <- subset(locs, AnimalID == elk) 
  temp_dat_fall14b <- subset(temp_dat_fall14b, MigHR == "Summer 2014" | MigHR == "Winter 2015")
  
  #Get xy points, write out the points to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_fall14b$Long,"y"=temp_dat_fall14b$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_fall14b, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection; store results
  ao <- kerneloverlap(xy.spdf.sp[,22], method = "HR", percent = 95)
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  fall14b[[i,1]] <- elk
  fall14b[[i,2]] <- ao[2,1]
  fall14b[[i,3]] <- vol[2,1]
}    
hrb <- full_join(fall14b, spr14b, by = "AnimalID")

######################
#SPRING 2015 MIGRATION

list.spr15b <- read.csv("spr15.csv", header = TRUE)
  numelk.spr15b <- nrow(list.spr15b)

spr15b <- data.frame(matrix(ncol = 3, nrow = numelk.spr15b)) #create df wo NAs
colnames(spr15b) <- c("AnimalID", "spr15bao", "spr15bvi")

for(i in 1:numelk.spr15b) {
  elk <- list.spr15b[i,]
  
  #subset individual and seasonal locations
  temp_dat_spr15b <- subset(locs, AnimalID == elk) 
  temp_dat_spr15b <- subset(temp_dat_spr15b, MigHR == "Winter 2015" | MigHR == "Summer 2015")
  
  #Get xy points, write out the points to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_spr15b$Long,"y"=temp_dat_spr15b$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_spr15b, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection; store results
  ao <- kerneloverlap(xy.spdf.sp[,22], method = "HR", percent = 95)
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  spr15b[[i,1]] <- elk
  spr15b[[i,2]] <- ao[2,1]
  spr15b[[i,3]] <- vol[2,1]
}    
hrb <- full_join(hrb, spr15b, by = "AnimalID")

######################
#FALL 2015 MIGRATION

list.fall15b <- read.csv("fall15.csv", header = TRUE)
  numelk.fall15b <- nrow(list.fall15b)
  
fall15b <- data.frame(matrix(ncol = 3, nrow = numelk.fall15b)) #create df wo NAs
colnames(fall15b) <- c("AnimalID", "fall15bao", "fall15bvi")

for(i in 1:numelk.fall15b) {
  elk <- list.fall15b[i,]
  
  #subset individual and seasonal locations
  temp_dat_fall15b <- subset(locs, AnimalID == elk) 
  temp_dat_fall15b <- subset(temp_dat_fall15b, MigHR == "Summer 2015" | MigHR == "Winter 2016")
  
  #Get xy points, write out the points to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_fall15b$Long,"y"=temp_dat_fall15b$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_fall15b, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection; store results
  ao <- kerneloverlap(xy.spdf.sp[,22], method = "HR", percent = 95)
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  fall15b[[i,1]] <- elk
  fall15b[[i,2]] <- ao[2,1]
  fall15b[[i,3]] <- vol[2,1]
} 
hrb <- full_join(hrb, fall15b, by = "AnimalID")

hrb <- hrb[,c("AnimalID", "spr14bao", "spr14bvi", "fall14bao", "fall14bvi",
                         "spr15bao", "spr15bvi", "fall15bao", "fall15bvi")]
						 

#ISSUES - AREA OVERLAP = 1; VOLUME INTERSECTION = 0
#[2,1] 
	#spr14
		#141500
	#fall14
		#NA
	#spr15
		#140800
	#fall15
		#NA
#[1,2] 
	#spr14
		#141500
	#fall14
		#NA
	#spr15
		#140800
	#fall15
		#NA