######################################################
## Migration - Area and Volume Overlap Calculations ##
########  NSERP - Kristin Barker - June 2016  ########
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
library(raster) #prob don't need this one here
library(rgdal)
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

#LISTS OF ANIMALS TO RUN
#Because code to automate this takes too long to run on my subpar computer
list.spr14 <- read.csv("spr14.csv", header = TRUE)
  numelk.spr14 <- nrow(list.spr14)
list.fall14 <- read.csv("fall14.csv", header = TRUE)
  numelk.fall14 <- nrow(list.fall14)
list.spr15 <- read.csv("spr15.csv", header = TRUE)
  numelk.spr15 <- nrow(list.spr15)
list.fall15 <- read.csv("fall15.csv", header = TRUE)
  numelk.fall15 <- nrow(list.fall15)

#DEFINE PROJECTIONS
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")

###########################################################################################
#CALCULATE AREA AND VOLUME OVERLAP
###########################################################################################

######################
#SPRING 2014 MIGRATION

spr14 <- data.frame(matrix(ncol = 3, nrow = numelk.spr14)) #create df wo NAs
colnames(spr14) <- c("AnimalID", "spr14ao", "spr14vi")

for(i in 1:numelk.spr14) {
  elk <- list.spr14[i,]
  
  #subset individual and seasonal locations
  temp_dat_spr14 <- subset(locs, AnimalID == elk) 
  temp_dat_spr14 <- subset(temp_dat_spr14, MigHR == "Winter 2014" | MigHR == "Summer 2014")
  
  #Get xy points, write out the points to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_spr14$Long,"y"=temp_dat_spr14$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_spr14, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection; store results
  ao <- kerneloverlap(xy.spdf.sp[,22], method = "HR", percent = 95)
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  spr14[[i,1]] <- elk
  spr14[[i,2]] <- ao[2,1]
  spr14[[i,3]] <- vol[2,1]
}    

######################
#FALL 2014 MIGRATION

fall14 <- data.frame(matrix(ncol = 3, nrow = numelk.fall14)) #create df wo NAs
colnames(fall14) <- c("AnimalID", "fall14ao", "fall14vi")

for(i in 1:numelk.fall14) {
  elk <- list.fall14[i,]
  
  #subset individual and seasonal locations
  temp_dat_fall14 <- subset(locs, AnimalID == elk) 
  temp_dat_fall14 <- subset(temp_dat_fall14, MigHR == "Summer 2014" | MigHR == "Winter 2015")
  
  #Get xy points, write out the points to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_fall14$Long,"y"=temp_dat_fall14$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_fall14, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection; store results
  ao <- kerneloverlap(xy.spdf.sp[,22], method = "HR", percent = 95)
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  fall14[[i,1]] <- elk
  fall14[[i,2]] <- ao[2,1]
  fall14[[i,3]] <- vol[2,1]
}    
hr <- full_join(fall14, spr14, by = "AnimalID")

######################
#SPRING 2015 MIGRATION

spr15 <- data.frame(matrix(ncol = 3, nrow = numelk.spr15)) #create df wo NAs
colnames(spr15) <- c("AnimalID", "spr15ao", "spr15vi")

for(i in 1:numelk.spr15) {
  elk <- list.spr15[i,]
  
  #subset individual and seasonal locations
  temp_dat_spr15 <- subset(locs, AnimalID == elk) 
  temp_dat_spr15 <- subset(temp_dat_spr15, MigHR == "Winter 2015" | MigHR == "Summer 2015")
  
  #Get xy points, write out the points to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_spr15$Long,"y"=temp_dat_spr15$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_spr15, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection; store results
  ao <- kerneloverlap(xy.spdf.sp[,22], method = "HR", percent = 95)
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  spr15[[i,1]] <- elk
  spr15[[i,2]] <- ao[2,1]
  spr15[[i,3]] <- vol[2,1]
}    
hr <- full_join(hr, spr15, by = "AnimalID")

######################
#FALL 2015 MIGRATION

fall15 <- data.frame(matrix(ncol = 3, nrow = numelk.fall15)) #create df wo NAs
colnames(fall15) <- c("AnimalID", "fall15ao", "fall15vi")

for(i in 1:numelk.fall15) {
  elk <- list.fall15[i,]
  
  #subset individual and seasonal locations
  temp_dat_fall15 <- subset(locs, AnimalID == elk) 
  temp_dat_fall15 <- subset(temp_dat_fall15, MigHR == "Summer 2015" | MigHR == "Winter 2016")
  
  #Get xy points, write out the points to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_fall15$Long,"y"=temp_dat_fall15$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_fall15, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection; store results
  ao <- kerneloverlap(xy.spdf.sp[,22], method = "HR", percent = 95)
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  fall15[[i,1]] <- elk
  fall15[[i,2]] <- ao[2,1]
  fall15[[i,3]] <- vol[2,1]
} 
hr <- full_join(hr, fall15, by = "AnimalID")

hr <- hr[ ,c("AnimalID", "spr14ao", "spr14vi", "fall14ao", "fall14vi",
                         "spr15ao", "spr15vi", "fall15ao", "fall15vi"])