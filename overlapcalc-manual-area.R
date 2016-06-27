##########################################################
## Migration - Area Overlap Calculation - no adehabitat ##
########  NSERP - Kristin Barker - June 2016  ############
##########################################################

## SET WD

#work computer, personal laptop, or external hard drive
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

## PACKAGES, DATA, LISTS, PROJECTION INFO

#packages
library(rgeos)
library(rgdal)
library(adehabitatHR)
library(dplyr)

#data
locs <- read.csv("locsMigHR2.csv", as.is = TRUE, header = TRUE)

#lists
list.spr14 <- read.csv("spr14.csv", header = TRUE)
numelk.spr14 <- nrow(list.spr14)
list.fall14 <- read.csv("fall14.csv", header = TRUE)
numelk.fall14 <- nrow(list.fall14)
list.spr15 <- read.csv("spr15.csv", header = TRUE)
numelk.spr15 <- nrow(list.spr15)
list.fall15 <- read.csv("fall15.csv", header = TRUE)
numelk.fall15 <- nrow(list.fall15)

#projections
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")


###########################################################################################
#CALCULATE AREA OVERLAP 
###########################################################################################


######################
#SPRING 2014 MIGRATION
spr14 <- data.frame(matrix(ncol = 2, nrow = numelk.spr14)) #create df wo NAs
colnames(spr14) <- c("AnimalID", "spr14ao")

for(i in 1:numelk.spr14) {
  elk <- list.spr14[i,]
  
  #winter 2014
  temp.win14 <- subset(locs, AnimalID == elk & MigHR == "Winter 2014")
  xy.win14 <- data.frame("x" = temp.win14$Long, "y" = temp.win14$Lat)
  ll.win14 <- SpatialPointsDataFrame(xy.win14, temp.win14, proj4string = latlong)
  sp.win14 <- spTransform(ll.win14, stateplane)
  kud.win14 <- kernelUD(sp.win14, h="href", grid = 5000)
  vol.win14 <- getverticeshr(kud.win14, percent = 95, ida = NULL, unin = "m", unout = "km")
  a.win14 <- sapply(slot(vol.win14, "polygons"), slot, "area")
  
  #summer 2014
  temp.sum14 <- subset(locs, AnimalID == elk & MigHR == "Summer 2014")
  xy.sum14 <- data.frame("x" = temp.sum14$Long, "y" = temp.sum14$Lat)
  ll.sum14 <- SpatialPointsDataFrame(xy.sum14, temp.sum14, proj4string = latlong)
  sp.sum14 <- spTransform(ll.sum14, stateplane)
  kud.sum14 <- kernelUD(sp.sum14, h="href", grid = 5000)
  vol.sum14 <- getverticeshr(kud.sum14, percent = 95, ida = NULL, unin = "m", unout = "km")
  
  #overlap - calc and store
  i.spr14 <- gIntersection(vol.win14, vol.sum14, byid=FALSE)
  ifelse(is.null(i.spr14), ai.spr14 <- 0, ai.spr14 <- sapply(slot(i.spr14, "polygons"), slot, "area"))
  ao.spr14 <- ai.spr14/a.win14  
  spr14[[i,1]] <- elk
  spr14[[i,2]] <- ao.spr14
} 

######################
#FALL 2014 MIGRATION
fall14 <- data.frame(matrix(ncol = 2, nrow = numelk.fall14)) #create df wo NAs
colnames(fall14) <- c("AnimalID", "fall14ao")

for(i in 1:numelk.fall14) {
  elk <- list.fall14[i,]
  
  #summer 2014
  temp.sum14 <- subset(locs, AnimalID == elk & MigHR == "Summer 2014")
  xy.sum14 <- data.frame("x" = temp.sum14$Long, "y" = temp.sum14$Lat)
  ll.sum14 <- SpatialPointsDataFrame(xy.sum14, temp.sum14, proj4string = latlong)
  sp.sum14 <- spTransform(ll.sum14, stateplane)
  kud.sum14 <- kernelUD(sp.sum14, h="href", grid = 5000)
  vol.sum14 <- getverticeshr(kud.sum14, percent = 95, ida = NULL, unin = "m", unout = "km")
  
  #winter 2015
  temp.win15 <- subset(locs, AnimalID == elk & MigHR == "Winter 2015")
  xy.win15 <- data.frame("x" = temp.win15$Long, "y" = temp.win15$Lat)
  ll.win15 <- SpatialPointsDataFrame(xy.win15, temp.win15, proj4string = latlong)
  sp.win15 <- spTransform(ll.win15, stateplane)
  kud.win15 <- kernelUD(sp.win15, h="href", grid = 5000)
  vol.win15 <- getverticeshr(kud.win15, percent = 95, ida = NULL, unin = "m", unout = "km")
  a.win15 <- sapply(slot(vol.win15, "polygons"), slot, "area")
  
  #overlap - calc and store
  i.fall14 <- gIntersection(vol.win15, vol.sum14, byid=FALSE)
  ifelse(is.null(i.fall14), ai.fall14 <- 0, ai.fall14 <- sapply(slot(i.fall14, "polygons"), slot, "area"))
  ao.fall14 <- ai.fall14/a.win15  
  fall14[[i,1]] <- elk
  fall14[[i,2]] <- ao.fall14
} 

hr.manual <- full_join(fall14, spr14, by = "AnimalID")

######################
#SPRING 2015 MIGRATION
spr15 <- data.frame(matrix(ncol = 2, nrow = numelk.spr15)) #create df wo NAs
colnames(spr15) <- c("AnimalID", "spr15ao")

for(i in 1:numelk.spr15) {
  elk <- list.spr15[i,]
  
  #winter 2015
  temp.win15 <- subset(locs, AnimalID == elk & MigHR == "Winter 2015")
  xy.win15 <- data.frame("x" = temp.win15$Long, "y" = temp.win15$Lat)
  ll.win15 <- SpatialPointsDataFrame(xy.win15, temp.win15, proj4string = latlong)
  sp.win15 <- spTransform(ll.win15, stateplane)
  kud.win15 <- kernelUD(sp.win15, h="href", grid = 5000)
  vol.win15 <- getverticeshr(kud.win15, percent = 95, ida = NULL, unin = "m", unout = "km")
  a.win15 <- sapply(slot(vol.win15, "polygons"), slot, "area")
  
  #summer 2015
  temp.sum15 <- subset(locs, AnimalID == elk & MigHR == "Summer 2015")
  xy.sum15 <- data.frame("x" = temp.sum15$Long, "y" = temp.sum15$Lat)
  ll.sum15 <- SpatialPointsDataFrame(xy.sum15, temp.sum15, proj4string = latlong)
  sp.sum15 <- spTransform(ll.sum15, stateplane)
  kud.sum15 <- kernelUD(sp.sum15, h="href", grid = 5000)
  vol.sum15 <- getverticeshr(kud.sum15, percent = 95, ida = NULL, unin = "m", unout = "km")
  
  #overlap - calc and store
  i.spr15 <- gIntersection(vol.win15, vol.sum15, byid=FALSE)
  ifelse(is.null(i.spr15), ai.spr15 <- 0, ai.spr15 <- sapply(slot(i.spr15, "polygons"), slot, "area"))
  ao.spr15 <- ai.spr15/a.win15  
  spr15[[i,1]] <- elk
  spr15[[i,2]] <- ao.spr15
} 

hr.manual <- full_join(hr.manual, spr15, by = "AnimalID")

######################
#FALL 2015 MIGRATION
fall15 <- data.frame(matrix(ncol = 2, nrow = numelk.fall15)) #create df wo NAs
colnames(fall15) <- c("AnimalID", "fall15ao")

for(i in 1:numelk.fall15) {
  elk <- list.fall15[i,]
  
  #summer 2015
  temp.sum15 <- subset(locs, AnimalID == elk & MigHR == "Summer 2015")
  xy.sum15 <- data.frame("x" = temp.sum15$Long, "y" = temp.sum15$Lat)
  ll.sum15 <- SpatialPointsDataFrame(xy.sum15, temp.sum15, proj4string = latlong)
  sp.sum15 <- spTransform(ll.sum15, stateplane)
  kud.sum15 <- kernelUD(sp.sum15, h="href", grid = 5000)
  vol.sum15 <- getverticeshr(kud.sum15, percent = 95, ida = NULL, unin = "m", unout = "km")
  
  #winter 2016
  temp.win16 <- subset(locs, AnimalID == elk & MigHR == "Winter 2016")
  xy.win16 <- data.frame("x" = temp.win16$Long, "y" = temp.win16$Lat)
  ll.win16 <- SpatialPointsDataFrame(xy.win16, temp.win16, proj4string = latlong)
  sp.win16 <- spTransform(ll.win16, stateplane)
  kud.win16 <- kernelUD(sp.win16, h="href", grid = 5000)
  vol.win16 <- getverticeshr(kud.win16, percent = 95, ida = NULL, unin = "m", unout = "km")
  a.win16 <- sapply(slot(vol.win16, "polygons"), slot, "area")
  
  #overlap - calc and store
  i.fall15 <- gIntersection(vol.win16, vol.sum15, byid=FALSE)
  ifelse(is.null(i.fall15), ai.fall15 <- 0, ai.fall15 <- sapply(slot(i.fall15, "polygons"), slot, "area"))
  ao.fall15 <- ai.fall15/a.win16
  fall15[[i,1]] <- elk
  fall15[[i,2]] <- ao.fall15
} 

######################
# COMBINE AND EXPORT DATA
hr.manual <- full_join(hr.manual, fall15, by = "AnimalID")
hr.manual <- hr.manual[,c("AnimalID", "spr14ao", "fall14ao", "spr15ao", "fall15ao")]
write.csv(hr.manual, file = "overlap-manual.csv", row.names = FALSE)