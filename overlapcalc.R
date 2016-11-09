######################################################
## Migration - Volume Overlap Calculations ##
########  NSERP - Kristin Barker - June 2016  ########
######################################################

##SET WD
####Work computer, personal laptop, or external hard drive
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Migration\\HRoverlap"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Migration\\HRoverlap"
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
#library(raster) #prob don't need this one here
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

# summer = july and august (to coincide with NSD dates)
# winter only through mid-march (bc i think one indiv migrated in late march one year)
locs$MigHR <- ifelse(between(locs$Date, as.Date("2014-01-01"), as.Date("2014-03-15")), "Winter 2014", 
                     ifelse(between(locs$Date, as.Date("2014-07-01"), as.Date("2014-08-31")), "Summer 2014", 
                            ifelse(between(locs$Date, as.Date("2015-01-01"), as.Date("2015-03-15")), "Winter 2015", 
                                   ifelse(between(locs$Date, as.Date("2015-07-01"), as.Date("2015-08-31")), "Summer 2015",
                                          ifelse(between(locs$Date, as.Date("2016-01-01"), as.Date("2016-03-15")), "Winter 2016",
                                                 ifelse(NA))))))
locs$IndivYr <- ifelse(locs$Date < "2015-01-01", 
                       paste(locs$AnimalID, "-14", sep=""),
                       paste(locs$AnimalID, "-15", sep=""))
# write.csv(locs, file = "locsMigHR3.csv", row.names = FALSE)

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
#CALCULATE VOLUME OVERLAP
###########################################################################################

######################
#SPRING 2014 MIGRATION

spr14 <- data.frame(matrix(ncol = 2, nrow = numelk.spr14)) #create df wo NAs
colnames(spr14) <- c("AnimalID", "SprVI")

for(i in 1:numelk.spr14) {
  elk <- list.spr14[i,]
  
  #subset individual and seasonal locations
  temp_dat_spr14 <- subset(locs, AnimalID == elk) 
  temp_dat_spr14 <- subset(temp_dat_spr14, MigHR == "Winter 2014" | MigHR == "Summer 2014")
  
  #Get xy points, write to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_spr14$Long,"y"=temp_dat_spr14$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_spr14, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  spr14[[i,1]] <- elk
  spr14[[i,2]] <- vol[2,1]
}    
spr14$IndivYr <- paste(spr14$AnimalID, "-14", sep="")

######################
#FALL 2014 MIGRATION

fall14 <- data.frame(matrix(ncol = 2, nrow = numelk.fall14)) #create df wo NAs
colnames(fall14) <- c("AnimalID", "FallVI")

for(i in 1:numelk.fall14) {
  elk <- list.fall14[i,]
  
  #subset individual and seasonal locations
  temp_dat_fall14 <- subset(locs, AnimalID == elk) 
  temp_dat_fall14 <- subset(temp_dat_fall14, MigHR == "Summer 2014" | MigHR == "Winter 2015")
  
  #Get xy points, write to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_fall14$Long,"y"=temp_dat_fall14$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_fall14, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  fall14[[i,1]] <- elk
  fall14[[i,2]] <- vol[2,1]
}    
fall14$IndivYr <- paste(fall14$AnimalID, "-14", sep="")
hr14 <- full_join(spr14, fall14, by = c("IndivYr", "AnimalID")) %>%
  select(IndivYr, AnimalID, SprVI, FallVI) # order columns
  

######################
#SPRING 2015 MIGRATION

spr15 <- data.frame(matrix(ncol = 2, nrow = numelk.spr15)) #create df wo NAs
colnames(spr15) <- c("AnimalID", "SprVI")

for(i in 1:numelk.spr15) {
  elk <- list.spr15[i,]
  
  #subset individual and seasonal locations
  temp_dat_spr15 <- subset(locs, AnimalID == elk) 
  temp_dat_spr15 <- subset(temp_dat_spr15, MigHR == "Winter 2015" | MigHR == "Summer 2015")
  
  #Get xy points, write points to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_spr15$Long,"y"=temp_dat_spr15$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_spr15, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  spr15[[i,1]] <- elk
  spr15[[i,2]] <- vol[2,1]
}    
spr15$IndivYr <- paste(spr15$AnimalID, "-15", sep="")

######################
#FALL 2015 MIGRATION

fall15 <- data.frame(matrix(ncol = 2, nrow = numelk.fall15)) #create df wo NAs
colnames(fall15) <- c("AnimalID", "FallVI")

for(i in 1:numelk.fall15) {
  elk <- list.fall15[i,]
  
  #subset individual and seasonal locations
  temp_dat_fall15 <- subset(locs, AnimalID == elk) 
  temp_dat_fall15 <- subset(temp_dat_fall15, MigHR == "Summer 2015" | MigHR == "Winter 2016")
  
  #Get xy points, write to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_fall15$Long,"y"=temp_dat_fall15$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_fall15, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate area overlap and volume intersection
  vol <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  fall15[[i,1]] <- elk
  fall15[[i,2]] <- vol[2,1]
} 
fall15$IndivYr <- paste(fall15$AnimalID, "-15", sep="")

hr15 <- full_join(spr15, fall15, by = c("IndivYr", "AnimalID")) %>%
  select(IndivYr, AnimalID, SprVI, FallVI) # order columns

# REMOVE MALES AND EXPORT
sex <- locs %>%
  dplyr::select(IndivYr, Sex) %>%
  distinct()
hr <- bind_rows(hr14, hr15) %>%
  left_join(sex) %>%
  filter(Sex == "Female") %>% 
  dplyr::select(IndivYr, AnimalID, SprVI, FallVI)
write.csv(hr, file = "volumeintersection.csv", row.names = FALSE)


#################################################
#### UPDATES FOR NUTRITION/SURVIVAL ANALYSIS ####
# making time pd correspond to nutrition time pd #
# only looking at summer migration #
#################################################

#GPS DATA FROM COLLARS, PLUS MIGRATION SEASON/YEAR
locs <- read.csv("../../ElkDatabase/collardata-locsonly-equalsampling.csv", as.is = TRUE, header = TRUE)
locs$Date <- as.Date(locs$Date, "%Y-%m-%d")

# summer = july 15 - aug 31 (match biomass sampling/gdm estimation time pd)
# winter = starts feb 26 2014 bc day after last capture
  # other yrs set to be same length of time as summer timeframe
locs$MigHR <- ifelse(between(locs$Date, as.Date("2014-02-26"), as.Date("2014-03-31")), "Winter 2014", 
                     ifelse(between(locs$Date, as.Date("2014-07-15"), as.Date("2014-08-31")), "Summer 2014", 
                            ifelse(between(locs$Date, as.Date("2015-02-15"), as.Date("2015-03-31")), "Winter 2015", 
                                   ifelse(between(locs$Date, as.Date("2015-07-15"), as.Date("2015-08-31")), "Summer 2015",
                                          ifelse(between(locs$Date, as.Date("2016-02-15"), as.Date("2016-03-31")), "Winter 2016",
                                                 ifelse(NA))))))

# projections
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")

# lists of animals per searon
list.spr14 <- read.csv("spr14.csv", header = TRUE)
numelk.spr14 <- nrow(list.spr14)
list.spr15 <- read.csv("spr15.csv", header = TRUE)
numelk.spr15 <- nrow(list.spr15)

#############
#### 95% ####

######################
#SPRING 2014 MIGRATION

spr14 <- data.frame(matrix(ncol = 2, nrow = numelk.spr14)) #create df wo NAs
colnames(spr14) <- c("AnimalID", "SprVI")

for(i in 1:numelk.spr14) {
  elk <- list.spr14[i,]
  
  #subset individual and seasonal locations
  temp_dat_spr14 <- subset(locs, AnimalID == elk) 
  temp_dat_spr14 <- subset(temp_dat_spr14, MigHR == "Winter 2014" | MigHR == "Summer 2014")
  
  #Get xy points, write to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_spr14$Long,"y"=temp_dat_spr14$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_spr14, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)

  #calculate kde and 95% volume intersection ([,2] is AnimalID; [,20] is MigHR)
  kud <- kernelUD(xy.spdf.sp[,20], h = "href", same4all=TRUE) #LSCV not alwys converged
  vol <- kerneloverlaphr(kud, method = "VI", percent = 95, conditional = TRUE)
  
  #store results
  spr14[[i,1]] <- elk
  spr14[[i,2]] <- vol[2,1]
}    
spr14$IndivYr <- paste(spr14$AnimalID, "-14", sep="")

######################
#SPRING 2015 MIGRATION

spr15 <- data.frame(matrix(ncol = 2, nrow = numelk.spr15)) #create df wo NAs
colnames(spr15) <- c("AnimalID", "SprVI")

for(i in 1:numelk.spr15) {
  elk <- list.spr15[i,]
  
  #subset individual and seasonal locations
  temp_dat_spr15 <- subset(locs, AnimalID == elk) 
  temp_dat_spr15 <- subset(temp_dat_spr15, MigHR == "Winter 2015" | MigHR == "Summer 2015")
  
  #Get xy points, write points to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_spr15$Long,"y"=temp_dat_spr15$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_spr15, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate kde and 95% volume intersection ([,2] is AnimalID; [,20] is MigHR)
  kud <- kernelUD(xy.spdf.sp[,20], h = "href", same4all=TRUE) #LSCV not alwys converged
  vol <- kerneloverlaphr(kud, method = "VI", percent = 95, conditional = TRUE)
  
  #store results
  spr15[[i,1]] <- elk
  spr15[[i,2]] <- vol[2,1]
}    
spr15$IndivYr <- paste(spr15$AnimalID, "-15", sep="")

sex <- distinct(dplyr::select(locs, AnimalID, Sex))
vi <- bind_rows(spr14, spr15) %>%
  left_join(sex, by = "AnimalID") %>%
  filter(Sex == "Female")

write.csv(vi, file = "volumeintersection.csv", row.names=F)


#############
#### 50% ####

######################
#SPRING 2014 MIGRATION

spr14 <- data.frame(matrix(ncol = 2, nrow = numelk.spr14)) #create df wo NAs
colnames(spr14) <- c("AnimalID", "SprVI")

for(i in 1:numelk.spr14) {
  elk <- list.spr14[i,]
  
  #subset individual and seasonal locations
  temp_dat_spr14 <- subset(locs, AnimalID == elk) 
  temp_dat_spr14 <- subset(temp_dat_spr14, MigHR == "Winter 2014" | MigHR == "Summer 2014")
  
  #Get xy points, write to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_spr14$Long,"y"=temp_dat_spr14$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_spr14, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate kde and 50% volume intersection ([,2] is AnimalID; [,20] is MigHR)
  kud <- kernelUD(xy.spdf.sp[,20], h = "href", same4all=TRUE) #LSCV not alwys converged
  vol <- kerneloverlaphr(kud, method = "VI", percent = 50, conditional = TRUE)
  
  #store results
  spr14[[i,1]] <- elk
  spr14[[i,2]] <- vol[2,1]
}    
spr14$IndivYr <- paste(spr14$AnimalID, "-14", sep="")

######################
#SPRING 2015 MIGRATION

spr15 <- data.frame(matrix(ncol = 2, nrow = numelk.spr15)) #create df wo NAs
colnames(spr15) <- c("AnimalID", "SprVI")

for(i in 1:numelk.spr15) {
  elk <- list.spr15[i,]
  
  #subset individual and seasonal locations
  temp_dat_spr15 <- subset(locs, AnimalID == elk) 
  temp_dat_spr15 <- subset(temp_dat_spr15, MigHR == "Winter 2015" | MigHR == "Summer 2015")
  
  #Get xy points, write points to dataframe, to spatial data frame, to stateplane projection
  xy <- data.frame("x"=temp_dat_spr15$Long,"y"=temp_dat_spr15$Lat)
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_spr15, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  
  #calculate kde and 50% volume intersection ([,2] is AnimalID; [,20] is MigHR)
  kud <- kernelUD(xy.spdf.sp[,20], h = "href", same4all=TRUE) #LSCV not alwys converged
  vol <- kerneloverlaphr(kud, method = "VI", percent = 50, conditional = TRUE)
  
  #store results
  spr15[[i,1]] <- elk
  spr15[[i,2]] <- vol[2,1]
}    
spr15$IndivYr <- paste(spr15$AnimalID, "-15", sep="")

sex <- distinct(dplyr::select(locs, AnimalID, Sex))
vi50 <- bind_rows(spr14, spr15) %>%
  left_join(sex, by = "AnimalID") %>%
  filter(Sex == "Female")

write.csv(vi50, file = "volumeintersection50.csv", row.names=F)