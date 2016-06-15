################################################
### MISC CODE RELATED TO SEASONAL HR OVERLAP ###
########## Most of it doesn't work #############
################################################


#elk loop fails after first indiv
##even though it didn't do that in the original code run
testelklist <- read.csv("testing123/testelklist.csv", as.is = T, header = TRUE)
#coordinate formatting is the same as in original code
testgpsdata <- read.csv("testing123/dailylocs_latlong.csv", as.is = T, header = TRUE)
rm(testdata)
#never mind, I just failed to fix the summer code to use animalid instead of deviceid

##############
#trying to tell the for loop to skip an individual if it doesn't have locations
##during the specified time period
#############

for(i in 1:numelk) {
elk=elklist[i,]
temp_dat_win<-subset(gpsdata, gpsdata$AnimalID == elk) #Subset individual animal
temp_dat_win <- subset(temp_dat_win, temp_dat_win$Date > as.Date("2014-01-01") & temp_dat_win$Date < as.Date("2014-04-01")) #Define winter data
if(exists("temp_dat_win")){
  xy <- data.frame("x"=temp_dat_win$Long,"y"=temp_dat_win$Lat)
  latlong = CRS("+init=epsg:4326")
  stateplane = CRS("+init=epsg:2818")
  xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_win, proj4string = latlong)
  xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
  writeOGR(xy.spdf.sp, ".", paste(elk, "_win_pts", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
} else {
  next
} 
}
#Error in .local(obj, ...) : 
#  cannot derive coordinates from non-numeric matrix 
#8 stop("cannot derive coordinates from non-numeric matrix") 
#7 .local(obj, ...) 
#6 coordinates(as.matrix(obj)) 
#5 coordinates(as.matrix(obj)) 
#4 .local(obj, ...) 
#3 coordinates(coords) 
#2 coordinates(coords) 
#1 SpatialPointsDataFrame(xy, temp_dat_win, proj4string = latlong) 
##i think temp_dat_win does still exist; it just doesn't have any observations

for(i in 1:numelk) {
  elk=elklist[i,]
  temp_dat_win<-subset(gpsdata, gpsdata$AnimalID == elk) #Subset individual animal
  temp_dat_win <- subset(temp_dat_win, temp_dat_win$Date > as.Date("2014-01-01") & temp_dat_win$Date < as.Date("2014-04-01")) #Define winter data
  if(exists("temp_dat_win[3,]")){
   xy <- data.frame("x"=temp_dat_win$Long,"y"=temp_dat_win$Lat)
    latlong = CRS("+init=epsg:4326")
    stateplane = CRS("+init=epsg:2818")
    xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_win, proj4string = latlong)
    xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
    writeOGR(xy.spdf.sp, ".", paste(elk, "_win_pts", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  } else {
    next
  } 
}
##no errors, just skips through all individuals - never makes indiv files

##totally fucking works!
for(i in 1:numelk) {
  elk=elklist[i,]
  temp_dat_win<-subset(gpsdata, gpsdata$AnimalID == elk) #Subset individual animal
  temp_dat_win <- subset(temp_dat_win, temp_dat_win$Date > as.Date("2014-01-01") & temp_dat_win$Date < as.Date("2014-04-01")) #Define winter data

  if(nrow(temp_dat_win)>1){ #skip indivs that don't have locations during this time
    xy <- data.frame("x"=temp_dat_win$Long,"y"=temp_dat_win$Lat)
    latlong = CRS("+init=epsg:4326")
    stateplane = CRS("+init=epsg:2818")
    xy.spdf.ll <- SpatialPointsDataFrame(xy, temp_dat_win, proj4string = latlong)
    xy.spdf.sp <- spTransform(xy.spdf.ll,stateplane)
    writeOGR(xy.spdf.sp, ".", paste(elk, "_win_pts", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
  } else {
    next
  }
}
##but it takes about an hour per individual...
#ain't nobody got time for that
##sadly, it'll be faster to manually create elklist for each season and year.


##remove results of loop
rm(temp_dat_win, xy, elk, i, latlong, stateplane, xy.spdf.ll, xy.spdf.sp, kud)

##########cannot allocate vector of size 107 MB
#memory.limit(size = 4000) #4000 Mb = 4 Gb, for 64-bit R
#seeing if Windows limits me to 4 Gb or 8 Tb... I think should be >4
memory.limit(size = 8000)
memory.limit(size = NA) #prints current memory limit, in gigs

##########
#readOGR failing to create shps
#############
##this has something to do with the max number of allowable 
##connections with ESRI
?writeOGR
install.packages("maptools")
library(maptools)
?writeSpatialShape
#fix = writeSpatialShape instead of writeOGR
#e.g. writeSpatialShape(xy.spdf.sp, paste(elk, "_sum14_pts", sep=""))


##############
#remove odd hour locations from 3300s
##to match iridium data
#############
#try writing a function to define even/odd

#1. make function correctly identify odd numbers
xtest <- c(0,1,2,3,4,5,6,7,8,9,10) #playdata
is.odd <- function(x) { x %% 2 == 1}
is.odd(xtest)

#2. make function correctly identify odd HOURS (in hour:minute data)
###current data format
xtest <- as.character(c("00:00", "01:01", "02:01", "03:02", "04:02", 
                        "05:03", "06:03"))
xtest2 <- as.POSIXlt(xtest, format = "%H:%M")
is.odd <- function(x) { x %% 2 == 1}
is.odd(xtest2$hour)

#2.5. actually, identifying even makes more sense, duh
xtest <- c(0,1,2,3,4,5,6,7,8,9,10) #playdata
#is.even <- function(x) { x %% 2 !== 1} #no double equals
is.even <- function(x) { x %% 2 != 1}
is.even(xtest2$hour)

#3. identify AND remove odd hour data
ytest <- subset(xtest2, is.even(xtest2$hour))
#damn i'm good

#4. figure out how to use real data
elklocs.eq <- elklocs.nona 
elklocs.eq$Time <- as.POSIXlt(elklocs.eq$Time, format = "%H:%M")

elklocs.eq2 <- subset(elklocs.eq, !is.odd(Time$hour) & (AnimalID == 140560 | 
                      AnimalID == 140910 | AnimalID == 141490))
#this only subsets for those indivs; removes all other collars

elklocs.eq2 <- ifelse((elklocs.eq$AnimalID == 140560 | 
                      elklocs.eq$AnimalID == 140910 | 
                      elklocs.eq$AnimalID == 141490),
   subset(elklocs.eq, !is.odd(Time$hour)), elklocs.eq)
#does nothing and kinda kills rstudio

#pull everything that's not those animals and odd?
elklocs.eq2 <- elklocs.nona 
elklocs.eq2 <- subset(elklocs.eq2, !(AnimalID == 140910 & is.odd(Time$hour)))
#works for one indiv. work for all 3?

elklocs.eq2 <- elklocs.eq 
elklocs.eq2 <- subset(elklocs.eq2, !(AnimalID == 140560 & is.odd(Time$hour)) &
                                   !(AnimalID == 140910 & is.odd(Time$hour)) &
                                   !(AnimalID == 141490 & is.odd(Time$hour)))
#yay me!

#checking results and cleaning up
d140560 <- subset(elklocs.nona, AnimalID == 140560)
  d140560a <- subset(elklocs.eq2, AnimalID == 140560)
d140910 <- subset(elklocs.nona, AnimalID == 140910)
  d140910a <- subset(elklocs.eq2, AnimalID == 140910)
d141490 <- subset(elklocs.nona, AnimalID == 141490)
  d141490a <- subset(elklocs.eq2, AnimalID == 141490)
rm(d140560, d140560a, d140910, d140910a, d140910, d140910a, elklocs.eq, elklocs.eq2)

#now hone your mad pipe-laying skillz
elklocs.eq2 <- elklocs.nona %>%
  mutate(Hour = as.POSIXlt(elklocs.eq$Time, format = "%H:%M")) %>%
  subset(!(AnimalID == 140560 & is.odd(Hour$hour)) & !(AnimalID == 140910 & is.odd(Hour$hour)) & !(AnimalID == 141490 & is.odd(Hour$hour))) %>%
  subset(elklocs.eq, select = -Hour)
#never mind, can't use as.POSIXlt with mutate

###########################################################################################
##CALCULATE AREA OF OVERLAP BETWEEN SEASONAL RANGES
##Loop attempt
###########################################################################################

######can i bind multiple datasets?
##tl;dr - nope. 2 at a time.
a <- data.frame(matrix(ncol = 3, nrow = 3))
a[1,] <- c(1, 1, 5)
a[2,] <- c(2, 2, 6)
a[3,] <- c(3, 3, 7)
View(a)
colnames(a) <- c("indiv", "w14a", "w14vol")
View(a)

b <- data.frame(matrix(ncol = 3, nrow = 3))
b[1,] <- c(2, 8, 12)
b[2,] <- c(3, 9, 14)
b[3,] <- c(4, 1, 14)
colnames(b) <- c("indiv", "s14a", "s14vol")
View(b)

c <- data.frame(matrix(ncol = 3, nrow = 3))
c[1,] <- c(1, 7, 22)
c[2,] <- c(2, 6, 23)
c[3,] <- c(5, 5, 24)
colnames(c) <- c("indiv", "w15a", "w15vol")
View(c)

j1 <- full_join(a, b, by = "indiv"); View(j1)
j2 <- full_join(a, b, c, by = "indiv"); View(j2)
j3 <- full_join(a, c, by = "indiv"); View(j3)
j4 <- full_join(j1, c, by = "indiv"); View(j4)

######create dataframe for area/vol calcs
a <- read.csv("w14.csv", header = TRUE)
w14 <- data.frame(AnimalID = a, w14a = NA, w14vol = NA)
area <- 7.6
volume <- 3.2
w14$w14a <- NA
w14$w14vol <- NA

for(i in 1:nrow(a)) {
  #animal = a[i,]
  w14$w14a[i] <- 7*i
  w14$w14vol[i] <- 2+i
}

##############
## volume calc
data(puechabonsp)
vi <- kerneloverlap(puechabonsp$relocs[,1],
                    grid=200, meth="VI", conditional=TRUE)
hr <- kerneloverlap(puechabonsp$relocs[,1],
                     grid=200, method="HR", conditional=TRUE)
#must store area and volume overlaps separately bc
##does matrix of allpairs
##later pull the overlaps you want and combine into df
plswork <- kerneloverlap(xy.spdf.sp, method = "VI", lev = 95)

kerneloverlap(test.xy.spdf.ll[,22], method = "HR", percent = 95)
kerneloverlap(test.xy.spdf.ll[,22], method = "VI", percent = 95, conditional = TRUE)
kerneloverlap(test.xy.spdf.ll[,22], method = "VI", percent = 95, conditional = FALSE)

###############
## storing results

###dataprep

#1. figure out which overlap is which season 
  #(want overlap of winter)
kerneloverlap
#out of the season listed on the ROW
#e.g. [1,2] is percent of summer HR
#i want [2,1] for winter overlap

#2. verify summer/winter always same order
#check

#3. index overlap/intersection results
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")

locs <- read.csv("collardata-locsonly-equalsampling.csv", as.is = TRUE, header = TRUE)
locs$Date <- as.Date(locs$Date, "%Y-%m-%d")
locs$MigHR <- ifelse(between(locs$Date, as.Date("2014-01-01"), as.Date("2014-03-31")), "Winter 2014", 
                     ifelse(between(locs$Date, as.Date("2014-06-01"), as.Date("2014-08-31")), "Summer 2014", 
                            ifelse(between(locs$Date, as.Date("2015-01-01"), as.Date("2015-03-31")), "Winter 2015", 
                                   ifelse(between(locs$Date, as.Date("2015-06-01"), as.Date("2015-08-31")), "Summer 2015",
                                          ifelse(between(locs$Date, as.Date("2016-01-01"), as.Date("2016-03-31")), "Winter 2016",
                                                 ifelse(NA))))))

test <- subset(locs, AnimalID == 140600 & MigHR == "Summer 2015" | 
                     AnimalID == 140600 & MigHR == "Winter 2015")
unique(test$MigHR)

testxy <- data.frame("x" = test$Long, "y" = test$Lat)

testxy.spdf.ll <- SpatialPointsDataFrame(testxy, test, proj4string = latlong)
testxy.spdf.sp <- spTransform(testxy.spdf.ll,stateplane)
testxy.spdf.sp@coords
testxy.spdf.sp[,22]
kerneloverlap(testxy.spdf.sp[,22], method = "HR", percent = 95, conditional=TRUE)
kerneloverlap(testxy.spdf.sp[,22], method = "VI", percent = 95, conditional=FALSE)

testao <- kerneloverlap(testxy.spdf.sp[,22], method = "HR", percent = 95, conditional=TRUE)
testvi <- kerneloverlap(testxy.spdf.sp[,22], method = "VI", percent = 95, conditional=TRUE)

testao[2,1]

##note when you subset the data to just use winter/summer locs, 
  #all the remaining factors are still stored, so you get an error about
  #not having enough relocs bc those are null
  #that's why you removed the as.factor part
#> unique(test$MigHR)
#[1] Winter 2014 Summer 2014
#Levels: Summer 2014 Summer 2015 Winter 2014 Winter 2015 Winter 2016

rm(test, testxy, testao, testvo, testao, testvi)


## STORE RESULTS
testlist <- read.csv("s15.csv", header = TRUE)

test14 <- data.frame(matrix(ncol = 3, nrow = 1)) #create df wo NAs
colnames(test14) <- c("AnimalID", "spr14ao", "spr14vi")
test14[[1,1]] <- 151879
test14[[1,2]] <- testao[2,1]
test14[[1,3]] <- testvi[2,1]
View(test14)

#########################
## investigating 1.00 area overlap with 0.00 volume intersection
# cuz that's just wrong
#########################
library(dplyr)
locs <- read.csv("collardata-locsonly-equalsampling.csv", as.is = TRUE, header = TRUE)
locs$Date <- as.Date(locs$Date, "%Y-%m-%d")
locs <- subset(locs, select = -c(X, FixStatus, DOP, TempC, EndTime,
                                 EndLat, EndLong, Hour))
locs$MigHR <- ifelse(between(locs$Date, as.Date("2014-01-01"), as.Date("2014-03-31")), "Winter 2014", 
                     ifelse(between(locs$Date, as.Date("2014-06-01"), as.Date("2014-08-31")), "Summer 2014", 
                            ifelse(between(locs$Date, as.Date("2015-01-01"), as.Date("2015-03-31")), "Winter 2015", 
                                   ifelse(between(locs$Date, as.Date("2015-06-01"), as.Date("2015-08-31")), "Summer 2015",
                                          ifelse(between(locs$Date, as.Date("2016-01-01"), as.Date("2016-03-31")), "Winter 2016",
                                                 ifelse(NA))))))
write.csv(locs, file = "locsMigHR.csv")
wtf <- subset(locs, AnimalID == 141500)
View(wtf)

hro <- read.csv("homerangeoverlap.csv")
look <- read.csv("migstatus-prelimlook.csv")
lookstatus <- look[, c("AnimalID", "Status")]
hro.look <- full_join(hro, lookstatus, by = "AnimalID")
hro.look[55,]

  library(rgdal)
  library(adehabitatHR)
  latlong <- CRS("+init=epsg:4326")
  stateplane <- CRS("+init=epsg:2818")
wtf.xy <- data.frame("x"=wtf$Long,"y"=wtf$Lat)
wtf.spdf.ll <- SpatialPointsDataFrame(wtf.xy, wtf, proj4string = latlong)
wtf.spdf.sp <- spTransform(wtf.spdf.ll,stateplane)
kerneloverlap(wtf.spdf.sp[,14], method = "HR", percent = 95)

a <- kerneloverlap(wtf.spdf.sp[,14], method = "HR", percent = 95)
a[2,1]
#the above is indexed correctly, but somehow that's not the 
#number that ends up in the final data. huh.

#things to try

#1. check other indivs to see if those numbers are the correct ones

mig <- subset(locs, AnimalID == 140300)
mig <- subset(mig, MigHR == "Winter 2014" | MigHR == "Summer 2014")
mig.xy <- data.frame("x"=mig$Long,"y"=mig$Lat)
mig.spdf.ll <- SpatialPointsDataFrame(mig.xy, mig, proj4string = latlong)
mig.spdf.sp <- spTransform(mig.spdf.ll,stateplane)
  plot(mig.spdf.sp, pch = 1)

kerneloverlap(mig.spdf.sp[,14], method = "HR", percent = 95)
amig <- kerneloverlap(mig.spdf.sp[,14], method = "HR", percent = 95)
amig[2,1]

res <- subset(locs, AnimalID == 140380)
res <- subset(res, MigHR == "Winter 2014" | MigHR == "Summer 2014")
res.xy <- data.frame("x"=res$Long,"y"=res$Lat)
res.spdf.ll <- SpatialPointsDataFrame(res.xy, res, proj4string = latlong)
res.spdf.sp <- spTransform(res.spdf.ll,stateplane)
kerneloverlap(res.spdf.sp[,14], method = "HR", percent = 95)
ares <- kerneloverlap(res.spdf.sp[,14], method = "HR", percent = 95)
ares[2,1]
#they are - something's just weird with the 141500 data
###OK - look at those shps/locs when you get to the office
rm(list=ls())

#2. check whether other indivs have 1s in overlap numbers

#140070: spr15, fall15 (not spr14, fall14)
#ACCURATE
  #win15 2 polygons; sum15 almost identical but larger;
    #win16 entirely contained in one poly of both prev
  #looks like win15 is including sum15 points, yeesh
  #the 1 in fall2015 does make sense 
    #bc entire winter range is contained within summer
  w0070 <- subset(locs, AnimalID == 140070)
  w0070 <- subset(w0070, MigHR == "Winter 2015" | MigHR == "Summer 2015")
  write.csv(w0070, file = "miglocs140070-spr15.csv")
  #ok the wrong HR is due to something weird in the shapefile code,
    #not the overlap code. Ignoring for now.
  w0070 <- subset(w0070, MigHR == "Winter 2015")
  wxy <- data.frame("x"=w0070$Long,"y"=w0070$Lat)
  wxy.spdf <- SpatialPointsDataFrame(wxy, w0070, proj4string = latlong)
  wkud <- kernelUD(wxy.spdf, h="href", grid = 5000)
  image(wkud)
  #oh wait, i can't do the above because my computer blows
  #spr15: makes sense
  w0070 <- subset(locs, AnimalID == 140070)
  w00702 <- subset(w0070, MigHR == "Winter 2016")
  write.csv(w00702, file = "wtf140070_w16.csv")
  #yeah fall2015 makes sense too

#140800: spr15 (all others 0 until fall15)
  wtf0800 <- subset(locs, AnimalID == 140800)
  w0800 <- subset(wtf0800, MigHR == "Winter 2015")
    write.csv(w0800, file = "wtfw0800_w15.csv")
  s0800 <- subset(wtf0800, MigHR == "Summer 2015")
    write.csv(s0800, file = "wtfs0800_s15.csv")
  
  #totally distinct polygons - why is this a 1??
    #try plotting here in r rather than in arcmap
  library(sp) 
  library(rgdal)
  latlong <- CRS("+init=epsg:4326")
  stateplane <- CRS("+init=epsg:2818")
par(mfrow=c(1,1))
  wtf0800 <- subset(locs, AnimalID == 140800)
  hm0800 <- subset(wtf0800, MigHR == "Winter 2015" | MigHR == "Summer 2015")
  hm.xy <- data.frame("x"=hm0800$Long,"y"=hm0800$Lat)
  hm.xy.ll <- SpatialPointsDataFrame(hm.xy, hm0800, proj4string = latlong)
  hm.col <- factor(hm.xy.ll@data$MigHR)
  plot(hm.xy.ll, col = hm.col, pch = 1)
  #the good news: plotting spdf's is easy
  #the bad news: i still can't understand why the f'ing overlap is 1...
  
  library(adehabitatHR) 
  hm.xy.sp <- spTransform(hm.xy.ll, stateplane)
  hm.ao <- kerneloverlap(hm.xy.sp[,22], method = "HR", percent = 95)
  hm.ao
  #wtffffffff
  
#141500
  wtf1500 <- subset(locs, AnimalID == 141500)
  wtf1500 <- subset(wtf1500, MigHR == "Winter 2014" | MigHR == "Summer 2014")
  wtf.xy <- data.frame("x"=wtf1500$Long,"y"=wtf1500$Lat)
  wtf.xy.ll <- SpatialPointsDataFrame(wtf.xy, wtf1500, proj4string = latlong)
  wtf.col <- factor(wtf.xy.ll@data$MigHR)
  plot(wtf.xy.ll, col = wtf.col, pch = 1)

  wtf.xy.sp <- spTransform(wtf.xy.ll,stateplane)
  wtf.ao <- kerneloverlap(wtf.xy.sp[,22], method = "HR", percent = 95)
  wtf.vi <- kerneloverlap(wtf.xy.sp[,22], method = "VI", percent = 95)
  wtf.ao
  
#3. rerun analysis but indexed differently

#SPRING 2015 MIGRATION
list.spr15 <- read.csv("spr15.csv", header = TRUE)
numelk.spr15 <- nrow(list.spr15)

spr15.2 <- data.frame(matrix(ncol = 3, nrow = numelk.spr15)) #create df wo NAs
colnames(spr15.2) <- c("AnimalID", "spr15ao", "spr15vi")

# *run libraries, locs, CRS before below
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
  ao.2 <- kerneloverlap(xy.spdf.sp[,22], method = "HR", percent = 95)
  vol.2 <- kerneloverlap(xy.spdf.sp[,22], method = "VI", percent = 95, conditional = FALSE)
  
  #store results
  spr15.2[[i,1]] <- elk
  spr15.2[[i,2]] <- ao.2[1,2]
  spr15.2[[i,3]] <- vol.2[1,2]
}    
View(spr15.2)
#just one of the same issue... 141150

wtf1150 <- subset(locs, AnimalID == 141150)
wtf1150 <- subset(wtf1150, MigHR == "Winter 2015" | MigHR == "Summer 2015")
wtf.xy <- data.frame("x"=wtf1150$Long,"y"=wtf1150$Lat)
wtf.xy.ll <- SpatialPointsDataFrame(wtf.xy, wtf1150, proj4string = latlong)
wtf.col <- factor(wtf.xy.ll@data$MigHR, labels=c("blue", "green"))
plot(wtf.xy.ll, col = wtf.col)
#fucking kill me. I don't get it.

#4. rerun all code twice, indexed differently each time

## investigate similarities bt animals that have this issue
  #I'll just do this in notepad++ and paste it in
  #to save the 1000-line code and to make replacing easier

#5. check some other random stuff

#5a) do these indivs have weird mig'n timing/dates?
#####no.
#5b) i don't knowwwwww

#6. "manually" calculate area overlap for the weirdos

#based primarily on Kelly's code
  library(rgeos)
  library(rgdal)
  library(adehabitatHR)
  latlong <- CRS("+init=epsg:4326")
  stateplane <- CRS("+init=epsg:2818")
##141500 spr14
  wtf141500 <- subset(locs, AnimalID == 141500)
#winter 2014
  win141500 <- subset(wtf141500, MigHR == "Winter 2014")
  xy.win141500 <- data.frame("x"=win141500$Long,"y"=win141500$Lat)
  ll.win141500 <- SpatialPointsDataFrame(xy.win141500, win141500, proj4string = latlong)
  sp.win141500 <- spTransform(ll.win141500, stateplane)
  kud.win141500 <- kernelUD(sp.win141500, h="href", grid = 5000)
  vol.win141500 <- getverticeshr(kud.win141500, percent = 95, ida = NULL, unin = "m", unout = "km")
  rm(kud.win141500, ll.win141500, xy.win141500) #attempt to save memory
  writeOGR(vol.win141500, ".", paste("141500_win14_vol95", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
#summer 2014
  sum141500 <- subset(wtf141500, MigHR == "Summer 2014")
  xy.sum141500 <- data.frame("x"=sum141500$Long,"y"=sum141500$Lat)
  ll.sum141500 <- SpatialPointsDataFrame(xy.sum141500, sum141500, proj4string = latlong)
  sp.sum141500 <- spTransform(ll.sum141500, stateplane)
  kud.sum141500 <- kernelUD(sp.sum141500, h="href", grid = 5000)
  vol.sum141500 <- getverticeshr(kud.sum141500, percent = 95, ida = NULL, unin = "m", unout = "km")
  rm(kud.sum141500, ll.sum141500, xy.sum141500) #attempt to save memory
  writeOGR(vol.sum141500, ".", paste("141500_sum14_vol95", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
#overlap
  w141500 <- readOGR(dsn = "C:/Users/kristin.barker/Documents/GitHub/HRoverlap", layer = "141500_win14_vol95")
  	w141500_area <- area(w141500)
  sum141500 <- readOGR(dsn = "C:/Users/kristin.barker/Documents/GitHub/HRoverlap", layer = "141500_sum14_vol95")
  	sum141500_area <- area(sum141500)
  res141500 <- gIntersection(w141500, s141500, byid=FALSE, drop_lower_td=FALSE)
  ao141500 <- area(res141500)
  po141500 <- ao141500/win141500_area
  
##140800 spr15
  wtf140500 <- subset(locs, AnimalID == 140500)
#winter 2014
  win140500 <- subset(wtf140500, MigHR == "Winter 2015")
  xy.win140500 <- data.frame("x"=win140500$Long,"y"=win140500$Lat)
  ll.win140500 <- SpatialPointsDataFrame(xy.win140500, win140500, proj4string = latlong)
  sp.win140500 <- spTransform(ll.win140500, stateplane)
  kud.win140500 <- kernelUD(sp.win140500, h="href", grid = 5000)
  vol.win140500 <- getverticeshr(kud.win140500, percent = 95, ida = NULL, unin = "m", unout = "km")
  rm(kud.win140800, ll.win140800, xy.win140800) #attempt to save memory
  writeOGR(vol.win140500, ".", paste("140500_win14_vol95", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
#summer 2014
  sum140500 <- subset(wtf140500, MigHR == "Summer 2015")
  xy.sum140500 <- data.frame("x"=sum140500$Long,"y"=sum140500$Lat)
  ll.sum140500 <- SpatialPointsDataFrame(xy.sum140500, sum140500, proj4string = latlong)
  sp.sum140500 <- spTransform(ll.sum140500, stateplane)
  kud.sum140500 <- kernelUD(sp.sum140500, h="href", grid = 5000)
  vol.sum140500 <- getverticeshr(kud.sum140500, percent = 95, ida = NULL, unin = "m", unout = "km")
  rm(kud.sum140800, ll.sum140800, xy.sum140800) #attempt to save memory
  writeOGR(vol.sum140500, ".", paste("140500_sum14_vol95", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
#overlap
  w140500 <- readOGR(dsn = "C:/Users/kristin.barker/Documents/GitHub/HRoverlap", layer = "140500_win14_vol95")
  	w140500_area <- area(w140500)
  sum140500 <- readOGR(dsn = "C:/Users/kristin.barker/Documents/GitHub/HRoverlap", layer = "140500_sum14_vol95")
  	sum140500_area <- area(sum140500)
  res140500 <- gIntersection(w140500, s140500, byid=FALSE, drop_lower_td=FALSE)
  ao140500 <- area(res140500)
  po140800 <- ao140500/win140500_area


############################
## MISC HELPFUL STUFF
############################

#check whether there are any NAs in df or column
any(is.na(testxy$x))

#look at one row based on a cell value (here, indiv 140800)
hro.look[hro.look$AnimalID %in% 140800,]