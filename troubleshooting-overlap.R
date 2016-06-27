################################################
### MISC CODE RELATED TO SEASONAL HR OVERLAP ###
########## Most of it doesn't work #############
################################################
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
locs <- read.csv("locsMigHR.csv", as.is = TRUE, header = TRUE)

#meh
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

#setup
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\HRoverlap"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\HRoverlap"
wd_external <- "E:\\Kristins\\HRoverlap"
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
library(rgeos)
library(rgdal)
library(adehabitatHR)
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")
locs <- read.csv("locsMigHR.csv", as.is = TRUE, header = TRUE)
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
  w141500 <- readOGR(dsn = "E:/Kristins/HRoverlap", layer = "141500_win14_vol95")
  s141500 <- readOGR(dsn = "E:/Kristins/HRoverlap", layer = "141500_sum14_vol95")
  res141500 <- gIntersection(w141500, s141500, byid=FALSE)
  ifelse(is.null(res141500), ovrlp141500 <- 0, ovrlp141500 <- res141500)
  po141500 <- ovrlp141500/w141500@data$area
  
##140800 spr15
  wtf140800 <- subset(locs, AnimalID == 140800)
#winter 2014
  win140800 <- subset(wtf140800, MigHR == "Winter 2015")
  xy.win140800 <- data.frame("x"=win140800$Long,"y"=win140800$Lat)
  ll.win140800 <- SpatialPointsDataFrame(xy.win140800, win140800, proj4string = latlong)
  sp.win140800 <- spTransform(ll.win140800, stateplane)
  kud.win140800 <- kernelUD(sp.win140800, h="href", grid = 5000)
  vol.win140800 <- getverticeshr(kud.win140800, percent = 95, ida = NULL, unin = "m", unout = "km")
  rm(kud.win140800, ll.win140800, xy.win140800) #attempt to save memory
  writeOGR(vol.win140800, ".", paste("140800_win14_vol95", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
#summer 2014
  sum140800 <- subset(wtf140800, MigHR == "Summer 2015")
  xy.sum140800 <- data.frame("x"=sum140800$Long,"y"=sum140800$Lat)
  ll.sum140800 <- SpatialPointsDataFrame(xy.sum140800, sum140800, proj4string = latlong)
  sp.sum140800 <- spTransform(ll.sum140800, stateplane)
  kud.sum140800 <- kernelUD(sp.sum140800, h="href", grid = 5000)
  vol.sum140800 <- getverticeshr(kud.sum140800, percent = 95, ida = NULL, unin = "m", unout = "km")
  rm(kud.sum140800, ll.sum140800, xy.sum140800) #attempt to save memory
  writeOGR(vol.sum140800, ".", paste("140800_sum14_vol95", sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
#overlap
  w140800 <- readOGR(dsn = "E:/Kristins/HRoverlap", layer = "140800_win14_vol95")
  s140800 <- readOGR(dsn = "E:/Kristins/HRoverlap", layer = "140800_sum14_vol95")
  res140800 <- gIntersection(w140800, s140800, byid=FALSE)
  ifelse(is.null(res140800), ovrlp140800 <- 0, ovrlp140800 <- res140800)
  po140800 <- ovrlp140800/w140800@data$area
#display
po141500; po140800

#double-check with one that actually has overlap
##140060 spr15
  wtf140060 <- subset(locs, AnimalID == 140060)
#winter 2014
  win140060 <- subset(wtf140060, MigHR == "Winter 2014")
  xy.win140060 <- data.frame("x"=win140060$Long,"y"=win140060$Lat)
  ll.win140060 <- SpatialPointsDataFrame(xy.win140060, win140060, proj4string = latlong)
  sp.win140060 <- spTransform(ll.win140060, stateplane)
  kud.win140060 <- kernelUD(sp.win140060, h="href", grid = 5000)
  vol.win140060 <- getverticeshr(kud.win140060, percent = 95, ida = NULL, unin = "m", unout = "km")
  rm(kud.win140060, ll.win140060, xy.win140060) 
#summer 2014
  sum140060 <- subset(wtf140060, MigHR == "Summer 2014")
  xy.sum140060 <- data.frame("x"=sum140060$Long,"y"=sum140060$Lat)
  ll.sum140060 <- SpatialPointsDataFrame(xy.sum140060, sum140060, proj4string = latlong)
  sp.sum140060 <- spTransform(ll.sum140060, stateplane)
  kud.sum140060 <- kernelUD(sp.sum140060, h="href", grid = 5000)
  vol.sum140060 <- getverticeshr(kud.sum140060, percent = 95, ida = NULL)
  rm(kud.sum140060, ll.sum140060, xy.sum140060)
#overlap
  res140060 <- gIntersection(vol.win140060, vol.sum140060, byid=FALSE)
  ai140060 <- sapply(slot(res140060, "polygons"), slot, "area")
  wi140060 <- sapply(slot(w140060, "polygons"), slot, "area")
  ifelse(is.null(res140060), po140060 <- 0, po140060 <- ai140060/wi140060)
#jesus christ that took a lifetime
#but i totally did it
#aaaand it doesn't match the kerneloverlap number, fuck

#check out ploygons
plot(vol.win140060); plot(vol.sum140060, add=TRUE)
plot(res140060, col = "blue", add = TRUE)


####################################
## code to calculate area overlap
## since kerneloverlap may be wrong
#####################################

#1. name dfs, files, etc based on animalid?
list.test <- data.frame(c(141500, 140800, 140060))
  colnames(list.test) <- "AnimalID"
	  num.test <- nrow(list.test)
	
for(i in 1:num.test) {
  elk <- list.test[i,]
assign(paste0("sub", elk), subset(list.test, AnimalID == elk))
}    
#nice. use this to subset real data?

#########HERE GOES##########
#packages
	library(rgeos)
  library(rgdal)
  library(adehabitatHR)
#lists
  list.spr14 <- read.csv("spr14.csv", header = TRUE)
    numelk.spr14 <- nrow(list.spr14)
  list.fall14 <- read.csv("fall14.csv", header = TRUE)
    numelk.fall14 <- nrow(list.fall14)
  list.spr15 <- read.csv("spr15.csv", header = TRUE)
    numelk.spr15 <- nrow(list.spr15)
  list.fall15 <- read.csv("fall15.csv", header = TRUE)
    numelk.fall15 <- nrow(list.fall15)
#data	  
  locs <- read.csv("locsMigHR.csv", as.is = TRUE, header = TRUE, skipNul=TRUE)
  # note to self: you get "EOF within quoted string" here
#projections 
	latlong <- CRS("+init=epsg:4326")
	stateplane <- CRS("+init=epsg:2818")

######################
#SPRING 2014 MIGRATION
spr14 <- data.frame(matrix(ncol = 2, nrow = numelk.spr14)) #create df wo NAs
colnames(spr14) <- c("AnimalID", "spr14ao")

for(i in 1:numelk.spr14) {
  elk <- list.spr14[i,]

  #season 1 (here, winter 2014)
  temp.win14 <- subset(locs, AnimalID == elk & MigHR == "Winter 2014")
  xy.win14 <- data.frame("x" = temp.win14$Long, "y" = temp.win14$Lat)
  ll.win14 <- SpatialPointsDataFrame(xy.win14, temp.win14, proj4string = latlong)
  sp.win14 <- spTransform(ll.win14, stateplane)
  kud.win14 <- kernelUD(sp.win14, h="href", grid = 5000)
  vol.win14 <- getverticeshr(kud.win14, percent = 95, ida = NULL, unin = "m", unout = "km")
  a.win14 <- sapply(slot(vol.win14, "polygons"), slot, "area")

  #season 2
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


## OOPSIES ##

#this pulls both seasons rather than just one at a time
for(i in 1:numelk.spr14) {
  elk <- list.spr14[i,]
  assign(paste0("sub", elk), subset(locs, AnimalID == elk & MigHR == "Winter 2014" | 
										  AnimalID == elk & MigHR == "Summer 2014"))
} 

	latlong <- CRS("+init=epsg:4326")
	stateplane <- CRS("+init=epsg:2818")
	
	
#this stores all data that i want to keep as temp
#also can;t use it in later lines
  # subset related seasons
  assign(paste0("win14", elk), subset(locs, AnimalID == elk & MigHR == "Winter 2014")
  assign(paste0("sum14", elk), subset(locs, AnimalID == elk & MigHR == "Summer 2014")

############################
## FIRST RUN TROUBLESHOOTING
############################

############
##final dataframe extra rows mostly filled with NAs
# NA rows # 27 - 1485
# missing overlaps are NAs instead of 0s
##issue 2: non-numeric object error
#fails in fall14, spr15, and fall15 for same indiv - 140940

#1. fix error on data read-in (EOF within quoted string)  
locs <- read.csv("locsMigHR.csv", as.is = TRUE, header = TRUE)

#newp

locs <- read.csv("locsMigHR.csv", as.is = TRUE, header = TRUE, skipNul=TRUE, quote = "")
locs <- read.csv("locsMigHR.csv", header = TRUE, skipNul=TRUE, quote = "")
#no error, but does weird stuff to the data & column names

locs <- read.csv("locsMigHR.csv", as.is = TRUE, header = TRUE, skipNul=TRUE, row.names = FALSE)
#like above but with extra errors 

locs <- read.csv("locsMigHR.csv", as.is = TRUE, header = TRUE, skipNul=TRUE, sep = ",")
locs <- as.data.frame(read.csv("locsMigHR.csv", as.is = TRUE, header = TRUE, skipNul=TRUE))
#same as the original code

#GPS DATA FROM COLLARS, PLUS MIGRATION SEASON/YEAR
locs <- read.csv("collardata-locsonly-equalsampling.csv", as.is = TRUE, header = TRUE)
locs$Date <- as.Date(locs$Date, "%Y-%m-%d")
#can remove extraneous columns here#
library(dplyr)
locs$MigHR <- ifelse(between(locs$Date, as.Date("2014-01-01"), as.Date("2014-03-31")), "Winter 2014", 
                ifelse(between(locs$Date, as.Date("2014-06-01"), as.Date("2014-08-31")), "Summer 2014", 
                  ifelse(between(locs$Date, as.Date("2015-01-01"), as.Date("2015-03-31")), "Winter 2015", 
                    ifelse(between(locs$Date, as.Date("2015-06-01"), as.Date("2015-08-31")), "Summer 2015",
                      ifelse(between(locs$Date, as.Date("2016-01-01"), as.Date("2016-03-31")), "Winter 2016",
                        ifelse(NA))))))
locs <- select(locs, -c(X, FixStatus, DOP, TempC, CaptureYr, EndTime, EndLat, EndLong, Season, Hour))
write.csv(locs, file = "locsMigHR2.csv", row.names = FALSE)

# using this file instead of the original one does not give the error
#and all locations are read in
# re-running with this file

##############
#learning to run on multiple processors
#############
library(snowfall)
#below code copied/slightly modified from help file

# tell R to run on more than one cpu
sfInit(parallel=TRUE, cpus=3)
# ask whether you're running on multiple and how many 
if( sfParallel() ) {
  cat( "Running in parallel mode on", sfCpus(), "nodes.\n" )
} else {
  cat( "Running in sequential mode.\n" )
}

#sfInit initializes the multiple running thing
#sfStop stops it (obvs)
#sfParallel() tells you whether or not you're parallel
  #but not how many cores you're using
  #(that's what the above code is for)
# aaand apparently that's all there is to it

##############
#running fall15ao one step at a time
#bc when you reran it after fixing the seasons (winter15 -> 16)
#it failed and R aborted
#hopefully just bc i messed up the memory settings
#############
######################
#FALL 2015 MIGRATION

  #summer 2015
  temp.sum15 <- subset(locs, AnimalID == 140560 & MigHR == "Summer 2015")
  xy.sum15 <- data.frame("x" = temp.sum15$Long, "y" = temp.sum15$Lat)
  ll.sum15 <- SpatialPointsDataFrame(xy.sum15, temp.sum15, proj4string = latlong)
  sp.sum15 <- spTransform(ll.sum15, stateplane)
  kud.sum15 <- kernelUD(sp.sum15, h="href", grid = 5000)
  vol.sum15 <- getverticeshr(kud.sum15, percent = 95, ida = NULL, unin = "m", unout = "km")
  
  #winter 2016
  temp.win16 <- subset(locs, AnimalID == 140560 & MigHR == "Winter 2016")
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
 
##############
#loading in seasonal data
#to create final df
#after running falll15 season separately
############# 
spr14 <- read.csv("spr14ao.csv")
fall14 <- read.csv("fall14ao.csv")
spr15 <- read.csv("spr15ao.csv")

hr.manual <- full_join(fall14, spr14, by = "AnimalID")
	hr.manual <- full_join(hr.manual, spr15, by = "AnimalID")
	hr.manual <- full_join(hr.manual, fall15, by = "AnimalID")
	hr.manual <- hr.manual[,c("AnimalID", "spr14ao", "fall14ao", "spr15ao", "fall15ao")]
#you could probably pipe this... if you have time to play


############################
## MISC HELPFUL STUFF
############################

#check whether there are any NAs in df or column
any(is.na(testxy$x))

#look at one row based on a cell value (here, indiv 140800)
hro.look[hro.look$AnimalID %in% 140800,]

#index area slot of spatialpolygon (here, df res140060)
sapply(slot(res140060, "polygons"), slot, "area")

#f%^&ing memory issues
memory.limit(size = NA) #prints current memory limit, in megs i think
memory.size(max = TRUE) #max amt of memory obtained from OS
memory.size(max = FALSE) #amt currently in use
memory.size(max = NA) #actual limit - same as 1st line above
memory.limit(size = 7500000) #just shy of 8 tb limit i think
