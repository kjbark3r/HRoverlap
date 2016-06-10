####################################################
## Working with the Area and Volume Overlap Data  ##
########  NSERP - Kristin Barker - June 2016  ######
####################################################

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

##DATA AND LIBRARIES
hro <- read.csv("homerangeoverlap.csv")
library(dplyr)

################
# histograms

#area overlap in each mig'n season
par(mfrow=c(2, 2))
hist(hro$spr14ao, main = "Spring 2014", xlab = "Proportion Area Overlap")
hist(hro$spr15ao, main = "Spring 2015", xlab = "Proportion Area Overlap")
hist(hro$fall14ao, main = "Fall 2014", xlab = "Proportion Area Overlap")
hist(hro$fall15ao, main = "Fall 2015", xlab = "Proportion Area Overlap")

#volume intersection in each mig'n season
par(mfrow=c(2, 2))
hist(hro$spr14vi, main = "Spring 2014", xlab = "Volume Intersection")
hist(hro$spr15vi, main = "Spring 2015", xlab = "Volume Intersection")
hist(hro$fall14vi, main = "Fall 2014", xlab = "Volume Intersection")
hist(hro$fall15vi, main = "Fall 2015", xlab = "Volume Intersection")

################
# boxplots

#area overlap in each mig'n season
par(mfrow=c(1,1))
boxplot(hro$spr14ao, hro$spr15ao, hro$fall14ao, hro$fall15ao, 
        main = "Area Overlap",
        names = c("Spr14", "Spr15", "Fall14", "Fall15"))

#volume intersection in each mig'n season
boxplot(hro$spr14vi, hro$spr15vi, hro$fall14vi, hro$fall15vi, 
        main = "Volume Intersection",
        names = c("Spr14", "Spr15", "Fall14", "Fall15"))

################
# lines and scatters

#area overlap vs volume intersection
par(mfrow=c(2, 2))
plot(hro$spr14ao~hro$spr14vi, main = "Spring 2014")
plot(hro$spr15ao~hro$spr15vi, main = "Spring 2015")
plot(hro$fall14ao~hro$fall14vi, main = "Fall 2014")
plot(hro$fall15ao~hro$fall15vi, main = "Fall 2015")

################
# with prelim glance results

#create df
look <- read.csv("migstatus-prelimlook.csv")
lookstatus <- look[, c("AnimalID", "Status")]
hro.look <- full_join(hro, lookstatus, by = "AnimalID")

#boxplots
boxplot(hro$spr14vi, hro$spr15vi, hro$fall14vi, hro$fall15vi, 
        main = "Volume Intersection",
        names = c("Spr14", "Spr15", "Fall14", "Fall15"))
