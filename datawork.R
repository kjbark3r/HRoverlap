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
hist(hro$spr14ao, breaks = 8, main = "Spring 2014", xlab = "Proportion Area Overlap")
hist(hro$spr15ao, breaks = 8, main = "Spring 2015", xlab = "Proportion Area Overlap")
hist(hro$fall14ao, breaks = 8, main = "Fall 2014", xlab = "Proportion Area Overlap")
hist(hro$fall15ao, breaks = 8, main = "Fall 2015", xlab = "Proportion Area Overlap")

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

#completely meaningless plots - volume intersection
s14vi <- hro.look %>%
  arrange(spr14vi) %>%
  subset(select = c(AnimalID, spr14vi, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(s14vi$spr14vi ~ s14a$Rank, main = "Spring 2014",
     col = s14vi$Status)

f14vi <- hro.look %>%
  arrange(fall14vi) %>%
  subset(select = c(AnimalID, fall14vi, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(f14vi$fall14vi ~ f14vi$Rank, main = "Fall 2014",
     col = f14vi$Status)

s15vi <- hro.look %>%
  arrange(spr15vi) %>%
  subset(select = c(AnimalID, spr15vi, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(s15vi$spr15vi ~ s15vi$Rank, main = "Spring 2015",
     col = s15vi$Status)

f15vi <- hro.look %>%
  arrange(fall15vi) %>%
  subset(select = c(AnimalID, fall15vi, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(f15vi$fall15vi ~ f15vi$Rank, main = "Fall 2015",
     col = f15vi$Status)

#completely meaningless plots - area overlap
s14a <- hro.look %>%
  arrange(spr14ao) %>%
  subset(select = c(AnimalID, spr14ao, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(s14a$spr14ao ~ s14a$Rank, main = "Spring 2014",
     col = s14a$Status)

f14a <- hro.look %>%
  arrange(fall14ao) %>%
  subset(select = c(AnimalID, fall14ao, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(f14a$fall14ao ~ f14a$Rank, main = "Fall 2014",
     col = f14a$Status)

s15a <- hro.look %>%
  arrange(spr15ao) %>%
  subset(select = c(AnimalID, spr15ao, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(s15a$spr15ao ~ s15a$Rank, main = "Spring 2015",
     col = s15a$Status)

f15a <- hro.look %>%
  arrange(fall15ao) %>%
  subset(select = c(AnimalID, fall15ao, Status)) %>%
  mutate(Rank = row_number()) %>%
  na.omit()
plot(f15a$fall15ao ~ f15a$Rank, main = "Fall 2015",
     col = f15a$Status)

################
# with prelim glance results

#create df
look <- read.csv("migstatus-prelimlook.csv")
lookstatus <- look[, c("AnimalID", "Status")]
hro.look <- full_join(hro, lookstatus, by = "AnimalID")

#boxplots
boxplot(spr14ao ~ Status, data = hro.look, main = "Spr14 AO")
boxplot(spr14vi ~ Status, data = hro.look, main = "Spr14 VI")
boxplot(spr15ao ~ Status, data = hro.look, main = "Spr15 AO")
boxplot(spr15vi ~ Status, data = hro.look, main = "Spr15 VI")

