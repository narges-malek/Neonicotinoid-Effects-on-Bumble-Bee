library(dplyr)
names(bb)
names(bb)[1] = 'County'
County.lat.long <- bb[!duplicated(bb$County),c('County', 'Lat', 'Long')]
head(County.lat.long)

pd.cord <- read.csv(file = "C:\\Users\\Narges\\Desktop\\DataAnalytics\\CIND 820\\Literature\\geographicord (1).txt")
sd.cord <- read.csv(file = "C:\\Users\\Narges\\Desktop\\DataAnalytics\\CIND 820\\Literature\\soilandstream.txt")
sw.cord <- read.csv(file = "C:\\Users\\Narges\\Desktop\\DataAnalytics\\CIND 820\\Literature\\soilandstream.txt")
dw.cord <- read.csv(file = "C:\\Users\\Narges\\Desktop\\DataAnalytics\\CIND 820\\Literature\\drinkingwater.txt")
# wide format of soil data
# use option fill = as.numeric(NA) in dcast to substitute NA instead of NaN where there is no value
names(sd)
#package needed for wide format ->dcast() long format-> melt() 
#package data.table for as.data.table()

library(reshape2)
library(data.table)

sd.wide <- dcast(as.data.table(sd), sd.Loc + sd.Date + Season ~ paste0("sd.", `Soil.Depth..cm.`) + sd.Parameter, value.var = "sd.Value", fun.aggregate = mean, fill = as.numeric(NA))

names(sw)
dim(sw)
sw.wide <- dcast(as.data.table(sw), sw.Loc + sw.Date ~ paste0("sw.", sw.PARAMETER), value.var= "sw.RESULT", fun.aggregate = mean, fill = as.numeric(NA))
names(sw.wide)

names(dw)
class(dw$dw.Date)
dw.wide <- dcast(as.data.table(dw), `dw.Loc` + dw.Date ~ paste0("dw.", dw.Type) + dw.Parameter, value.var = "dw.Result", fun.aggregate = mean, fill = as.numeric(NA))
names(dw.wide)



names(pd)
# to delete insecticides other than 4 famous one  
names(pd)[5:14] <- c('pd.Clothianidin...MOECC', 'pd.Clothianidin...AFL', 'pd.Dinotefuran...MOECC', 'pd.Flonicamid...MOECC', 'pd.Imidacloprid...MOECC', 'pd.Imidacloprid...AFL', 'pd.Nitenpyram...MOECC', 'pd.Thiacloprid...MOECC', 'pd.Thiamethoxam...MOECC', 'pd.Thiamethoxam...AFL')
names(pd)

pd.wide <- group_by(pd, pd.Loc, pd.Date, pd.Type) %>%
  summarise_at(4-14, mean)

summary(pd.wide)
# delete insecticide columns have only zero or NA
dim(pd.wide)
pd.wide <- pd.wide[, -c(7, 8, 10, 11)]




levels(as.factor(bb$County))
County.lat.long <- bb[!duplicated(bb$County), c('County', 'Lat', 'Long')]






#__________________________________________________________________
# functions to call later
# 1st function
# distance_finder to compute distance between two locations by their lat and log
distance_finder <- function(lat1, lon1, lat2, lon2) {
  # degree to radian
  rlat1 = lat1 * pi/180
  rlon1 = lon1 * pi/180
  rlat2 = lat2 * pi/180
  rlon2 = lon2 * pi/180
  
  dlat = rlat2 - rlat1
  dlon = rlon2 - rlon1
  
  a = sin(dlat / 2)**2 + cos(rlat1) * cos(rlat2) * sin(dlon / 2)**2
  
  c= 2 * asin(sqrt(a))
  
  r = 6371
  
  return(c*r)
  
}

# example to test distance_finder
lat1 = 53.32055555555556
lat2 = 53.31861111111111
lon1 = -1.7297222222222221
lon2 =  -1.6997222222222223

print(distance_finder(lat1, lon1, lat2, lon2))


#__________________________________________________________________
# 2nd function
# function to determine tow closest locations by compute all distances between all locations 

locations_min_dist <- function(dat1, dat2) {
  saving.df = data.frame(character(), character(), double())
  dist_col_name = paste0(deparse(substitute(dat2)),".dist")
  names(saving.df) <- c("County", paste0(gsub("\\..*", "", deparse(substitute(dat2))),".Loc"), paste0(gsub("\\..*", "", deparse(substitute(dat2))),".Loc.dist"))
  
  for (i in row.names(dat1)) {
    lat1 = as.numeric(str_trim(dat1[i, 'Lat']))
    lon1 = as.numeric(str_trim(dat1[i, 'Long']))
    mymat = matrix(0, nrow(dat2), 3)
    for (j in row.names(dat2)) {
      lat2 = as.numeric(str_trim(dat2[j, 'Lat']))
      lon2 = as.numeric(str_trim(dat2[j, 'Long']))
      
      distance = distance_finder(lat1, lon1, lat2, lon2)
      mymat[as.numeric(j),] = c(dat1[i, 'County'], dat2[j,'area'], distance)
    }
    min_dist <- mymat[which.min(mymat[,3]),]
    cat(min_dist, "\n")
    saving.df[nrow(saving.df) + 1,] <- min_dist
  }
  return(saving.df)
}


#__________________________________________________________________
# 3rd function
# function to determine tow closest dates by compute all differences between all dates 

dates_min_diff <- function(dat1, dat2) {
  saving.df = data.frame(character(), character(), as.Date(character()), character(), as.Date(character()), double())
  
  dat2.loc.col = paste0(gsub("\\..*", "", deparse(substitute(dat2))),".Loc")
  dat2.date.col = paste0(gsub("\\..*", "", deparse(substitute(dat2))),".Date")
  dates.diff.col = paste0(gsub("\\..*", "", deparse(substitute(dat2))),".date.diff")
  names(saving.df) = c("County", "bb.site", "bb.Date", dat2.loc.col, dat2.date.col, dates.diff.col)
  
  for (i in row.names(dat1)) {
    this.Loc = dat1[i, dat2.loc.col]
    mymat = matrix(0, nrow(dat2[dat2[dat2.loc.col] == this.Loc, ]), 6)
    for (j in 1:nrow(dat2[dat2[dat2.loc.col] == this.Loc, ])) {
      
      DateDiff = abs(dat1[i,"bb.Date"] - dat2[dat2[dat2.loc.col] == this.Loc, which(names(dat2)==dat2.date.col)][j])
      mymat[as.numeric(j),] = c(dat1[i,"County"], dat1[i,"bb.site"],as.character(dat1[i,"bb.Date"]), this.Loc, as.character(dat2[dat2[dat2.loc.col] == this.Loc, which(names(dat2)==dat2.date.col)][j]), DateDiff)
    }
    min_diff <- mymat[which.min(mymat[,6]),]
    cat(min_diff, "\n")
    saving.df[nrow(saving.df) + 1,] <- min_diff
  }
  return(saving.df)
}


#__________________________________________________________________

# to have identical "SITE NAME" spells in files

#names(pd.wide)[1] <- 'pd.Loc'
#names(sd.wide)[1] <- 'sd.Loc'
#names(sw.wide)[1] <- 'sw.Loc'
#names(dw.wide)[1] <- 'dw.Loc'

class(sw.wide$sw.Loc)
sw.wide$sw.Loc <- as.character(sw.wide$sw.Loc)
# to have similar spel for location names in sd and sw files
sw.wide$`sw.Loc`[sw.wide$`sw.Loc`=="BIG CREEK"] <- 'Big Creek'
sw.wide$`sw.Loc`[sw.wide$`sw.Loc`=="GARVEY GLEN"] <- 'Garvey Glen Creek'
sw.wide$`sw.Loc`[sw.wide$`sw.Loc`=="LITTLE AUSABLE RIVER"] <- 'Little Ausable Creek'
sw.wide$`sw.Loc`[sw.wide$`sw.Loc`=="NORTH CREEK"] <- 'North Creek'
sw.wide$`sw.Loc`[sw.wide$`sw.Loc`=="WHITE ASH CREEK"] <- 'White Ash Creek'
sw.wide$`sw.Loc`[sw.wide$`sw.Loc`=="SPRING CREEK"] <- 'Spring Creek'


#__________________________________________________________________
# pollen and bb



# pollen and bb minimum locations distance 
bb.pollen.loc <- locations_min_dist(County.lat.long, pd.cord)
# pollen and bb location  merge
bb_pollen.merge.loc = merge(bb, bb.pollen.loc)
dim(bb_pollen.merge.loc)

# pollen and bb minimum dates difference
class(pd.wide)
pd.wide <- tibble(pd.wide)
pd.wide <- data.frame(pd.wide)
dim(pd.wide)
names(pd.wide)
bb.pollen.loc.date <- dates_min_diff(bb_pollen.merge.loc, pd.wide)
# pollen and bb dates merge
names(bb.pollen.loc.date)
names(bb_pollen.merge.loc)
bb_pollen.merge.loc.date = merge(bb_pollen.merge.loc, bb.pollen.loc.date)
dim(bb_pollen.merge.loc.date)



# sw_wide and sd_wide have same location and sw_wide has one more location


#__________________________________________________________________
# soil data and Bumblebee


# soil data and Bumblebee minimum locations distance 
bb.sd.loc <- locations_min_dist(County.lat.long, sd.cord)
# soil data and Bumblebee location  merge
bb.sd.merge.loc = merge(bb_pollen.merge.loc.date, bb.sd.loc)

# soil data and Bumblebee minimum dates difference
sd.wide <- as.data.frame(sd.wide)
bb.sd.loc.date <- dates_min_diff(bb.sd.merge.loc, sd.wide)

# soil data and Bumblebee dates merge
bb.pollen.sd.merge.loc.date = merge(bb.sd.merge.loc, bb.sd.loc.date)
dim(bb.pollen.sd.merge.loc.date)

#__________________________________________________________________
# streaming water data and Bumblebee

# streaming water data and Bumblebee minimum locations distance 
bb.sw.loc <- locations_min_dist(County.lat.long, sw.cord)
# streaming water data and Bumblebee location  merge
bb.sw.merge.loc = merge(bb.pollen.sd.merge.loc.date, bb.sw.loc)

# streaming water data and Bumblebee minimum dates difference
sw.wide <- as.data.frame(sw.wide)

bb.sw.loc.date <- dates_min_diff(bb.sw.merge.loc, sw.wide)


# streaming water data and Bumblebee dates merge
bb.pollen.sd.sw.merge.loc.date = merge(bb.sw.merge.loc, bb.sw.loc.date)
dim(bb.pollen.sd.sw.merge.loc.date)


#__________________________________________________________________
# Drinking water data and Bumblebee

# Drinking water data and Bumblebee minimum locations distance 
bb.dw.loc <- locations_min_dist(County.lat.long, dw.cord)
# Drinking water data and Bumblebee location  merge
bb.dw.merge.loc = merge(bb.pollen.sd.sw.merge.loc.date, bb.dw.loc)

# Drinking water data and Bumblebee minimum dates difference
dw.wide <- as.data.frame(dw.wide)
class(dw.wide)
names(dw.wide)
class(dw.wide$dw.Date)
bb.dw.loc.date <- dates_min_diff(bb.dw.merge.loc, dw.wide)

# Drinking water data and Bumblebee dates merge
bb.pollen.sd.sw.dw.merge.loc.date = merge(bb.dw.merge.loc, bb.dw.loc.date)
dim(bb.pollen.sd.sw.dw.merge.loc.date)
names(bb.pollen.sd.sw.dw.merge.loc.date)


#__________________________________________________________________
# Merging created file to other data files pollen.wide, sd.wide, sw.wide, dw.wide
nrow(unique(bb.pollen.sd.sw.dw.merge.loc.date[c("County","bb.site","bb.Date")]))

dim(bb.pollen.sd.sw.dw.merge.loc.date)
bb.pollen.params <- merge(bb.pollen.sd.sw.dw.merge.loc.date, pd.wide)
dim(bb.pollen.params)
bb.pollen.sd.params <- merge(bb.pollen.params, sd.wide)
dim(bb.pollen.sd.params)
bb.pollen.sd.sw.params <- merge(bb.pollen.sd.params, sw.wide)
dim(bb.pollen.sd.sw.params)
bb.pollen.sd.sw.dw.params <- merge(bb.pollen.sd.sw.params, dw.wide)
dim(bb.pollen.sd.sw.dw.params)

format(bb.pollen.sd.sw.dw.params)






