#######################################################

#### Code to subset camera trap images for tagging ###

#######################################################

# this is the most up-to-date code and replaces the previous one named "Data_sort"

# it will select images based on a fixed time interval between triggers - alos used to obtain 'independent' records  

library(data.table) #for rbindlist, used in read.multi
library(readxl) #for read_xls and read_xlsx, used in read.multi

# source useful functions for CT data from https://github.com/MarcusRowcliffe/camtools/blob/master/camtools.R
# must first download and save the file in the working directory 
source("camtools_MarcusRowcliffe.R") 

#the thin.events function will select images based on a time interval between sequential triggers
#it creates a data frame with a row per independent event, with non-independence defined as
#within a given time of a prior record

#INPUT
# obsdat: dataframe of camera observations, one row per image data; must include columns:
#    species: character species identifier
#    station: character camera station identifier
#    date: character or POSIX date-times of observations; converted using as.POSIXct
# interval: independence interval in hours
# format: format for date conversion passed to as.POSIXct
# tz: time zone for date conversion, passed to as.POSIXct

#OUTPUT
# A dataframe with the same columns as the input obsdat but (potentially) fewer rows
thin.events <- function(obsdat, interval, format="%Y:%m:%d %H:%M:%S", tz="UTC"){
  error.check(obsdat, tz, format)
  
  sp.stn <- paste(obsdat$species, obsdat$station, sep=".")
  date <- as.POSIXct(obsdat$date, format=format, tz=tz)
  i <- order(sp.stn, date)
  obsdat <- obsdat[i, ]
  sp.stn <- sp.stn[i]
  date <- date[i]
  
  ii <- i <- 1
  while(i<length(date)){
    base <- tail(ii,1)
    i <- base+1
    while(difftime(date[i], date[base], units="hours")<interval & 
          sp.stn[i]==sp.stn[base] & 
          i<length(date)) i <- i+1
    if(difftime(date[i], date[base], units="hours")>=interval | 
       sp.stn[i]!=sp.stn[base]) ii <- c(ii,i)
  }
  res <- obsdat[ii, ]
  res$time <- as.numeric(format(date[ii], "%H")) + 
    as.numeric(format(date[ii], "%M"))/60 + 
    as.numeric(format(date[ii], "%H"))/60^2
  res
}


# reading data and organising it
# Nepal's 2019 exif data
data <- fread("X:/xxxxxxx/xxxxx/xxxxx/nepal/processed_data/2019_CTimages_exifdata_Nepal_complete.csv") # part of file path ommited in the public version
str(data)

# selecting a small subset to test the thin.events function
subdata <- data[1:5000,c("site_cam.x", "fixed_date_time", "date_fixed", "new_file_structure", "Time")] 
names(subdata)

# creating a species column with the same mock species in all rows, as independent records are for each individual spp
# could use any species name
subdata$species <- rep("svenaticus",5000)  # number of repetitions must be the same as the number of obs in the DF - 5000 in this case

tail(subdata) # just checking
colnames(subdata)[1] <- "station" # changing col names to match thin.events function
colnames(subdata)[2] <- "date" # ditto

#important to keep an eye at the separators, it must match the input data frame
# interval is in hours, so "interval =1" means 1-hour interval between sequential triggers at the same site
thin.subdata <- thin.events(subdata, interval= 1, format="%Y-%m-%d %H:%M:%S", tz="UTC") 

#now testing function with intervals < 1h
thin.subdata.halfhour <- thin.events(subdata, interval= 0.5, format="%Y-%m-%d %H:%M:%S", tz="UTC") #30-min interval as independece threshold (1/2= 0.5)
thin.subdata.1min <- thin.events(subdata, interval= 0.01666667, format="%Y-%m-%d %H:%M:%S", tz="UTC") #1-min interval as independece threshold (1/60= 0.01666667)
write.csv(thin.subdata.1min,"thin_subdata_1min.csv")
