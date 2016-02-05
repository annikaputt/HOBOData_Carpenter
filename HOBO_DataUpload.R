############################
# HOBO_DataUpload.R
# HOBOData_Carpenter.Rproj

# Uploads raw temperature data from tidbits and prepares for plotting.
# Make sure that the site name is the first characters of the file name and the date
# range is listed as date_date in the file name. This will result in the proper site name
# in the data frame.

# Created August 28, 2015
# A Putt
#############################
library(lubridate)
library(plyr)

# !!!!!!!!!! Note...if plots aren't working it's likely a date conversion issue from when I was combining csv files in excel

# Upload all csv files from the UploadData folder
file_path  <- list.files(path="UploadData", pattern="*.csv") # Pull all files
file_list  <- list()

for(i in 1:length(file_path)){
  temp_df <- read.csv(sprintf("UploadData/%s",file_path[i]),head=TRUE,skip=1) # Read in the data
  temp_df <- temp_df[,2:3] # Remove unwanted columns
  names(temp_df) <- c("fulldate","temp") 
  temp_df$site <- gsub(".csv","",file_path[i]) # Pull the site name from the file name
  temp_df$site <- as.factor(temp_df$site)
  #temp_df$fulldate <- strptime(temp_df$fulldate,format="%m/%d/%y %I:%M:%S %p") # Convert to date and to the 24 hour clock
  temp_df$fulldate <- strptime(temp_df$fulldate,format="%m/%d/%Y %H:%M") # Convert to date and to the 24 hour clock
  file_list[[i]] <- temp_df
}

temps <- do.call("rbind",file_list) # Combine into one large data frame
rownames(temps) <- 1:nrow(temps) # Get rid of the long row names that are automatically created
temps <- arrange(temps, fulldate) # If you look at the header this will show whether all of the dates are proper


# Get rid of data from when the tidbits were in the air before and after deployment
temps$justdate <- format(temps$fulldate,"%Y-%m-%d")
temps <- subset(temps, !(justdate %in% c("2014-09-03","2015-04-10","2015-04-11","2015-04-12","2015-04-13",
                                         "2015-04-14","2015-04-15")))

# Get rid of hurleybridge outlier
hurl <- subset(temps,site=="hurleybridge")
hurl <- subset(hurl, !(justdate %in% c("2015-04-20","2015-04-21","2015-07-27","2015-07-28")))
temps <- subset(temps,site!="hurleybridge")
temps <- rbind(temps,hurl)


# Uploard reservoir elevation data
elevations <- read.csv("AllDataForExport_copiedfromreservoirlevelsproject.csv",head=TRUE)
elevations$fulldate <- as.POSIXct(elevations$fulldate)

# Upload visual survey data 
surveys <- read.csv("WalkstoExport_copiedfromstreamsurveys.csv",head=TRUE)
surveys$date <- as.POSIXct(surveys$date)

