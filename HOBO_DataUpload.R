############################
# HOBO_DataUpload.R
# HOBOData_Carpenter.Rproj

# Uploads raw temperature data from tidbits and prepares for plotting.
# Make sure that the site name is the first characters of the file name and the date
# range is listed as date_date in the file name. This will result in the proper site name
# in the data frame.


# !!!!!!!!!! Note...if plots aren't working it's likely a date conversion issue from when I was combining csv files in excel
# In excel the date column has to be formated as mm/dd/yyyy hh:mm
# Note: Both marshall and mcdonald were flooded between from march 2015 to the end of the summer when the rsv went back down
# It's really tricky to see because it might not be rsv water; just backwatered creek water so temps might be ok

# Created August 28, 2015
# A Putt
#############################
library(lubridate)
library(plyr)


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
temps$justdate <- format(temps$fulldate,"%Y-%m-%d")

# Uploard reservoir elevation data
elevations <- read.csv("AllDataForExport_copiedfromreservoirlevelsproject.csv",head=TRUE)
elevations$fulldate <- as.POSIXct(elevations$fulldate)

# Upload visual survey data 
surveys <- read.csv("WalkstoExport_copiedfromstreamsurveys.csv",head=TRUE)
surveys$date <- as.POSIXct(surveys$date)

