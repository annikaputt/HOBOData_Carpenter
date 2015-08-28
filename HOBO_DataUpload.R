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

# Upload all csv files from the UploadData folder
file_path  <- list.files(path="UploadData", pattern="*.csv") # Pull all files
file_list  <- list()

for(i in 1:length(file_path)){
  temp_df <- read.csv(sprintf("UploadData/%s",file_path[i]),head=TRUE,skip=1) # Read in the data
  temp_df <- temp_df[,2:3] # Remove unwanted columns
  names(temp_df) <- c("fulldate","temp") 
  temp_df$site <- gsub("\\d","",file_path[i]) # Pull the site name from the file name
  temp_df$site <- as.factor(substr(temp_df$site,1,nchar(temp_df$site)-5)) # Edit the site name
  temp_df$fulldate <- strptime(temp_df$fulldate,format="%m/%d/%y %I:%M:%S %p") # Convert to date and to the 24 hour clock
  file_list[[i]] <- temp_df
}

temps <- do.call("rbind",file_list) # Combine into one large data frame
rownames(temps) <- 1:nrow(temps) # Get rid of the long row names that are automatically created

# Get rid of data from when the tidbits were in the air before deployment
# Maybe one day before or after the first and last dates
# To do this I subset based on the first and last dates + the number of seconds in one day!
# Actually I'm taking a bit more off of the end because it must have been out longer
first.date <- min(temps$fulldate,na.rm=TRUE)
last.date <- max(temps$fulldate,na.rm=TRUE)
temps <- subset(temps,fulldate > first.date+86400 & fulldate < last.date-86400*3)
