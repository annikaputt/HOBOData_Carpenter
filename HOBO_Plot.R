############################
# HOBO_Plot.R
# HOBOData_Carpenter.Rproj

# Plots HOBO data from the Carpenter streams

# Created August 28, 2015
# A Putt
#############################
library(ggplot2)

source("HOBO_DataUpload.R") # Data frame is called temps

# Create a plot by station
ggplot(temps,aes(x=fulldate,y=temp,colour=site,group=site)) + geom_line() +
  labs(x="Date", y=expression(paste("Temperature ( ", degree ~ C, " )")), title="Carpenter Watershed Stream Temperatures\n") +
  theme(axis.title.x=element_text(vjust=-0.35),axis.title.y=element_text(vjust=0.35)) +
  scale_color_brewer(palette="Set1")

# Make a multi-panel plot
ggplot(temps, aes(fulldate,temp))+geom_line()+
  facet_wrap(~site, ncol=2)
