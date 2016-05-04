############################
# HOBO_Plot.R
# HOBOData_Carpenter.Rproj

# Plots HOBO data from the Carpenter streams

# Created August 28, 2015
# A Putt
#############################
library(ggplot2)
library(lattice)
library(plyr)

source("HOBO_DataUpload.R") # Data frame is called dailyavg
head(temps)
temps$datefactor <- factor(temps$justdate)
dailyavg <- aggregate(temps[,2],by=list(temps$justdate,temps$site),FUN=mean)
names(dailyavg) <- c("justdate","site","temp")
dailyavg$justdate <- as.POSIXct(dailyavg$justdate)

# Create a plot by station
windows()
ggplot(temps,aes(x=fulldate,y=temp,colour=site,group=site)) + geom_line() +
  labs(x="Date", y=expression(paste("Temperature ( ", degree ~ C, " )")), title="Carpenter Watershed Stream Temperatures\n") +
  theme(axis.title.x=element_text(vjust=-0.35),axis.title.y=element_text(vjust=0.35)) +
  theme_bw()

windows()
ggplot(dailyavg,aes(x=justdate,y=temp,colour=site,group=site)) + geom_line() +
  labs(x="Date", y=expression(paste("Avg Daily Temperature ( ", degree ~ C, " )")), title="Carpenter Watershed Stream Temperatures\n") +
  theme(axis.title.x=element_text(vjust=-0.35),axis.title.y=element_text(vjust=0.35)) +
  scale_color_brewer(palette="Set1") + 
  theme_bw()

# Make a multi-panel plot
windows()
ggplot(temps, aes(fulldate,temp))+geom_line()+
  facet_wrap(~site, ncol=2) + 
  labs(x="", y=expression(paste("Temperature ( ", degree~C, " )"))) + 
  theme_bw()

windows()
ggplot(dailyavg, aes(justdate,temp))+geom_line()+
  facet_wrap(~site, ncol=2) + 
  labs(x="", y=expression(paste("Avg Daily Temperature ( ", degree~C, " )"))) + 
  theme_bw()

# Create a plot with stacked panels; including elevation data for the correct time period
windows()
par(mar=c(0,0,0,0),oma=c(6,6,4,6),xpd=FALSE)
Lay <- layout(matrix(c(1,2,3,4), nrow=4, ncol=1, byrow = TRUE))
panelxlim <- as.numeric(c(min(dailyavg$justdate,na.rm=TRUE),max(dailyavg$justdate,na.rm=TRUE)))
panelylim <- c(0,20)
plot(subset(dailyavg,site=="girl")$justdate,subset(dailyavg,site=="girl")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="Girl Creek",adj=0.05,line=-1.5)
plot(subset(dailyavg,site=="gun")$justdate,subset(dailyavg,site=="gun")$temp,type="l",cex.axis=0.7,xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="Gun Creek",adj=0.05,line=-1.5)
plot(subset(dailyavg,site=="hurley")$justdate,subset(dailyavg,site=="hurley")$temp,cex.axis=0.7,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="Hurley River",adj=0.05,line=-1.5)
plot(subset(dailyavg,site=="marshall")$justdate,subset(dailyavg,site=="marshall")$temp,cex.axis=0.7,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="Marshall Creek",adj=0.05,line=-1.5)

# Add x axis
axisseries <- seq(from=min(dailyavg$justdate,na.rm=TRUE),to=max(dailyavg$justdate,na.rm=TRUE),by="1 month")
axis.POSIXct(side=1,at=axisseries,labels=FALSE)
text(x=axisseries,y=620,labels=format(axisseries,"%b %Y"),srt=45,adj=1,xpd=NA,cex=1)
# Add y labels 
mtext(side=2,line=4,text="Daily Average Temperature C",outer=TRUE)

windows()
par(mar=c(0,0,0,0),oma=c(6,6,4,6),xpd=FALSE)
Lay <- layout(matrix(c(1,2,3,4), nrow=4, ncol=1, byrow = TRUE))
panelxlim <- as.numeric(c(min(dailyavg$justdate,na.rm=TRUE),max(dailyavg$justdate,na.rm=TRUE)))
panelylim <- c(0,20)
plot(subset(dailyavg,site=="mcdonald")$justdate,subset(dailyavg,site=="mcdonald")$temp,cex.axis=0.7,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="McDonald Creek",adj=0.05,line=-1.5)
plot(subset(dailyavg,site=="hurleybridge")$justdate,subset(dailyavg,site=="hurleybridge")$temp,cex.axis=0.7,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="Middle Bridge River",adj=0.05,line=-1.5)
plot(subset(dailyavg,site=="sucker")$justdate,subset(dailyavg,site=="sucker")$temp,type="l",cex.axis=0.7,xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="Sucker Creek",adj=0.05,line=-1.5)
plot(subset(dailyavg,site=="truax")$justdate,subset(dailyavg,site=="truax")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="Truax Creek",adj=0.05,line=-1.5)

# Add x axis
axisseries <- seq(from=min(dailyavg$justdate,na.rm=TRUE),to=max(dailyavg$justdate,na.rm=TRUE),by="1 month")
axis.POSIXct(side=1,at=axisseries,labels=FALSE)
text(x=axisseries,y=620,labels=format(axisseries,"%b %Y"),srt=45,adj=1,xpd=NA,cex=1)
# Add y labels 
mtext(side=2,line=4,text="Daily Average Temperature C",outer=TRUE)

windows()
par(mar=c(0,0,0,0),oma=c(6,6,4,6),xpd=FALSE)
Lay <- layout(matrix(c(1,2,3,4), nrow=4, ncol=1, byrow = TRUE))
panelxlim <- as.numeric(c(min(dailyavg$justdate,na.rm=TRUE),max(dailyavg$justdate,na.rm=TRUE)))
panelylim <- c(0,20)
plot(subset(dailyavg,site=="keary")$justdate,subset(dailyavg,site=="keary")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="Keary Creek",adj=0.05,line=-1.5)
plot(subset(dailyavg,site=="marshallwaterfall")$justdate,subset(dailyavg,site=="marshallwaterfall")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="Marshall Waterfall",adj=0.05,line=-1.5)
#plot(elevations$fulldate,elevations$carpenterelev,type="l",xlim=panelxlim,,ylim=c(625,650),cex.axis=0.7,xlab="",ylab="",xaxt="n",las=2)
#title(main="Carpenter Reservoir Elevations",adj=0.05,line=-1.5)

# Add x axis
axisseries <- seq(from=min(dailyavg$justdate,na.rm=TRUE),to=max(dailyavg$justdate,na.rm=TRUE),by="1 month")
axis.POSIXct(side=1,at=axisseries,labels=FALSE)
text(x=axisseries,y=620,labels=format(axisseries,"%b %Y"),srt=45,adj=1,xpd=NA,cex=1)
# Add y labels 
mtext(side=2,line=4,text="Daily Average Temperature C",outer=TRUE)

#############################
# Plots with spawner counts #
#############################
# Create a plot with stacked panels; including elevation data for the correct time period
# Girl
windows()
par(mar=c(0,5,0,0),oma=c(6,1,4,6),xpd=FALSE)
Lay <- layout(matrix(c(1,2,3), nrow=3, ncol=1, byrow = TRUE))

plot(subset(surveys,stream=="girl creek")$date,subset(surveys,stream=="girl creek")$kocount,type="b",xlim=panelxlim,xlab="",ylab="Count",las=2,xaxt="n",pch=19)
points(subset(surveys,stream=="girl creek")$date,subset(surveys,stream=="girl creek")$rbcount,type="b",xlim=panelxlim,xlab="",ylab="Count",las=2,xaxt="n")
title(main="Spawner Counts",adj=0.05,line=-1.5)
plot(subset(dailyavg,site=="girl")$justdate,subset(dailyavg,site=="girl")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="Avg Daily Temp (C)",xaxt="n",las=2)
title(main=" Girl Creek Temperature",adj=0.05,line=-1.5)
plot(elevations$fulldate,elevations$carpenterelev,type="l",xlim=panelxlim,,ylim=c(625,650),xlab="",ylab="Elevation (m)",xaxt="n",las=2)
title(main="Carpenter Reservoir Elevations",adj=0.05,line=-1.5)

# Add x axis
axisseries <- seq(from=min(dailyavg$justdate,na.rm=TRUE),to=max(dailyavg$justdate,na.rm=TRUE),by="1 month")
axis.POSIXct(side=1,at=axisseries,labels=FALSE)
text(x=axisseries,y=620,labels=format(axisseries,"%b %Y"),srt=45,adj=1,xpd=NA,cex=1)

# McDonald
windows()
par(mar=c(0,5,0,0),oma=c(6,1,4,6),xpd=FALSE)
Lay <- layout(matrix(c(1,2,3), nrow=3, ncol=1, byrow = TRUE))

plot(subset(surveys,stream=="macdonald creek")$date,subset(surveys,stream=="macdonald creek")$kocount,type="b",xlim=panelxlim,xlab="",ylab="Count",las=2,xaxt="n",pch=19)
points(subset(surveys,stream=="macdonald creek")$date,subset(surveys,stream=="macdonald creek")$rbcount,type="b",xlim=panelxlim,xlab="",ylab="Count",las=2,xaxt="n")
title(main="Spawner Counts",adj=0.05,line=-1.5)
plot(subset(dailyavg,site=="mcdonald")$justdate,subset(dailyavg,site=="mcdonald")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="Avg Daily Temp (C)",xaxt="n",las=2)
title(main="McDonald Creek Temperature",adj=0.05,line=-1.5)
plot(elevations$fulldate,elevations$carpenterelev,type="l",xlim=panelxlim,,ylim=c(625,650),xlab="",ylab="Elevation (m)",xaxt="n",las=2)
title(main="Carpenter Reservoir Elevations",adj=0.05,line=-1.5)

# Add x axis
axisseries <- seq(from=min(dailyavg$justdate,na.rm=TRUE),to=max(dailyavg$justdate,na.rm=TRUE),by="1 month")
axis.POSIXct(side=1,at=axisseries,labels=FALSE)
text(x=axisseries,y=620,labels=format(axisseries,"%b %Y"),srt=45,adj=1,xpd=NA,cex=1)

# Marhall
windows()
par(mar=c(0,5,0,0),oma=c(6,1,4,6),xpd=FALSE)
Lay <- layout(matrix(c(1,2,3), nrow=3, ncol=1, byrow = TRUE))

plot(subset(surveys,stream=="marshall creek")$date,subset(surveys,stream=="marshall creek")$kocount,type="b",xlim=panelxlim,xlab="",ylab="Count",las=2,xaxt="n",pch=19)
points(subset(surveys,stream=="marshall creek")$date,subset(surveys,stream=="marshall creek")$rbcount,type="b",xlim=panelxlim,xlab="",ylab="Count",las=2,xaxt="n")
title(main="Spawner Counts",adj=0.05,line=-1.5)
plot(subset(dailyavg,site=="marshall")$justdate,subset(dailyavg,site=="marshall")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="Avg Daily Temp (C)",xaxt="n",las=2)
title(main="Marhsall Creek Temperature",adj=0.05,line=-1.5)
plot(elevations$fulldate,elevations$carpenterelev,type="l",xlim=panelxlim,,ylim=c(625,650),xlab="",ylab="Elevation (m)",xaxt="n",las=2)
title(main="Carpenter Reservoir Elevations",adj=0.05,line=-1.5)

# Add x axis
axisseries <- seq(from=min(dailyavg$justdate,na.rm=TRUE),to=max(dailyavg$justdate,na.rm=TRUE),by="1 month")
axis.POSIXct(side=1,at=axisseries,labels=FALSE)
text(x=axisseries,y=620,labels=format(axisseries,"%b %Y"),srt=45,adj=1,xpd=NA,cex=1)

# Truax
windows()
par(mar=c(0,5,0,0),oma=c(6,1,4,6),xpd=FALSE)
Lay <- layout(matrix(c(1,2,3), nrow=3, ncol=1, byrow = TRUE))

plot(subset(surveys,stream=="truax creek")$date,subset(surveys,stream=="truax creek")$kocount,type="b",xlim=panelxlim,xlab="",ylab="Count",las=2,xaxt="n",pch=19)
points(subset(surveys,stream=="truax creek")$date,subset(surveys,stream=="truax creek")$rbcount,type="b",xlim=panelxlim,xlab="",ylab="Count",las=2,xaxt="n")
title(main="Spawner Counts",adj=0.05,line=-1.5)
plot(subset(dailyavg,site=="truax")$justdate,subset(dailyavg,site=="truax")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="Avg Daily Temp (C)",xaxt="n",las=2)
title(main="Truax Creek Temperature",adj=0.05,line=-1.5)
plot(elevations$fulldate,elevations$carpenterelev,type="l",xlim=panelxlim,,ylim=c(625,650),xlab="",ylab="Elevation (m)",xaxt="n",las=2)
title(main="Carpenter Reservoir Elevations",adj=0.05,line=-1.5)

# Add x axis
axisseries <- seq(from=min(dailyavg$justdate,na.rm=TRUE),to=max(dailyavg$justdate,na.rm=TRUE),by="1 month")
axis.POSIXct(side=1,at=axisseries,labels=FALSE)
text(x=axisseries,y=620,labels=format(axisseries,"%b %Y"),srt=45,adj=1,xpd=NA,cex=1)
