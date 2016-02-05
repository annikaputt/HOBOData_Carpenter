############################
# HOBO_Plot.R
# HOBOData_Carpenter.Rproj

# Plots HOBO data from the Carpenter streams

# Created August 28, 2015
# A Putt
#############################
library(ggplot2)
library(lattice)

source("HOBO_DataUpload.R") # Data frame is called temps

# Create a plot by station
windows()
ggplot(temps,aes(x=fulldate,y=temp,colour=site,group=site)) + geom_line() +
  labs(x="Date", y=expression(paste("Temperature ( ", degree ~ C, " )")), title="Carpenter Watershed Stream Temperatures\n") +
  theme(axis.title.x=element_text(vjust=-0.35),axis.title.y=element_text(vjust=0.35)) +
  scale_color_brewer(palette="Set1")

# Make a multi-panel plot
windows()
ggplot(temps, aes(fulldate,temp))+geom_line()+
  facet_wrap(~site, ncol=2)

# Create a plot with stacked panels; including elevation data for the correct time period
windows()
par(mar=c(0,0,0,0),oma=c(6,6,4,6),xpd=FALSE)
Lay <- layout(matrix(c(1,2,3,4,5,6,7), nrow=7, ncol=1, byrow = TRUE))
panelxlim <- as.numeric(c(min(temps$fulldate,na.rm=TRUE),max(temps$fulldate,na.rm=TRUE)))
panelylim <- c(0,20)

#plot(subset(temps,site=="girl")$fulldate,subset(temps,site=="girl")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
#title(main="Girl Creek",adj=0.05,line=-1.5)
plot(subset(temps,site=="gun")$fulldate,subset(temps,site=="gun")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="Gun Creek",adj=0.05,line=-1.5)
plot(subset(temps,site=="hurley")$fulldate,subset(temps,site=="hurley")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="Hurley River",adj=0.05,line=-1.5)
plot(subset(temps,site=="marshall")$fulldate,subset(temps,site=="marshall")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="Marshall Creek",adj=0.05,line=-1.5)
plot(subset(temps,site=="mcdonald")$fulldate,subset(temps,site=="mcdonald")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="McDonald Creek",adj=0.05,line=-1.5)
plot(subset(temps,site=="hurleybridge")$fulldate,subset(temps,site=="hurleybridge")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="Middle Bridge River",adj=0.05,line=-1.5)
plot(subset(temps,site=="sucker")$fulldate,subset(temps,site=="sucker")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
#title(main="Sucker Creek",adj=0.05,line=-1.5)
#plot(subset(temps,site=="truax")$fulldate,subset(temps,site=="truax")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="",xaxt="n",las=2)
title(main="Truax Creek",adj=0.05,line=-1.5)
plot(elevations$fulldate,elevations$carpenterelev,type="l",xlim=panelxlim,,ylim=c(625,650),xlab="",ylab="",xaxt="n",las=2)
title(main="Carpenter Reservoir Elevations",adj=0.05,line=-1.5)

# Add x axis
axisseries <- seq(from=min(temps$fulldate,na.rm=TRUE),to=max(temps$fulldate,na.rm=TRUE),by="1 month")
axis.POSIXct(side=1,at=axisseries,labels=FALSE)
text(x=axisseries,y=620,labels=format(axisseries,"%b %Y"),srt=45,adj=1,xpd=NA,cex=1)
# Add y labels 
mtext(side=2,line=4,text="Temperature C",outer=TRUE)

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
plot(subset(temps,site=="girl")$fulldate,subset(temps,site=="girl")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="Temp (C)",xaxt="n",las=2)
title(main="Creek Temperature",adj=0.05,line=-1.5)
plot(elevations$fulldate,elevations$carpenterelev,type="l",xlim=panelxlim,,ylim=c(625,650),xlab="",ylab="Elevation (m)",xaxt="n",las=2)
title(main="Carpenter Reservoir Elevations",adj=0.05,line=-1.5)

# Add x axis
axisseries <- seq(from=min(temps$fulldate,na.rm=TRUE),to=max(temps$fulldate,na.rm=TRUE),by="1 month")
axis.POSIXct(side=1,at=axisseries,labels=FALSE)
text(x=axisseries,y=620,labels=format(axisseries,"%b %Y"),srt=45,adj=1,xpd=NA,cex=1)

# McDonald
windows()
par(mar=c(0,5,0,0),oma=c(6,1,4,6),xpd=FALSE)
Lay <- layout(matrix(c(1,2,3), nrow=3, ncol=1, byrow = TRUE))

plot(subset(surveys,stream=="macdonald creek")$date,subset(surveys,stream=="macdonald creek")$kocount,type="b",xlim=panelxlim,xlab="",ylab="Count",las=2,xaxt="n",pch=19)
points(subset(surveys,stream=="macdonald creek")$date,subset(surveys,stream=="macdonald creek")$rbcount,type="b",xlim=panelxlim,xlab="",ylab="Count",las=2,xaxt="n")
title(main="Spawner Counts",adj=0.05,line=-1.5)
plot(subset(temps,site=="mcdonald")$fulldate,subset(temps,site=="mcdonald")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="Temp (C)",xaxt="n",las=2)
title(main="Creek Temperature",adj=0.05,line=-1.5)
plot(elevations$fulldate,elevations$carpenterelev,type="l",xlim=panelxlim,,ylim=c(625,650),xlab="",ylab="Elevation (m)",xaxt="n",las=2)
title(main="Carpenter Reservoir Elevations",adj=0.05,line=-1.5)

# Add x axis
axisseries <- seq(from=min(temps$fulldate,na.rm=TRUE),to=max(temps$fulldate,na.rm=TRUE),by="1 month")
axis.POSIXct(side=1,at=axisseries,labels=FALSE)
text(x=axisseries,y=620,labels=format(axisseries,"%b %Y"),srt=45,adj=1,xpd=NA,cex=1)

# Marhall
windows()
par(mar=c(0,5,0,0),oma=c(6,1,4,6),xpd=FALSE)
Lay <- layout(matrix(c(1,2,3), nrow=3, ncol=1, byrow = TRUE))

plot(subset(surveys,stream=="marshall creek")$date,subset(surveys,stream=="marshall creek")$kocount,type="b",xlim=panelxlim,xlab="",ylab="Count",las=2,xaxt="n",pch=19)
points(subset(surveys,stream=="marshall creek")$date,subset(surveys,stream=="marshall creek")$rbcount,type="b",xlim=panelxlim,xlab="",ylab="Count",las=2,xaxt="n")
title(main="Spawner Counts",adj=0.05,line=-1.5)
plot(subset(temps,site=="marshall")$fulldate,subset(temps,site=="marshall")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="Temp (C)",xaxt="n",las=2)
title(main="Creek Temperature",adj=0.05,line=-1.5)
plot(elevations$fulldate,elevations$carpenterelev,type="l",xlim=panelxlim,,ylim=c(625,650),xlab="",ylab="Elevation (m)",xaxt="n",las=2)
title(main="Carpenter Reservoir Elevations",adj=0.05,line=-1.5)

# Add x axis
axisseries <- seq(from=min(temps$fulldate,na.rm=TRUE),to=max(temps$fulldate,na.rm=TRUE),by="1 month")
axis.POSIXct(side=1,at=axisseries,labels=FALSE)
text(x=axisseries,y=620,labels=format(axisseries,"%b %Y"),srt=45,adj=1,xpd=NA,cex=1)

# Truax
windows()
par(mar=c(0,5,0,0),oma=c(6,1,4,6),xpd=FALSE)
Lay <- layout(matrix(c(1,2,3), nrow=3, ncol=1, byrow = TRUE))

plot(subset(surveys,stream=="truax creek")$date,subset(surveys,stream=="truax creek")$kocount,type="b",xlim=panelxlim,xlab="",ylab="Count",las=2,xaxt="n",pch=19)
points(subset(surveys,stream=="truax creek")$date,subset(surveys,stream=="truax creek")$rbcount,type="b",xlim=panelxlim,xlab="",ylab="Count",las=2,xaxt="n")
title(main="Spawner Counts",adj=0.05,line=-1.5)
plot(subset(temps,site=="truax")$fulldate,subset(temps,site=="truax")$temp,type="l",xlim=panelxlim,ylim=panelylim,xlab="",ylab="Temp (C)",xaxt="n",las=2)
title(main="Creek Temperature",adj=0.05,line=-1.5)
plot(elevations$fulldate,elevations$carpenterelev,type="l",xlim=panelxlim,,ylim=c(625,650),xlab="",ylab="Elevation (m)",xaxt="n",las=2)
title(main="Carpenter Reservoir Elevations",adj=0.05,line=-1.5)

# Add x axis
axisseries <- seq(from=min(temps$fulldate,na.rm=TRUE),to=max(temps$fulldate,na.rm=TRUE),by="1 month")
axis.POSIXct(side=1,at=axisseries,labels=FALSE)
text(x=axisseries,y=620,labels=format(axisseries,"%b %Y"),srt=45,adj=1,xpd=NA,cex=1)
