
#Need to add conditional calls to plot SS metrics depending on user-input
#Add different color maps for different SS metrics

#Marmap tutorial: https://www.molecularecologist.com/2015/07/marmap/

#Required libraries
library(marmap)
library(RSQLite)
library(viridis)
library(tidyverse)
library(plotfunctions)
library(data.table)
source('E:/code/cutoutliers2.R')

# Modified version of Drift.map.dasbr.events created by AES, 30 August 2019
# Function plots the path of a specified DASBR and associated soundscape metrics along its path

# INPUTS to Function:
# station.numbers = a vector.  Each list element contains station numbers from a data file to plot, e.g., station.numbers = c(1,3,7).
# spotcsvfile = a list containing names of csv file/s containing date-time-locations for a set of spot devices, created using Advanced Download in the SPOT website
# shiptrack.xy = two-column matrix with longitude and latitude coordinates of the ship's progress, in decimal degrees
# lookup = lookup table indicating spotID and deployment period for each DASBR station.  LOOKUP MUST ONLY INCLUDE DEVICES THAT WILL BE PLOTTED.
# SSmetric = a list soundscape metrics to include in the plot; e.g. list(c("BB","TOL","PSD")) - **NOT IMPLEMENTED YET**

# See example after function code

DriftMapSS <- function(outfilename, station.numbers = NULL, 
                                      MapDir=NULL,SSDir=NULL,
                                      spotcsvfile, DriftFile='Drift_FileLookup.csv',
                                      lookupfile="spotlookup_US&MX_RETRIEVED.csv"){
  
  if(is.null(MapDir)){
    MapDir<-choose.dir(caption = "Choose folder containing map data")} 
  if(is.null(SSDir)) {
    SSDir<-choose.dir(caption = "Choose folder containing databases with soundscape metrics")} 
  
  setwd(MapDir)
  
  #Create folder to save maps
  dir.create(paste(MapDir,"SoundscapeMaps/",Sys.Date(), sep=""), showWarnings = FALSE,recursive = TRUE)
  
  #Create color palettes
  blues<-c("royalblue4","royalblue3",
           "royalblue2","royalblue1")
  greys<-c(grey(0.8),grey(0.93),grey(0.99))
  dasbr.ptColor = "black"  # color for DASBR track and deployment points
  
  #Load in lookup file
  lookup = read.csv(lookupfile)
  ncsvfiles = length(spotcsvfile[[1]])
  stations <- unique(station.numbers)
  
  # combine GPS locations from multiple csv files into a single table, with most recent data at the top
  spotcsv = read.csv(spotcsvfile[[1]][ncsvfiles],header=FALSE)
  if(ncsvfiles>1){
    for(p in (ncsvfiles-1):1){spotcsv = rbind(spotcsv,read.csv(spotcsvfile[[1]][p],header=FALSE))}
    if(!is.null(station.numbers)){  # if user has specified only a subset of DASBRs, this filters the data accordingly
      spotcsv = spotcsv[spotcsv$V2 %in% lookup$spot.number[lookup$station %in% station.numbers] , ]
    }}
  
  colnames(spotcsv) = c("dateTime", "spotID", "readingType", "lat", "long")
  dateTime = strptime(spotcsv$dateTime, "%m/%d/%Y %H:%M",tz="UTC")
  n.stations = length(station.numbers)
  
  #Read in Drift Database lookup for Event Info
  setwd(MapDir)
  DriftDB<-read.csv(DriftFile)
  
  #Create Plot
  #Loop through data for each station and plot DASBR tracks
  #OffsetV and offsetH parameter moves text label slightly away (V=vetical, H=horizontal)...
  #from deployment point; User may need to adjust offset for different map boundaries 
  offsetV<-0.2
  offsetH<-0
  
  for(n in 1:n.stations){
    if(is.na(lookup$dateTimeStart[n*2])) next  # for DASBRS that have not yet been deployed, skip to next record
    if(!is.na(lookup$dateTimeStart[n*2])){  # for DASBRS that have been deployed...
      
      # spot data for station n
      data.n <- spotcsv[spotcsv$spotID %in% lookup$spot.number[lookup$station==stations[n]], ]  
      # date-time info (in date time format) for station n
      dateTime.n <- dateTime[spotcsv$spotID %in% lookup$spot.number[lookup$station==stations[n]]]
      
      # truncate data to only include location records while DASBR was deployed, and plot the tracks
      if(!is.na(lookup$dateTimeEnd[n*2])){ # for DASBRs that have been retrieved
        DriftStart<-strptime(unique(lookup$dateTimeStart[lookup$station==stations[n]]), "%m/%d/%Y %H:%M",tz="UTC")
        DriftEnd<-strptime(unique(lookup$dateTimeEnd[lookup$station==stations[n]]), "%m/%d/%Y %H:%M",tz="UTC")
        data.n.trunc <- data.n[dateTime.n >= DriftStart & dateTime.n <= DriftEnd, ]
        data.n.trunc$dateTime<-strptime(data.n.trunc$dateTime,format="%m/%d/%Y %H:%M",tz="UTC")
        
        #Remove outliers based on speed between detections (set to 1 km/hour here)
        data.n.trunc<-cutoutliers(data.n.trunc,4)
        if(stations[n] %in% c(14,21)){buffer<-0.5} 
        if(stations[n] %in% c(7,12,16,17,18,20,22,23)){buffer<-1}
        if(stations[n] %in% c(4,8,10,13,19)){buffer<-2}
        lat1<-min(data.n.trunc$lat)-buffer
        lat2<-max(data.n.trunc$lat)+buffer
        lon1<-min(data.n.trunc$lon)-buffer
        lon2<-max(data.n.trunc$lon)+buffer
        
        #Extract bathymetry data from NOAA (saves local file to speed future performance)
        bat<-getNOAA.bathy(lon1,lon2,lat1,lat2,resolution=1,keep=TRUE)
        
       # offset for station number labels
        if(stations[n] %in% c(19)){offsetV<- -0.3}
        if(stations[n] %in% c(18)){offsetV<- -0.2}
        if(stations[n] %in% c(20)){offsetV<- -0.1}
        if(stations[n] %in% c(21)){offsetV<- -0.15}
        if(stations[n] %in% c(14)){offsetV<- 0.1}
        if(stations[n] %in% c(12)){offsetV<- 0.1}
        
        if(stations[n] %in% c(10,13,16)){offsetH<- 0.2}
        if(stations[n] %in% c(23)){offsetH<- 0.1}
        if(stations[n] %in% c(22)){offsetH<- -0.2}
        if(stations[n] %in% c(14)){offsetH<- -0.1}
        if(stations[n] %in% c(12)){offsetH<- 0.1}
         
        ##Add points showing Soundscape metrics
        #Load in appropriate metrics
        setwd(SSDir)
        fileBB<-list.files(pattern='BB_2min')
        BB<-read.csv(fileBB,header=TRUE)
        BB$Date<-strptime(BB$yyyy.mm.ddTHH.MM.SSZ,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
        BB$Date<-as.POSIXct(BB$Date)
        BB<-filter(BB,Date>DriftStart & Date<DriftEnd) #only keep noise data for times after recorder was in the water
        
        fileTOL<-list.files(pattern='TOL_2min')
        TOL<-read.csv(fileTOL,header=TRUE)
        TOL$Date<-strptime(TOL$yyyy.mm.ddTHH.MM.SSZ,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
        TOL$Date<-as.POSIXct(TOL$Date)
        TOL<-filter(TOL,Date>DriftStart & Date<DriftEnd) #only keep noise data for times after recorder was in the water
        
        #Compare Soundscape Start Time with Time in Drift GPS data.frame (data.n.trunc)
        for(e in 1:nrow(BB)){
          TD<-as.numeric(difftime(BB$Date[e],data.n.trunc$dateTime,units="min"))
          GPSind<-which.min(abs(TD))
          BB$lat[e]<-data.n.trunc$lat[GPSind]
          BB$lon[e]<-data.n.trunc$long[GPSind]
          TOL$lat[e]<-data.n.trunc$lat[GPSind]
          TOL$lon[e]<-data.n.trunc$long[GPSind]
        }
        
        #Compute Daily Average
        TOL$Date<-as.Date(TOL$Date,format="%Y-%M-%D")
        TOL63Day<- TOL %>%
          group_by(Date) %>%
          summarize(TOLMed = median(TOL_63),lat=mean(lat),lon=mean(lon))
        
    #Plot
        #BB
    BBfilenameN<-paste(outfilename,'Drift',station.numbers[n],'Broadband.png',sep="_")
    # plotfile<-paste(MapDir,"/","SoundscapeMaps/",Sys.Date(),'/',BBfilenameN,'.png', sep="")
    # png(plotfile,width=6,height=6,units="in",res=300)
    
    BBPlot<-autoplot(bat,geom=c('tile'),coast=TRUE)+
      coord_cartesian(expand = 0)+
      geom_contour(aes(z=z),breaks=c(-100,-500,-1000),color="gray24")+
      scale_fill_gradientn(values = scales::rescale(c(-4000, 0, 300, 2000)),
                           colors = c("midnightblue", "royalblue3", "grey50", "grey80"),name="Depth (m)")+
      new_scale_color()+
      geom_point(data=BB,aes(x=lon,y=lat,color=BB_20.24000))+
      scale_color_viridis(option="B",name=expression(paste('SPL','(dB re 1uPa'^2,')',sep="")))+
      geom_point(data=data.n.trunc,aes(long[1], lat[1]),shape=17,size=3)+
      geom_point(data=data.n.trunc,aes(long[nrow(data.n.trunc)], lat[nrow(data.n.trunc)]),shape=15,size=3)+
      geom_point(data=data.n.trunc,aes(long[1]+offsetH, lat[1]+offsetV),shape=15,size=9,color="lightgray",alpha=1)+
      annotate(geom="text",x=data.n.trunc$long[1]+offsetH,y=data.n.trunc$lat[1]+offsetV,label='7',size=8)+
      xlab('Longitude')+ylab('Latitude')+ggtitle('Broadband Noise 20 - 24,000 Hz')
  ggsave(BBfilenameN,plot=BBPlot,device="png",path=paste(MapDir,"/","SoundscapeMaps/",Sys.Date(),sep=""),width=6,height=6,units=c("in"),dpi=300)
    
     #TOL
    TOLfilenameN<-paste(outfilename,'Drift',station.numbers[n],'TOL.png',sep="_")
   
   TOLPlot<- autoplot(bat,geom=c('tile'),coast=TRUE)+
      coord_cartesian(expand = 0)+
      geom_contour(aes(z=z),breaks=c(-100,-500,-1000),color="gray24")+
      scale_fill_gradientn(values = scales::rescale(c(-4000, 0, 300, 2000)),
                           colors = c("midnightblue", "royalblue3", "grey50", "grey80"),name="Depth (m)")+
      new_scale_color()+
      geom_point(data=TOL,aes(x=lon,y=lat,color=TOL_63))+
      scale_color_viridis(option="B",name=expression(paste('TOL','(dB re 1 uPa)',sep="")))+
      geom_point(data=data.n.trunc,aes(long[1], lat[1]),shape=17,size=3)+
      geom_point(data=data.n.trunc,aes(long[nrow(data.n.trunc)], lat[nrow(data.n.trunc)]),shape=15,size=3)+
      geom_point(data=data.n.trunc,aes(long[1]+offsetH, lat[1]+offsetV),shape=15,size=9,color="lightgray",alpha=1)+
      annotate(geom="text",x=data.n.trunc$long[1]+offsetH,y=data.n.trunc$lat[1]+offsetV,label='7',size=8)+
      xlab('Longitude')+ylab('Latitude')+ggtitle('Third Octave Level @ 63 Hz')
   ggsave(TOLfilenameN,plot=TOLPlot,device="png",path=paste(MapDir,"/","SoundscapeMaps/",Sys.Date(),sep=""),width=6,height=6,units=c("in"),dpi=300)
    
    }
}}}

# #Example call to function 
# #Plot Soundscape Metrics (BB and TOL) on a map of a single drift
# DriftMapSS(outfilename='test',station.numbers = c(7),
#            spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
#                                 "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
#                                 "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
#                                 "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
#                                 "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
#                                 "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv")),
#            MapDir = 'E:/code/CCE Map Data',
#            SSDir='C:/Users/anne.simonis/Documents/Soundscapes/Metrics/Drift7',DriftFile='Drift_FileLookup.csv',lookupfile="spotlookup_US&MX_RETRIEVED.csv"
# )


###Sandbox for PSD metrics ###

# filePSD<-list.files(pattern='PSD_2min')
# PSD<-fread(filePSD)
# PSD$DateTime<-PSD$`yyyy-mm-ddTHH:MM:SSZ`
# PSD<-select(PSD,-`yyyy-mm-ddTHH:MM:SSZ`)
# PSDs<-PSD[,c(1:5000,23982)]
# 
# PSDs<-melt(PSDs,id.vars=('DateTime'))
# PSDs$Day<-yday(PSDs$DateTime)
# PSDs$Date<-as.Date(PSDs$DateTime,format="%Y-%m-%d")
# PSDs<-filter(PSDs,Date>DriftStart & Date<DriftEnd) #only keep noise data for times after recorder was in the water
# 
# #Summarize by day 
# PSDDay<- PSDs %>%
#   group_by(Date,variable) %>%
#   summarize(PSDMed = median(value),PSD10=quantile(value,probs=c(.10)),PSD90=quantile(value,probs=c(.90)))
# 
# PSDDay$variable<-as.character(PSDDay$variable)
# PSDDay$Freq<-as.numeric(substr(PSDDay$variable,5,nchar(PSDDay$variable))) 
# 

#     
#     #determine unique days
# DriftDays<-unique(PSDDay$Date)
# for(d in 1:length(DriftDays)){
#   #find position at noon on each day
#   NoonToday<-as.POSIXct(paste(DriftDays[1],'12:00:00'),format = "%Y-%m-%d %H:%M:%S",tz="UTC")
#   GPSind<-which(data.n.trunc$dateTime>NoonToday)
#   
#   #find nearest GPS index to that time
#   TD<-as.numeric(difftime(BB$Date[e],data.n.trunc$dateTime,units="min"))
#   GPSind<-which.min(abs(TD))
#   
#   #assign position to each point
#   today<-which(PSDDay$Date ==DriftDays[d])}
#   
