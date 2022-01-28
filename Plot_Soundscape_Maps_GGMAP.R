
library(tibble)
library(ggnewscale)
library(viridis)

outfilename = 'test'
station.numbers=c(7)

spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                     "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                     "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                     "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                     "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                     "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv"))
DriftFile='Drift_FileLookup.csv'
lookupfile="spotlookup_US&MX_RETRIEVED.csv"
figtitle='Test'
SSmetric<-list(c('BB','TOL'))
MapDir='E:/code/CCE Map Data'
SSDir='C:/Users/anne.simonis/Documents/Soundscapes/Metrics/Drift7'

#Extract bathymetry data from NOAA (saves local file to speed future performance)
bat<-getNOAA.bathy(lon1,lon2,lat1,lat2,resolution=1,keep=TRUE)

autoplot(bat,geom=c('tile'),coast=TRUE)+
  coord_cartesian(expand = 0)+
  geom_contour(aes(z=z),breaks=c(-100,-500,-1000),color="gray24")+
  scale_fill_gradientn(values = scales::rescale(c(-4000, 0, 300, 2000)),
                       colors = c("midnightblue", "royalblue3", "grey50", "grey80"),name="Depth (m)")+
  new_scale_color()+
  geom_point(data=BB,aes(x=lon,y=lat,color=BB_20.24000))+
  scale_color_viridis(option="B",name=expression(paste('SPL','(dB re 1uPa'^2,')',sep="")))+
  geom_point(data=data.n.trunc,aes(long[1], lat[1]),shape=17,size=3)+
  geom_point(data=data.n.trunc,aes(long[nrow(data.n.trunc)], lat[nrow(data.n.trunc)]),shape=15,size=3)+
  annotate(geom="text",x=data.n.trunc$long[1]+offsetH,y=data.n.trunc$lat[1]+offsetV,label='7',size=8)
  
xlab('Longitude')+ylab('Latitude')+ggtitle('Broadband Noise 20 - 24,000 Hz')

  
#Load in SS data
setwd(SSDir)
fileBB<-list.files(pattern='BB_2min')
BB<-read.csv(fileBB,header=TRUE)
BB$Date<-strptime(BB$yyyy.mm.ddTHH.MM.SSZ,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
BB$Date<-as.POSIXct(BB$Date)
BB<-filter(BB,Date>DriftStart & Date<DriftEnd) #only keep noise data for times after recorder was in the water

for(e in 1:nrow(BB)){
  TD<-as.numeric(difftime(BB$Date[e],data.n.trunc$dateTime,units="min"))
  GPSind<-which.min(abs(TD))
  BB$lat[e]<-data.n.trunc$lat[GPSind]
  BB$long[e]<-data.n.trunc$long[GPSind]
  
}

  
BB$lon<-BB$long
TOL63Day$lon<-TOL63Day$long


