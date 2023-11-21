#Required Packages
library(tidyverse)
library(viridis)
library(fields)
library(PAMpal)
library(ggplot2)

#Load in soundscape metrics & GPS
SSDir<-'H:/Soundscape/metrics'
Dep<-paste0('ADRIFT_','0',c(46:48,51:52))
DepFiles<-file.path(SSDir,Dep,paste0(Dep,'_TOL_2min.csv'))

GPSDir<-'C:/Users/anne.simonis/Documents/ADRIFT/GPS'
GPSFiles<-file.path(GPSDir,paste0(Dep,'_GPS.csv'))


#Read in TOL metrics for single drift to check autocorrelation (daily? crepuscular?)
  TOL<-read.csv(DepFiles[1])

  TOL$DateTime<-TOL[,1]
  TOL$UTC<-as.POSIXct(TOL$DateTime,format='%Y-%m-%dT%H:%M:%OS',tz='UTC')

  ggplot(TOL,aes(UTC,TOL_200))+geom_point()+scale_x_datetime(breaks='1 day')
  acf(TOL$TOL_1000,lag.max = 800)

  #Calculate hourly average for 2 minute files (median metric)
  TOL<-TOL%>%
    group_by(UTC=floor_date(UTC, '1 hour')) %>% 
    summarize(TOL_200M=mean(TOL_200),
              TOL_1000M=mean(TOL_1000))
  
  #Check ACF of individual drifts (hourly timescale)
  acf(TOL$TOL_200M,lag.max = 30)
  acf(TOL$TOL_1000M,lag.max = 30)
  
  #combine TOL values from multiple drifts, add GPS
  for(D in 1:length(Dep)){
  TOL<-read.csv(DepFiles[D])
  TOL$DriftName<-Dep[D]
  TOL$DateTime<-TOL[,1]
  TOL$UTC<-as.POSIXct(TOL$DateTime,format='%Y-%m-%dT%H:%M:%OS',tz='UTC')
  TOL<-select(TOL,UTC,DriftName,TOL_160,TOL_200,TOL_1000)
   
  GPS<-read.csv(GPSFiles[D])
  GPS$UTC<-as.POSIXct(GPS$UTC,format='%Y-%m-%d %H:%M:%S',tz='UTC')
  GPS<-select(GPS,Latitude,Longitude,UTC,DriftName)
  
  TOL<-PAMpal::timeJoin(TOL,GPS,thresh = 1800)
  
  TOL<-TOL%>%
    group_by(UTC=floor_date(UTC, '1 hour')) %>%
    summarize(TOL_200M=mean(TOL_200),
              TOL_1000M=mean(TOL_1000),
              Latitude=mean(Latitude),Longitude=mean(Longitude))
  
  if(D==1){TOLAll<-TOL}else{
  TOLAll<-rbind(TOL,TOLAll)}
  }
  TOLAll<-na.omit(TOLAll)
  
  ggplot(TOLAll,aes(Latitude,Longitude))+geom_point()
  
  #Create sound maps with fields package
  quilt.plot(TOLAll$Longitude,TOLAll$Latitude,TOLAll$TOL_200M)
  quilt.plot(TOLAll$Longitude,TOLAll$Latitude,TOLAll$TOL_1000M)
  
  loc<-cbind(TOLAll$Longitude,TOLAll$Latitude) #locations
  obj<-spatialProcess(loc,TOLAll$TOL_1000M,Distance = "rdist.earth",
                      cov.args = list(Covariance ="Exponential") )
  surface(obj)
  
  