#Plot soundscape metrics: BB, TOL, PSD
#Summarize data by day and month

#Anne Simonis 11March2021

#Required Packages
library(lubridate)
library(tidyverse)
library(grid)
library(gridExtra)
library(scales)
library(data.table)
library(reshape)
library(viridis)

SSDir<-'E:/Analysis/ADRIFT/Soundscape/metrics/ADRIFT_003'
savedir<-'E:/Analysis/ADRIFT/Soundscape/metrics/ADRIFT_003/Plots'
Dep<-'ADRIFT_003'
fileTOL<-paste(Dep,'_TOL_1h.csv',sep="")
fileBB<-paste(Dep,'_BB_1h.csv',sep="")
filePSD<-paste(Dep,'_PSD_1h.csv',sep="")

PDFname<-paste(Dep,'.pdf',sep="")
PDFtitle<-paste(Dep,sep="")

#Load Soundscape Metrics
setwd(SSDir)
BB<-read.csv(fileBB,header=TRUE)
BB$DateTime<-strptime(BB$yyyy.mm.ddTHH.MM.SSZ,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
BB$DateTime<-as.POSIXct(BB$DateTime)
BB$Hour<-hour(BB$DateTime)
BB$Date<-as.Date(BB$Date)

TOL<-read.csv(fileTOL,header = TRUE)
TOL$DateTime<-strptime(TOL$yyyy.mm.ddTHH.MM.SSZ,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
TOL$DateTime<-as.POSIXct(TOL$Date)
TOL<-select(TOL, -yyyy.mm.ddTHH.MM.SSZ)
TOL<-melt(TOL,id.vars=c('DateTime'))
TOL$Month<-month(TOL$DateTime)
TOL$Date<-as.Date(TOL$DateTime,format='%Y-%m-%d')


TOLsub<-TOL %>%
mutate(interval=floor_date(DateTime,unit="hour")+minutes(floor(minute(DateTime)/10)*10), Hour=hour(DateTime)) %>%
filter(variable %in% c('TOL_63','TOL_125','TOL_1000','TOL_2000'))
  

PSD<-fread(filePSD)
# PSD$DateTime<-strptime(PSD$yyyy.mm.ddTHH.MM.SSZ,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
PSD$DateTime<-PSD$`yyyy-mm-ddTHH:MM:SSZ`
PSD<-select(PSD,-`yyyy-mm-ddTHH:MM:SSZ`)
PSDs<-PSD[,c(1:980,23982)]
PSDs<-melt(PSDs,id.vars=('DateTime'))
PSDs$Month<-month(PSDs$DateTime)
PSDs$Day<-yday(PSDs$DateTime)
PSDs$Date<-as.Date(PSDs$DateTime,format="%Y-%m-%d")

#Summarize by day 
TOLDay<- TOL %>%
  group_by(Date,variable) %>%
  summarize(TOLMed = median(value),TOL10=quantile(value,probs=c(.10)),TOL90=quantile(value,probs=c(.90)))

PSDDay<- PSDs %>%
  group_by(Date,variable) %>%
  summarize(PSDMed = median(value),PSD10=quantile(value,probs=c(.10)),PSD90=quantile(value,probs=c(.90)))

PSDDay$variable<-as.character(PSDDay$variable)
PSDDay$Freq<-as.numeric(substr(PSDDay$variable,5,nchar(PSDDay$variable))) 

#Summarize by month
TOLMonth<- TOL %>%
  group_by(Month,variable) %>%
  summarize(TOLMed = median(value),TOL10=quantile(value,probs=c(.10)),TOL90=quantile(value,probs=c(.90)))

TOLMonth$variable<-as.character(TOLMonth$variable)
TOLMonth$CenterTOL<-as.numeric(substr(TOLMonth$variable,5,nchar(TOLMonth$variable)))  

PSDMonth<- PSDs %>%
  group_by(Month,variable) %>%
  summarize(PSDMed = median(value),PSD10=quantile(value,probs=c(.10)),PSD90=quantile(value,probs=c(.90)))

PSDMonth$variable<-as.character(PSDMonth$variable)
PSDMonth$Freq<-as.numeric(substr(PSDMonth$variable,5,nchar(PSDMonth$variable))) 


SubTOL<-filter(TOLDay,variable %in% c('TOL_63','TOL_125','TOL_1000','TOL_2000'))
TOL1k<-filter(TOLsub,variable=="TOL_1000")

#PLOTS
  ggplot(TOLsub,aes(Hour,Date,fill=value))+geom_tile()+
  scale_fill_viridis(name='dB re 1 uPa')+facet_grid(variable~.)
 
   
TOLHeat<-ggplot(TOL1k,aes(Hour,Date,fill=value))+geom_tile()+
  scale_fill_viridis(name='dB re 1 uPa')+ggtitle("Drift 19: Third Octave Level @ 1 kHz (median)")

PlotTOLDay<-ggplot(SubTOL,aes(Date,TOLMed,col=variable))+geom_line()+
  theme_bw()+ggtitle('Median Daily TOL Bands')+
  ylab('dB re 1 uPa')+theme(legend.title = element_blank())+
  scale_color_discrete(labels = c("63 Hz", "125 Hz", "1 kHz", "20 kHz"))
  
# PlotTOLMonth<-ggplot(TOLMonth,aes(CenterTOL,TOLMed,col=factor(Month)))+
#   geom_line()+geom_point()+geom_ribbon(aes(ymin=TOL10,ymax=TOL90,fill=factor(Month)),linetype=0,alpha=0.1)+
#   theme_bw()+ggtitle('TOL')+ylab('Spectral probability density: Third Octave Noise Bands')

breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

PlotPSDMonth<-ggplot(PSDMonth,aes(Freq,PSDMed,col=factor(Month)))+
  geom_line()+geom_ribbon(aes(ymin=PSD10,ymax=PSD90,fill=factor(Month)),linetype=0,alpha=0.1)+
  theme_bw()+scale_x_log10(breaks = breaks, minor_breaks = minor_breaks)+ annotation_logticks(sides="b") +theme(legend.title=element_blank())+
  labs(title='Median Monthly Power Spectral Density',x='Frequency (Hz)',
       y=expression(paste('dB re 1uPa'^2,'/Hz',sep="")),fill = 'Month',color="Month")

PlotPSDDay<-ggplot(PSDDay,aes(Freq,PSDMed,col=factor(Date)))+
  geom_line()+geom_ribbon(aes(ymin=PSD10,ymax=PSD90,fill=factor(Date)),linetype=0,alpha=0.1)+
  theme_bw()+scale_x_log10(breaks = breaks, minor_breaks = minor_breaks)+ 
  annotation_logticks(sides="b") +theme(legend.position = "none")+
  labs(title='Median Daily Power Spectral Density',x='Frequency (Hz)',
       y=expression(paste('dB re 1uPa'^2,'/Hz',sep="")),fill = 'Date',color="Date")

PlotBB<-ggplot(BB,aes(DateTime,BB_20.24000))+geom_hex(aes(colour= ..count..))+geom_smooth(colour='yellow')+
  theme_bw()+labs(title='Median Broadband Noise 20-24,000Hz',x='Date',
                  y=expression(paste('dB re 1uPa'^2,sep="")))


setwd(savedir)
pdf(PDFname,8.5,11)
grid.arrange(PlotBB,PlotTOLDay,PlotPSDMonth,PlotPSDDay,nrow=4)
dev.off()

# Plot heatmap of individual TOL band
# pdf('CCES_19_TOLHeatmap.pdf',8,8)
# TOLHeat
# dev.off()

#Zoom in to see daily patterns
# ggplot(BB[1:400,],aes(DateTime,BB_20.24000))+geom_point()+geom_smooth()+
# theme_bw()+labs(title='Median Broadband Noise 20-24,000Hz',
#                 x='Date',y=expression(paste('SPL (dB re 1uPa'^2,')',sep="")))+
#   scale_x_datetime(breaks="12 hours",labels=date_format("%b %d - %H:%M"))

# ##Combine plots with separate legends
# library(patchwork)
# combined<-PlotPSDMonth + PlotTOLDay & theme(legend.position="bottom")

# #Daily PSD colored by week
# PSDDay$Week<-week(PSDDay$Date)
# PSDDay %>% group_by(Week)
# ggplot(PSDDay,aes(Freq,PSDMed,col=factor(Week)))+geom_line(alpha=0.4,size=0.1)

