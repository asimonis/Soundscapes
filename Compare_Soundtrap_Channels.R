#Plot soundscape metrics: BB, TOL, PSD
#Compare different hydrophones on CCES Drifts

#Anne Simonis 17March2021

#Required Packages
library(lubridate)
library(tidyverse)
library(grid)
library(gridExtra)
library(scales)
library(data.table)
library(reshape)


#Define path for hydrophone data 
savedir<-'E:/Soundscapes/Plots/CCES22_2Ch'
SSDir<-c('E:/Soundscapes/Drift_22_48kHz','E:/Soundscapes/Drift_22_CH2_48kHz')
Dep<-c('CCES_22','CCES_22_CH2')
Ch<-c('HTI-92-WB','HTI-96-min')
outfile<-c('CCES_22_Ch1.rda','CCES_22_Ch2.rda')
           
#Compare Loud and Quiet Periods
QuietStart<-as.POSIXct("2018-11-16 19:00",tz="UTC")
QuietStop<-as.POSIXct("2018-11-16 20:00",tz="UTC")
LoudStart<-as.POSIXct("2018-11-13 16:00",tz="UTC")
LoudStop<-as.POSIXct("2018-11-13 17:00",tz="UTC")

#Define Plot Filenames
TOLbox<-'CCES22_2Ch_TOL.pdf'
TOL2min<-'CCES22_2Ch_TOL_2min.pdf'
TOLSepBands<-'CCES22_2Ch_TOL_SepBands.pdf'
BBplot<-'CCES22_2Ch_BB.pdf'
PSDplot<-'CCES22_2Ch_PSD_2min.png'
TOL8k_Deployment<-c('CCES22_Ch1_TOL8k_Deployment.pdf','CCES22_Ch2_TOL8k_Deployment.pdf')
TOL_p1_Deployment<-'CCES22_TOL_p1.pdf'

dir.create(savedir)
for(h in 1:length(Dep)){

fileTOL<-paste(Dep[h],'_1Hz_1s_TOL_2min.csv',sep="")
fileTOLp1<-paste(Dep[h],'_1Hz_1s_TOL_pct01_2min.csv',sep="")
fileBB<-paste(Dep[h],'_1Hz_1s_BB_2min.csv',sep="")
filePSD<-paste(Dep[h],'_1Hz_1s_PSD_2min.csv',sep="")

#Load Soundscape Metrics
setwd(SSDir[h])
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
TOL8000<-filter(TOL,variable=='TOL_8000')

#TOL 1%
TOLp1<-read.csv(fileTOLp1,header = TRUE)
TOLp1$DateTime<-strptime(TOLp1$yyyy.mm.ddTHH.MM.SSZ,format="%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
TOLp1$DateTime<-as.POSIXct(TOLp1$Date)
TOLp1<-select(TOLp1, -yyyy.mm.ddTHH.MM.SSZ)
TOLp1<-melt(TOLp1,id.vars=c('DateTime'))
TOLp1$Month<-month(TOLp1$DateTime)
TOLp1$Date<-as.Date(TOLp1$DateTime,format='%Y-%m-%d')
TOLp1$Ch<-Ch[h]

if(h==1){TOLp1_1<-TOLp1}else{
  TOLp2_1<-TOLp1
  TOLp1<-rbind(TOlp1_1,TOLp1_2)}

PSD<-fread(filePSD)
PSD$DateTime<-PSD$`yyyy-mm-ddTHH:MM:SSZ`
PSD<-select(PSD,-`yyyy-mm-ddTHH:MM:SSZ`)

BBquiet<-filter(BB,DateTime>QuietStart & DateTime<QuietStop)
BBloud<-filter(BB,DateTime>LoudStart & DateTime<LoudStop)

TOLquiet<-filter(TOL,DateTime>QuietStart & DateTime<QuietStop)
TOLloud<-filter(TOL,DateTime>LoudStart & DateTime<LoudStop)

PSDquiet<-filter(PSD,DateTime>QuietStart & DateTime<QuietStop)
PSDloud<-filter(PSD,DateTime>LoudStart & DateTime<LoudStop)

PSDquiet<-melt(PSDquiet,id.vars=('DateTime'))
PSDquiet$Date<-as.Date(PSDquiet$DateTime,format="%Y-%m-%d")
PSDloud<-melt(PSDloud,id.vars=('DateTime'))
PSDloud$Date<-as.Date(PSDloud$DateTime,format="%Y-%m-%d")

BBquiet$Ch<-Ch[h]
BBloud$Ch<-Ch[h]

TOLquiet$Ch<-Ch[h]
TOLloud$Ch<-Ch[h]

PSDquiet$Ch<-Ch[h]
PSDloud$Ch<-Ch[h]

setwd(savedir)

pdf(TOL8k_Deployment[h],10,4)
ggplot(TOL8000,aes(DateTime,value))+geom_point()+geom_smooth(colour='yellow')+
  theme_bw()+labs(title=paste(' Ch ',h,' TOL Band centered at 8kHz',sep=""),x='Date',
                  y=expression(paste('dB re 1uPa'^2,sep="")))+
  geom_vline(xintercept=QuietStart,col="cyan",size=.5)+
  geom_vline(xintercept=QuietStop,col="cyan",size=.5)+
  geom_text(aes(x=(QuietStart-hours(10)),y=90,label='Quiet'),col="cyan")+
  geom_vline(xintercept=LoudStart,col="orange",size=.5)+
  geom_vline(xintercept=LoudStop,col="orange",size=.5)+
  geom_text(aes(x=(LoudStart-hours(10)),y=90,label='Loud'),col="orange")+
  scale_x_datetime(breaks="2 days")
dev.off()

save(BBquiet,BBloud,TOLquiet,TOLloud,PSDquiet,PSDloud,file=outfile[h])

}

#Combine datasets from two channels

load(outfile[2])
BBquiet2<-BBquiet
BBloud2<-BBloud
TOLquiet2<-TOLquiet
TOLloud2<-TOLloud
PSDquiet2<-PSDquiet
PSDloud2<-PSDloud

load(outfile[1])

BBquietAll<-rbind(BBquiet,BBquiet2)
BBloudAll<-rbind(BBloud,BBloud2)
TOLquietAll<-rbind(TOLquiet,TOLquiet2)
TOLloudAll<-rbind(TOLloud,TOLloud2)
PSDquietAll<-rbind(PSDquiet,PSDquiet2)
PSDloudAll<-rbind(PSDloud,PSDloud2)

BBquietAll$Category<-factor('quiet',levels=c('quiet','loud'))
BBloudAll$Category<-factor('loud',levels=c('quiet','loud'))
TOLquietAll$Category<-factor('quiet',levels=c('quiet','loud'))
TOLloudAll$Category<-factor('loud',levels=c('quiet','loud'))
PSDquietAll$Category<-factor('quiet',levels=c('quiet','loud'))
PSDloudAll$Category<-factor('loud',levels=c('quiet','loud'))

BBAll<-rbind(BBquietAll,BBloudAll)
TOLAll<-rbind(TOLquietAll,TOLloudAll)
PSDAll<-rbind(PSDquietAll,PSDloudAll)

BBAll$Time<-strftime(BBAll$DateTime,format="%H:%M")

TOLAll$variable<-gsub('TOL_',"",TOLAll$variable)
TOLAll$variable<-as.numeric(TOLAll$variable)
TOLselect<-filter(TOLAll,variable %in% c(63,125,500,1000,2000,4000,8000,16000))
TOLselect$Time<-strftime(TOLselect$DateTime,format="%H:%M")

PSDAll$variable<-as.character(PSDAll$variable)
PSDAll$Freq<-as.numeric(substr(PSDAll$variable,5,nchar(PSDAll$variable))) 

#PLOTS
breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

TOLSel<-filter(TOLp1,variable %in% c('TOL_125','TOL_1000','TOL_2000','TOL_4000','TOL_6300','TOL_8000','TOL_16000','TOL_20000'))


pdf(TOL_p1_Deployment,8.5,11)
ggplot(TOLSel,aes(DateTime,value))+facet_grid(variable~Ch)+scale_x_datetime(breaks="5 days",labels=date_format('%b-%d'))+
  geom_point(size=.5)+ggtitle('Select TOL 1st percentiles for each hydrophone')+theme_bw()
dev.off()

testsetwd(savedir)
pdf(TOLbox,11,8.5)
ggplot(TOLAll,aes(factor(variable),value,col=factor(Ch)))+geom_boxplot()+
  facet_grid(.~Category,scales="free")+xlab('Center of TOL Band (Hz)')+
  labs(col="Hydrophone")+theme_bw()+
  ylab('dB re 1 uPa')+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

pdf(TOL2min,11,8.5)
ggplot(TOLAll,aes(variable,value,col=factor(Ch)))+geom_point()+
  facet_grid(factor(DateTime)~Category,scales="free")+xlab('Center of TOL Band (Hz)')+
  ylab('dB re 1 uPa')+geom_line()+
  scale_x_log10(breaks = breaks, minor_breaks = minor_breaks)+
  labs(col="Hydrophone")+ ggtitle('TOL during quiet and loud periods')
dev.off()

BBTime<-ggplot(BBAll,aes(Time,BB_20.24000,col=Ch))+geom_point()+
  facet_grid(Category~.)+theme_bw()+theme(legend.position="none")+
  scale_y_continuous(breaks=seq(90,105,2))+
  ylab(expression(paste('dB re 1uPa'^2,sep="")))+ggtitle('Broadband Noise 20-24,000Hz')

BBBox<-ggplot(BBAll,aes(Category,BB_20.24000,col=Ch))+geom_boxplot()+
  scale_y_continuous(breaks=seq(90,105,2))+
  theme_bw()+labs(x='Date',y=expression(paste('dB re 1uPa'^2,sep="")),col='Hydrophone')

pdf(BBplot,11,8.5)
grid.arrange(BBTime,BBBox,nrow=1)
dev.off()

png(PSDplot,width=11,height=8.5,units="in",res=300)
ggplot(PSDAll,aes(Freq,value,col=factor(Ch)))+facet_grid(factor(DateTime)~Category)+
  geom_point(size=.1)+geom_line()+scale_x_log10(breaks = breaks, minor_breaks = minor_breaks)+
  labs(col="Hydrophone",x='Frequency (Hz)',y=expression(paste('dB re 1uPa'^2,'/Hz',sep="")))+
  ggtitle("PSD of 2 min files in quiet and loud periods")
dev.off()

pdf(TOLSepBands,9,11)
ggplot(TOLselect,aes(Time,value,color=factor(Ch)))+geom_point()+
  facet_grid(variable~Category,scales="free")+theme_bw()+
  ylab('TOL (dB re 1 uPa)')+labs(col="Hydrophone")+
  ggtitle('TOL during quiet and loud periods')
dev.off()
