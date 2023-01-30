##PLOT PSD OF ALL DRIFTS TOGETHER






SSDir<-'E:/Soundscapes/CCES_07'
savedir<-'E:/Soundscapes/Plots'
Dep<-c('CCES_07','CCES_10','CCES_12','CCES_13','CCES_14','CCES_16','CCES_19')

#Load Soundscape Metrics


for(D in 1:length(Dep)){
  SSDir<-paste('E:/Soundscapes/',Dep[D],sep="")
  setwd(SSDir)
  filePSD<-paste(Dep[D],'_1Hz_1s_PSD_2min.csv',sep="")

PSD<-fread(filePSD)
PSD$DateTime<-PSD$`yyyy-mm-ddTHH:MM:SSZ`
PSD<-select(PSD,-`yyyy-mm-ddTHH:MM:SSZ`)
PSD<-PSD[10:(nrow(PSD)-5),] #Remove first and last few observations

PSDs<-PSD[,c(1:980,23982)]
PSDs<-melt(PSDs,id.vars=('DateTime'))

PSDMed<- PSDs %>%
  group_by(variable) %>%
  summarize(PSDMed = median(value),PSD10=quantile(value,probs=c(.10)),PSD90=quantile(value,probs=c(.90)))

PSDMed$variable<-as.character(PSDMed$variable)
PSDMed$Freq<-as.numeric(substr(PSDMed$variable,5,nchar(PSDMed$variable))) 
PSDMed$Drift<-Dep[D]
if(D==1){PSDAll<-PSDMed}else{
PSDAll<-rbind(PSDAll,PSDMed)}
}

beep()

setwd(savedir)
breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

PlotPSDDrifts<-ggplot(PSDAll,aes(Freq,PSDMed,col=factor(Drift)))+
  geom_line()+geom_ribbon(aes(ymin=PSD10,ymax=PSD90,fill=factor(Drift)),linetype=0,alpha=0.1)+
  theme_bw()+scale_x_log10(breaks = breaks, minor_breaks = minor_breaks)+ annotation_logticks(sides="b") +
  labs(title='Median Power Spectral Density',x='Frequency (Hz)',
       y=expression(paste('dB re 1uPa'^2,'/Hz',sep="")),fill = 'Drift',color='Drift')

pdf('PSD_AllDrifts.pdf',8,6)
PlotPSDDrifts
dev.off()

