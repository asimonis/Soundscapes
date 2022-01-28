#Compare Soundscape Metrics from Pamguard and SanctSound (Matlab based)
#Anne Simonis
#11/24/2020

#Required libraries
devtools::install_github('taikisan21/PamBinaries',force=TRUE)

library(lubridate)
library(dplyr)
library(PamBinaries)
library(tictoc)

#Pamguard Methods
#Choose folder where noise binaries are stored
# binaryDir<-'E:/Soundscape Analysis/Binaries/PP01_Take2'

binaryDir<-'E:/Soundscape Analysis/Binaries/PP01_NoOffset'
savedir<-'E:/Soundscape Analysis/Binaries/PP01_Soundscape/Dataframes'
DepName<-'PP01'

setwd(binaryDir)
binaryList<-list.files(path=binaryDir,pattern =glob2rx('*Noise*.pgdf*'),recursive=TRUE,include.dirs=TRUE)
NoiseAll<-data.frame()


tic()
for(b in 1:length(binaryList)){
  binaryData <- loadPamguardBinaryFile(binaryList[b],convertDate=TRUE) #read date in POSIXct format
  if(length(binaryData$data)<=0){next}
  binDF<-pbToDf(binaryData)
  
  #combine Mean and Peak of each band into single dataframe
  NoiseDF<-select(binDF,noiseMean,noisePeak,octaveBand,nMeasures,date)

  #combine all binaries into single dataframe
  if(b==1){
    NoiseAll<-NoiseDF}
  else{
    NoiseAll<-rbind(NoiseAll,NoiseDF)
    }
}
toc()

TOL_1000<-filter(NoiseAll,octaveBand==21)
TOL_100<-filter(NoiseAll,octaveBand==11)
# PG_BB<-filter(NoiseAll,octaveBand==max(octaveBand))

#subtract 2 min from PAMGuard TOL metrics
TOL_1000$date<-TOL_1000$date -minutes(2)
TOL_100$date<-TOL_100$date -minutes(2)
# PG_BB$date<-PG_BB$date -minutes(2)

# #SanctSound Metrics
# PP01_TOL$Date_Time<-strptime(PP01_TOL$yyyy.mm.ddTHH.MM.SSZ,format="%Y-%m-%dT%H:%M:%OSZ")
setwd('E:/Soundscape Analysis/PP01/metrics')
PP01_TOL2min<-read.csv('PP01_TOL_mean_2min.csv',header = TRUE)

PP01_TOL1000<-select(PP01_TOL2min,TOL_1000,yyyy.mm.ddTHH.MM.SSZ)
PP01_TOL1000$Date<-strptime(PP01_TOL1000$yyyy.mm.ddTHH.MM.SSZ,format="%Y-%m-%dT%H:%M:%OSZ")
PP01_TOL1000$Date<-as.POSIXct(PP01_TOL1000$Date)

PP01_TOL100<-select(PP01_TOL2min,TOL_100,yyyy.mm.ddTHH.MM.SSZ)
PP01_TOL100$Date<-strptime(PP01_TOL100$yyyy.mm.ddTHH.MM.SSZ,format="%Y-%m-%dT%H:%M:%OSZ")
PP01_TOL100$Date<-as.POSIXct(PP01_TOL100$Date)


#Get rid of measurements on the 0:08, 0:38) because PAMGuard didn't save those
eights<-seq(from=5,to=nrow(PP01_TOL100),by=5)
PP01_TOL1000$Date[eights]<-NA
PP01_TOL1000<-na.omit(PP01_TOL1000)

PP01_TOL100$Date[eights]<-NA
PP01_TOL100<-na.omit(PP01_TOL100)

#1000 Hz
TOL1000Diff<-PP01_TOL1000$TOL_1000[1:nrow(TOL_1000)] - TOL_1000$noiseMean
hist(TOL1000Diff,50,main='Delta of TOL 1000 Hz in PAMGuard and Sanctsound Metrics')
plot(1:nrow(TOL_100),TOL1000Diff,pch='.')

#100 Hz
TOL100Diff<-PP01_TOL100$TOL_100[1:nrow(TOL_100)] - TOL_100$noiseMean
hist(TOL100Diff,100,main='Delta of TOL 100 Hz in PAMGuard and Sanctsound Metrics')
plot(1:nrow(TOL_100),TOL100Diff,pch='.')




#Plot metric differences 
plot(PP01_TOL1000$TOL_1000[1:100]-TOL_1000$noiseMean[1:100],pch='*')

#Save each in a csv file
setwd('E:/Soundscape Analysis/Dataframes')
write.csv(TOL_1000,file='PP01_TOL1000_PAMGuard.csv')
write.csv(PP01_TOL1000,file='PP01_TOL1000_SanctSound.csv')





# setwd('E:/Soundscape Analysis/SB02/metrics/SB02_LTSAS_01-27')
# SB02_BB<-read.csv('SB02_BB_10min.csv',header = TRUE)
# SB02_PSD10min<-read.csv('SB02_PSD_10min.csv',header = TRUE,nrows=3)
# SB02_TOL<-read.csv('SB02_TOL_10min.csv',header = TRUE)
# 
# setwd('E:/Soundscape Analysis/SB02/metrics/test')
# SB02_BB177<-read.csv('SB02_BB_10min.csv',header = TRUE)
# SB02_TOL177<-read.csv('SB02_TOL_10min.csv',header = TRUE)
# 
# setwd('E:/Soundscape Analysis/PP01/metrics/')
# PP01_PSD10min<-read.csv('PP01_PSD_10min.csv', header=TRUE,nrows=3)


#2 plots
par(mfrow=c(2,1))
plot(seq(2:200),PP01_PSD10min[1,2:200],pch='.')
lines(seq(2:200),PP01_PSD10min[1,2:200])

plot(seq(2:200),SB02_PSD10min[1,2:200],pch='.')
lines(seq(2:200),SB02_PSD10min[1,2:200])

plot(seq(2:200),SB02_PSD177[1,2:200],pch='.')
lines(seq(2:200),SB02_PSD177[1,2:200])

par(mfrow=c(2,1))

plot(PP01_TOL$Date_Time[1:100],PP01_TOL$TOL_1000[1:100],pch=".")
plot(TOL_1000$date[1:100],TOL_1000$noiseMean[1:100],pch=".")
