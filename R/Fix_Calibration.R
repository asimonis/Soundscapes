#Code to correct Soundscape metrics calculated with incorrect calibration value
#
#MATLAB CODE: SOUNDSCAPE REMORA
#REMORA.sm.cmpt.caldb is the user-defined single calibration value

# % conversion of sensitivity to counts
# REMORA.sm.cmpt.counts= 2^15; %16 bit; +/- bit range
# % calibration value in dB re counts
# dbcounts= abs(20*log10(REMORA.sm.cmpt.counts)-REMORA.sm.cmpt.caldb);
# % adjust for single value
# REMORA.sm.cmpt.pre.psd = REMORA.sm.cmpt.pre.psd + dbcounts;

#For cal value "-1.8", the adjustment=92.1090 dB.
#For cal value "177" (note positive value), the adjustment=86.6910
#For cal value "178" (note positive value), the adjustment=87.6910
#difference of 5.417997 dB, or +1 

#Ocean Instruments published wrong calibration value for some ST640s
#Instead of -5.7, it's -4. 
#Old calibration = abs(-155.1-5.7)=160.7
#Correction calibration = abs(-155.1-4)=159.1

#ADRIFT 19, 25: CorrectionValue = -1.7
#ADRIFT 13,14,15,16,20,21,22,23,24,26,27,28,34,67: CorrectionValue= -1.6
library(data.table)
library(tictoc)
library(beepr)

CorrectionValue = -1.6

origdir<-'E:/Analysis/ADRIFT/Soundscape/metrics/Incorrect Calibration/ADRIFT_013'
savedir<-'E:/Analysis/ADRIFT/Soundscape/metrics/ADRIFT_013'

dir.create(savedir)

#PSD files
PSDfiles<-list.files(origdir,pattern="*_PSD")
tic()
for(p in 1:length(PSDfiles)){
setwd(origdir)
Data_PSD<-fread(file=PSDfiles[p])
Data_PSD[,2:nrow(Data_PSD)]<-Data_PSD[,2:nrow(Data_PSD)]+CorrectionValue

setwd(savedir)
fwrite(Data_PSD,file=PSDfiles[p])
remove(Data_PSD)
gc() #Garbage collector (clean up memory)
}

setwd(origdir)
#Broadband
BBFiles<-list.files(origdir,pattern="*_BB")
for(b in 1:length(BBFiles)){
  BBMetric<-read.csv(BBFiles[b],header = TRUE)
  BBMetric[,2]<-BBMetric[,2]+CorrectionValue
  write.csv(BBMetric,file=paste0(savedir,'/',BBFiles[b]),row.names = FALSE)
  }

#TOL
TOLFiles<-list.files(origdir,pattern="*_TOL")
for(t in 1:length(TOLFiles)){
  TOLMetric<-read.csv(TOLFiles[t],header = TRUE)
  TOLMetric[,2:ncol(TOLMetric)]<-TOLMetric[,2:ncol(TOLMetric)]+CorrectionValue
  write.csv(TOLMetric,file=paste0(savedir,'/',TOLFiles[t]),row.names = FALSE)
}

#OL
OLFiles<-list.files(origdir,pattern="*_OL")
for(o in 1:length(OLFiles)){
  OLMetric<-read.csv(OLFiles[o],header = TRUE)
  OLMetric[,2:ncol(OLMetric)]<-OLMetric[,2:ncol(OLMetric)]+CorrectionValue
  write.csv(OLMetric,file=paste0(savedir,'/',OLFiles[o]),row.names = FALSE)
}
toc()
beep(4)