#Create clips for Citizen Science Project 
#Anne Simonis
#6/1/2022

# 1. Extract times from directory of wav files
# 2. Create dataframe with start, end, id
# 3. Generate clips 
# 4. Will need to decimate clips in separate step

#load packages
library(PAMpal)
library(tuneR)
library(lubridate)
source('C:/Users/anne.simonis.NMFS/Documents/GitHub/Soundscapes/getClipFromDf.R')

WavDir<-'D:/CCES_2018_RECORDINGS/Drift-10'
DepID<-'CCES_010'
OutDir<-'D:/Zooniverse Clips/CCES_010/Batch2'
dir.create(OutDir)

# 1. Extract times from directory of wav files
WavFiles<-dir(WavDir,pattern='.wav')
WavFiles<-WavFiles[101:500]  ##Testing with first 100 files##
TimeString<-substr(WavFiles,12,23)
WavTimes<-strptime(TimeString,format='%y%m%d%H%M%S',tz="UTC")  
start<-WavTimes+seconds(5)
end<-WavTimes+seconds(25)

# 2. Create dataframe with start, end, id
 id<-c(paste0(DepID,'_',101:500))
# id<-c(paste0(DepID,'_','00',1:9),paste0(DepID,'_','0',10:99),paste0(DepID,'_','100'))##Testing with first 1-100 files##
df<-data.frame(start,end,id,WavFiles)

# change to whatever format your dates are in
df$start <- force_tz(df$start, tz='UTC')
df$end <- force_tz(df$end, tz='UTC')

# 3. Generate clips 
writeClip <- function(wav, name, time, channel, mode, dir='.', ...) {
  if(!dir.exists(dir)) {
    dir.create(dir)
  }
  file <- file.path(dir, paste0(name, '.wav'))
  writeWave(wav, file, extensible = FALSE)
  # return the file name for keeping track of
  file
}
# this will write clips and put them in folder "dir"
clipFiles <- getClipFromDf(df, WavDir, FUN=writeClip, dir=OutDir)

#4. Save Dataframe which contains mapping of ids and times
filename=paste0(OutDir,'/',DepID,'_IDTimes.rda')
save(df,file=filename)


