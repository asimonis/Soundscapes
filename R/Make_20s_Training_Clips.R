#Create training clips for Citizen Science Project 
#Anne Simonis
#6/1/2022

# 1. Load dataframe from manual logs
# 2. Generate clips 

#load packages
library(PAMpal)
library(tuneR)
library(lubridate)
source('C:/Users/anne.simonis.NMFS/Documents/GitHub/Soundscapes/getClipFromDf.R')

ShipDetFile<-'D:/Analysis/CCES/Soundscapes/Ship Detections/CCES_010/CCES_010_ShipLogs_aes.csv'
WavDir<-'F:/CCES_2018_RECORDINGS/Drift-10'
DepID<-'CCES_010_Ship'
OutDir<-'D:/Recordings/CCES_20s clips/CCES_010_ShipDetections'

#1. Load dataframe from manual logs
df <- read.csv(ShipDetFile, stringsAsFactors = FALSE)
# change to whatever format your dates are in
df$start <- as.POSIXct(df$start, format='%m/%d/%Y %H:%M:%OS', tz='UTC')
df$end<-df$start+seconds(20)

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


library(phonTools)
library(av)
wavdir<-'C:/Users/anne.simonis.NMFS/Documents/CitizenScience/Zooniverse Project/Sound Clips/Mn_examples_12kHz_amplified'
wavfiles<-dir(path=wavdir,pattern='.wav')
mp3names<-gsub('.wav','.mp3',wavfiles)
mp4names<-gsub('.wav','.mp4',wavfiles)
pngnames<-gsub('.wav','.png',wavfiles)

setwd(wavdir)
for(w in 1:length(wavfiles)){
# av_audio_convert(wavfiles[w],output=mp3names[w])
##convert to mp3 and save mp4
# av_audio_convert(wavfiles[w],output=mp3names[w])
# av_spectrogram_video(mp3names[w],output=mp4names[w])


#save spectrogram
  sound<-loadsound(wavfiles[w])
png(file=pngnames[w], width=1200, height=700)
spectrogram(sound, fs = 12000, windowlength = 50,
            timestep = 10 , padding = 5,
            maxfreq = 6000, colors = TRUE,
            dynamicrange = 22, nlevels = dynamicrange, maintitle = "",
            show = TRUE, window = 'kaiser', windowparameter = 3,
            quality = FALSE)
dev.off()
}
