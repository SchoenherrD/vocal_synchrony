###Merging all pitch files from Praat
#Script written by D. Schoenherr

#set working directory
setwd("path/example")

#List all files with 'pitch'
nam <- list.files(getwd(), "pitch")

#output vector
dataset_all=c()

# loop through all files
for (i in 2:length(nam))
{
  #test which file (patient, therapist, silence, noise_segments)
  ifelse(sapply(strsplit(nam[i],"[_]"),function(m) m[2])=="therapist", therapist <- "yes", therapist <- "no")
  ifelse(sapply(strsplit(nam[i],"[_]"),function(m) m[2])=="patient", patient <- "yes", patient <- "no")
  ifelse(sapply(strsplit(nam[i],"[_]"),function(m) m[2])=="noise", noise_segment <- "yes", noise_segment <- "no")
  ifelse(sapply(strsplit(nam[i],"[_]"),function(m) m[2])=="silence", silence <- "yes", silence <- "no")
  
  #load first dataset
  pitch_data <- read.csv2(nam[i],dec=".",as.is=TRUE)

if (length(pitch_data$start) > 0){
  #extract ID
  ID=as.numeric(gsub("[^0-5]", "",  nam[i]))
  
  #build variable showing annotation of the segment
  pitch_data$PT2 <- ifelse(patient == "yes", "patient", ifelse(therapist == "yes", "therapist", ifelse(noise_segment== "yes", "noise", "silence")))
  
 
##fill table for silence and noise segments
  if (length(pitch_data)<7){
    pitch_data$median =NA
    pitch_data$X5.quantile=NA
    pitch_data$X95.quantile=NA
  }
  
  #Recode undefined values to missings
  pitch_data$median[pitch_data$median == "--undefined--"] <- NA
  pitch_data$X5.quantile[pitch_data$X5.quantile == "--undefined--"] <- NA
  pitch_data$X95.quantile[pitch_data$X95.quantile == "--undefined--"] <- NA
  
  #convert to numeric
  pitch_data$median =as.numeric(pitch_data$median)
  pitch_data$X5.quantile=as.numeric(pitch_data$X5.quantile)
  pitch_data$X95.quantile=as.numeric(pitch_data$X95.quantile)
  pitch_data$nr=as.numeric(pitch_data$nr)
  
  ###round
  pitch_data$start <- round(pitch_data$start, digits=2)
  pitch_data$end <- round(pitch_data$end, digits=2)
  pitch_data$median <- round(pitch_data$median, digits=2)
  pitch_data$X5.quantile <- round(pitch_data$X5.quantile, digits=2)
  pitch_data$X95.quantile <- round(pitch_data$X95.quantile, digits=2)
  
  dataset= data.frame(ID, pitch_data$nr, pitch_data$start, pitch_data$end, pitch_data$median, pitch_data$X5.quantile, pitch_data$X95.quantile, pitch_data$PT2)
  dataset_all=rbind(dataset_all, dataset)
  }
}

colnames(dataset_all)= c("ID", "segment", "start", "end", "med_pit", "P5_pit", "P95_pit", "class")

dataset_all$class=as.factor(dataset_all$class)

####Export data

currentDate <- Sys.Date()
txtFileName <- paste(currentDate,"_pit_results.txt",sep="")

write.table(dataset_all,
            file      = txtFileName,
            dec       = ".",
            sep       = ";",
            row.names = F,
            col.names = T)


