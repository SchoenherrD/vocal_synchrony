###Merge two datasets

#read tables
pitch= read.table(file= "path/Date_pit_results.txt",
                   dec       = ".",
                   sep       = ";",
                   header=T,
                  colClasses = c(ID="numeric", segment = "numeric", start= "numeric", end="numeric", med_pit= "numeric",
                                 P5_pit= "numeric", P95_pit= "numeric"),
                  stringsAsFactors = F)


syllables= read.table(file= "path/Date_syllables.txt",
             dec       = ".",
             sep       = ";",
             header=T,
             colClasses = c(ID="numeric", segment = "numeric", N_syllables = "numeric"),
             stringsAsFactors = F)

#Merge

library(plyr)
total=join(pitch, syllables, by=c("ID", "segment"), type="full")



#compute duration and speech rate
total$dur=total$end-total$start
total$speed=total$N_syllables/total$dur


####Export data####

currentDate <- Sys.Date()
txtFileName <- paste(currentDate,"_dataset.txt",sep="")

write.table(total,
            file      = txtFileName,
            dec       = ".",
            sep       = ";",
            row.names = F,
            col.names = T)
