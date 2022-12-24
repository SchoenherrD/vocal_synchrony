###Counting syllables in R
###Script written by D. Schoenherr

###Packages

install.packages("sylly.en")
install.packages("sylly")
install.packages("readtext")
install.packages("Hmisc")

library(sylly)
library(sylly.en)
library(readtext)
library(Hmisc)
library(base)

#Set working directory
setwd("path/example")

#List all files with name 'transcript'
nam <- list.files(getwd(), "transcript")

# vector for output
res_all_IDs=c()

##loop through all files 
for (i in 2:length(nam))
{
  transcript=readtext(nam[i])
  
  #separate lines
  list_lines=string.break.line(transcript$text)
  lines_text=unlist(list_lines)
  
  #extract ID from name
  ID=as.numeric(gsub("[^0-5]", "",  nam[i]))
  
  res_all=c()  # predefined vector for output
  
  #loop through all lines, extract segment number, words and count syllables
  for (k in 2:length(lines_text)){
    #extract segment number
    segment=as.numeric(gsub("[^0-9]", "",  lines_text[k]))
    
    #extract words in concerning line
    words=gsub('[[:digit:]]+', '', lines_text[k])
    words_split=strsplit(words, split=" ")
    words_split2=unlist(words_split)  
    
    #count syllables
    hyph.txt.en <- hyphen(words_split2, hyph.pattern="en")
    out=hyphenText(hyph.txt.en)
    syl_count=sum(out$syll) 
    
    # prepare output
    res=data.frame(ID, segment, syl_count) 
    
    #delete row with NA
    res=res[complete.cases(res), ]
    
    #one dataset for all segments
    res_all=rbind(res_all, res)
  } # end loop lines
  
  # one dataset for all transcripts
  res_all_IDs=rbind(res_all_IDs, res_all)
  
} # end loop files

colnames(res_all_IDs)=c("ID", "segment", "N_syllables")

####Export data####

currentDate <- Sys.Date()
txtFileName <- paste(currentDate,"_syllables.txt",sep="")

write.table(res_all_IDs,
            file      = txtFileName,
            dec       = ".",
            sep       = ";",
            na        = "999",
            row.names = F,
            col.names = T)

