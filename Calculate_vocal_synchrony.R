###Script Calculating vocal synchrony

#install and load packages
install.packages("psych")
install.packages("utils")
library(psych)
library(utils)

#load dataset
data_sync=read.table("path/Date_dataset.txt", header = TRUE, dec = ".",  sep  = ";")

#as.factor
data_sync$class=as.factor(data_sync$class)

#calculate range of pitch
data_sync$range=data_sync$P95_pit-data_sync$P5_pit

#get vector ID variables
ID=unique(data_sync$ID)

#define output vector
sync_all=c()

###loop all IDs
for (p in 1:length(ID)) {
  #one ID
  ID2=ID[p]
  Vpn=subset(data_sync, data_sync$ID == ID2)
  
  #sort by segment number
  Vpn =Vpn[order(Vpn$segment),]

  # filter IPUs: only consecutive speaker turn without interruption by silence or noise segments  
    for (i in 1:length(Vpn$segment)){
      Vpn$pat_leading[i]=ifelse((( Vpn$class[i]=="patient") & (Vpn$class[i+1]=="therapist" )),1, 0)
      Vpn$ther_leading[i]=ifelse((( Vpn$class[i]=="therapist") & (Vpn$class[i+1]=="patient" )),1, 0)
    }
  
    Vpn$pat_leading2=c(0, Vpn$pat_leading[-length(Vpn$segment)])
    Vpn$ther_leading2=c(0, Vpn$ther_leading[-length(Vpn$segment)])
    
    Vpn$pat_leading3=Vpn$pat_leading+Vpn$pat_leading2
    Vpn$ther_leading3=Vpn$ther_leading+Vpn$ther_leading2
    
    
    ##subsets patient leading, therapist leading
    Vpn_pat_led= subset(Vpn, Vpn$pat_leading3 > 0)
    Vpn_ther_led= subset(Vpn, Vpn$ther_leading3 > 0)
    
    #patient leading synchrony
    pat_leading_P=subset(Vpn_pat_led, Vpn_pat_led$class == "patient")
    pat_leading_T=subset(Vpn_pat_led, Vpn_pat_led$class == "therapist")
    
    sync_pat_lead=cor(pat_leading_P$med_pit,pat_leading_T$med_pit, use="pairwise.complete.obs")
    sync_range_pat_lead=cor(pat_leading_P$range,pat_leading_T$range, use="pairwise.complete.obs")
    sync_speed_pat_lead=cor(pat_leading_P$speed,pat_leading_T$speed, use="pairwise.complete.obs")   
    
    #absolute values & Fishers z-transformation
    zsync_pat_lead=fisherz(abs(sync_pat_lead))
    zsync_range_pat_lead=fisherz(abs(sync_range_pat_lead))
    zsync_speed_pat_lead=fisherz(abs(sync_speed_pat_lead))
    
    #therapist leading synchrony
    ther_leading_P=subset(Vpn_ther_led, Vpn_ther_led$class == "patient")
    ther_leading_T=subset(Vpn_ther_led, Vpn_ther_led$class == "therapist")
    
    sync_ther_lead=cor(ther_leading_T$med_pit,ther_leading_P$med_pit, use="pairwise.complete.obs")
    sync_range_ther_lead=cor(ther_leading_T$range,ther_leading_P$range, use="pairwise.complete.obs")
    sync_speed_ther_lead=cor(ther_leading_T$speed,ther_leading_P$speed, use="pairwise.complete.obs")   
    
    #absolute values & Fishers z-transformation
    zsync_ther_lead=fisherz(abs(sync_ther_lead))
    zsync_range_ther_lead=fisherz(abs(sync_range_ther_lead))
    zsync_speed_ther_lead=fisherz(abs(sync_speed_ther_lead))
    
   
  ###sum results
  
  sync_final=as.data.frame(cbind(ID[p],zsync_pat_lead, zsync_range_pat_lead, zsync_speed_pat_lead,
                                 zsync_ther_lead, zsync_range_ther_lead, zsync_speed_ther_lead ))
  colnames(sync_final)=c("ID","F0_sync_pat_lead", "Range_sync_pat_lead","Speed_sync_pat_lead", 
                         "F0_sync_ther_lead", "Range_sync_ther_lead","Speed_sync_ther_lead")
  sync_all=as.data.frame(rbind(sync_all, sync_final))
  colnames(sync_all)=c("ID","F0_sync_pat_lead", "Range_sync_pat_lead","Speed_sync_pat_lead", 
                       "F0_sync_ther_lead", "Range_sync_ther_lead","Speed_sync_ther_lead")
  rm(list=setdiff(ls(), c("data_sync","sync_all", "ID", "p")))
}

###save results

currentDate <- Sys.Date()
txtFileName <- paste(currentDate,"_vocal_synchrony.txt",sep="")

write.table(sync_all,
            file      = txtFileName,
            dec       = ".",
            sep       = ";",
            row.names = F,
            col.names = T)

