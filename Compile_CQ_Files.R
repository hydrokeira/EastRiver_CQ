library(dataRetrieval)
library(dplyr)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/PH")

ph_files<-list.files(path = ".", pattern = ".csv")

ph_list<-list()

for (i in 1:length(ph_files)) {
  
  chem1<-read.csv(ph_files[i])
  
  chem1<-chem1[,-3]
  
  col_name<-colnames(chem1[2])
  
  colnames(chem1)<-c("date", "value")
  
  chem1$variable<-col_name
  
  chem1$date<-as.Date(chem1$date)
  
  chem1[,2]<-as.numeric(chem1[,2])
  
  chem1<-chem1[!chem1$value==0,]
  
  ph_list[[i]]<-chem1
  
}

ph_all_chem<-do.call(bind_rows, ph_list)

dl<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/PH_Cation_DL.csv")

colnames(dl)[3]<-"variable_units"

dl$variable<-sub("_.*", "", dl$variable_units)

dl[45,5]<-"ammonia_n"

ph_all_chem_dl<-full_join(ph_all_chem, dl)

ph_all_chem_dl$DL_ppb[is.na(ph_all_chem_dl$DL_ppb)]<-0

anions<-c("Cl", "Fl", "NO3", "PO4", "SO4")

#remove all anions with value = 0 (ND value)
ph_all_chem_remove <- ph_all_chem_dl %>%
  dplyr::filter(!c(Element %in% anions & value == 0))

ph_all_chem_remove_ppb <- ph_all_chem_remove %>%
  mutate(value=case_when(
    Element=="DIC"~value*1000,
    Element=="DOC"~value*1000,
    Element=="Cl"~value*35.45,
    Element=="Fl"~value*18.99,
    Element=="NO3"~value*62.005,
    Element=="PO4"~value*94.97,
    Element=="SO4"~value*96.06,
    Element=="NH3_N"~value*1000,
    .default = value
  ))

ph_all_chem_remove_ppb<-subset(ph_all_chem_remove_ppb, ph_all_chem_remove_ppb$value > ph_all_chem_remove_ppb$DL_ppb)

ph_all_chem_remove_ppb<-ph_all_chem_remove_ppb %>%
  dplyr::filter(!c(variable=="aluminum" & value > 2000)) %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(mean_val=mean(value), sd_val=sd(value))

write.csv(ph_all_chem_remove_ppb, "PH_All_Chem.csv")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

ph_q<-read.csv("Daily_ER_PH_Q.csv")

ph_q<-ph_q[,-1]

colnames(ph_q)<-c("date", "discharge")

ph_q$date<-as.Date(ph_q$date)

ph_all_chem_q<-merge(ph_all_chem_remove_ppb, ph_q, by="date")

write.csv(ph_all_chem_q, "PH_CQ.csv")

#### same for Coal Creek ####

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/Coal11")

coal_files<-list.files(path = ".", pattern = ".csv")

coal_list<-list()

for (i in 1:length(coal_files)) {
  
  chem1<-read.csv(coal_files[i])
  
  chem1<-chem1[,-3]
  
  col_name<-colnames(chem1[2])
  
  colnames(chem1)<-c("date", "value")
  
  chem1$variable<-col_name
  
  chem1$date<-as.Date(chem1$date)
  
  chem1[,2]<-as.numeric(chem1[,2])
  
  chem1<-chem1[!chem1$value==0,]
  
  coal_list[[i]]<-chem1
  
}

coal_all_chem<-do.call(bind_rows, coal_list)

dl<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/PH_Cation_DL.csv")

colnames(dl)[3]<-"variable_units"

dl$variable<-sub("_.*", "", dl$variable_units)

dl[45,5]<-"ammonia_n"

coal_all_chem_dl<-full_join(coal_all_chem, dl)

coal_all_chem_dl$DL_ppb[is.na(coal_all_chem_dl$DL_ppb)]<-0

anions<-c("Cl", "Fl", "NO3", "PO4", "SO4")

#remove all anions with value = 0 (ND value)
coal_all_chem_remove <- coal_all_chem_dl %>%
  dplyr::filter(!c(Element %in% anions & value == 0))

coal_all_chem_remove_ppb <- coal_all_chem_remove %>%
  mutate(value=case_when(
    Element=="DIC"~value*1000,
    Element=="DOC"~value*1000,
    Element=="Cl"~value*35.45,
    Element=="Fl"~value*18.99,
    Element=="NO3"~value*62.005,
    Element=="PO4"~value*94.97,
    Element=="SO4"~value*96.06,
    Element=="NH3_N"~value*1000,
    .default = value
  ))

coal_all_chem_remove_ppb<-subset(coal_all_chem_remove_ppb, coal_all_chem_remove_ppb$value > coal_all_chem_remove_ppb$DL_ppb)

coal_all_chem_remove_ppb<-coal_all_chem_remove_ppb %>%
  dplyr::filter(!c(variable=="aluminum" & value > 2000)) %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(mean_val=mean(value), sd_val=sd(value))

write.csv(coal_all_chem_remove_ppb, "Coal_All_Chem.csv")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

cc_q<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/Coal_Creek_wy15_24_daily.csv")
cc_q$Date<-as.Date(cc_q$Date, "%m/%d/%y")
colnames(cc_q)[1]<-"date"

cc_cq<-merge(coal_all_chem_remove_ppb, cc_q, by="date")

write.csv(cc_cq, "CoalCreek_CQ.csv")

