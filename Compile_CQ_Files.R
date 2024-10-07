library(dataRetrieval)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/PH")

ph_files<-list.files(path = ".", pattern = ".csv")

ph_list<-list()

for (i in 1:length(ph_files)) {
  
  chem1<-read.csv(ph_files[i])
  
  col_name<-paste0(colnames(chem1[2]), "_", chem1[1,2])
  
  colnames(chem1)<-c("date", "value")
  
  chem1$variable<-col_name
  
  chem1<-chem1[-1,]
  
  chem1$date<-as.Date(chem1$date)
  
  chem1[,2]<-as.numeric(chem1[,2])
  
  chem1<-chem1[!chem1$value==0,]
  
  ph_list[[i]]<-chem1
  
}

ph_all_chem<-do.call(bind_rows, ph_list)

dl<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/PH_Cation_DL.csv")

ph_all_chem_dl<-full_join(ph_all_chem, dl)

ph_all_chem_dl$DL_ppb[is.na(ph_all_chem_dl$DL_ppb)]<-0

ph_all_chem_remove<-subset(ph_all_chem_dl, ph_all_chem_dl$value > ph_all_chem_dl$DL_ppb)

ph_all_chem_remove<-ph_all_chem_remove %>%
  group_by(variable) %>%
  mutate(mean_val=mean(value), sd_val=sd(value))

write.csv(ph_all_chem_remove, "PH_All_Chem.csv")

ph_q<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/PH_Discharge/PH_daily.csv")

ph_q_melt<-melt(ph_q, id.vars=c("Wyday", "date"))

ph_q_melt$date2<-as.Date(ph_q_melt$date, format = "%d-%b")

ph_q_melt$wateryear<-as.numeric(gsub("X","", ph_q_melt$variable))

ph_q_melt$year<-ifelse(ph_q_melt$Wyday < 93, ph_q_melt$wateryear-1, ph_q_melt$wateryear)

ph_q_melt$date_final<-gsub("2024", "", ph_q_melt$date2)

ph_q_melt$date_final<-as.Date(paste0(ph_q_melt$year, ph_q_melt$date_final))

ph_q<-ph_q_melt[,c(8,4)]

colnames(ph_q)<-c("date", "discharge")

ph_q$date<-as.Date(ph_q$date)

write.csv(ph_q, "Pumphouse_Q_Daily_Clean.csv")

ph_all_chem_q<-merge(ph_all_chem_remove, ph_q, by="date")

write.csv(ph_all_chem_q, "PH_CQ.csv")

ggplot(ph_all_chem_q, aes(log10(discharge),log10(value)))+geom_point()+facet_wrap(~variable, scales = "free")+
  theme_bw()

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/Coal11")

coal_files<-list.files(path = ".", pattern = ".csv")

coal_list<-list()

for (i in 1:length(coal_files)) {
  
  chem1<-read.csv(coal_files[i])
  
  col_name<-paste0(colnames(chem1[2]), "_", chem1[1,2])
  
  colnames(chem1)<-c("date", "value")
  
  chem1$variable<-col_name
  
  chem1<-chem1[-1,]
  
  chem1$date<-as.Date(chem1$date)
  
  chem1[,2]<-as.numeric(chem1[,2])
  
  chem1<-chem1[!chem1$value==0,]
  
  coal_list[[i]]<-chem1
  
}

coal_all_chem<-do.call(bind_rows, coal_list)

dl<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/PH_Cation_DL.csv")

coal_all_chem_dl<-full_join(coal_all_chem, dl)

coal_all_chem_dl$DL_ppb[is.na(coal_all_chem_dl$DL_ppb)]<-0

coal_all_chem_remove<-subset(coal_all_chem_dl, coal_all_chem_dl$value > coal_all_chem_dl$DL_ppb)

coal_all_chem_remove<-coal_all_chem_remove %>%
  group_by(variable) %>%
  mutate(mean_val=mean(value), sd_val=sd(value))

write.csv(coal_all_chem_remove, "Coal_All_Chem.csv")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/Coal11")

cc_chem<-read.csv("Coal_All_Chem.csv")
cc_chem$date<-as.Date(cc_chem$date)

cc_q<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/Coal_Creek_wy15_22_daily.csv")
cc_q$date<-as.Date(cc_q$date, "%m/%d/%y")
cc_q<-cc_q[,c(1,2)]

cc_cq<-merge(cc_chem, cc_q, by="date")
cc_cq<-cc_cq[,-2]

write.csv(cc_cq, "CoalCreek_CQ.csv")

ggplot(cc_cq, aes(log10(CC_Q_cms),log10(value)))+geom_point()+facet_wrap(~variable, scales = "free")+
  theme_bw()

