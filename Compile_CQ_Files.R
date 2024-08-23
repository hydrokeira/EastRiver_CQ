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

write.csv(ph_all_chem, "PH_All_Chem.csv")

ph_q<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/PH_Discharge/Daily_ER_PH_Q.csv")

ph_q<-ph_q[,c(2,3)]

ph_q$date<-as.Date(ph_q$date)

ph_all_chem_q<-merge(ph_all_chem, ph_q, by="date")

write.csv(ph_all_chem_q, "PH_CQ.csv")

ggplot(ph_all_chem_q, aes(log10(daily_q_cms),log10(value)))+geom_point()+facet_wrap(~variable, scales = "free")+
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

write.csv(coal_all_chem, "Coal_All_Chem.csv")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/Coal11")

cc_chem<-read.csv("Coal_All_Chem.csv")
cc_chem$date<-as.Date(cc_cq$date)

cc_q<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/Coal_Creek_wy15_22_daily.csv")
cc_q$date<-as.Date(cc_q$date, "%m/%d/%y")
cc_q<-cc_q[,c(1,2)]

cc_cq<-merge(cc_chem, cc_q, by="date")
cc_cq<-cc_cq[,-2]

write.csv(cc_cq, "CoalCreek_CQ.csv")



