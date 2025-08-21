setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

ph<-read.csv("Coal_11_pH.csv")

chem<-read.csv("CoalCreek_CQ.csv")

chem_cast<-chem[,c(2,3,5,11)] %>%
  dplyr::group_by(date, CC_Q_cms, Element) %>%
  dplyr::summarise(mean_val=mean(value)) %>%
  pivot_wider(names_from = Element, values_from = mean_val)

chem_cast$date<-as.Date(chem_cast$date)

ph$date<-as.Date(ph$Date, "%m/%d/%y")

ph<-ph[,c("date","Temp", "ORP","pH")]

chem_pH<-merge(ph, chem_cast, by="date")

write.csv(chem_pH, "CC_Chem_WQ.csv")


ph<-read.csv("PH_pH.csv")

chem<-read.csv("PH_CQ.csv")

chem_cast<-chem[,c(2,3,5,11)] %>%
  dplyr::group_by(date, discharge, Element) %>%
  dplyr::summarise(mean_val=mean(value)) %>%
  pivot_wider(names_from = Element, values_from = mean_val)

chem_cast$date<-as.Date(chem_cast$date)

ph$date<-as.Date(as.character(ph$date), "%Y%m%d")

ph<-ph[,c("date","temp", "ORP","pH")]

chem_pH<-merge(ph, chem_cast, by="date")

write.csv(chem_pH, "PH_Chem_WQ.csv")
