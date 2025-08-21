setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/PH_Discharge")

ph_files<-list.files(path = ".", pattern = ".csv")

ph_list_q<-list()

for (i in 1:length(ph_files)) {
  
  q1<-read.csv(ph_files[i])
  
  colnames(q1)<-c("date", "daily_q_cms")
  
  q1$date<-as.Date(q1$date, "%m/%d/%y")
  
  q1$daily_q_cms<-as.numeric(q1$daily_q_cms)
  
  ph_list_q[[i]]<-q1
  
}

ph_all_q<-do.call(bind_rows, ph_list_q)

ph_all_q<-ph_all_q[,c(1,2)]

write.csv(ph_all_q, "Daily_ER_PH_Q.csv")

ggplot(ph_all_q, aes(date, daily_q_cms))+geom_line()
