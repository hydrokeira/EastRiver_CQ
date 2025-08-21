setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

ysi<-read.csv("PH_YSI.csv", skip = 1)
ysi<-ysi[-c(1,2),]
ysi$date<-as.Date(ysi$TIMESTAMP, "%m/%d/%y")

ysi_range<-ysi %>%
  dplyr::mutate(pH_Avg=as.numeric(pH_Avg)) %>%
  dplyr::filter(pH_Avg > 0) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(min_pH=min(pH_Avg), max_pH=max(pH_Avg), range_pH=max_pH-min_pH)

ggplot(ysi_range, aes(date, range_pH))+geom_line()

ysi %>%
  dplyr::mutate(pH_Avg=as.numeric(pH_Avg), ODO_mgL_Avg=as.numeric(ODO_mgL_Avg)) %>%
  dplyr::filter(pH_Avg > 0) %>%
  dplyr::filter(year(date) == 2024) %>%
  dplyr::group_by(date) %>%
  #dplyr::summarise(avg_pH=median(pH_Avg)) %>%
  ggplot(aes(x=date))+geom_line(mapping=aes(y=ODO_mgL_Avg))+geom_line(mapping=aes(y=pH_Avg), col="blue")+
  ylab("DO (mg/L) [black] and pH [blue]")+theme_classic()+theme(text = element_text(size = 20))

         