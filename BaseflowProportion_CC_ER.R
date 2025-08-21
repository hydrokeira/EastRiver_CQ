require(dplyr)
require(ggplot2)
require(ggpubr)

#calculate baseflow proportion for CC and ER
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

ERCQ<-read.csv("CoalCreek_CQ.csv")

ERCQ$date<-as.Date(ERCQ$date)

ERCQ<-subset(ERCQ, ERCQ$variable=="calcium")

runoff<-quantile(ERCQ$value, 0.01)
gw<-quantile(ERCQ$value, 0.99)

ERCQ$bf<-ERCQ$CC_Q_cms*((ERCQ$value-runoff)/(gw-runoff))

ERCQ$bf_percent<-ERCQ$bf/ERCQ$CC_Q_cms

ERCQ<-subset(ERCQ, ERCQ$bf_percent > 0 & ERCQ$bf_percent < 1)

ERCQ<-ERCQ %>%
  mutate(q_quantile = 
           case_when(CC_Q_cms < quantile(CC_Q_cms, 0.25) ~ "0-25",
                     CC_Q_cms > quantile(CC_Q_cms, 0.25) & CC_Q_cms < quantile(CC_Q_cms, 0.50) ~ "25-50",
                     CC_Q_cms > quantile(CC_Q_cms, 0.50) & CC_Q_cms < quantile(CC_Q_cms, 0.75) ~ "50-75",
                     CC_Q_cms > quantile(CC_Q_cms, 0.75) ~ "75-100")
         )

ERCQ<-ERCQ[complete.cases(ERCQ),]
ERCQ$month<-as.factor(month(ERCQ$date))

ERCQ %>%
  group_by(month) %>%
  summarise(mean(bf_percent))

write.csv(ERCQ, "CC_bf_prop.csv")

p1<-ggplot(ERCQ, aes(date, bf_percent))+
  geom_area(ERCQ, mapping=aes(date, CC_Q_cms*0.1), alpha=0.4, fill="blue")+
  geom_abline(slope = 0, intercept = mean(ERCQ$bf_percent), col="red")+
  geom_line(lty="dashed")+geom_point(size=1)+
  ylim(0,1)+theme_classic()+theme(text = element_text(size=20))+
  labs(x="Date", y="Baseflow Proportion/\nScaled Discharge")+ggtitle("Coal Creek")

p1

p3<-ggplot(ERCQ, aes(x=bf_percent))+geom_density()+theme_bw()+theme(text = element_text(size=20))+
  labs(x="baseflow proportion", y="density")+ylim(0,2)

p3<-ggplot(ERCQ, aes(y=bf_percent, x=month, fill=month))+geom_boxplot()+theme_classic()+
  theme(text = element_text(size=20), legend.position = "null")+
  labs(x="Month", y="Baseflow Proportion")+ylim(0,1)+
  scale_fill_manual(values = cet_pal(12, "cbtc1"))

p3

p5<-ggplot(ERCQ, aes(x=bf_percent, fill=month))+
  geom_density(alpha=0.5)+theme_classic()+
  theme(text = element_text(size=20), legend.position = "null")+
  labs(x="Baseflow Proportion", y="Density", fill="Month")+
  scale_fill_manual(values = cet_pal(12, "cbtc1"))

p5

cc<-ggarrange(p1, p3, p5, nrow = 3, align = "v")

ERCQ<-read.csv("PH_CQ.csv")

ERCQ$date<-as.Date(ERCQ$date)
ERCQ<-subset(ERCQ, year(ERCQ$date) > 2015)

ERCQ<-subset(ERCQ, ERCQ$variable=="calcium")

runoff<-quantile(ERCQ$value, 0.01)
gw<-quantile(ERCQ$value, 0.99)

ERCQ$bf<-ERCQ$discharge*((ERCQ$value-runoff)/(gw-runoff))

ERCQ$bf_percent<-ERCQ$bf/ERCQ$discharge

ERCQ<-subset(ERCQ, ERCQ$bf_percent > 0 & ERCQ$bf_percent < 1)

ERCQ<-ERCQ %>%
  mutate(q_quantile = 
           case_when(discharge < quantile(discharge, 0.25) ~ "0-25",
                     discharge > quantile(discharge, 0.25) & discharge < quantile(discharge, 0.50) ~ "25-50",
                     discharge > quantile(discharge, 0.50) & discharge < quantile(discharge, 0.75) ~ "50-75",
                     discharge > quantile(discharge, 0.75) ~ "75-100")
  )

ERCQ<-ERCQ[complete.cases(ERCQ),]
ERCQ$month<-as.factor(month(ERCQ$date))

ERCQ %>%
  group_by(month) %>%
  summarise(mean(bf_percent))

write.csv(ERCQ, "PH_bf_prop.csv")

p2<-ggplot(ERCQ, aes(date, bf_percent))+
  geom_area(ERCQ, mapping=aes(date, discharge*0.08), alpha=0.4, fill="blue")+
  geom_abline(slope = 0, intercept = mean(ERCQ$bf_percent, na.rm = T), col="red")+
  geom_line(lty="dashed")+geom_point(size=1)+
  ylim(0,1)+theme_classic()+theme(text = element_text(size=20))+
  labs(x="Date", y="")+ggtitle("East River")

p2

p4<-ggplot(ERCQ, aes(x=bf_percent))+geom_density()+theme_bw()+theme(text = element_text(size=20))+
  labs(x="baseflow proportion", y="")+ylim(0,2)

p4<-ggplot(ERCQ, aes(y=bf_percent, x=month, fill=month))+geom_boxplot()+theme_classic()+
  theme(text = element_text(size=20), legend.position = "null")+
  labs(x="Month", y="")+ylim(0,1)+
  scale_fill_manual(values = cet_pal(12, "cbtc1"))

p4

p6<-ggplot(ERCQ, aes(x=bf_percent, fill=month))+geom_density(alpha=0.5)+theme_classic()+
  theme(text = element_text(size=20), legend.position="null")+
  labs(x="Baseflow Proportion", y="", fill="Month")+
  scale_fill_manual(values = cet_pal(12, "cbtc1"))+
  ylim(0,15)

er<-ggarrange(p2, p4, p6, align = "v", nrow = 3)

pdf("ER_CC_Baseflow_Updated.pdf", width = 12, height = 9)

ggarrange(cc, er)

dev.off()

ggarrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2, heights = c(0.3, 0.3, 0.4))
