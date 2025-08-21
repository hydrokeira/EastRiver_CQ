setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

co_var=function(x){
  val=mean(x)/sd(x)
  return(val)
}

#read in Coal Creek and ER data and then just change the plot calls (p1, p2 - East River; p3, p4 - Coal Creek)
#need to change "discharge" to "CC_Q_cms"
ERCQ<-read.csv("PH_CQ.csv")

ERCQ$date<-as.Date(ERCQ$date)

ERCQ$month<-month(ERCQ$date)

ercq_monthly<-ERCQ %>%
  dplyr::group_by(month, variable) %>%
  dplyr::summarise(
    slope=coef(lm(log10(value)~log10(discharge)))[2],
    cvc_cvq=co_var(value)/co_var(discharge)
  )

ercq_monthly$scale<-"monthly"

dl<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/PH_Cation_DL.csv")

colnames(dl)[3]<-"variable_units"

dl$variable<-sub("_.*", "", dl$variable_units)

dl[45,5]<-"ammonia_n"

ercq_allscales<-full_join(ercq_monthly, dl, by="variable")

ercq_allscales<-ercq_allscales[complete.cases(ercq_allscales$slope),]

k1<-ggplot(ercq_allscales, aes(cvc_cvq, slope))+
  geom_point(aes(shape=Class, col=as.factor(month)), size=3)+
  ylim(-2,2)+xlim(0,12.5)+
  scale_color_manual(values = cet_pal(12, "c4s"))+
  theme_bw()+labs(x="CV(C)/CV(Q)", y="CQ Slope", col="Month", shape="Solute Class")+
  theme(text = element_text(size = 20), legend.position = "null")+ggtitle("East River")

k3<-ggplot(ercq_allscales, aes(cvc_cvq))+
  geom_density(aes(color=Class), alpha=0.5, linewidth=2)+xlim(0,12.5)+
  theme_bw()+labs(x="CV(C)/CV(Q)", y="Density", col="Solute Class")+
  theme(text = element_text(size = 20), legend.position = "null")+
  scale_color_manual(values = c("biologic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))

k5<-ggplot(ercq_allscales, aes(cvc_cvq))+
  geom_density(aes(col=as.factor(month)), alpha=0.5, linewidth=2)+xlim(0,12.5)+
  theme_bw()+labs(x="CV(C)/CV(Q)", y="Density", fill="Month")+
  theme(text = element_text(size = 20), legend.position = "null")+
  scale_color_manual(values = cet_pal(12, "c4s"))

k5

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

ER_bf<-read.csv("PH_bf_prop.csv")
ER_bf$date<-as.Date(ER_bf$date)

ER_bf$month<-month(ER_bf$date)

ER_bf_monthly<-ER_bf %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(mean_monthly_bf=mean(bf_percent),
                   monthly_CV_bf=co_var(bf_percent))

ercq_allscales<-left_join(ercq_allscales, ER_bf_monthly)

ercq_allscales<-ercq_allscales %>%
  dplyr::group_by(Element) %>%
  dplyr::mutate(keep = if_else(max(cvc_cvq, na.rm = TRUE) > 2, "yes", "no"))

pdf("PH_BF_CVC_CVQ.pdf", width=12, height = 10)

ercq_allscales %>%
  filter(keep=="yes") %>%
  ggplot(aes(mean_monthly_bf, slope))+geom_point()+
  geom_smooth(aes(group=Element), method = "lm", se=F, col="grey")+
  facet_wrap(~Element, scales="free")+theme_bw()

ercq_allscales %>%
  filter(keep=="yes") %>%
  ggplot(aes(mean_monthly_bf, cvc_cvq))+geom_point()+
  geom_smooth(aes(group=Element), method = "lm", se=F, col="grey")+
  facet_wrap(~Element, scales="free")+theme_bw()

ercq_allscales %>%
  filter(keep=="yes") %>%
  ggplot(aes(monthly_CV_bf, cvc_cvq))+geom_point()+
  geom_smooth(aes(group=Element), method = "lm", se=F, col="grey")+
  facet_wrap(~Element, scales="free")+theme_bw()

dev.off()


#read in Coal Creek and ER data and then just change the plot calls (p1, p2 - East River; p3, p4 - Coal Creek)
#need to change "discharge" to "CC_Q_cms"
ERCQ<-read.csv("CoalCreek_CQ.csv")

ERCQ$date<-as.Date(ERCQ$date)

ERCQ$month<-month(ERCQ$date)

ercq_monthly<-ERCQ %>%
  dplyr::group_by(month, variable) %>%
  dplyr::summarise(
    slope=coef(lm(log10(value)~log10(CC_Q_cms)))[2],
    cvc_cvq=co_var(value)/co_var(CC_Q_cms)
  )

ercq_monthly$scale<-"monthly"

dl<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/PH_Cation_DL.csv")

colnames(dl)[3]<-"variable_units"

dl$variable<-sub("_.*", "", dl$variable_units)

dl[45,5]<-"ammonia_n"

ercq_allscales<-full_join(ercq_monthly, dl, by="variable")

ercq_allscales<-ercq_allscales[complete.cases(ercq_allscales$slope),]

ercq_allscales %>%
  filter(Class=="metal") %>%
  ggplot(aes(cvc_cvq, slope))+
  geom_point(aes(shape=Class, col=as.factor(month)), size=3)+
  ylim(-2,2)+xlim(0,12.5)+
  scale_color_manual(values = cet_pal(12, "c4s"))+
  theme_bw()+labs(x="CV(C)/CV(Q)", y="CQ Slope", col="Month", shape="Solute Class")+
  theme(text = element_text(size = 20))+ggtitle("Coal Creek")+
  facet_wrap(~Element)

keep_these_solutes<-c("Ca", "K", "Mg", "Na","Si", "SO4", "Sr", "DIC", "TDN", "As", "Ba", "Rb", "U")

k2<-ggplot(ercq_allscales, aes(cvc_cvq, slope))+
  geom_point(aes(shape=Class, col=as.factor(month)), size=3)+
  ylim(-2,2)+xlim(0,12.5)+
  scale_color_manual(values = cet_pal(12, "c4s"))+
  theme_bw()+labs(x="CV(C)/CV(Q)", y="CQ Slope", col="Month", shape="Solute Class")+
  theme(text = element_text(size = 20))+ggtitle("Coal Creek")+
  facet_wrap(~Element)

k2

k4<-ggplot(ercq_allscales, aes(cvc_cvq))+
  geom_density(aes(col=Class), alpha=0.5, linewidth=2)+xlim(0,12.5)+
  theme_bw()+labs(x="CV(C)/CV(Q)", y="Density", color="Solute Class")+
  theme(text = element_text(size = 20))+
  scale_color_manual(values = c("biologic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))

k4

k6<-ggplot(ercq_allscales, aes(cvc_cvq))+
  geom_density(aes(col=as.factor(month)), alpha=0.5, linewidth=2)+xlim(0,12.5)+
  theme_bw()+labs(x="CV(C)/CV(Q)", y="Density", color="Month")+
  theme(text = element_text(size = 20))+
  scale_color_manual(values = cet_pal(12, "c4s"))

k6

ggarrange(k5, k6, widths = c(0.4, 0.5))

pdf("CVC_CVQ_density.pdf", width = 12, height = 12)

ggarrange(k1, k2, k3, k4, k5, k6, widths = c(0.4, 0.5), heights = c(0.5, 0.4, 0.4),
          nrow = 3, ncol=2)

dev.off()

pdf("CVC_CVQ.pdf", width = 12, height = 5)

ggarrange(k1, k2, widths = c(0.4, 0.5))

dev.off()

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

CC_bf<-read.csv("CC_bf_prop.csv")
CC_bf$date<-as.Date(CC_bf$date)

CC_bf$month<-month(CC_bf$date)

CC_bf_monthly<-CC_bf %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(mean_monthly_bf=mean(bf_percent),
                   monthly_CV_bf=co_var(bf_percent))

ercq_allscales<-left_join(ercq_allscales, CC_bf_monthly)

ercq_allscales<-ercq_allscales %>%
  dplyr::group_by(Element) %>%
  dplyr::mutate(keep = if_else(max(cvc_cvq, na.rm = TRUE) > 2, "yes", "no"))

pdf("CC_BF_CVC_CVQ.pdf", width=12, height = 10)

ercq_allscales %>%
  filter(Element %in% keep_these_solutes) %>%
  ggplot(aes(mean_monthly_bf, slope))+geom_point()+
  geom_smooth(aes(group=Element), method = "lm", se=F, col="grey")+
  facet_wrap(~Element, scales="free")+theme_bw()

ercq_allscales %>%
  filter(Element %in% keep_these_solutes) %>%
  ggplot(aes(mean_monthly_bf, cvc_cvq))+geom_point()+
  geom_smooth(aes(group=Element), method = "lm", se=F, col="grey")+
  facet_wrap(~Element, scales="free")+theme_bw()

ercq_allscales %>%
  filter(Element %in% keep_these_solutes) %>%
  ggplot(aes(monthly_CV_bf, cvc_cvq))+geom_point()+
  geom_smooth(aes(group=Element), method = "lm", se=F, col="grey")+
  facet_wrap(~Element, scales="free")+theme_bw()

dev.off()


