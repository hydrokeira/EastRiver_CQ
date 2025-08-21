require(dplyr)
require(lubridate)

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
}

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

CC_bf<-read.csv("CC_bf_prop.csv")
CC_bf$date<-as.Date(CC_bf$date)

CC_bf$month<-month(CC_bf$date)

CC_bf_monthly<-CC_bf %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(mean_monthly_bf=mean(bf_percent))


CC_CQ<-read.csv("CoalCreek_CQ.csv")

CC_CQ$date<-as.Date(CC_CQ$date)
CC_CQ$month<-month(CC_CQ$date)

CC_CQ_monthly<-CC_CQ %>%
  dplyr::group_by(month, Element) %>%
  dplyr::summarise(
    slope=coef(lm(log(value)~log(CC_Q_cms)))[2],
    conc_load=mean(CC_Q_cms)*mean(value)
  )

CC_CQ_monthly$norm_load=normalize(CC_CQ_monthly$conc_load)

CC_CQ_bf_monthly<-left_join(CC_CQ_monthly, CC_bf_monthly)

bf_cq_stats<-CC_CQ_bf_monthly %>%
  dplyr::group_by(Element) %>%
  dplyr::summarise(
    res_std_error=summary(lm(slope~mean_monthly_bf))$sigma,
    r2=summary(lm(slope~mean_monthly_bf))$r.squared
  )

ggplot(bf_cq_stats, aes(res_std_error))+geom_density()
ggplot(bf_cq_stats, aes(r2))+geom_density()

class<-read.csv("PH_Cation_DL.csv")

bf_cq_stats<-left_join(bf_cq_stats, class[,c(1,4)])

p1<-ggplot(bf_cq_stats, aes(x = r2)) +
  stat_ecdf(geom = "step", aes(col=Class)) +
  stat_ecdf(geom = "step") +
  theme_bw()+theme(text = element_text(size = 20), legend.position = "null")

p2<-ggplot(bf_cq_stats, aes(x = r2, y = Class, fill = Class)) +
  geom_density_ridges() +
  theme_bw()+theme(text = element_text(size = 20), legend.position = "null")

p3<-ggplot(bf_cq_stats, aes(x = Class, y = r2, fill=Class)) +
  geom_violin() +
  theme_bw()+theme(text = element_text(size = 20), legend.position = "null")

p4<-ggplot(bf_cq_stats, aes(x = res_std_error)) +
  stat_ecdf(geom = "step", aes(col=Class)) +
  stat_ecdf(geom = "step") +
  theme_bw()+theme(text = element_text(size = 20), legend.position = "null")

p5<-ggplot(bf_cq_stats, aes(x = res_std_error, y = Class, fill = Class)) +
  geom_density_ridges() +
  theme_bw()+theme(text = element_text(size = 20), legend.position = "null")

p6<-ggplot(bf_cq_stats, aes(x = Class, y = res_std_error, fill=Class)) +
  geom_violin() +
  theme_bw()+theme(text = element_text(size = 20), legend.position = "null")

p7<-ggarrange(p1, p4, p2, p5, p3, p6, nrow=3, ncol=2, align = "hv")


pdf("CQ_BF_performance_CoalCreek.pdf", width = 12, height = 12)

annotate_figure(p7, top = text_grob("Coal Creek", size=20))

dev.off()

pdf("CQmonthly_BF_CoalCreek.pdf", width = 15, height = 12)

ggplot(CC_CQ_bf_monthly, aes(mean_monthly_bf, slope))+geom_point()+
  geom_smooth(method = "lm", se=F, col="black", size=0.5)+
  facet_wrap(~Element, scales = "free")+
  theme(text = element_text(size=15))+theme_bw()

dev.off()

ecdf_fun<-ecdf(bf_cq_stats$r2)

threshold <- 0.5
df_high <- bf_cq_stats[ecdf_fun(bf_cq_stats$r2) > threshold,]

p1<-ggplot(df_high, aes(Class, fill=Class))+geom_bar(stat="count")+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  ggtitle("Coal Creek")+labs(x="")

p2<-ggplot(df_high, aes(Class, r2, fill=Class))+geom_boxplot()+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="")

p3<-ggplot(df_high, aes(Class, res_std_error, fill=Class))+geom_boxplot()+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="")

pdf("CDF_Stats_CoalCreek.pdf", width = 6, height = 10)

ggarrange(p1, p2, p3, nrow = 3)

dev.off()

pdf("CQmonthly_BF_CDF50_CoalCreek.pdf", width = 15, height = 12)

CC_CQ_bf_monthly %>%
  dplyr::filter(Element %in% df_high$Element) %>%
  ggplot(aes(mean_monthly_bf, slope))+geom_point()+
    geom_smooth(method = "lm", se=F, col="black", size=0.5)+
    facet_wrap(~Element, scales = "free")+
    theme(text = element_text(size=15))+theme_bw()

dev.off()


setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

ER_bf<-read.csv("PH_bf_prop.csv")
ER_bf$date<-as.Date(ER_bf$date)

ER_bf$month<-month(ER_bf$date)

ER_bf_monthly<-ER_bf %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(mean_monthly_bf=mean(bf_percent))

ER_CQ<-read.csv("PH_CQ.csv")

ER_CQ$date<-as.Date(ER_CQ$date)
ER_CQ$month<-month(ER_CQ$date)

ER_CQ_monthly<-ER_CQ %>%
  dplyr::group_by(month, Element) %>%
  dplyr::summarise(
    slope=coef(lm(log(value)~log(discharge)))[2],
    conc_load=mean(discharge)*mean(value)
  )

ER_CQ_monthly$norm_load=normalize(ER_CQ_monthly$conc_load)

ER_CQ_bf_monthly<-left_join(ER_CQ_monthly, ER_bf_monthly)

bf_cq_stats<-ER_CQ_bf_monthly %>%
  dplyr::group_by(Element) %>%
  dplyr::summarise(
    res_std_error=summary(lm(slope~mean_monthly_bf))$sigma,
    r2=summary(lm(slope~mean_monthly_bf))$r.squared
  )

class<-read.csv("PH_Cation_DL.csv")

bf_cq_stats<-left_join(bf_cq_stats, class[,c(1,4)])

p1<-ggplot(bf_cq_stats, aes(x = r2)) +
  stat_ecdf(geom = "step", aes(col=Class)) +
  stat_ecdf(geom = "step") +
  theme_bw()+theme(text = element_text(size = 20), legend.position = "null")

p2<-ggplot(bf_cq_stats, aes(x = r2, y = Class, fill = Class)) +
  geom_density_ridges() +
  theme_bw()+theme(text = element_text(size = 20), legend.position = "null")

p3<-ggplot(bf_cq_stats, aes(x = Class, y = r2, fill=Class)) +
  geom_violin() +
  theme_bw()+theme(text = element_text(size = 20), legend.position = "null")

p4<-ggplot(bf_cq_stats, aes(x = res_std_error)) +
  stat_ecdf(geom = "step", aes(col=Class)) +
  stat_ecdf(geom = "step") +
  theme_bw()+theme(text = element_text(size = 20), legend.position = "null")

p5<-ggplot(bf_cq_stats, aes(x = res_std_error, y = Class, fill = Class)) +
  geom_density_ridges() +
  theme_bw()+theme(text = element_text(size = 20), legend.position = "null")

p6<-ggplot(bf_cq_stats, aes(x = Class, y = res_std_error, fill=Class)) +
  geom_violin() +
  theme_bw()+theme(text = element_text(size = 20), legend.position = "null")

p7<-ggarrange(p1, p4, p2, p5, p3, p6, nrow=3, ncol=2, align = "hv")

pdf("CQ_BF_performance_EastRiver.pdf", width = 12, height = 12)

annotate_figure(p7, top = text_grob("East River", size=20))

dev.off()

pdf("CQmonthly_BF_EastRiver.pdf", width = 15, height = 12)

ggplot(ER_CQ_bf_monthly, aes(mean_monthly_bf, slope))+geom_point()+geom_smooth(method = "lm", se=F, col="black", size=0.5)+
  facet_wrap(~Element, scales = "free")+
  theme(text = element_text(size=15))+theme_bw()

dev.off()

ecdf_fun<-ecdf(bf_cq_stats$r2)

threshold <- 0.5
df_high <- bf_cq_stats[ecdf_fun(bf_cq_stats$r2) > threshold,]

p1<-ggplot(df_high, aes(Class, fill=Class))+geom_bar(stat="count")+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  ggtitle("East River")+labs(x="")

p2<-ggplot(df_high, aes(Class, r2, fill=Class))+geom_boxplot()+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="")

p3<-ggplot(df_high, aes(Class, res_std_error, fill=Class))+geom_boxplot()+
  theme_classic()+theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="")

pdf("CDF_Stats_EastRiver.pdf", width = 6, height = 10)

ggarrange(p1, p2, p3, nrow = 3)

dev.off()

pdf("CQmonthly_BF_CDF50_EastRiver.pdf", width = 15, height = 12)

ER_CQ_bf_monthly %>%
  dplyr::filter(Element %in% df_high$Element) %>%
  ggplot(aes(mean_monthly_bf, slope))+geom_point()+
  geom_smooth(method = "lm", se=F, col="black", size=0.5)+
  facet_wrap(~Element, scales = "free")+
  theme(text = element_text(size=15))+theme_bw()

dev.off()


