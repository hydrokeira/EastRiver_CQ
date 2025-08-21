require(dplyr)
require(zoo)
require(ggplot2)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

kep_solutes<-read.csv("Solutes_Retained.csv")
solute_class<-read.csv("solute_class_updated.csv")

#make top figure
ERCQ<-read.csv("PH_CQ.csv")

ERCQ <- ERCQ %>%
  filter(variable %in% kep_solutes$solutes & year(as.Date(date)) > 2015) %>%
  group_by(variable) %>%
  mutate(sd_sol=sd(value), mean_sol=mean(value), lower=mean_sol - 2*sd_sol, upper=mean_sol + 2*sd_sol) %>%
  ungroup() %>%
  filter(value > lower & value < upper)

unique(ERCQ$Element)

ERCQ<-left_join(ERCQ[,c(2:5,11)], solute_class)

ERCQ_norm<-ERCQ %>%
  group_by(Class) %>%
  mutate(norm_value=scale(value))

ERCQ_norm<-ERCQ_norm %>%
  mutate(discharge=scale(discharge))

ERCQ_norm$date<-as.Date(ERCQ_norm$date)

ERCQ_norm <- ERCQ_norm %>%
  mutate(DOY = yday(date))

# Step 2: Summarize by DOY and Class
summary_data <- ERCQ_norm %>%
  group_by(DOY, Class) %>%
  summarise(mean_log = mean(norm_value, na.rm = TRUE),
            sd_log = sd(norm_value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(lower = mean_log - sd_log,
         upper = mean_log + sd_log)

summary_data <- summary_data %>%
  group_by(Class) %>%
  mutate(moving_avg=rollmean(mean_log, k=7, fill=NA, align = "center"))

sum_data_q<-ERCQ_norm %>%
  group_by(DOY) %>%
  summarise(mean_q=mean(discharge, na.rm = TRUE)) %>%
  mutate(moving_q=rollmean(mean_q, k=7, fill=NA, align = "center"))

summary_data$Class<-factor(summary_data$Class, levels = c("geogenic", "metal", "biogenic"))

all_sum<-left_join(summary_data, sum_data_q)

# Step 3: Plot
p3<-ggplot() +
  #geom_ribbon(summary_data, mapping=aes(x=DOY, ymin = lower, ymax = upper), alpha = 0.1, color=NA) +
  geom_ribbon(sum_data_q, mapping = aes(x=DOY, ymax=moving_q, ymin=min(mean_q)), color=NA, alpha=0.15, fill="black")+
  geom_line(summary_data, mapping=aes(x = DOY, y = moving_avg, color = Class)) +
  #geom_point(data = ERCQ_norm, aes(x = DOY, y = log(norm_value), color = Class), alpha = 0.3, size = 0.7) +
  theme_classic() +
  labs(x = "Day of Year", y = "", col="Solute Class")+
  theme(text = element_text(size = 20))+
  scale_color_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  scale_fill_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  ylim(-0.8,2.3)

p3

summary_CQ <- ERCQ %>%
  group_by(date, Class, discharge) %>%
  summarise(mean_log = mean(value, na.rm = TRUE),
            sd_log = sd(value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(lower = mean_log - sd_log,
         upper = mean_log + sd_log)

p4<-ggplot(summary_CQ, aes(log(discharge), log(mean_log), col=Class)) +
  geom_point()+
  theme_classic() +
  labs(x = "Log[Discharge (cms)]", y="", col="Solute Class")+
  theme(text = element_text(size = 20))+
  scale_color_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  scale_fill_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  geom_smooth(method = "lm", se=F)+ylim(0,10)

p4

ERCQ %>%
  filter(Class=="metal") %>%
  ggplot(aes(log(discharge), log(value), col=as.factor(month(date)))) + facet_wrap(~Element, scales = "free")+
  geom_point()+
  theme_classic() +
  labs(x = "Log[Discharge (cms)]", y="Log[Solute Concentration (ppb)]", col="Month")+
  theme(text = element_text(size = 20))+
  scale_color_manual(values = cet_pal(12, "cbtc1"))

ERCQ %>%
  filter(Class=="biogenic") %>%
  ggplot(aes(log(discharge), log(value), col=as.factor(month(date)))) + facet_wrap(~Element, scales = "free")+
  geom_point()+
  theme_classic() +
  labs(x = "Log[Discharge (cms)]", y="Log[Solute Concentration (ppb)]", col="Month")+
  theme(text = element_text(size = 20))+
  scale_color_manual(values = cet_pal(12, "cbtc1"))

ERCQ %>%
  group_by(Element, Class) %>%
  summarise(slope=coef(lm(log(value)~log(discharge)))[2]) %>%
  group_by(Class) %>%
  summarise(mean_slope=mean(slope))

ER_test<-ERCQ %>%
  group_by(Element, Class) %>%
  summarise(slope=coef(lm(log(value)~log(discharge)))[2])

ERCQ_slopes<-left_join(ERCQ, ER_test)

ERCQ_slopes$element_slope<-paste0(ERCQ_slopes$Element, " (b = ", round(ERCQ_slopes$slope, 2), ")")

pdf("ER_allSolutes_CQ.pdf", width = 14, height = 10)

ggplot(ERCQ_slopes, aes(log(discharge), log(value), col=Class)) + facet_wrap(~element_slope, scales = "free")+
  geom_point()+
  theme_classic() +
  labs(x = "Log[Discharge (cms)]", y="Log[Mean Solute Concentration (ppb)]", col="Solute Class")+
  theme(text = element_text(size = 20), legend.position = "null")+
  scale_color_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  scale_fill_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  geom_smooth(method = "lm", se=F)

dev.off()  

###Coal Creek
ERCQ<-read.csv("CoalCreek_CQ.csv")

ERCQ <- ERCQ %>%
  filter(variable %in% kep_solutes$solutes & year(as.Date(date)) > 2015) %>%
  group_by(variable) %>%
  mutate(sd_sol=sd(value), mean_sol=mean(value), lower=mean_sol - 2*sd_sol, upper=mean_sol + 2*sd_sol) %>%
  ungroup() %>%
  filter(value > lower & value < upper)

ERCQ<-left_join(ERCQ[,c(2:5,11)], solute_class)

ERCQ_norm<-ERCQ %>%
  group_by(Class) %>%
  mutate(norm_value=scale(value))

ERCQ_norm<-ERCQ_norm %>%
  mutate(CC_Q_cms=scale(CC_Q_cms))

ERCQ_norm$date<-as.Date(ERCQ_norm$date)

ERCQ_norm <- ERCQ_norm %>%
  mutate(DOY = yday(date))

ERCQ %>%
  filter(Class=="metal") %>%
  ggplot(aes(yday(date), value))+geom_point()+facet_wrap(~Element, scales = "free")

mine_metals<-c("Al","Co","Cu","Mn","Ni","Zn")

# Step 2: Summarize by DOY and Class
summary_data <- ERCQ_norm %>%
  group_by(DOY, Class) %>%
  summarise(mean_log = mean(norm_value, na.rm = TRUE),
            sd_log = sd(norm_value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(lower = mean_log - sd_log,
         upper = mean_log + sd_log)

summary_data <- summary_data %>%
  group_by(Class) %>%
  mutate(moving_avg=rollmean(mean_log, k=7, fill=NA, align = "center"))

sum_data_q<-ERCQ_norm %>%
  group_by(DOY) %>%
  summarise(mean_q=mean(CC_Q_cms, na.rm = TRUE)) %>%
  mutate(moving_q=rollmean(mean_q, k=7, fill=NA, align = "center"))

# Step 3: Plot
p5<-ggplot() +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, color=NA) +
  geom_ribbon(sum_data_q, mapping = aes(x=DOY, ymax=moving_q, ymin=min(mean_q)), color=NA, alpha=0.15, fill="black")+
  geom_line(summary_data, mapping=aes(x = DOY, y = moving_avg, color = Class)) +
  #geom_point(data = coal_cq_norm, aes(x = DOY, y = log(norm_value), color = Class), alpha = 0.3, size = 0.7) +
  theme_classic() +
  labs(x = "Day of Year", y = "Z-Score Normalized \n Concentration & Discharge")+
  theme(text = element_text(size = 20), legend.position = "null")+
  scale_color_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  scale_fill_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  ylim(-0.8,2.3)

p5

summary_CQ <- ERCQ %>%
  group_by(date, Class, CC_Q_cms) %>%
  summarise(mean_log = mean(value, na.rm = TRUE),
            sd_log = sd(value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(lower = mean_log - sd_log,
         upper = mean_log + sd_log)

p6<-ggplot(summary_CQ, aes(log(CC_Q_cms), log(mean_log), col=Class)) +
  geom_point()+
  theme_classic() +
  labs(x = "Log[Discharge (cms)]", y="Log[Mean Solute \nConcentration (ppb)]", col="Solute Class")+
  theme(text = element_text(size = 20), legend.position = "null")+
  scale_color_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  scale_fill_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  geom_smooth(method = "lm", se=F)+ylim(0,10)

p6

ERCQ %>%
  filter(Class=="metal") %>%
  ggplot(aes(log(CC_Q_cms), log(value), col=as.factor(month(date)))) + facet_wrap(~Element, scales = "free")+
  geom_point()+
  theme_classic() +
  labs(x = "Log[Discharge (cms)]", y="Log[Solute Concentration (ppb)]", col="Month")+
  theme(text = element_text(size = 20))+
  scale_color_manual(values = cet_pal(12, "cbtc1"))

ERCQ %>%
  filter(Class=="biogenic") %>%
  ggplot(aes(log(CC_Q_cms), log(value), col=as.factor(month(date)))) + facet_wrap(~Element, scales = "free")+
  geom_point()+
  theme_classic() +
  labs(x = "Log[Discharge (cms)]", y="Log[Solute Concentration (ppb)]", col="Month")+
  theme(text = element_text(size = 20))+
  scale_color_manual(values = cet_pal(12, "cbtc1"))

ERCQ %>%
  filter(Element %in% c("Mg")) %>%
  ggplot(aes(log(CC_Q_cms), log(value), col=as.factor(month(date)))) + facet_wrap(~Element, scales = "free")+
  geom_point()+
  theme_classic() +
  labs(x = "Log[Discharge (cms)]", y="Log[Solute Concentration (ppb)]", col="Month")+
  theme(text = element_text(size = 20))+
  scale_color_manual(values = cet_pal(12, "cbtc1"))

ER_test<-ERCQ %>%
  group_by(Element, Class) %>%
  summarise(slope=coef(lm(log(value)~log(CC_Q_cms)))[2])

ERCQ_slopes<-left_join(ERCQ, ER_test)

ERCQ_slopes$element_slope<-paste0(ERCQ_slopes$Element, " (b = ", round(ERCQ_slopes$slope, 2), ")")

pdf("CC_allSolutes_CQ.pdf", width = 14, height = 10)

ggplot(ERCQ_slopes, aes(log(CC_Q_cms), log(value), col=Class)) + facet_wrap(~element_slope, scales = "free")+
  geom_point()+
  theme_classic() +
  labs(x = "Log[Discharge (cms)]", y="Log[Mean Solute Concentration (ppb)]", col="Solute Class")+
  theme(text = element_text(size = 20), legend.position = "null")+
  scale_color_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  scale_fill_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  geom_smooth(method = "lm", se=F)

dev.off()  


ERCQ %>%
  group_by(Element, Class) %>%
  summarise(slope=coef(lm(log(value)~log(CC_Q_cms)))[2]) %>%
  group_by(Class) %>%
  summarise(mean_slope=mean(slope))

cc<-ggarrange(p5, p6, align = "v", nrow = 2)
cc

er<-ggarrange(p3, p4, align = "v", nrow = 2)
er

pdf("AvgSoluteClassBehaviour.pdf", width = 12, height = 8)

ggarrange(cc, er, align="h", widths = c(0.42, 0.55))

dev.off()
