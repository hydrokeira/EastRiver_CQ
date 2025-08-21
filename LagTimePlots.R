require(dplyr)
require(ggplot2)
require(lubridate)
require(ggpubr)

###for Coal Creek
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

kep_solutes<-read.csv("Solutes_Retained.csv")

#read in Coal Creek and ER data and then just change the plot calls (p1, p2 - East River; p3, p4 - Coal Creek)
#need to change "discharge" to "CC_Q_cms"
ERCQ<-read.csv("CoalCreek_CQ.csv")

ERCQ <- ERCQ %>%
  filter(variable %in% kep_solutes$solutes & year(as.Date(date)) > 2015) %>%
  select(-c(Class)) %>%
  group_by(variable) %>%
  mutate(sd_sol=sd(value), mean_sol=mean(value), lower=mean_sol - 2*sd_sol, upper=mean_sol + 2*sd_sol) %>%
  ungroup() %>%
  filter(value > lower & value < upper) %>%
  left_join(solute_class)

ER_Q<-read.csv("Coal_Creek_wy15_24_daily.csv")
ER_Q$date<-as.Date(ER_Q$Date, "%m/%d/%y")

ggplot(ERCQ, aes(x=as.Date(date), y=value))+geom_point()+geom_line(ER_Q, mapping=aes(date, CC_Q_cms))+
  facet_wrap(~variable, scales = "free")

mean_q<-ER_Q %>%
  group_by(yday(as.Date(date))) %>%
  summarise(mean_val=mean(CC_Q_cms))

mean_q_expanded <- expand_grid(
  Element = unique(ERCQ$Element),
  mean_q
)

y_ranges <- ERCQ %>%
  group_by(Element) %>%
  summarise(ymin = min(value, na.rm = TRUE),
            ymax = max(value, na.rm = TRUE))

# Step 2: Join y-range to mean_q and rescale discharge accordingly
mean_q_scaled <- mean_q_expanded %>%
  left_join(y_ranges) %>%
  group_by(Element) %>%
  mutate(scaled_val = rescale(mean_val, to = range(c(ymin[1], ymax[1])))) %>%
  ungroup()

pdf("CC_timeseries_allsolutes.pdf", width = 16, height = 10)

# Step 3: Plot raw ERCQ values and scaled discharge overlay
ggplot(ERCQ, aes(x = yday(as.Date(date)), y = value)) +
  geom_area(data = mean_q_scaled,
            aes(x = `yday(as.Date(date))`, y = scaled_val),
            fill = "grey", alpha=0.5) +
  geom_point(aes(col = Class), alpha=0.8, size=0.7) +
  facet_wrap(~Element, scales = "free_y") +
  labs(y = "Concentration (ppb)", x = "Day of Year") +
  theme_classic()+
  scale_color_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  theme(text = element_text(size=20))

dev.off()

ERCQ_max<-ERCQ %>%
  group_by(variable, year(as.Date(date))) %>%
  slice_max(value, with_ties = F) %>%
  mutate(yday_solute=yday(date)) %>%
  distinct()

ERCQ_max<-ERCQ_max[,c(5,16,17)]

ERCQ_min<-ERCQ %>%
  group_by(variable, year(as.Date(date))) %>%
  slice_min(value, with_ties = F) %>%
  mutate(yday_solute=yday(date)) %>%
  distinct()

ERCQ_min<-ERCQ_min[,c(5,16,17)]

peak_day<-ER_Q %>%
  group_by(year(as.Date(date))) %>%
  mutate(yday_discharge=yday(date)) %>%
  slice_max(CC_Q_cms, with_ties = F)

ERCQ_max<-left_join(ERCQ_max, peak_day[,c(2,4,5)])
ERCQ_min<-left_join(ERCQ_min, peak_day[,c(2,4,5)])

solute_class<-read.csv("solute_class_updated.csv")

ERCQ_max_CC <- ERCQ_max %>%
  mutate(solute_lag = yday_solute-yday_discharge, site="Coal Creek") %>%
  left_join(solute_class)

ERCQ_min_CC <- ERCQ_min %>%
  mutate(solute_lag = yday_solute-yday_discharge, site="Coal Creek") %>%
  left_join(solute_class)

###for East River
ERCQ<-read.csv("PH_CQ.csv")

ERCQ <- ERCQ %>%
  select(-c(Class)) %>%
  filter(variable %in% kep_solutes$solutes & year(as.Date(date)) > 2015) %>%
  group_by(variable) %>%
  mutate(sd_sol=sd(value), mean_sol=mean(value), lower=mean_sol - 2*sd_sol, upper=mean_sol + 2*sd_sol) %>%
  ungroup() %>%
  filter(value > lower & value < upper) %>%
  left_join(solute_class)

ER_Q<-read.csv("Daily_ER_PH_Q.csv")

mean_q<-ER_Q %>%
  group_by(yday(as.Date(date))) %>%
  summarise(mean_val=mean(daily_q_cms))

ggplot(ERCQ, aes(x=yday(as.Date(date)), y=value))+geom_point(aes(col=Class))+
  geom_line(mean_q, mapping=aes(`yday(as.Date(date))`, mean_val))+
  facet_wrap(~variable, scales = "free")

mean_q_expanded <- expand_grid(
  Element = unique(ERCQ$Element),
  mean_q
)

y_ranges <- ERCQ %>%
  group_by(Element) %>%
  summarise(ymin = min(value, na.rm = TRUE),
            ymax = max(value, na.rm = TRUE))

# Step 2: Join y-range to mean_q and rescale discharge accordingly
mean_q_scaled <- mean_q_expanded %>%
  left_join(y_ranges) %>%
  group_by(Element) %>%
  mutate(scaled_val = rescale(mean_val, to = range(c(ymin[1], ymax[1])))) %>%
  ungroup()

pdf("ER_timeseries_allsolutes.pdf", width = 14, height = 10)

# Step 3: Plot raw ERCQ values and scaled discharge overlay
ggplot(ERCQ, aes(x = yday(as.Date(date)), y = value)) +
  geom_area(data = mean_q_scaled,
            aes(x = `yday(as.Date(date))`, y = scaled_val),
            fill = "grey", alpha=0.5) +
  geom_point(aes(col = Class), alpha=0.8, size=0.7) +
  facet_wrap(~Element, scales = "free_y") +
  labs(y = "Concentration (ppb)", x = "Day of Year") +
  theme_classic()+
  scale_color_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  theme(text = element_text(size=20))

dev.off()

ERCQ_max<-ERCQ %>%
  group_by(variable, year(as.Date(date))) %>%
  slice_max(value) %>%
  mutate(yday_solute=yday(date)) %>%
  distinct()

ERCQ_max<-ERCQ_max[,c(5,16,17)]

ERCQ_min<-ERCQ %>%
  group_by(variable, year(as.Date(date))) %>%
  slice_min(value) %>%
  mutate(yday_solute=yday(date)) %>%
  distinct()

ERCQ_min<-ERCQ_min[,c(5,16,17)]

peak_day<-ER_Q %>%
  group_by(year(as.Date(date))) %>%
  mutate(yday_discharge=yday(date)) %>%
  slice_max(daily_q_cms)

ERCQ_max<-left_join(ERCQ_max, peak_day[,c(3,4,5)])
ERCQ_min<-left_join(ERCQ_min, peak_day[,c(3,4,5)])

solute_class<-read.csv("solute_class_updated.csv")

ERCQ_max_ER <- ERCQ_max %>%
  mutate(solute_lag = yday_solute-yday_discharge, site="East River") %>%
  left_join(solute_class)

ERCQ_min_ER <- ERCQ_min %>%
  mutate(solute_lag = yday_solute-yday_discharge, site="East River") %>%
  left_join(solute_class)

ERCQ_min_tot<-bind_rows(ERCQ_min_CC, ERCQ_min_ER)
ERCQ_max_tot<-bind_rows(ERCQ_max_CC, ERCQ_max_ER)

ERCQ_max_tot$Class<-factor(ERCQ_max_tot$Class, levels = c("geogenic", "metal", "biogenic"))
ERCQ_min_tot$Class<-factor(ERCQ_min_tot$Class, levels = c("geogenic", "metal", "biogenic"))

lag_sum<-ERCQ_max_tot %>%
  group_by(Element, Class, site) %>%
  summarise(mean_lag=mean(solute_lag))

lag_sum<-ERCQ_min_CC %>%
  group_by(Element) %>%
  summarise(mean_lag=mean(solute_lag))

lag_sum<-ERCQ_max_CC %>%
  group_by(Class) %>%
  summarise(mean_lag=mean(solute_lag))

p2<-ERCQ_max_tot %>%
  filter(Class %in% c("metal", "biogenic")) %>%
  ggplot(aes(Class, solute_lag))+geom_boxplot(outliers=F, aes(fill=site), alpha=0.3)+
  geom_jitter(aes(col=site), position=position_jitterdodge(jitter.width = 0.3,
                                                           jitter.height = 0.3,
                                                           dodge.width = 0.9), size=3)+
  theme_classic()+geom_hline(yintercept =0)+
  labs(y="Lag between Peak Q and Max [Solute] (days)", x="", col="", fill="", tag="b")+
  theme(text = element_text(size = 20))+
  scale_fill_manual(values = c("Coal Creek"="grey20", "East River"="grey70"))+
  scale_color_manual(values = c("Coal Creek"="grey20", "East River"="grey70"))+
  ylim(-135,205)

p2

ERCQ_max_tot %>%
  filter(Class %in% c("metal", "biogenic")) %>%
  ggplot(aes(Class, solute_lag))+geom_boxplot(outliers=F, aes(fill=site), alpha=0.3)+
  geom_jitter(aes(col=site), position=position_jitterdodge(jitter.width = 0.3,
                                                           jitter.height = 0.3,
                                                           dodge.width = 0.9))+
  theme_classic()+geom_hline(yintercept =0)+
  labs(y="Lag between Max [Solute] and Peak Q (days)", x="")+
  facet_wrap(~`year(as.Date(date))`)+
  theme(text = element_text(size = 20))+
  scale_fill_manual(values = c("Coal Creek"="grey20", "East River"="grey70"))+
  scale_color_manual(values = c("Coal Creek"="grey20", "East River"="grey70"))

p1<-ERCQ_min_tot %>%
  filter(Class %in% c("geogenic")) %>%
  ggplot(aes(Class, solute_lag))+geom_boxplot(outliers=F, aes(fill=site), alpha=0.3)+
  geom_jitter(aes(col=site), position=position_jitterdodge(jitter.width = 0.3,
                                                           jitter.height = 0.3,
                                                           dodge.width = 0.75), size=3)+
  theme_classic()+geom_hline(yintercept =0)+
  labs(y="Lag between Peak Q and Min [Solute] (days)", x="", tag="a")+
  theme(text = element_text(size = 20), legend.position = "null")+
  scale_fill_manual(values = c("Coal Creek"="grey20", "East River"="grey70"))+
  scale_color_manual(values = c("Coal Creek"="grey20", "East River"="grey70"))+
  ylim(-135,205)

p1

pdf("CCERCQ_LagTimePlots.pdf", width = 12, height = 7)

k1<-ggarrange(p1, p2, widths = c(0.35, 0.7), align = "h")

k1

dev.off()

ERCQ_min_tot %>%
  filter(Class=="geogenic") %>%
  ggplot(aes(Class, solute_lag))+geom_boxplot(outliers=F, aes(fill=site), alpha=0.3)+
  geom_jitter(aes(col=site), position=position_jitterdodge(jitter.width = 0.3,
                                                           jitter.height = 0.3,
                                                           dodge.width = 0.75))+
  theme_classic()+geom_hline(yintercept =0)+
  labs(y="Lag between Min [Solute] and Peak Q (days)", x="")+
  facet_wrap(~`year(as.Date(date))`)+
  scale_fill_manual(values = c("Coal Creek"="grey70", "East River"="grey20"))+
  scale_color_manual(values = c("Coal Creek"="grey70", "East River"="grey20"))+
  theme(text = element_text(size=20))

ERCQ_min_tot %>%
  filter(Class=="metal") %>%
  ggplot(aes(Class, solute_lag))+geom_boxplot(outliers=F, aes(fill=site), alpha=0.3)+
  geom_jitter(aes(col=site), position=position_jitterdodge(jitter.width = 0.3,
                                                           jitter.height = 0.3,
                                                           dodge.width = 0.75))+
  theme_classic()+geom_hline(yintercept =0)+
  labs(y="Lag between Min [Solute] and Peak Q (days)", x="")+
  facet_wrap(~`year(as.Date(date))`)+
  scale_fill_manual(values = c("Coal Creek"="grey70", "East River"="grey20"))+
  scale_color_manual(values = c("Coal Creek"="grey70", "East River"="grey20"))+
  theme(text = element_text(size=20))

ERCQ_min_tot %>%
  filter(Class=="biogenic") %>%
  ggplot(aes(Class, solute_lag))+geom_boxplot(outliers=F, aes(fill=site), alpha=0.3)+
  geom_jitter(aes(col=site), position=position_jitterdodge(jitter.width = 0.3,
                                                           jitter.height = 0.3,
                                                           dodge.width = 0.75))+
  theme_classic()+geom_hline(yintercept =0)+
  labs(y="Lag between Min [Solute] and Peak Q (days)", x="")+
  facet_wrap(~`year(as.Date(date))`)+
  scale_fill_manual(values = c("Coal Creek"="grey70", "East River"="grey20"))+
  scale_color_manual(values = c("Coal Creek"="grey70", "East River"="grey20"))+
  theme(text = element_text(size=20))

ERCQ_min_tot %>%
  filter(Class=="geogenic") %>%
  ggplot(aes(Element, solute_lag))+geom_boxplot(outliers=F, aes(fill=site), alpha=0.2)+
  geom_jitter(aes(col=site), position=position_jitterdodge(jitter.width = 0.3,
                                                           jitter.height = 0.3,
                                                           dodge.width = 0.75))+
  theme_classic()+geom_hline(yintercept =0)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  labs(y="Lag between Min [Solute] and Peak Q (days)", x="")+
  scale_fill_manual(values = c("Coal Creek"="grey70", "East River"="grey20"))+
  scale_color_manual(values = c("Coal Creek"="grey70", "East River"="grey20"))+
  theme(text = element_text(size=20))

ERCQ_max_tot %>%
  filter(Class %in% c("metal")) %>%
  ggplot(aes(Element, solute_lag))+geom_boxplot(outliers=F, aes(fill=site), alpha=0.2)+
  geom_jitter(aes(col=site), position=position_jitterdodge(jitter.width = 0.3,
                                                           jitter.height = 0.3,
                                                           dodge.width = 0.75))+
  theme_classic()+geom_hline(yintercept =0)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  labs(y="Lag between Max [Solute] and Peak Q (days)", x="")+
  scale_fill_manual(values = c("Coal Creek"="grey70", "East River"="grey20"))+
  scale_color_manual(values = c("Coal Creek"="grey70", "East River"="grey20"))+
  theme(text = element_text(size=20))

ERCQ_max_tot %>%
  filter(Class %in% c("biogenic")) %>%
  ggplot(aes(Element, solute_lag))+geom_boxplot(outliers=F, aes(fill=site), alpha=0.2)+
  geom_jitter(aes(col=site), position=position_jitterdodge(jitter.width = 0.3,
                                                           jitter.height = 0.3,
                                                           dodge.width = 0.75))+
  theme_classic()+geom_hline(yintercept =0)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  labs(y="Lag between Max [Solute] and Peak Q (days)", x="")+
  scale_fill_manual(values = c("Coal Creek"="grey70", "East River"="grey20"))+
  scale_color_manual(values = c("Coal Creek"="grey70", "East River"="grey20"))+
  theme(text = element_text(size=20))



