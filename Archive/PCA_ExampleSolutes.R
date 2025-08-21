require(ggpubr)
require(EflowStats)
require(lubridate)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

coal_chem<-read.csv("CoalCreek_CQ.csv")

coal_cq<-coal_chem %>%
  filter(Element %in% c("DOC", "Mg", "Zn"))

coal_cq <- coal_cq %>%
  mutate(Class=case_when(
    Element=="Mn"~"metal",
    .default = Class
  ))

coal_cq$date<-as.Date(coal_cq$date)

coal_cq <- coal_cq %>%
  mutate(DOY = yday(date))

# Step 2: Summarize by DOY and Class
summary_data <- coal_cq %>%
  group_by(DOY, Class) %>%
  summarise(mean_log = mean(log(value), na.rm = TRUE),
            sd_log = sd(log(value), na.rm = TRUE),
            .groups = "drop") %>%
  mutate(lower = mean_log - sd_log,
         upper = mean_log + sd_log)

# Step 3: Plot
p1<-ggplot(summary_data, aes(x = DOY, y = mean_log, color = Class, fill = Class)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(size = 1) +
  geom_point(data = coal_cq, aes(x = DOY, y = log(value), color = Class), alpha = 0.3, size = 0.7) +
  theme_classic() +
  labs(x = "Day of Year", y = "log(Concentration)")+
  theme(text = element_text(size = 20), legend.position = "null")+
  ggtitle("Coal Creek")+
  scale_color_manual(values = c("biologic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  scale_fill_manual(values = c("biologic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  ylim(-5,10)

p1

p2<-ggplot(coal_cq, aes(log(CC_Q_cms), log(value), col=Class))+geom_point(alpha=0.5)+
  theme_classic()+labs(x="log(Q)", y="log(Concentration)")+
  geom_smooth(se=F, method = "lm")+
  theme(text = element_text(size = 20), legend.position = "null")+
  scale_color_manual(values = c("biologic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  ylim(-5,10)
p2

baseflow<-read.csv("CC_bf_prop.csv")
baseflow$date<-as.Date(baseflow$date)

coal_cq_bf<-left_join(coal_cq, baseflow[,c(3,12)])

coal_cq_bf <- coal_cq_bf %>%
  filter(!duplicated(paste(date, Element)))

coal_cq_bf_monthly <-coal_cq_bf %>%
  group_by(Element, month(date)) %>%
  summarise(mean_bf=mean(bf_percent, na.rm = T), cq_slope=coef(lm(log(value)~log(CC_Q_cms)))[2])

lm<-coal_cq_bf_monthly %>%
  filter(Element=="Mg") %>%
  lm(cq_slope~mean_bf,data=.)

summary(lm)

coal_cq_bf_monthly_geo<-subset(coal_cq_bf_monthly, coal_cq_bf_monthly$Element=="Mg")

p3<-ggplot(coal_cq_bf_monthly, aes(mean_bf, cq_slope, col=Element))+
  geom_point(size=4, alpha=0.5, position = position_jitter(width = 0.02, height = 0))+theme_classic()+
  theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="Baseflow Proportion", y="C-Q Slope", col="")+geom_hline(yintercept = 0)+
  xlim(0,1)+
  geom_smooth(coal_cq_bf_monthly_geo, mapping=aes(mean_bf, cq_slope, col=Element), method = "lm", se=F)+
  scale_color_manual(values = c("DOC"="dodgerblue", "Zn"="goldenrod", "Mg"="firebrick"))+
  ylim(-1.5,1)+ggtitle("Coal Creek")

p3

ggarrange(p1, p2, p3, nrow = 3)


ER_chem<-read.csv("PH_CQ.csv")

ER_cq<-ER_chem %>%
  filter(Element %in% c("DOC", "Mg", "Zn"))

ER_cq <- ER_cq %>%
  mutate(Class=case_when(
    Element=="Mn"~"metal",
    .default = Class
  ))

ER_cq$date<-as.Date(ER_cq$date)

ER_cq <- ER_cq %>%
  mutate(DOY = yday(date))

# Step 2: Summarize by DOY and Class
summary_data <- ER_cq %>%
  group_by(DOY, Class) %>%
  summarise(mean_log = mean(log(value), na.rm = TRUE),
            sd_log = sd(log(value), na.rm = TRUE),
            .groups = "drop") %>%
  mutate(lower = mean_log - sd_log,
         upper = mean_log + sd_log)

# Step 3: Plot
p4<-ggplot(summary_data, aes(x = DOY, y = mean_log, color = Class, fill = Class)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(size = 1) +
  geom_point(data = ER_cq, aes(x = DOY, y = log(value), color = Class), alpha = 0.3, size = 0.7) +
  theme_classic() +
  labs(x = "Day of Year", y = "log(Concentration)")+
  theme(text = element_text(size = 20), legend.position = "null")+
  ggtitle("East River")+
  scale_color_manual(values = c("biologic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  scale_fill_manual(values = c("biologic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  ylim(-5,10)
p4

p5<-ggplot(ER_cq, aes(log(discharge), log(value), col=Class))+geom_point(alpha=0.5)+
  theme_classic()+labs(x="log(Q)", y="log(Concentration)")+
  geom_smooth(se=F, method = "lm")+
  theme(text = element_text(size = 20), legend.position = "null")+
  scale_color_manual(values = c("biologic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  ylim(-5,10)

p5

ER_cq$date<-as.Date(ER_cq$date)

baseflow<-read.csv("PH_bf_prop.csv")
baseflow$date<-as.Date(baseflow$date)

ER_cq_bf<-left_join(ER_cq, baseflow[,c(3,10)])

ER_cq_bf <- ER_cq_bf %>%
  filter(!duplicated(paste(date, Element)))

ER_cq_bf_monthly <-ER_cq_bf %>%
  group_by(Element, month(date)) %>%
  summarise(mean_bf=mean(bf_percent, na.rm = T), cq_slope=coef(lm(log(value)~log(discharge)))[2])

lm<-ER_cq_bf_monthly %>%
  filter(Element=="Mg") %>%
  lm(cq_slope~mean_bf,data=.)

summary(lm)

ER_cq_bf_monthly_geo<-subset(ER_cq_bf_monthly, ER_cq_bf_monthly$Element=="Mg")

ER_cq_bf_monthly$Element<-factor(ER_cq_bf_monthly$Element, levels=c("Mg","Zn","DOC"))

p6<-ggplot(ER_cq_bf_monthly, aes(mean_bf, cq_slope, col=Element))+
  geom_point(size=4, alpha=0.5, position = position_jitter(width = 0.02, height = 0))+theme_classic()+
  theme(text = element_text(size = 20))+
  labs(x="Baseflow Proportion", y="", col="")+geom_hline(yintercept = 0)+
  xlim(0,1)+
  geom_smooth(ER_cq_bf_monthly_geo, mapping=aes(mean_bf, cq_slope, col=Element), method = "lm", se=F)+
  scale_color_manual(values = c("DOC"="dodgerblue", "Zn"="goldenrod", "Mg"="firebrick"))+
  ylim(-1.5, 1)+ggtitle("East River")

pdf("ER_CQ_bf_solute.pdf", width = 10, height = 4)

ggarrange(p3, p6, widths = c(0.5, 0.6))

dev.off()

pdf("CC_ER_ThreeSolutes.pdf", width = 8, height = 10)

ggarrange(p1, p4, p2, p5, p3, p6, nrow = 3, ncol=2)

dev.off()
