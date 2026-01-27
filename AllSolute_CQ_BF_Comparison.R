setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

coal_chem<-read.csv("CoalCreek_CQ.csv")

kept_solutes<-c("Ba", "Ca", "Cl", "DIC", "Mg", "Si", "Na", "Sr", "SO4", "U", "Al", "As", "Co", "Cu",
                "Fe", "Mn", "Ni", "V", "Zn", "DOC", "NO3", "K")

coal_cq <- coal_chem %>%
  filter(Element %in% kept_solutes) %>%
  mutate(Class=case_when(
    Element=="Mn"~"metal",
    Element=="DIC"~"geogenic",
    .default = Class
  ))

coal_cq$date<-as.Date(coal_cq$date)

coal_cq <- coal_cq %>%
  mutate(DOY = yday(date))

# Step 2: Summarize by DOY and Class
summary_data <- coal_cq %>%
  group_by(DOY, Element) %>%
  summarise(mean_log = mean(log(value), na.rm = TRUE),
            sd_log = sd(log(value), na.rm = TRUE),
            .groups = "drop") %>%
  mutate(lower = mean_log - sd_log,
         upper = mean_log + sd_log)

baseflow<-read.csv("CC_bf_prop.csv")
baseflow$date<-as.Date(baseflow$date)

coal_cq_bf<-left_join(coal_cq, baseflow[,c(3,12)])

coal_cq_bf <- coal_cq_bf %>%
  filter(!duplicated(paste(date, Element)))

coal_cq_bf_monthly <-coal_cq_bf %>%
  group_by(Element, month(date), Class) %>%
  summarise(mean_bf=mean(bf_percent, na.rm = T), cq_slope=coef(lm(log(value)~log(CC_Q_cms)))[2])

coal_cq_bf_results <- coal_cq_bf_monthly %>%
  group_by(Element, Class) %>%
  summarise(
    model = list(lm(cq_slope ~ mean_bf, data = cur_data())),
    .groups = "drop"
  ) %>%
  mutate(
    bf_effect = map_dbl(model, ~ coef(.x)[2]),
    p_val     = map_dbl(model, ~ summary(.x)$coefficients[2, 4]),
    r2        = map_dbl(model, ~ summary(.x)$r.squared),
    n         = map_int(model, ~ length(.x$fitted.values))
  ) %>%
  select(-model) %>%
  mutate(sig=case_when(
    p_val < 0.05 ~"significant",
    .default = "non significant"
  ))

coal_cq_bf_monthly<-left_join(coal_cq_bf_monthly, coal_cq_bf_results)

pdf("AllSolutes_CQ_BF_Relationship_CC.pdf", width = 14, height = 10)

ggplot(coal_cq_bf_monthly, aes(mean_bf, cq_slope, col=Class))+
  geom_point(size=4, alpha=0.5, position = position_jitter(width = 0.02, height = 0))+theme_classic()+
  theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="Baseflow Proportion", y="C-Q Slope", col="")+geom_hline(yintercept = 0)+
  xlim(0,1)+
  geom_smooth(coal_cq_bf_monthly, mapping=aes(mean_bf, cq_slope, color=Class, lty=sig), method = "lm", se=F)+
  scale_color_manual(values = c("biologic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  #ylim(-1.5,1)+
  ggtitle("Coal Creek")+facet_wrap(~Element)+
  scale_linetype_manual(values = c("significant" = "solid", "non significant" = "blank"))

dev.off()



setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

coal_chem<-read.csv("PH_CQ.csv")

kept_solutes<-c("Ba", "Ca", "Cl", "DIC", "Mg", "Si", "Na", "Sr", "SO4", "U", "Al", "As", "Co", "Cu",
                "Fe", "Mn", "Ni", "V", "Zn", "DOC", "NO3", "K")

coal_cq <- coal_chem %>%
  filter(Element %in% kept_solutes) %>%
  mutate(Class=case_when(
    Element=="Mn"~"metal",
    Element=="DIC"~"geogenic",
    .default = Class
  ))

coal_cq$date<-as.Date(coal_cq$date)

coal_cq <- coal_cq %>%
  mutate(DOY = yday(date))

# Step 2: Summarize by DOY and Class
summary_data <- coal_cq %>%
  group_by(DOY, Element) %>%
  summarise(mean_log = mean(log(value), na.rm = TRUE),
            sd_log = sd(log(value), na.rm = TRUE),
            .groups = "drop") %>%
  mutate(lower = mean_log - sd_log,
         upper = mean_log + sd_log)

baseflow<-read.csv("PH_bf_prop.csv")
baseflow$date<-as.Date(baseflow$date)

coal_cq_bf<-left_join(coal_cq, baseflow[,c(3,10)])

coal_cq_bf <- coal_cq_bf %>%
  filter(!duplicated(paste(date, Element)))

coal_cq_bf_monthly <-coal_cq_bf %>%
  group_by(Element, month(date), Class) %>%
  summarise(mean_bf=mean(bf_percent, na.rm = T), cq_slope=coef(lm(log(value)~log(discharge)))[2])

coal_cq_bf_results <- coal_cq_bf_monthly %>%
  group_by(Element, Class) %>%
  summarise(
    model = list(lm(cq_slope ~ mean_bf, data = cur_data())),
    .groups = "drop"
  ) %>%
  mutate(
    bf_effect = map_dbl(model, ~ coef(.x)[2]),
    p_val     = map_dbl(model, ~ summary(.x)$coefficients[2, 4]),
    r2        = map_dbl(model, ~ summary(.x)$r.squared),
    n         = map_int(model, ~ length(.x$fitted.values))
  ) %>%
  select(-model) %>%
  mutate(sig=case_when(
    p_val < 0.05 ~"significant",
    .default = "non significant"
  ))

coal_cq_bf_monthly<-left_join(coal_cq_bf_monthly, coal_cq_bf_results)

pdf("AllSolutes_CQ_BF_Relationship_ER.pdf", width = 14, height = 10)

ggplot(coal_cq_bf_monthly, aes(mean_bf, cq_slope, col=Class))+
  geom_point(size=4, alpha=0.5, position = position_jitter(width = 0.02, height = 0))+theme_classic()+
  theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="Baseflow Proportion", y="C-Q Slope", col="")+geom_hline(yintercept = 0)+
  xlim(0,1)+
  geom_smooth(coal_cq_bf_monthly, mapping=aes(mean_bf, cq_slope, color=Class, lty=sig), method = "lm", se=F)+
  scale_color_manual(values = c("biologic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  #ylim(-1.5,1)+
  ggtitle("East River")+facet_wrap(~Element)+
  scale_linetype_manual(values = c("significant" = "solid", "non significant" = "blank"))

dev.off()

