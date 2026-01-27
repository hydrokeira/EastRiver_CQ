#subset data to only include april-october
#identify day of peak flow
#split into 30 intervals before peak flow and after peak flow

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

CC_bf<-read.csv("CC_bf_prop.csv")
CC_bf$date<-as.Date(CC_bf$date)

CC_bf$month<-month(CC_bf$date)

CC_CQ<-read.csv("CoalCreek_CQ.csv")

kept_solutes<-c("Ba", "Ca", "Cl", "DIC", "Mg", "Si", "Na", "Sr", "SO4", "U", "Al", "As", "Co", "Cu",
                "Fe", "Mn", "Ni", "V", "Zn", "DOC", "NO3", "K")

CC_CQ$date<-as.Date(CC_CQ$date)
CC_CQ$month<-month(CC_CQ$date)

flow_months<-seq(4,8,1)

CC_hyst_test<-CC_CQ %>%
  filter(Element %in% kept_solutes) %>%
  dplyr::filter(month %in% flow_months) %>%
  select(date, Element, value) %>%
  distinct()

CC_hyst_test<-left_join(CC_hyst_test[,c("date", "Element", "value")], CC_bf[,c("date", "CC_Q_cms")])

CC_hyst_test <- CC_hyst_test %>%
  filter(!is.na(CC_Q_cms)) %>%
  dplyr::group_by(Element, year(date)) %>%
  mutate(max_Q=max(CC_Q_cms, na.rm = T), date_max_Q=(date[max_Q==CC_Q_cms])[1],
         sd_val=sd(value), mean_val=mean(value)) %>%
  filter(!(value < mean_val - 4*sd_val)) %>%
  filter(!(value > mean_val + 4*sd_val)) %>%
  mutate(min_conc=min(value), max_conc=max(value), scaled_conc = (2 * ((value - min_conc) / (max_conc - min_conc)) - 1)) %>%
  distinct()

#ggplot(CC_hyst_test, aes(date, value))+geom_point(aes(col=time_tag))

CC_hyst_test<-CC_hyst_test %>%
  mutate(time_tag=case_when(
    date < date_max_Q ~ "rising",
    date > date_max_Q ~ "falling",
    .default = "peak"
  )) %>%
  filter(!time_tag=="peak")

k1<-ggplot()+geom_line(CC_bf, mapping=aes(date, CC_Q_cms, group=year(date)))+
  geom_point(CC_hyst_test, mapping=aes(date, CC_Q_cms, col=time_tag))+
  theme_classic()+theme(text = element_text(size = 20))+
  labs(x="Date", y="Discharge (cms)", col="limb")+
  ggtitle("Coal Creek")

CC_hyst_test_binned <- CC_hyst_test %>%
  group_by(Element, `year(date)`, time_tag) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    frac_pos = (row_number() - 1) / (n() - 1),
    interval = cut(
      frac_pos,
      breaks = seq(0, 1, length.out = 21),
      labels = 1:20,
      include.lowest = TRUE
    )
  ) %>%
  group_by(Element, `year(date)`, time_tag, interval) %>%
  summarise(
    mean_value = mean(scaled_conc, na.rm = TRUE),
    .groups = "drop"
  )

bin_counts <- CC_hyst_test_binned %>%
  group_by(Element, `year(date)`, time_tag) %>%
  summarise(
    n_bins = n_distinct(interval))

good_years <- bin_counts %>%
  filter(n_bins == 20) %>%
  mutate(unique_tag=paste(Element, `year(date)`, time_tag))

good_years <- bin_counts %>%
  filter(n_bins == 20) %>%
  group_by(Element) %>%
  count(`year(date)`) %>%        # counts limbs
  filter(n == 2) %>%
  mutate(unique_tag=paste(Element, `year(date)`))

CC_hyst_test_binned_complete <- CC_hyst_test_binned %>%
  mutate(unique_tag2=paste(Element, `year(date)`)) %>%
  filter(unique_tag2 %in% good_years$unique_tag)

CC_hyst_test_binned_HI <- CC_hyst_test_binned_complete %>%
  pivot_wider(names_from  = time_tag, values_from = mean_value) %>%
  group_by(Element, interval, `year(date)`) %>%   # interval 
  summarise(HI = rising - falling)

CC_hyst_test_binned_FI <- CC_hyst_test_binned_complete %>%
  filter(time_tag=="rising" & interval %in% c(1,20)) %>%
  pivot_wider(names_from  = interval, values_from = mean_value) %>%
  group_by(Element, `year(date)`) %>%   # interval 
  summarise(FI = `20` - `1`)

CC_hyst_all<-left_join(CC_hyst_test_binned_HI, CC_hyst_test_binned_FI)

CC_hyst_all_avg<-CC_hyst_all %>%
  group_by(Element) %>%
  summarise(mean_HI=mean(HI), mean_FI=mean(FI))

solute_class<-read.csv("solute_class_updated.csv")

CC_hyst_all_avg<-left_join(CC_hyst_all_avg, solute_class)

CC_hyst_all_avg_solutes<-CC_hyst_all_avg %>%
  filter(Element %in% c("Mg", "Zn", "DOC"))

p1<-ggplot(CC_hyst_all_avg, aes(mean_FI, mean_HI))+
  lims(x=c(-2,2), y=c(-2,2))+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
  geom_hline(yintercept = -0.1, lty="dashed", col="grey")+geom_hline(yintercept = 0.1, lty="dashed", col="grey")+
  geom_vline(xintercept = -0.1, lty="dashed", col="grey")+geom_vline(xintercept = 0.1, lty="dashed", col="grey")+
  geom_point(aes(col=Class), size=3, alpha=0.5)+theme_classic()+theme(text = element_text(size = 20))+
  geom_point(CC_hyst_all_avg_solutes, mapping=aes(col=Class), size=3)+
  labs(x="Flushing Index", y="Hysteresis Index")+
  geom_text_repel(
    aes(label = Element),   # or Stream, Element, etc.
    size = 5,
    box.padding = 0.4,
    point.padding = 0.3,
    max.overlaps = Inf)+
  scale_color_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  ggtitle("Coal Creek")

p1



setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

CC_bf<-read.csv("PH_bf_prop.csv")
CC_bf$date<-as.Date(CC_bf$date)

CC_bf$month<-month(CC_bf$date)

CC_CQ<-read.csv("PH_CQ.csv")

kept_solutes<-c("Ba", "Ca", "Cl", "DIC", "Mg", "Si", "Na", "Sr", "SO4", "U", "Al", "As", "Co", "Cu",
                "Fe", "Mn", "Ni", "V", "Zn", "DOC", "NO3", "K")

CC_CQ$date<-as.Date(CC_CQ$date)
CC_CQ$month<-month(CC_CQ$date)

flow_months<-seq(4,8,1)

CC_hyst_test<-CC_CQ %>%
  filter(Element %in% kept_solutes) %>%
  dplyr::filter(month %in% flow_months) %>%
  select(date, Element, value) %>%
  distinct()

CC_hyst_test<-left_join(CC_hyst_test[,c("date", "Element", "value")], CC_bf[,c("date", "discharge")])

CC_hyst_test <- CC_hyst_test %>%
  filter(!is.na(discharge)) %>%
  dplyr::group_by(Element, year(date)) %>%
  mutate(max_Q=max(discharge, na.rm = T), date_max_Q=(date[max_Q==discharge])[1],
         sd_val=sd(value), mean_val=mean(value)) %>%
  filter(!(value < mean_val - 4*sd_val)) %>%
  filter(!(value > mean_val + 4*sd_val)) %>%
  mutate(min_conc=min(value), max_conc=max(value), scaled_conc = (2 * ((value - min_conc) / (max_conc - min_conc)) - 1)) %>%
  distinct()

CC_hyst_test<-CC_hyst_test %>%
  mutate(time_tag=case_when(
    date < date_max_Q ~ "rising",
    date > date_max_Q ~ "falling",
    .default = "peak"
  )) %>%
  filter(!time_tag=="peak")

k2<-ggplot()+geom_line(CC_bf, mapping=aes(date, discharge, group=year(date)))+
  geom_point(CC_hyst_test, mapping=aes(date, discharge, col=time_tag))+
  theme_classic()+theme(text = element_text(size = 20))+
  labs(x="Date", y="Discharge (cms)", col="limb")+
  ggtitle("East River")

k2

CC_hyst_test_binned <- CC_hyst_test %>%
  group_by(Element, `year(date)`, time_tag) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    frac_pos = (row_number() - 1) / (n() - 1),
    interval = cut(
      frac_pos,
      breaks = seq(0, 1, length.out = 21),
      labels = 1:20,
      include.lowest = TRUE
    )
  ) %>%
  group_by(Element, `year(date)`, time_tag, interval) %>%
  summarise(
    mean_value = mean(scaled_conc, na.rm = TRUE),
    .groups = "drop"
  )

bin_counts <- CC_hyst_test_binned %>%
  group_by(Element, `year(date)`, time_tag) %>%
  summarise(
    n_bins = n_distinct(interval))

good_years <- bin_counts %>%
  filter(n_bins == 20) %>%
  mutate(unique_tag=paste(Element, `year(date)`, time_tag))

good_years <- bin_counts %>%
  filter(n_bins == 20) %>%
  group_by(Element) %>%
  count(`year(date)`) %>%        # counts limbs
  filter(n == 2) %>%
  mutate(unique_tag=paste(Element, `year(date)`))

CC_hyst_test_binned_complete <- CC_hyst_test_binned %>%
  mutate(unique_tag2=paste(Element, `year(date)`)) %>%
  filter(unique_tag2 %in% good_years$unique_tag)

CC_hyst_test_binned_HI <- CC_hyst_test_binned_complete %>%
  pivot_wider(names_from  = time_tag, values_from = mean_value) %>%
  group_by(Element, interval, `year(date)`) %>%   # interval 
  summarise(HI = rising - falling)

CC_hyst_test_binned_FI <- CC_hyst_test_binned_complete %>%
  filter(time_tag=="rising" & interval %in% c(1,20)) %>%
  pivot_wider(names_from  = interval, values_from = mean_value) %>%
  group_by(Element, `year(date)`) %>%   # interval 
  summarise(FI = `20` - `1`)

CC_hyst_all<-left_join(CC_hyst_test_binned_HI, CC_hyst_test_binned_FI)

CC_hyst_all_avg<-CC_hyst_all %>%
  group_by(Element) %>%
  summarise(mean_HI=mean(HI), mean_FI=mean(FI))

solute_class<-read.csv("solute_class_updated.csv")

CC_hyst_all_avg<-left_join(CC_hyst_all_avg, solute_class)

CC_hyst_all_avg_solutes<-CC_hyst_all_avg %>%
  filter(Element %in% c("Mg", "Zn", "DOC"))

p2<-ggplot(CC_hyst_all_avg, aes(mean_FI, mean_HI))+
  lims(x=c(-2,2), y=c(-2,2))+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
  geom_hline(yintercept = -0.1, lty="dashed", col="grey")+geom_hline(yintercept = 0.1, lty="dashed", col="grey")+
  geom_vline(xintercept = -0.1, lty="dashed", col="grey")+geom_vline(xintercept = 0.1, lty="dashed", col="grey")+
  geom_point(aes(col=Class), size=3, alpha=0.5)+theme_classic()+theme(text = element_text(size = 20))+
  geom_point(CC_hyst_all_avg_solutes, mapping=aes(col=Class), size=3)+
  labs(x="Flushing Index", y="Hysteresis Index")+
  geom_text_repel(
    aes(label = Element),   # or Stream, Element, etc.
    size = 5,
    box.padding = 0.4,
    point.padding = 0.3,
    max.overlaps = Inf)+
  scale_color_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  ggtitle("East River")
p2

pdf("Solutes_HI_FI.pdf", width = 7, height = 10)

ggarrange(p1, p2, nrow = 2)

dev.off()

pdf("Hysteresis_Samples_Plot.pdf", width = 12, height = 8)

ggarrange(k1, k2, nrow = 2, align = "v")

dev.off()


