ERCQ<-read.csv("CoalCreek_CQ.csv")

kep_solutes<-read.csv("Solutes_Retained.csv")
solute_class<-read.csv("solute_class_updated.csv")

CC_all <- ERCQ %>%
  filter(variable %in% kep_solutes$solutes & year(as.Date(date)) > 2015) %>%
  group_by(variable) %>%
  mutate(sd_sol=sd(value), mean_sol=mean(value), lower=mean_sol - 2*sd_sol, upper=mean_sol + 2*sd_sol) %>%
  ungroup() %>%
  filter(value > lower & value < upper) %>%
  filter(Element %in% c("Mg", "Zn", "DOC")) %>%
  mutate(site="CC")

colnames(CC_all)[11]<-"discharge"

CC<-CC_all %>%
  mutate(Element=factor(Element, levels=c("Mg", "Zn", "DOC"))) %>%
  group_by(Element, month(date)) %>%
  summarise(monthly_conc = mean(value), monthly_Q=mean(CC_Q_cms)) %>%
  mutate(site="CC")

ERCQ<-read.csv("PH_CQ.csv")

kep_solutes<-read.csv("Solutes_Retained.csv")
solute_class<-read.csv("solute_class_updated.csv")

ER_all <- ERCQ %>%
  filter(variable %in% kep_solutes$solutes & year(as.Date(date)) > 2015) %>%
  group_by(variable) %>%
  mutate(sd_sol=sd(value), mean_sol=mean(value), lower=mean_sol - 2*sd_sol, upper=mean_sol + 2*sd_sol) %>%
  ungroup() %>%
  filter(value > lower & value < upper) %>%
  filter(Element %in% c("Mg", "Zn", "DOC")) %>%
  mutate(site="PH")

PH<-ER_all %>%
  filter(Element %in% c("Mg", "Zn", "DOC")) %>%
  mutate(Element=factor(Element, levels=c("Mg", "Zn", "DOC"))) %>%
  group_by(Element, month(date)) %>%
  summarise(monthly_conc = mean(value), monthly_Q=mean(discharge)) %>%
  mutate(site="PH")

all<-bind_rows(PH, CC)
all_all<-bind_rows(ER_all[,c(2,3,5,11,16)], CC_all[,c(2,3,5,11,16)])

all_all$Element<-factor(all_all$Element, levels = c("Mg", "Zn", "DOC"))

all_all$Element<-factor(all_all$Element, levels = c("Mg", "Zn", "DOC"))

pdf("Combined_ThreeSolute_CQ.pdf", width = 12, height = 4.5)

ggplot() + 
  geom_point(all_all, mapping=aes(log(discharge), log(value), col=as.factor(month(date))), alpha=0.45)+
  geom_path(all, mapping=aes(log(monthly_Q), log(monthly_conc), col=as.factor(`month(date)`), group=site), lwd=1.5)+
  facet_wrap(~Element, scales = "free", nrow = 1)+
  theme_classic() +
  labs(x = "Log[Discharge (cms)]", y="Log[Solute Concentration (ppb)]", col="Month", fill="Month")+
  theme(text = element_text(size = 20), legend.position = "null")+
  scale_color_manual(values = cet_pal(12, "cbtc1"))+
  scale_fill_manual(values = cet_pal(12, "cbtc1"))

dev.off()

ggplot() + 
  facet_wrap(~Element, scales = "free", nrow = 1)+
  geom_point(all_all, mapping=aes(log(discharge), log(value), col=site), alpha=0.45)+
  geom_path(all, mapping=aes(log(monthly_Q), log(monthly_conc), col=site, group=site), lwd=1.5)+
  theme_classic() +
  labs(x = "Log[Discharge (cms)]", y="Log[Solute Concentration (ppb)]", col="Month", fill="Month")+
  theme(text = element_text(size = 20))
#scale_color_manual(values = cet_pal(12, "cbtc1"))+
#scale_fill_manual(values = cet_pal(12, "cbtc1"))

