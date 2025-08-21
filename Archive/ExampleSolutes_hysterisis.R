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

k1<-ERCQ %>%
  filter(Element %in% c("Mg", "Zn", "DOC")) %>%
  mutate(Element=factor(Element, levels=c("Mg", "Zn", "DOC"))) %>%
  ggplot(aes(log(discharge), log(value), col=as.factor(month(date)))) + facet_wrap(~Element, scales = "free", nrow = 3)+
  geom_point()+
  theme_classic() +
  labs(x = "Log[Discharge (cms)]", y="", col="Month")+
  theme(text = element_text(size = 20))+
  scale_color_manual(values = cet_pal(12, "cbtc1"))+
  ggtitle("East River")

ERCQ<-read.csv("CoalCreek_CQ.csv")

ERCQ <- ERCQ %>%
  filter(variable %in% kep_solutes$solutes & year(as.Date(date)) > 2015) %>%
  group_by(variable) %>%
  mutate(sd_sol=sd(value), mean_sol=mean(value), lower=mean_sol - 2*sd_sol, upper=mean_sol + 2*sd_sol) %>%
  ungroup() %>%
  filter(value > lower & value < upper)

k2<-ERCQ %>%
  filter(Element %in% c("Mg", "Zn", "DOC")) %>%
  mutate(Element=factor(Element, levels=c("Mg", "Zn", "DOC"))) %>%
  ggplot(aes(log(CC_Q_cms), log(value), col=as.factor(month(date)))) + facet_wrap(~Element, scales = "free", nrow = 3)+
  geom_point()+
  theme_classic() +
  labs(x = "Log[Discharge (cms)]", y="Log[Solute Concentration (ppb)]", col="Month")+
  theme(text = element_text(size = 20), legend.position = "null")+
  scale_color_manual(values = cet_pal(12, "cbtc1"))+
  ggtitle("Coal Creek")

pdf("RepSolute_CQplots.pdf", width = 8, height = 8)

ggarrange(k2, k1, widths = c(0.5, 0.62), align = "h")

dev.off()
