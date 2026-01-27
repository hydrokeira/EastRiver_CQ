require(PCAtools)
require(dplyr)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

coal_chem<-read.csv("Coal_All_Chem.csv")

coal_wide<- coal_chem[,c(2,3,5)] %>%
  pivot_wider(names_from = Element, values_from = value, values_fn = mean)

# kept_solutes<-c("Ba", "Ca", "Cl", "DIC", "Mg", "Si", "Na", "Sr", "SO4", "U", "Al", "As", "Co", "Cu",
#                 "Fe", "Mn", "Ni", "V", "Zn", "DOC", "NO3", "K")

na_table<-coal_wide %>% summarise_all(~ sum(is.na(.)))

na_table<-data.frame(t(na_table))

remove<-rownames(subset(na_table, na_table$t.na_table. > 200))

remove<-remove[-10]

#remove_total<-c(remove)
remove_total<-c(remove, remove2_cc)

coal_wide<-coal_wide %>%
  select(!all_of(remove_total))
# 
# coal_wide<-coal_wide %>%
#   select(all_of(c("date", kept_solutes)))

coal_wide_cc<-coal_wide[c(complete.cases(coal_wide)),]

coal_wide_cc[c(2:ncol(coal_wide_cc))]<-data.frame(sapply(coal_wide_cc[c(2:ncol(coal_wide_cc))], scale))

final_mat<-coal_wide_cc[c(2:ncol(coal_wide_cc))]
final_mat_t<-t(final_mat)
colnames(final_mat_t)<-coal_wide_cc$date

metadata<-coal_wide_cc[,1]

rownames(metadata)<-coal_wide_cc$date

pca<-pca(final_mat_t, metadata = metadata)

s1<-screeplot(pca)+theme_classic()+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Coal Creek")

s1

loadings_cc<-as.data.frame(pca$loadings)

pc_loadings_cc<-as.data.frame(pca$rotated)
pc_loadings_cc$date<-rownames(pc_loadings_cc)
pc_loadings_cc$date<-as.Date(pc_loadings_cc$date)

p1<-ggplot()+
  geom_segment(loadings_cc, mapping=aes(x=0, y=0, xend=PC1*30, yend=PC2*30))+
  geom_segment(loadings_cc, mapping=aes(x=0, y=0, xend=PC1*30, yend=PC2*30), 
              arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), 
              lty="blank", size=0.6, col="black")+
  geom_point(pc_loadings_cc, mapping = aes(PC1, PC2, col=as.factor(month(date))), size=2.5)+theme_classic()+
  geom_text_repel(data = loadings_cc,aes(PC1*30,PC2*30,label=rownames(loadings_cc)), size=4.5)+
  labs(x= "PC1 27% variation", y="PC2 18% variation", col="Month")+
  theme(text = element_text(size=20), legend.position = "null")+
  scale_color_manual(values = cet_pal(12, "cbtc1"))+
  ggtitle("Coal Creek")

p1

c1<-ggplot()+
  geom_segment(loadings_cc, mapping=aes(x=0, y=0, xend=PC1*30, yend=PC3*30))+
  geom_segment(loadings_cc, mapping=aes(x=0, y=0, xend=PC1*30, yend=PC3*30), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), 
               lty="blank", size=0.6, col="black")+
  geom_point(pc_loadings_cc, mapping = aes(PC1, PC3, col=as.factor(month(date))), size=2.5)+theme_classic()+
  geom_text_repel(data = loadings_cc,aes(PC1*30,PC3*30,label=rownames(loadings_cc)), size=4.5)+
  labs(x= "PC1 27% variation", y="PC3 9% variation", col="Month")+
  theme(text = element_text(size=20), legend.position = "null")+
  scale_color_manual(values = cet_pal(12, "cbtc1"))
  #ggtitle("Coal Creek")

c1

c2<-ggplot()+
  geom_segment(loadings_cc, mapping=aes(x=0, y=0, xend=PC2*30, yend=PC3*30))+
  geom_segment(loadings_cc, mapping=aes(x=0, y=0, xend=PC2*30, yend=PC3*30), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), 
               lty="blank", size=0.6, col="black")+
  geom_point(pc_loadings_cc, mapping = aes(PC2, PC3, col=as.factor(month(date))), size=2.5)+theme_classic()+
  geom_text_repel(data = loadings_cc,aes(PC2*30,PC3*30,label=rownames(loadings_cc)), size=4.5)+
  labs(x= "PC2 18% variation", y="PC3 9% variation", col="Month")+
  theme(text = element_text(size=20))+
  scale_color_manual(values = cet_pal(12, "cbtc1"))
  #ggtitle("Coal Creek")

c2

pdf("Coal_Creek_PCA_3PC.pdf", width = 15.5, height = 5)

ggarrange(p1, c1, c2, nrow = 1, align = "h", widths = c(0.5, 0.5, 0.6))

dev.off()

ph_chem<-read.csv("PH_All_Chem.csv")

ph_wide<- ph_chem[,c(2,3,5)] %>%
  pivot_wider(names_from = Element, values_from = value, values_fn = mean)

na_table<-ph_wide %>% summarise_all(~ sum(is.na(.)))

na_table<-data.frame(t(na_table))

remove<-rownames(subset(na_table, na_table$t.na_table. > 400))

remove<-remove[-c(8,13)]

#remove_total<-c(remove)
remove_total<-c(remove, remove2_ph)

outliers<-c("2016-01-03", "2019-09-23")

ph_wide<-ph_wide %>%
  filter(!c(date %in% outliers)) %>%
  select(!all_of(remove_total))

ph_wide_cc<-ph_wide[c(complete.cases(ph_wide)),]

ph_wide_cc[c(2:ncol(ph_wide_cc))]<-data.frame(sapply(ph_wide_cc[c(2:ncol(ph_wide_cc))], scale))

final_mat<-ph_wide_cc[c(2:ncol(ph_wide_cc))]
final_mat_t<-t(final_mat)
colnames(final_mat_t)<-ph_wide_cc$date

metadata<-ph_wide_cc[,1]

rownames(metadata)<-ph_wide_cc$date

pca<-pca(final_mat_t, metadata = metadata)

s2<-screeplot(pca)+theme_classic()+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("East River")

loadings_ph<-as.data.frame(pca$loadings)

pc_loadings_ph<-as.data.frame(pca$rotated)
pc_loadings_ph$date<-rownames(pc_loadings_ph)
pc_loadings_ph$date<-as.Date(pc_loadings_ph$date)

p2<-ggplot()+
  geom_segment(loadings_ph, mapping=aes(x=0, y=0, xend=PC1*30, yend=PC2*30))+
  geom_segment(loadings_ph, mapping=aes(x=0, y=0, xend=PC1*30, yend=PC2*30), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), 
               lty="blank", size=0.6, col="black")+
  geom_point(pc_loadings_ph, mapping = aes(PC1, PC2, col=as.factor(month(date))), size=2.5)+theme_classic()+
  geom_text_repel(data = loadings_ph,aes(PC1*30,PC2*30,label=rownames(loadings_ph)), size=4.5)+
  labs(x= "PC1 30% variation", y="PC2 12% variation", col="Month")+
  theme(text = element_text(size=20), legend.position = "null")+
  scale_color_manual(values = cet_pal(12, "cbtc1"))+
  ggtitle("East River")

p2

k1<-ggplot()+
  geom_segment(loadings_ph, mapping=aes(x=0, y=0, xend=PC1*30, yend=PC3*30))+
  geom_segment(loadings_ph, mapping=aes(x=0, y=0, xend=PC1*30, yend=PC3*30), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), 
               lty="blank", size=0.6, col="black")+
  geom_point(pc_loadings_ph, mapping = aes(PC1, PC3, col=as.factor(month(date))), size=2.5)+theme_classic()+
  geom_text_repel(data = loadings_ph,aes(PC1*30,PC3*30,label=rownames(loadings_ph)), size=4.5)+
  labs(x= "PC1 30% variation", y="PC3 8% variation", col="Month")+
  theme(text = element_text(size=20), legend.position = "null")+
  scale_color_manual(values = cet_pal(12, "cbtc1"))
  #ggtitle("East River")

k1

k2<-ggplot()+
  geom_segment(loadings_ph, mapping=aes(x=0, y=0, xend=PC2*30, yend=PC3*30))+
  geom_segment(loadings_ph, mapping=aes(x=0, y=0, xend=PC2*30, yend=PC3*30), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), 
               lty="blank", size=0.6, col="black")+
  geom_point(pc_loadings_ph, mapping = aes(PC2, PC3, col=as.factor(month(date))), size=2.5)+theme_classic()+
  geom_text_repel(data = loadings_ph,aes(PC2*30,PC3*30,label=rownames(loadings_ph)), size=4.5)+
  labs(x= "PC2 12% variation", y="PC3 8% variation", col="Month")+
  theme(text = element_text(size=20))+
  scale_color_manual(values = cet_pal(12, "cbtc1"))
  #ggtitle("East River")

k2

pdf("East_River_PCA_3PC.pdf", width = 15.5, height = 5)

ggarrange(p2, k1, k2, nrow = 1, align = "h", widths = c(0.5, 0.5, 0.6))

dev.off()

pdf("Scree_Plots_PCA.pdf", width = 14, height = 5)

ggarrange(s1, s2, nrow = 1)

dev.off()

pdf("ER_CC_PCA_Same_Solutes.pdf", width = 15, h11 ght = 7)

ggarrange(p1, p2, widths = c(0.45, 0.5))

dev.off()

remove2_cc<-setdiff(colnames(coal_wide_cc), colnames(ph_wide_cc))

remove2_ph<-setdiff(colnames(ph_wide_cc), colnames(coal_wide_cc))

solutes_retained<-data.frame(colnames(ph_wide_cc))
colnames(solutes_retained)<-"solutes"

write.csv(solutes_retained, "Solutes_Retained.csv")

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
  ggplot(aes(log(discharge), log(value), col=as.factor(month(date)))) + 
  facet_wrap(~Element, scales = "free", nrow = 1)+
  geom_point()+
  theme_classic() +
  labs(x = "Log[Discharge (cms)]", y="Log[Solute Concentration (ppb)]", col="Month")+
  theme(text = element_text(size = 20))+
  scale_color_manual(values = cet_pal(12, "cbtc1"))+
  ggtitle("East River")
k1


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
  ggplot(aes(log(CC_Q_cms), log(value), col=as.factor(month(date)))) + 
  facet_wrap(~Element, scales = "free", nrow = 1)+
  geom_point()+
  theme_classic() +
  labs(x = "Log[Discharge (cms)]", y="Log[Solute Concentration (ppb)]", col="Month")+
  theme(text = element_text(size = 20))+
  scale_color_manual(values = cet_pal(12, "cbtc1"))+
  ggtitle("Coal Creek")
k2

pdf("PCA_RepSolutes.pdf", width = 17, height = 10)

ggarrange(p1, k2, p2, k1, nrow = 2, ncol = 2, widths = c(0.45, 1))

dev.off()

pdf("RepSolute_CQplots.pdf", width = 8, height = 8)

ggarrange(k2, k1, heights = c(0.5, 0.62), nrow = 2)

dev.off()

p2
