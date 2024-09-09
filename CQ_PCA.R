require(EflowStats)
require(dplyr)
require(PCAtools)
require(lubridate)
require(reshape2)
require(cetcolor)
require(ggpubr)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

ERCQ<-read.csv("PH_CQ.csv")

ERCQ$date<-as.Date(ERCQ$date)

ERCQ$month<-month(ERCQ$date)
ERCQ$WY<-get_waterYear(ERCQ$date)

ERCQ<-subset(ERCQ, ERCQ$value > 0)

month_year_count<-ERCQ %>%
  group_by(month, variable) %>%
  summarise(n_obs=n_distinct(daily_q_cms))

ercq_monthly<-ERCQ %>%
  group_by(month, variable) %>%
  summarise(
    slope=coef(lm(log(value)~log(daily_q_cms)))[2]
  )

ercq_monthly_wide <- dcast(ercq_monthly, value.var = "slope", formula = month~variable)
ercq_monthly_wide<-ercq_monthly_wide[ , apply(ercq_monthly_wide, 2, function(x) !any(is.na(x)))]

df_master<-ercq_monthly_wide

df_master[c(2:ncol(df_master))]<-data.frame(sapply(df_master[c(2:ncol(df_master))], scale))

final_mat<-df_master[c(2:ncol(df_master))]
final_mat_t<-t(final_mat)
colnames(final_mat_t)<-seq(1,12,1)

metadata<-data.frame(df_master[,c(1)])

rownames(metadata)<-seq(1,12,1)

pca<-pca(final_mat_t, metadata = metadata)

screeplot(pca)

loadings<-data.frame(pca$loadings)

pc_loadings<-as.data.frame(pca$rotated)
pc_loadings$month<-as.character(seq(1,12,1))

t.test(loadings$PC1, conf.level = 0.9999)

loadings$PC1_sig<-ifelse(loadings$PC1 < -0.0756849, "sig",
                         ifelse(loadings$PC1 > 0.1193983, "sig", "not sig"))

t.test(loadings$PC2, conf.level = 0.9999)

loadings$PC2_sig<-ifelse(loadings$PC1 < -0.06592333, "sig",
                         ifelse(loadings$PC1 > 0.12713691, "sig", "not sig"))

loadings$sig<-ifelse(loadings$PC1_sig == "sig" & loadings$PC2_sig == "sig", "both",
                     ifelse(loadings$PC1_sig == "sig" & loadings$PC2_sig == "not sig", "PC1 sig",
                            ifelse(loadings$PC1_sig == "not sig" & loadings$PC2_sig == "sig", "PC2 sig", "neither")))


loadings<-subset(loadings, loadings$sig != "neither")

pdf("ER_Monthly_CQ_PCA.pdf", width = 9, height = 7)

p1<-ggplot()+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC1*20, yend=PC2*20),linewidth=0.6, col="grey50")+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC1*20, yend=PC2*20), col="grey50", 
             arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
  theme_classic()+
  geom_text_repel(data = loadings,aes(PC1*20,PC2*20,label=rownames(loadings)), 
                 size=4, max.overlaps = 20)+
  geom_point(pc_loadings, mapping = aes(PC1, PC2, col=month), size=4)+
  scale_color_manual(values=cet_pal(12, "c4s"),
                     limits= c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))+
  labs(x= "PC1", y="PC2",col="Month")+
  theme(text = element_text(size=20))

p1

dev.off()

ercq_monthly_cropped<-ercq_monthly[ercq_monthly$variable %in% rownames(loadings),]

pdf("CQ_Slope_Monthly.pdf", width = 18, height = 10)

p2<-ggplot(ercq_monthly_cropped, aes(month, slope))+geom_line()+geom_abline(slope = 0, intercept = 0, col="grey50", lty="dashed")+
  geom_point(aes(col=as.character(month)), size=3)+
  scale_color_manual(values=cet_pal(12, "c4s"),
                     limits= c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))+
  facet_wrap(~variable, scales = "free_y")+scale_x_continuous(labels = seq(1,12,1), breaks = seq(1,12,1))+
  theme_bw()+labs(x="Month", y="CQ Slope", col="Month")+
  theme(text = element_text(size = 17))

p2

dev.off()

#### now group by month and WY to look at temporally variable change in CQ slope ####

ercq_monthly<-ERCQ %>%
  group_by(month, WY, variable) %>%
  summarise(
    slope=coef(lm(log(value)~log(daily_q_cms)))[2]
  )

ercq_monthly_wide <- dcast(ercq_monthly, value.var = "slope", formula = month+WY~variable)

na_count <-sapply(ercq_monthly_wide, function(y) sum(length(which(is.na(y)))))

remove_these<-c("ammonia_n_ppm", "antimony_ppb", "beryllium_ppb", "fluoride_µmol/L; µM", "cadmium_ppb", "cesium_ppb", 
                "germanium_ppb","lead_ppb", "phosphate_µmol/L; µM", "thorium_ppb", "tdn_ug.L-1", "zirconium_ppb",
                "tin_ppb", "silver_ppb", "phosphorus_ppb", "vanadium_ppb")

ercq_monthly_wide<-ercq_monthly_wide[,-which(names(ercq_monthly_wide) %in% remove_these)]
ercq_monthly_wide<-ercq_monthly_wide[complete.cases(ercq_monthly_wide),]
ercq_monthly_wide$unique<-paste0(ercq_monthly_wide$month, "_", ercq_monthly_wide$WY)

ercq_monthly_wide<-ercq_monthly_wide[ , apply(ercq_monthly_wide, 2, function(x) !any(is.na(x)))]

df_master<-ercq_monthly_wide

df_master[c(3:31)]<-data.frame(sapply(df_master[c(3:31)], scale))

final_mat<-df_master[c(3:31)]
final_mat_t<-t(final_mat)
colnames(final_mat_t)<-ercq_monthly_wide$unique

metadata<-data.frame(df_master[,c(1,2,32)])

rownames(metadata)<-ercq_monthly_wide$unique

pca<-pca(final_mat_t, metadata = metadata)

screeplot(pca)

loadings<-data.frame(pca$loadings)

pc_loadings<-as.data.frame(pca$rotated)
pc_loadings<-cbind(metadata, pc_loadings)

t.test(loadings$PC1, conf.level = 0.9999)

loadings$PC1_sig<-ifelse(loadings$PC1 < -0.24440106, "sig",
                         ifelse(loadings$PC1 > 0.03239922, "sig", "not sig"))

t.test(loadings$PC2, conf.level = 0.9999)

loadings$PC2_sig<-ifelse(loadings$PC1 < -0.1982270, "sig",
                         ifelse(loadings$PC1 > 0.1108177, "sig", "not sig"))

loadings$sig<-ifelse(loadings$PC1_sig == "sig" & loadings$PC2_sig == "sig", "both",
                     ifelse(loadings$PC1_sig == "sig" & loadings$PC2_sig == "not sig", "PC1 sig",
                            ifelse(loadings$PC1_sig == "not sig" & loadings$PC2_sig == "sig", "PC2 sig", "neither")))


#loadings<-subset(loadings, loadings$sig != "neither")
pc_loadings$month<-as.character(pc_loadings$month)


p1<-ggplot()+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC1*20, yend=PC2*20),linewidth=0.6, col="grey50")+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC1*20, yend=PC2*20), col="grey50", 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
  theme_classic()+
  geom_text_repel(data = loadings,aes(PC1*20,PC2*20,label=rownames(loadings)), 
                  size=4, max.overlaps = 20)+
  geom_point(pc_loadings, mapping = aes(PC1, PC2, col=month), size=4)+
  scale_color_manual(values=cet_pal(12, "c4s"),
                     limits= c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))+
  labs(x= "PC1", y="PC2",col="Month")+
  theme(text = element_text(size=20))

p1




