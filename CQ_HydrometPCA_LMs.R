install.packages("snotelr")
require(snotelr)
require(PCAtools)

get_CQyear<-function(x){
  
  x<-as.Date(x)
  date_year<-year(x)
  date_month<-month(x)
  
  CQ_year<-ifelse(date_month < 4, date_year-1, date_year)
  
  return(CQ_year)
  
}

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

snotel<-snotel_download(site_id = 380, internal = T)

growing_season<-subset(snotel, month(snotel$date) > 4 & month(snotel$date) < 10)

temp<-snotel %>%
  group_by(get_waterYear(date)) %>%
  summarise(mean_temp=mean(temperature_mean, na.rm = T))

colnames(temp)<-c("year", "gs_temp")

snow_phen<-snotel_phenology(snotel)

snow_phen$melt_rate<-snow_phen$max_swe/(snow_phen$last_snow_melt_doy-snow_phen$max_swe_doy)

snow_phen<-snow_phen[c("year", "last_snow_melt_doy", "max_swe", "max_swe_doy", "melt_rate")]

discharge<-read.csv("Coal_Creek_wy15_22_daily.csv")

discharge$date<-as.Date(discharge$date, "%m/%d/%y")

discharge$year<-get_waterYear(discharge$date)

discharge_stats<-discharge %>%
  filter(month(date) > 3 & month(date) < 11) %>%
  group_by(year) %>%
  summarise(lowflow = quantile(CC_Q_cms, 0.05, na.rm = T), highflow=quantile(CC_Q_cms, 0.95, na.rm = T))

discharge_stats<-subset(discharge_stats, discharge_stats$year > 2014)

storage_metrics<-left_join(snow_phen, discharge_stats, by="year")

storage_metrics<-left_join(storage_metrics, temp, by="year")

storage_metrics$swe_lag<-lag(storage_metrics$max_swe, n=1)

storage_metrics$lowflow_lag<-lag(storage_metrics$lowflow, n=1)
storage_metrics$meltrate_lag<-lag(storage_metrics$melt_rate, n=1)
storage_metrics$gs_temp_lag<-lag(storage_metrics$gs_temp, n=1)

ERCQ_CC<-read.csv("CoalCreek_CQ.csv")
ERCQ_CC$site<-"Coal Creek"

ER_CQ_all<-bind_rows(ERCQ_CC, ERCQ_PH)

ggplot(ER_CQ_all, aes(site, value, fill=site))+geom_boxplot(outliers = F)+facet_wrap(~variable, scales = "free_y")

ERCQ$date<-as.Date(ERCQ$date)

ERCQ$month<-month(ERCQ$date)

ERCQ$year<-get_CQyear(ERCQ$date)

ercq_WY<-ERCQ %>%
  group_by(year, variable) %>%
  summarise(
    slope=coef(lm(log10(value)~log10(CC_Q_cms)))[2]
  )

ercq_storage<-left_join(ercq_WY, storage_metrics, by="year")

df_master<-storage_metrics

df_master<-subset(df_master, df_master$year > 2015 & df_master$year < 2023)

df_master[c(2:ncol(df_master))]<-data.frame(sapply(df_master[c(2:ncol(df_master))], scale))

final_mat<-df_master[c(2:ncol(df_master))]
final_mat_t<-t(final_mat)

metadata<-data.frame(df_master[,c(1)])

rownames(metadata)<-colnames(final_mat_t)

pca<-pca(final_mat_t, metadata = metadata)

screeplot(pca)

loadings<-data.frame(pca$loadings)

pc_loadings<-as.data.frame(pca$rotated)

p1<-ggplot()+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC1*20, yend=PC2*20),linewidth=0.6, col="grey50")+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC1*20, yend=PC2*20), col="grey50", 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
  theme_classic()+
  geom_text_repel(data = loadings,aes(PC1*20,PC2*20,label=rownames(loadings)), 
                  size=4, max.overlaps = 20)+
  geom_point(pc_loadings, mapping = aes(PC1, PC2), size=4)+
  labs(x= "PC1", y="PC2",col="Month")+
  theme(text = element_text(size=20))

p2<-ggplot()+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC1*20, yend=PC3*20),linewidth=0.6, col="grey50")+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC1*20, yend=PC3*20), col="grey50", 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
  theme_classic()+
  geom_text_repel(data = loadings,aes(PC1*20,PC3*20,label=rownames(loadings)), 
                  size=4, max.overlaps = 20)+
  geom_point(pc_loadings, mapping = aes(PC1, PC3), size=4)+
  labs(x= "PC1", y="PC3",col="Month")+
  theme(text = element_text(size=20))

p3<-ggplot()+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC2*20, yend=PC3*20),linewidth=0.6, col="grey50")+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC2*20, yend=PC3*20), col="grey50", 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
  theme_classic()+
  geom_text_repel(data = loadings,aes(PC2*20,PC3*20,label=rownames(loadings)), 
                  size=4, max.overlaps = 20)+
  geom_point(pc_loadings, mapping = aes(PC2, PC3), size=4)+
  labs(x= "PC2", y="PC3",col="Month")+
  theme(text = element_text(size=20))

ggarrange(p1, p2, p3, nrow=1)

pc_dat<-data.frame(c(seq(2016,2022,1)),
          c(pc_loadings$PC1),
          c(pc_loadings$PC2),
          c(pc_loadings$PC3))

colnames(pc_dat)<-c("year", "PC1", "PC2", "PC3")

cq_pca<-left_join(pc_dat, ercq_WY, by="year")

cq_pca_melt<-melt(cq_pca, id.vars = c("year", "variable", "slope"))
colnames(cq_pca_melt)[2]<-"solute"

cq_pca_melt<-cq_pca_melt %>%
  group_by(solute, variable) %>%
  mutate(p_val=coef(summary(lm(slope~value)))[2,4])

ggplot(cq_pca_melt, aes(value, slope, col=variable))+geom_point()+
  geom_smooth(data=cq_pca_melt[cq_pca_melt$p_val < 0.15,], aes(value, slope, col=variable), method = "lm", se=F)+
  facet_wrap(~solute, scales = "free_y")+theme_bw()

####Pumphouse####
snotel<-snotel_download(site_id = 737, internal = T)

growing_season<-subset(snotel, month(snotel$date) > 4 & month(snotel$date) < 10)

temp<-snotel %>%
  group_by(get_waterYear(date)) %>%
  summarise(mean_temp=mean(temperature_mean, na.rm = T))

colnames(temp)<-c("year", "gs_temp")

snow_phen<-snotel_phenology(snotel)

snow_phen$melt_rate<-snow_phen$max_swe/(snow_phen$last_snow_melt_doy-snow_phen$max_swe_doy)

snow_phen<-snow_phen[c("year", "last_snow_melt_doy", "max_swe", "max_swe_doy", "melt_rate")]

discharge<-read.csv("Pumphouse_Q_Daily_Clean.csv")

discharge$date<-as.Date(discharge$date)

discharge$year<-get_waterYear(discharge$date)

discharge_stats<-discharge %>%
  filter(month(date) > 3 & month(date) < 11) %>%
  group_by(year) %>%
  summarise(lowflow = quantile(discharge, 0.05, na.rm = T), highflow=quantile(discharge, 0.95, na.rm = T))

storage_metrics<-left_join(snow_phen, discharge_stats, by="year")

storage_metrics<-left_join(storage_metrics, temp, by="year")

storage_metrics$swe_lag<-lag(storage_metrics$max_swe, n=1)

storage_metrics$lowflow_lag<-lag(storage_metrics$lowflow, n=1)
storage_metrics$meltrate_lag<-lag(storage_metrics$melt_rate, n=1)
storage_metrics$gs_temp_lag<-lag(storage_metrics$gs_temp, n=1)

ERCQ_PH<-read.csv("PH_CQ.csv")
ERCQ_PH$site<-"East River"

ER_PH_oneday<-subset(ERCQ_PH, ERCQ_PH$date=="2016-04-28")

sum(ER_PH_oneday$value)

ERCQ$date<-as.Date(ERCQ$date)

ERCQ$month<-month(ERCQ$date)

ERCQ$year<-get_CQyear(ERCQ$date)

ercq_WY<-ERCQ %>%
  group_by(year, variable) %>%
  summarise(
    slope=coef(lm(log10(value)~log10(discharge)))[2]
  )

ercq_storage<-left_join(ercq_WY, storage_metrics, by="year")

df_master<-storage_metrics

df_master<-subset(df_master, df_master$year > 2014 & df_master$year < 2023)

df_master[c(2:ncol(df_master))]<-data.frame(sapply(df_master[c(2:ncol(df_master))], scale))

final_mat<-df_master[c(2:ncol(df_master))]
final_mat_t<-t(final_mat)

metadata<-data.frame(df_master[,c(1)])

rownames(metadata)<-colnames(final_mat_t)

pca<-pca(final_mat_t, metadata = metadata)

screeplot(pca)

loadings<-data.frame(pca$loadings)

pc_loadings<-as.data.frame(pca$rotated)

p1<-ggplot()+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC1*20, yend=PC2*20),linewidth=0.6, col="grey50")+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC1*20, yend=PC2*20), col="grey50", 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
  theme_classic()+
  geom_text_repel(data = loadings,aes(PC1*20,PC2*20,label=rownames(loadings)), 
                  size=4, max.overlaps = 20)+
  geom_point(pc_loadings, mapping = aes(PC1, PC2), size=4)+
  labs(x= "PC1", y="PC2",col="Month")+
  theme(text = element_text(size=20))

p2<-ggplot()+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC1*20, yend=PC3*20),linewidth=0.6, col="grey50")+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC1*20, yend=PC3*20), col="grey50", 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
  theme_classic()+
  geom_text_repel(data = loadings,aes(PC1*20,PC3*20,label=rownames(loadings)), 
                  size=4, max.overlaps = 20)+
  geom_point(pc_loadings, mapping = aes(PC1, PC3), size=4)+
  labs(x= "PC1", y="PC3",col="Month")+
  theme(text = element_text(size=20))

p3<-ggplot()+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC2*20, yend=PC3*20),linewidth=0.6, col="grey50")+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=PC2*20, yend=PC3*20), col="grey50", 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
  theme_classic()+
  geom_text_repel(data = loadings,aes(PC2*20,PC3*20,label=rownames(loadings)), 
                  size=4, max.overlaps = 20)+
  geom_point(pc_loadings, mapping = aes(PC2, PC3), size=4)+
  labs(x= "PC2", y="PC3",col="Month")+
  theme(text = element_text(size=20))

ggarrange(p1, p2, p3, nrow=1)

pc_dat<-data.frame(c(seq(2016,2022,1)),
                   c(pc_loadings$PC1),
                   c(pc_loadings$PC2),
                   c(pc_loadings$PC3))

colnames(pc_dat)<-c("year", "PC1", "PC2", "PC3")

cq_pca<-left_join(pc_dat, ercq_WY, by="year")

cq_pca_melt<-melt(cq_pca, id.vars = c("year", "variable", "slope"))
colnames(cq_pca_melt)[2]<-"solute"

cq_pca_melt<-cq_pca_melt %>%
  group_by(solute, variable) %>%
  mutate(p_val=coef(summary(lm(slope~value)))[2,4])

ggplot(cq_pca_melt, aes(value, slope, col=variable))+geom_point()+
  geom_smooth(data=cq_pca_melt[cq_pca_melt$p_val < 0.15,], aes(value, slope, col=variable), method = "lm", se=F)+
  facet_wrap(~solute, scales = "free_y")+theme_bw()

