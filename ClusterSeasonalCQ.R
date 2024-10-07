require(dtw)
require(plot.matrix)
require(factoextra)
require(dtwclust)
require(dplyr)
require(lubridate)
require(EflowStats)
require(cetcolor)
require(reshape2)
require(tibble)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

ERCQ<-read.csv("CoalCreek_CQ.csv")

ERCQ$date<-as.Date(ERCQ$date)

ERCQ$month<-month(ERCQ$date)
ERCQ$WY<-get_waterYear(ERCQ$date)

ercq_monthly<-ERCQ %>%
  group_by(month, variable) %>%
  summarise(
    slope=coef(lm(log10(value)~log10(CC_Q_cms)))[2]
  )

ggplot(ercq_monthly, aes(month, slope))+geom_line()+geom_abline(slope = 0, intercept = 0, col="grey50", lty="dashed")+
  geom_point(aes(col=as.character(month)), size=3)+
  scale_color_manual(values=cet_pal(12, "c4s"),
                     limits= c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))+
  facet_wrap(~variable, scales = "free_y")+scale_x_continuous(labels = seq(1,12,1), breaks = seq(1,12,1))+
  theme_bw()+labs(x="Month", y="CQ Slope", col="Month")+
  theme(text = element_text(size = 17))

ercq_monthly_norm <- ercq_monthly %>%
  group_by(variable) %>%
  mutate(normalized_slope=scale(slope))

colnames(ercq_monthly_norm)[4]<-"norm_slope"

ggplot(ercq_monthly_norm, aes(month, norm_slope))+geom_line()+geom_abline(slope = 0, intercept = 0, col="grey50", lty="dashed")+
  geom_point(aes(col=as.character(month)), size=3)+
  scale_color_manual(values=cet_pal(12, "c4s"),
                     limits= c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))+
  facet_wrap(~variable, scales = "free_y")+scale_x_continuous(labels = seq(1,12,1), breaks = seq(1,12,1))+
  theme_bw()+labs(x="Month", y="CQ Slope", col="Month")+
  theme(text = element_text(size = 17))

month_cast<-dcast(ercq_monthly_norm, formula = month~variable, value.var = "norm_slope")
month_norm<-month_cast
month_norm_t<-as.data.frame(t(month_norm[2:ncol(month_norm)]))
month_norm_t<-month_norm_t[complete.cases(month_norm_t),]

# #### Cluster Validity Indices ####
# #set up index "best" avlues - i.e. for "Sil" CVI, higher values (max) is better
# index<-c("Sil","D","COP","DB","DBstar","CH","SF")
# cvi_ideal<-c("max","max","min","min","min","max","max")
# cvi_index<-data.frame(index, cvi_ideal)
# 
# #test CVIs for 2-15 clusters
# clust.dba <- tsclust(month_norm_t, type="partitional", centroid = "dba", distance = "dtw",
#                      window.size=1L, k=2L:15L, seed = 8)
# 
# #put CVI output into list
# cvi_df<-lapply(clust.dba, cvi)
# 
# #put into df and melt
# cluster_stats<-do.call(rbind, cvi_df)
# cluster_stats_melt<-melt(cluster_stats)
# 
# #change column to reflect number of cluster (currently starts at 1, need to start at 2)
# cluster_stats_melt$Var1<-cluster_stats_melt$Var1+1
# 
# #rename column
# colnames(cluster_stats_melt)[2]<-"index"
# 
# #merge index df with CVI df
# cluster_stats_melt<-merge(cluster_stats_melt, cvi_index, by="index")
# colnames(cluster_stats_melt)[2]<-"number_of_clusters"
# cluster_stats_melt$cvi_goal<-paste0(cluster_stats_melt$index, "-", cluster_stats_melt$cvi_ideal)
# 
# #plot to evaluate CVIs
# ggplot(cluster_stats_melt, aes(number_of_clusters, value))+geom_line()+facet_wrap(~cvi_goal, scales = "free")+
#   theme_classic()

clust.dba <- tsclust(month_norm_t, type="partitional", centroid = "dba", distance = "dtw",
                     window.size=1, k=3L, seed = 8)

clust.dba

#plot the lines and centroids
plot(clust.dba, type = "sc")

#plot just the centroids
plot(clust.dba, type = "centroid")

#pull out individual lines
mydata<-clust.dba@datalist

#convert to df
data_df<-t(do.call(cbind, mydata))
#visualize clusters in PC space
fviz_cluster(object = list(data=data_df, cluster = clust.dba@cluster),
             data = month_norm_t, stand = TRUE,
             labelsize = 0, , ellipse.type = "t", ellipse.level = 0.9)+theme_bw()+
  theme(text = element_text(size = 20))+labs(col="Cluster", fill="Cluster", shape="Cluster")

#this saves the data into df of each cluster, site, and month value
month_clusters<-as.data.frame(cbind(clust.dba@cluster, data_df))
colnames(month_clusters)<-c("Cluster", paste(seq(1:12)))
month_clusters<-rownames_to_column(month_clusters, "Solute")
#month_clusters$Site<-str_sub(month_clusters$SiteYear,1,nchar(month_clusters$SiteYear)-5)

solute_class<-read.csv("solute_classification.csv")

conversion<-data.frame(t(data.frame(c("1", "geogenic"),
                       c("2", "bioavailable"),
                       c("3", "metal"),
                       c("4", "other"))))

colnames(conversion)<-c("number","class")
conversion$number<-as.integer(conversion$number)

month_clusters<-full_join(month_clusters, solute_class, by="Solute")
month_clusters<-full_join(month_clusters, conversion, by="number")

month_clusters<-month_clusters[complete.cases(month_clusters),]

month_clusters_melt<-melt(month_clusters, id.vars = c("Solute", "Cluster",
                                                      "number", "class"))


#plot lines by cluster
p2<-ggplot(month_clusters_melt, aes(variable, value))+
  geom_line(aes(col=class, group=Solute), size=0.7)+
  theme_bw()+facet_wrap(~Cluster, nrow = 1)+
  scale_color_manual(values = c("bioavailable"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  theme(text = element_text(size = 20), legend.position = "bottom")+
  labs(col="Solute Class", x="Month", y="Normalized CQ Slope")+
  ggtitle("Coal Creek")

p2

ggarrange(p1, p2, nrow=2, heights = c(0.47, 0.53))

ercq_monthly<-ERCQ %>%
  group_by(month, variable) %>%
  summarise(
    mean_C=mean(value, na.rm = T)
  )


