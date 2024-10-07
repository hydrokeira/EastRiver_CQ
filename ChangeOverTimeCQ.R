install.packages("trend")
require(trend)
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

get_CQyear<-function(x){
  
  x<-as.Date(x)
  date_year<-year(x)
  date_month<-month(x)
  
  CQ_year<-ifelse(date_month < 4, date_year-1, date_year)
  
  return(CQ_year)
  
}


ERCQ<-read.csv("CoalCreek_CQ.csv")

ERCQ$date<-as.Date(ERCQ$date)

ERCQ$month<-month(ERCQ$date)
ERCQ$WY<-get_CQyear(ERCQ$date)

ercq_monthly<-ERCQ %>%
  group_by(month, variable) %>%
  summarise(
    slope=coef(lm(log10(value)~log10(CC_Q_cms)))[2]
  )

length(which(abs(ercq_monthly$slope) > 1))/nrow(ercq_monthly)

# ercq_monthly_bigslope<-subset(ercq_monthly, abs(ercq_monthly$slope) > 1)
# 
# ERCQ_bigslope<-ERCQ %>%
#   inner_join(ercq_monthly_bigslope, by=c("month","variable"))
# 
# pdf("Pumphouse_BigSlopes.pdf", width = 14, height = 8)
# 
# ggplot(ERCQ_bigslope, aes(log10(discharge),log10(value), col=as.character(month)))+
#   geom_point()+facet_wrap(~variable, scales = "free")+theme_bw()+geom_smooth(method = "lm", se=F)+
#   scale_color_manual(values=cet_pal(12, "c4s"),
#                      limits= c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))+
#   labs(col="Month")
# 
# dev.off()

ercq_WY<-ERCQ %>%
  group_by(WY, variable) %>%
  summarise(
    slope=coef(lm(log10(value)~log10(CC_Q_cms)))[2]
  )

ercq_WY<-ERCQ %>%
  group_by(WY, variable) %>%
  summarise(
    mean_C=mean(value)
  )


pdf("CoalCreek_MonthlyCQ.pdf", width = 16, height = 8)

ggplot(ercq_monthly, aes(month, slope))+geom_line()+geom_abline(slope = 0, intercept = 0, col="grey50", lty="dashed")+
  geom_point(aes(col=as.character(month)), size=3)+
  scale_color_manual(values=cet_pal(12, "c4s"),
                     limits= c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))+
  facet_wrap(~variable, scales = "free_y")+scale_x_continuous(labels = seq(1,12,1), breaks = seq(1,12,1))+
  theme_bw()+labs(x="Month", y="CQ Slope", col="Month")+
  theme(text = element_text(size = 17))+ylim(-1,1)

dev.off()

pdf("CoalCreek_YearlyCQ.pdf", width = 16, height = 8)

ggplot(ercq_WY, aes(WY, slope))+geom_line()+geom_abline(slope = 0, intercept = 0, col="grey50", lty="dashed")+
  geom_point(aes(col=as.integer(WY)), size=3)+
  scale_color_gradient(low = "salmon", high = "forestgreen")+
  facet_wrap(~variable, scales = "free_y")+
  theme_bw()+labs(x="Year", y="CQ Slope", col="Year")+
  theme(text = element_text(size = 17))+ylim(-1,1)

dev.off()

modified_mk_test <- function(x, ...) {
  result <- mk.test(x, ...)
  
  result2<-sens.slope(x, ...)
  
  tibble(
    p.value = result$p.value,
    tau_mk = result$estimates[3],
    slope_ss=result2$estimates
    
    # and any other values you want to capture from the return
  )
  
  
}

#test significance of CQ slope over time
MK_WY<-ercq_WY %>%
  filter(variable != c("ammonia_n_ppm")) %>%
  group_by(variable) %>%
  group_modify(~ modified_mk_test(.x$slope))


# sig_solutes<-subset(MK_WY, MK_WY$p.value < 0.1)
# 
# sig_solutes_all<-subset(ERCQ, ERCQ$variable %in% sig_solutes$variable)
# 
# sig_month_count<-sig_solutes_all %>%
#   group_by(WY, month, variable) %>%
#   summarise(count=n_distinct(CC_Q_cms))

ercp_cv_monthly<-ercq_monthly %>%
  group_by(variable) %>%
  summarise(cv_monthly=abs(sd(slope)/mean(slope)))

ercp_cv_annual<-ercq_WY %>%
  group_by(variable) %>%
  summarise(cv_annual=abs(sd(slope)/mean(slope)))

cv_cq<-merge(ercp_cv_annual, ercp_cv_monthly, by="variable")

cv_cq_melt<-melt(cv_cq, id.vars="variable")
colnames(cv_cq_melt)<-c("solute", "slope_type", "cv")

cv_cq_melt$site<-"pumphouse"

coalcreek_cv<-cv_cq_melt

coalcreek_cv$site<-"coal"

cv_cq_all<-bind_rows(coalcreek_cv, cv_cq_melt)

ggplot(cv_cq_all, aes(slope_type, cv, fill=site))+geom_boxplot(outliers = F)+
  scale_fill_manual(values = c("coal"="salmon", "pumphouse"="forestgreen"))+theme_bw()+
  labs(x="CQ Slope Temporal Scale", y="Coefficient of Variation", fill="Site")

ER_Q<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/Pumphouse_Q_Daily_Clean.csv")
ER_Q$date<-as.Date(ER_Q$date)
ER_Q$month<-month(ER_Q$date)
ER_Q$WY<-get_waterYear(ER_Q$date)
ER_Q<-ER_Q[complete.cases(ER_Q),]

ER_Q<-read.csv("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ/Coal_Creek_wy15_22_daily.csv")
ER_Q$date<-as.Date(ER_Q$date, format = "%m/%d/%y")
ER_Q$month<-month(ER_Q$date)
ER_Q$WY<-get_waterYear(ER_Q$date)
ER_Q<-subset(ER_Q, ER_Q$WY > 2014 & ER_Q$WY < 2023)
ER_Q<-ER_Q[complete.cases(ER_Q$CC_Q_cms),]


ER_Q_monthly<-ER_Q %>%
  dplyr::group_by(WY, month) %>%
  summarise(
    mean_Q=mean(discharge, na.rm = T)
  )

ER_Q_monthly %>%
  group_by(month) %>%
  group_modify(~ modified_mk_test(.x$mean_Q))

mk.test(ER_Q_monthly$mean_Q)

ER_C_monthly<-ERCQ %>%
  group_by(WY, month, variable) %>%
  summarise(
    mean_C=mean(value, na.rm = T)
  )

ggplot(ER_Q_monthly, aes(WY, log(mean_Q), col=as.character(month)))+geom_point()+geom_line(aes(group=month))+
  scale_color_manual(values=cet_pal(12, "c4s"),
                     limits= c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))+theme_bw()

ggplot(ER_C_monthly, aes(WY, mean_C, col=as.character(month)))+geom_point()+geom_line(aes(group=month))+
  facet_wrap(~variable, scales="free_y")+
  scale_color_manual(values=cet_pal(12, "c4s"),
                     limits= c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))+theme_bw()+
  labs(col="Month")

ER_C_monthly$unique<-paste0(ER_C_monthly$month, "-", ER_C_monthly$WY)

counts<-ER_C_monthly %>%
  group_by(variable) %>%
  summarise(n_obs=n_distinct(unique))

keep_these<-subset(counts, counts$n_obs > 60)

ER_C_cropped<-subset(ER_C_monthly, ER_C_monthly$variable %in% keep_these$variable)

MK_solute<-ER_C_cropped %>%
  group_by(month, variable) %>%
  group_modify(~ modified_mk_test(.x$mean_C))

MK_solute_sig<-subset(MK_solute, MK_solute$p.value < 0.1)
