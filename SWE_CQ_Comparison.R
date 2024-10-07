install.packages("sfsmisc")
require(MASS)
library(sfsmisc)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

get_CQyear<-function(x){
  
  x<-as.Date(x)
  date_year<-year(x)
  date_month<-month(x)
  
  CQ_year<-ifelse(date_month < 4, date_year-1, date_year)
  
  return(CQ_year)
  
}

snotel<-read.delim("BUTTE_SNOTEL.txt", skip=63, sep = ",")

snotel$Date<-as.Date(snotel$Date)

snotel$WY<-get_waterYear(snotel$Date)

SWE_WY<-snotel %>%
  group_by(WY) %>%
  summarise(peakSWE=max(Snow.Water.Equivalent..in..Start.of.Day.Values))

SWE_WY$normSWE<-normalize(SWE_WY$peakSWE)

ERCQ<-read.csv("CoalCreek_CQ.csv")

ERCQ$date<-as.Date(ERCQ$date)

ERCQ$month<-month(ERCQ$date)

ERCQ$WY<-get_CQyear(ERCQ$date)

ERCQ$WY<-get_waterYear(ERCQ$date)

ercq_WY<-ERCQ %>%
  group_by(WY, variable) %>%
  summarise(
    slope=coef(lm(log10(value)~log10(CC_Q_cms)))[2]
  )

CQ_SWE<-left_join(ercq_WY, SWE_WY, by="WY")

solute_class<-read.csv("solute_classification.csv")
colnames(solute_class)[1]<-"variable"

conversion<-data.frame(t(data.frame(c("1", "geogenic"),
                                    c("2", "bioavailable"),
                                    c("3", "metal"),
                                    c("4", "other"))))

colnames(conversion)<-c("number","class")
conversion$number<-as.integer(conversion$number)

CQ_SWE<-full_join(CQ_SWE, solute_class, by="variable")
CQ_SWE<-full_join(CQ_SWE, conversion, by="number")

CQ_SWE<-CQ_SWE[complete.cases(CQ_SWE$slope),]

ggplot(CQ_SWE, aes(peakSWE, slope, col=class))+geom_point()+theme_bw()+facet_wrap(~variable, scales = "free_y")

CQ_SWE$site<-"CoalCreek"
CQ_SWE_CC<-CQ_SWE

snotel<-read.delim("SCHOFIELD_SNOTEL.txt", skip=63, sep = ",")

snotel$Date<-as.Date(snotel$Date)

snotel$WY<-get_waterYear(snotel$Date)

SWE_WY<-snotel %>%
  group_by(WY) %>%
  summarise(peakSWE=max(Snow.Water.Equivalent..in..Start.of.Day.Values))

SWE_WY$normSWE<-normalize(SWE_WY$peakSWE)

ERCQ<-read.csv("PH_CQ.csv")

ERCQ$date<-as.Date(ERCQ$date)

ERCQ$month<-month(ERCQ$date)
ERCQ$WY<-get_waterYear(ERCQ$date)

ERCQ$WY<-get_CQyear(ERCQ$date)

ercq_WY<-ERCQ %>%
  group_by(WY, variable) %>%
  summarise(
    slope=coef(lm(log10(value)~log10(discharge)))[2]
  )

CQ_SWE<-left_join(ercq_WY, SWE_WY, by="WY")

solute_class<-read.csv("solute_classification.csv")
colnames(solute_class)[1]<-"variable"

conversion<-data.frame(t(data.frame(c("1", "geogenic"),
                                    c("2", "bioavailable"),
                                    c("3", "metal"),
                                    c("4", "other"))))

colnames(conversion)<-c("number","class")
conversion$number<-as.integer(conversion$number)

CQ_SWE<-full_join(CQ_SWE, solute_class, by="variable")
CQ_SWE<-full_join(CQ_SWE, conversion, by="number")

CQ_SWE<-CQ_SWE[complete.cases(CQ_SWE$slope),]

CQ_SWE$site<-"East River"

CQ_SWE_PH<-CQ_SWE

CQ_all<-bind_rows(CQ_SWE_CC, CQ_SWE_PH)

CQ_all<-CQ_all %>%
  group_by(site, variable) %>%
  mutate(p_val=coef(summary(lm(slope~normSWE)))[2,4])

CQ_all<-CQ_all %>%
  filter(variable != c("ammonia_n_ppm")) %>%
  group_by(site, variable) %>%
  mutate(rob_p_val=f.robftest(rlm(slope~normSWE, maxit = 200))$p.value)

pdf("CQ_SWE_CQyear.pdf", width = 12, height = 7)

ggplot(CQ_all, aes(normSWE, slope, col=class, shape=site))+geom_point()+theme_bw()+
  facet_wrap(~variable, scales = "free_y")+
  scale_color_manual(values = c("bioavailable"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  geom_smooth(data=CQ_all[CQ_all$p_val < 0.2,], aes(normSWE, slope, linetype=site), method = "lm", se=F)+
  scale_shape_manual(values = c("East River" = 1,"CoalCreek"=19))+
  scale_linetype_manual(values = c("East River" = "dashed", "CoalCreek" = "solid"))

dev.off()


ercq_WY<-ERCQ %>%
  group_by(WY, variable) %>%
  summarise(
    mean_C=mean(value)
  )
