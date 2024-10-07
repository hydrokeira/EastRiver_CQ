require(ggpubr)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

get_CQyear<-function(x){
  
  x<-as.Date(x)
  date_year<-year(x)
  date_month<-month(x)
  
  CQ_year<-ifelse(date_month < 4, date_year-1, date_year)
  
  return(CQ_year)
  
}


#read in Coal Creek and ER data and then just change the plot calls (p1, p2 - East River; p3, p4 - Coal Creek)
#need to change "discharge" to "CC_Q_cms"
ERCQ<-read.csv("PH_CQ.csv")

ERCQ$date<-as.Date(ERCQ$date)

ERCQ$month<-month(ERCQ$date)
#ERCQ$WY<-get_waterYear(ERCQ$date)
ERCQ$WY<-get_CQyear(ERCQ$date)

ercq_overall<-ERCQ %>%
  group_by(variable) %>%
  summarise(
    slope=coef(lm(log10(value)~log10(discharge)))[2]
  )

ercq_overall$scale<-"overall"

ercq_monthly<-ERCQ %>%
  group_by(month, variable) %>%
  summarise(
    slope=coef(lm(log10(value)~log10(discharge)))[2]
  )

ercq_monthly$scale<-"monthly"

ercq_WY<-ERCQ %>%
  group_by(WY, variable) %>%
  summarise(
    slope=coef(lm(log10(value)~log10(discharge)))[2]
  )

ercq_WY$scale<-"annual"

# ercq_WY<-ERCQ %>%
#   group_by(WY, variable) %>%
#   summarise(
#     slope_WY=coef(lm(log10(value)~log10(CC_Q_cms)))[2]
#   )
# 
# ercq_both<-left_join(ercq_CQ, ercq_WY, by=c("variable","WY"))
# 
# solute_class<-read.csv("solute_classification.csv")
# colnames(solute_class)[1]<-"variable"
# 
# conversion<-data.frame(t(data.frame(c("1", "geogenic"),
#                                     c("2", "bioavailable"),
#                                     c("3", "metal"),
#                                     c("4", "other"))))
# 
# colnames(conversion)<-c("number","class")
# conversion$number<-as.integer(conversion$number)
# 
# ercq_both<-full_join(ercq_both, solute_class, by="variable")
# ercq_both<-full_join(ercq_both, conversion, by="number")
# 
# ercq_both<-ercq_both[complete.cases(ercq_both$slope_CQ),]
# 
# ggplot(ercq_both, aes(slope_WY, slope_CQ, col=class))+geom_point(size=2)+ylim(-1, 1)+geom_abline(slope = 1)+xlim(-1,1)+
#   theme_bw()+theme(text = element_text(size=20))+
#   scale_color_manual(values = c("bioavailable"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))
# 

ercq_allscales<-bind_rows(ercq_overall, ercq_monthly, ercq_WY)

ercq_allscales$scale<-factor(ercq_allscales$scale, levels = c("overall", "annual", "monthly"))

solute_class<-read.csv("solute_classification.csv")
colnames(solute_class)[1]<-"variable"

conversion<-data.frame(t(data.frame(c("1", "geogenic"),
                                    c("2", "bioavailable"),
                                    c("3", "metal"),
                                    c("4", "other"))))

colnames(conversion)<-c("number","class")
conversion$number<-as.integer(conversion$number)

ercq_allscales<-full_join(ercq_allscales, solute_class, by="variable")
ercq_allscales<-full_join(ercq_allscales, conversion, by="number")

ercq_allscales<-ercq_allscales[complete.cases(ercq_allscales$slope),]

p1<-ggplot(ercq_allscales, aes(scale, slope))+geom_jitter(alpha=0.5, aes(col=class))+
  geom_boxplot(alpha=0, outliers = F)+theme_bw()+ylim(-2,2)+
  scale_color_manual(values = c("bioavailable"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  theme(text = element_text(size = 20))+labs(x="Scale", y="CQ Slope")

p1

prop_cq_type<-ercq_allscales %>% 
  group_by(scale) %>%
  summarise(chemostatic=length(which(-0.2 < slope & slope < 0.2)),
            dilution=length(which(slope < -0.2)),
            mobilization=length(which(slope > 0.2)))

prop_cq_type$sum<-rowSums(prop_cq_type[,c(2:4)])

prop_cq_type_prop<-prop_cq_type[2:4]/prop_cq_type$sum

prop_cq_type_prop$scale<-prop_cq_type$scale

prop_cq_melt<-melt(prop_cq_type_prop, id.vars="scale")

prop_cq_melt$variable<-factor(prop_cq_melt$variable, levels = c("dilution", "chemostatic", "mobilization"))

prop_cq_melt$scale<-factor(prop_cq_melt$scale, levels = c("overall", "annual", "monthly"))

p2<-ggplot(prop_cq_melt, aes(scale, value, fill=variable))+geom_bar(position="dodge", stat = "identity")+
  scale_fill_manual(values = c("chemostatic"="grey", "dilution"="dodgerblue", "mobilization"="salmon"))+theme_bw()+
  labs(x="Scale", y="Proportion of Observations", fill="CQ Behavior")+
  theme(text = element_text(size=20))+ylim(0,1)

p4

ggarrange(p1, p2, p3, p4)




