require(ggpubr)
require(ggplot2)
require(EflowStats)
require(lubridate)
require(car)

co_var=function(x){
  val=mean(x)/sd(x)
  return(val)
}

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

kep_solutes<-read.csv("Solutes_Retained.csv")

#read in Coal Creek and ER data and then just change the plot calls (p1, p2 - East River; p3, p4 - Coal Creek)
#need to change "discharge" to "CC_Q_cms"
ERCQ<-read.csv("PH_CQ.csv")

ERCQ$date<-as.Date(ERCQ$date)

ERCQ$month<-month(ERCQ$date)
ERCQ$WY<-get_waterYear(ERCQ$date)

ERCQ %>%
  group_by(month) %>%
  tally()

ERCQ <- ERCQ %>%
  filter(variable %in% kep_solutes$solutes & year(as.Date(date)) > 2015) %>%
  group_by(variable) %>%
  mutate(sd_sol=sd(value), mean_sol=mean(value), lower=mean_sol - 2*sd_sol, upper=mean_sol + 2*sd_sol) %>%
  ungroup() %>%
  filter(value > lower & value < upper)

ercq_overall<-ERCQ %>%
  dplyr::group_by(Element) %>%
  dplyr::summarise(
    slope=coef(lm(log10(value)~log10(discharge)))[2]
  )

ercq_overall$scale<-"overall"

ercq_monthly<-ERCQ %>%
  dplyr::group_by(month, Element) %>%
  dplyr::summarise(
    slope=coef(lm(log10(value)~log10(discharge)))[2],
    cvc_cvq=co_var(value)/co_var(discharge)
  )

ercq_monthly$scale<-"monthly"

ercq_WY<-ERCQ %>%
  dplyr::group_by(WY, Element) %>%
  dplyr::summarise(
    slope=coef(lm(log10(value)~log10(discharge)))[2]
  )

ercq_WY$scale<-"annual"

ercq_allscales<-bind_rows(ercq_overall, ercq_monthly, ercq_WY)

ercq_allscales$scale<-factor(ercq_allscales$scale, levels = c("overall", "annual", "monthly"))

solute_class<-read.csv("solute_class_updated.csv")

colnames(solute_class)[1]<-"Element"

ercq_allscales<-full_join(ercq_allscales, solute_class)

ercq_allscales<-ercq_allscales[complete.cases(ercq_allscales$slope),]

ercq_allscales$Class<-factor(ercq_allscales$Class, levels = c("geogenic", "metal", "biogenic"))

p1<-ggplot(ercq_allscales, aes(scale, slope))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = -0.2, linetype="dashed")+
  geom_hline(yintercept = 0.2, linetype="dashed")+
  geom_jitter(alpha=0.3, aes(col=Class), position=position_jitterdodge(jitter.width = 0.15,
                                                                       jitter.height = 0.15,
                                                                       dodge.width = 0.9))+
  geom_violin(aes(col=Class), alpha=0, size=1, scale="width")+ylim(-2,2)+theme_classic()+
  #geom_boxplot(alpha=0, outliers = F)+theme_bw()+ylim(-2,2)+
  scale_fill_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  scale_color_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  theme(text = element_text(size = 20))+labs(x="", y="", fill="Solute Class", col="Solute Class")+
  scale_x_discrete(labels=c("9-year avg", "annual", "monthly"))

p1

p1_alt<-ggplot(ercq_allscales, aes(Class, slope))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = -0.2, linetype="dashed")+
  geom_hline(yintercept = 0.2, linetype="dashed")+
  geom_violin(aes(col=scale), alpha=0.7, size=0.7, scale="width", width=0.8,
              position = position_dodge(width = 0.9))+
  ylim(-2,2)+
  theme_classic()+
  geom_jitter(size=0.7, aes(col=scale), position=position_jitterdodge(jitter.width = 0.15,dodge.width = 0.9))+
  scale_fill_manual(values = c("overall"="black", "annual"="grey40", "monthly"="grey70"))+
  scale_color_manual(values = c("overall"="black", "annual"="grey40", "monthly"="grey70"))+
  theme(text = element_text(size = 20), legend.position = "bottom")+
  labs(x="", y="C-Q Slope", fill="", col="")+
  scale_x_discrete(labels=c("geogenic", "metal", "biogenic"))+
  ggtitle("East River")

p1_alt

CQ_sum<-ercq_allscales %>% 
  dplyr::filter(scale=="monthly") %>%
  dplyr::group_by(Class) %>%
  dplyr::summarise(mean_cq=mean(slope), sd_cq=sd(slope))

prop_cq_type<-ercq_allscales %>% 
  dplyr::group_by(scale) %>%
  dplyr::summarise(chemostatic=length(which(-0.2 < slope & slope < 0.2)),
                   dilution=length(which(slope < -0.2)),
                   mobilization=length(which(slope > 0.2)))

prop_cq_type$sum<-rowSums(prop_cq_type[,c(2:4)])

prop_cq_type_prop<-prop_cq_type[2:4]/prop_cq_type$sum

prop_cq_type_prop$scale<-prop_cq_type$scale

prop_cq_melt<-melt(prop_cq_type_prop, id.vars=c("scale"))

prop_cq_melt$variable<-factor(prop_cq_melt$variable, levels = c("dilution", "chemostatic", "mobilization"))

prop_cq_melt$scale<-factor(prop_cq_melt$scale, levels = c("overall", "annual", "monthly"))

p2<-ggplot(prop_cq_melt, aes(scale, value, fill=variable))+geom_bar(position="dodge", stat = "identity")+
  scale_fill_manual(values = c("chemostatic"="grey", "dilution"="dodgerblue", "mobilization"="salmon"))+theme_classic()+
  labs(x="", y="", fill="C-Q Behavior")+
  theme(text = element_text(size=20))+ylim(0,1)+
  scale_x_discrete(labels=c("9-year avg", "annual", "monthly"))

p2

###for facet plot
prop_cq_type_facet<-ercq_allscales %>% 
  dplyr::group_by(scale, Class) %>%
  dplyr::summarise(chemostatic=length(which(-0.2 < slope & slope < 0.2)),
                   dilution=length(which(slope < -0.2)),
                   mobilization=length(which(slope > 0.2)))

prop_cq_type_facet$sum<-rowSums(prop_cq_type_facet[,c(3:5)])

prop_cq_type_facet_prop<-prop_cq_type_facet[3:5]/prop_cq_type_facet$sum

prop_cq_type_facet_prop$scale<-prop_cq_type_facet$scale
prop_cq_type_facet_prop$Class<-prop_cq_type_facet$Class

prop_cq_melt_facet<-melt(prop_cq_type_facet_prop, id.vars=c("scale", "Class"))

prop_cq_melt_facet$variable<-factor(prop_cq_melt_facet$variable, levels = c("dilution", "chemostatic", "mobilization"))

prop_cq_melt_facet$scale<-factor(prop_cq_melt_facet$scale, levels = c("overall", "annual", "monthly"))

prop_cq_melt_facet$Class<-factor(prop_cq_melt_facet$Class, levels = c("geogenic", "metal", "biogenic"))

cq_facet_er<-ggplot(prop_cq_melt_facet, aes(scale, value, fill=variable))+geom_bar(position="dodge", stat = "identity")+
  scale_fill_manual(values = c("chemostatic"="grey", "dilution"="dodgerblue", "mobilization"="salmon"))+theme_classic()+
  labs(x="", y="Proportion of Observations", fill="")+
  theme(text = element_text(size=20), legend.position = "bottom")+ylim(0,1)+
  facet_wrap(~Class)+
  scale_x_discrete(labels=c("9-year avg", "annual", "monthly"))

cq_facet_er

er_gg<-ggarrange(p1_alt, cq_facet_er, nrow = 1, widths = c(0.4, 1), align = "h")

er_gg

#read in Coal Creek and ER data and then just change the plot calls (p1, p2 - East River; p3, p4 - Coal Creek)
#need to change "discharge" to "CC_Q_cms"
ERCQ<-read.csv("CoalCreek_CQ.csv")

ERCQ$date<-as.Date(ERCQ$date)

ERCQ$month<-month(ERCQ$date)
ERCQ$WY<-get_waterYear(ERCQ$date)

ERCQ <- ERCQ %>%
  filter(variable %in% kep_solutes$solutes & year(as.Date(date)) > 2015) %>%
  group_by(variable) %>%
  mutate(sd_sol=sd(value), mean_sol=mean(value), lower=mean_sol - 2*sd_sol, upper=mean_sol + 2*sd_sol) %>%
  ungroup() %>%
  filter(value > lower & value < upper)

ercq_overall<-ERCQ %>%
  dplyr::group_by(Element) %>%
  dplyr::summarise(
    slope=coef(lm(log10(value)~log10(CC_Q_cms)))[2]
  )

ercq_overall$scale<-"overall"

ercq_monthly<-ERCQ %>%
  dplyr::group_by(month, Element) %>%
  dplyr::summarise(
    slope=coef(lm(log10(value)~log10(CC_Q_cms)))[2],
    cvc_cvq=co_var(value)/co_var(CC_Q_cms)
  )

ercq_monthly$scale<-"monthly"

ercq_WY<-ERCQ %>%
  dplyr::group_by(WY, Element) %>%
  dplyr::summarise(
    slope=coef(lm(log10(value)~log10(CC_Q_cms)))[2]
    )

ercq_WY$scale<-"annual"

ercq_allscales<-bind_rows(ercq_overall, ercq_monthly, ercq_WY)

ercq_allscales$scale<-factor(ercq_allscales$scale, levels = c("overall", "annual", "monthly"))

solute_class<-read.csv("solute_class_updated.csv")

colnames(solute_class)[1]<-"Element"

ercq_allscales<-full_join(ercq_allscales, solute_class)

ercq_allscales<-ercq_allscales[complete.cases(ercq_allscales$slope),]

ercq_allscales$Class<-factor(ercq_allscales$Class, levels = c("geogenic", "metal", "biogenic"))

p4<-ggplot(ercq_allscales, aes(scale, slope))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = -0.2, linetype="dashed")+
  geom_hline(yintercept = 0.2, linetype="dashed")+
  geom_jitter(alpha=0.3, aes(col=Class), position=position_jitterdodge(jitter.width = 0.15,
                                                                       jitter.height = 0.15,
                                                                       dodge.width = 0.9))+
  geom_violin(aes(col=Class), alpha=0, size=1, scale="width")+ylim(-2,2)+theme_classic()+
  #geom_boxplot(alpha=0, outliers = F)+theme_bw()+ylim(-2,2)+
  scale_fill_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  scale_color_manual(values = c("biogenic"="dodgerblue", "metal"="goldenrod", "geogenic"="firebrick"))+
  theme(text = element_text(size = 20), legend.position = "null")+labs(x="", y="C-Q Slope")+
  scale_x_discrete(labels=c("9-year avg", "annual", "monthly"))

p4

p4_alt<-ggplot(ercq_allscales, aes(Class, slope))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = -0.2, linetype="dashed")+
  geom_hline(yintercept = 0.2, linetype="dashed")+
  geom_violin(aes(col=scale), alpha=0.7, size=0.7, scale="width", width=0.8,
              position = position_dodge(width = 0.9))+ylim(-2,2)+theme_classic()+
  geom_jitter(size=0.7, aes(col=scale), position=position_jitterdodge(jitter.width = 0.15,dodge.width = 0.9))+
  scale_fill_manual(values = c("overall"="black", "annual"="grey40", "monthly"="grey70"))+
  scale_color_manual(values = c("overall"="black", "annual"="grey40", "monthly"="grey70"))+
  theme(text = element_text(size = 20), legend.position = "null")+
  labs(x="", y="C-Q Slope", fill="Temporal Scale", col="Temporal Scale")+
  ggtitle("Coal Creek")

p4_alt

CQ_sum<-ercq_allscales %>% 
  dplyr::filter(scale=="annual") %>%
  dplyr::group_by(Class) %>%
  dplyr::summarise(mean_cq=mean(slope), sd_cq=sd(slope))

prop_cq_type<-ercq_allscales %>% 
  dplyr::group_by(scale) %>%
  dplyr::summarise(chemostatic=length(which(-0.2 < slope & slope < 0.2)),
                   dilution=length(which(slope < -0.2)),
                   mobilization=length(which(slope > 0.2)))

prop_cq_type$sum<-rowSums(prop_cq_type[,c(2:4)])

prop_cq_type_prop<-prop_cq_type[2:4]/prop_cq_type$sum

prop_cq_type_prop$scale<-prop_cq_type$scale

prop_cq_melt<-melt(prop_cq_type_prop, id.vars=c("scale"))

prop_cq_melt$variable<-factor(prop_cq_melt$variable, levels = c("dilution", "chemostatic", "mobilization"))

prop_cq_melt$scale<-factor(prop_cq_melt$scale, levels = c("overall", "annual", "monthly"))

p5<-ggplot(prop_cq_melt, aes(scale, value, fill=variable))+geom_bar(position="dodge", stat = "identity")+
  scale_fill_manual(values = c("chemostatic"="grey", "dilution"="dodgerblue", "mobilization"="salmon"))+theme_classic()+
  labs(x="", y="Proportion of Observations", fill="C-Q Behavior")+
  theme(text = element_text(size=20), legend.position = "null")+ylim(0,1)+
  scale_x_discrete(labels=c("9-year avg", "annual", "monthly"))

p5

###for facet plot
prop_cq_type_facet<-ercq_allscales %>% 
  dplyr::group_by(scale, Class) %>%
  dplyr::summarise(chemostatic=length(which(-0.2 < slope & slope < 0.2)),
                   dilution=length(which(slope < -0.2)),
                   mobilization=length(which(slope > 0.2)))

prop_cq_type_facet$sum<-rowSums(prop_cq_type_facet[,c(3:5)])

prop_cq_type_facet_prop<-prop_cq_type_facet[3:5]/prop_cq_type_facet$sum

prop_cq_type_facet_prop$scale<-prop_cq_type_facet$scale
prop_cq_type_facet_prop$Class<-prop_cq_type_facet$Class

prop_cq_melt_facet<-melt(prop_cq_type_facet_prop, id.vars=c("scale", "Class"))

prop_cq_melt_facet$variable<-factor(prop_cq_melt_facet$variable, levels = c("dilution", "chemostatic", "mobilization"))

prop_cq_melt_facet$scale<-factor(prop_cq_melt_facet$scale, levels = c("overall", "annual", "monthly"))

prop_cq_melt_facet$Class<-factor(prop_cq_melt_facet$Class, levels = c("geogenic", "metal", "biogenic"))

cq_facet_cc<-ggplot(prop_cq_melt_facet, aes(scale, value, fill=variable))+geom_bar(position="dodge", stat = "identity")+
  scale_fill_manual(values = c("chemostatic"="grey", "dilution"="dodgerblue", "mobilization"="salmon"))+theme_classic()+
  labs(x="", y="Proportion of Observations", fill="C-Q Behavior")+
  theme(text = element_text(size=20), legend.position = "null")+ylim(0,1)+
  facet_wrap(~Class)+
  scale_x_discrete(labels=c("9-year avg", "annual", "monthly"))

cq_facet_cc

cc_gg<-ggarrange(p4_alt, cq_facet_cc, nrow = 1, widths = c(0.4, 1), align = "h")

cc_gg

pdf("Figure2.pdf",width = 15, height = 9.5)

ggarrange(cc_gg, er_gg, nrow = 2, heights = c(1,1.2))

dev.off()



