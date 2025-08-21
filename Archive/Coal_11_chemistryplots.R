require(ggplot2)
require(lubridate)
require(cetcolor)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

cq<-read.csv("CoalCreek_CQ.csv")
cq$date<-as.Date(cq$date)
cq$month_collect<-month(cq$date)

solutes<-unique(cq$Element)

pdf("Coal11_CQ.pdf")

for (i in 1:length(solutes)) {
  
  one_sol<-subset(cq, cq$Element==solutes[i])
  
  p1<-ggplot(one_sol, aes(log(CC_Q_cms), log(value)))+geom_point(aes(col=as.factor(month_collect)))+
    theme_classic()+theme(text = element_text(size=20))+
    labs(x="Log(Q) (cms)", y="Log(concentration) (ppb)", col="Month")+
    ggtitle(solutes[i])+
    scale_color_manual(values = cet_pal(12, "c4s"))
  
  print(p1)
}

dev.off()

coal_chem<-read.csv("Coal_All_Chem.csv")
coal_chem$date<-as.Date(coal_chem$date)

pdf("Coal11_timeSeries.pdf")

for (i in 1:length(solutes)) {
  
  one_sol<-subset(coal_chem, coal_chem$Element==solutes[i])
  
  p1<-ggplot(one_sol, aes(date, value))+geom_point(aes(col=as.factor(month(date))))+
    theme_classic()+theme(text = element_text(size=20))+
    labs(x="Date", y="Concentration (ppb)", col="Month")+
    ggtitle(solutes[i])+
    scale_color_manual(values = cet_pal(12, "c4s"))
  
  print(p1)
}

dev.off()

