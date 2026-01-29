setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

kep_solutes<-read.csv("Solutes_Retained.csv")

#read in Coal Creek and ER data and then just change the plot calls (p1, p2 - East River; p3, p4 - Coal Creek)
#need to change "discharge" to "CC_Q_cms"
ERCQ<-read.csv("CoalCreek_CQ.csv")

ERCQ<-ERCQ %>%
  filter(Element %in% c("Cd", "Zn")) %>%
  filter(value < 4000)

pdf("Bulkhead_Metals.pdf", width = 13, height = 7)

ggplot(ERCQ, aes(as.Date(date), value))+geom_point()+theme_classic()+facet_wrap(~Element, nrow = 2, scales = "free_y")+
  theme(text = element_text(size=20))+labs(x="Date", y="Concentration (ppb)")+
  geom_vline(xintercept = as.Date("2018-10-01"))+
  geom_vline(xintercept = as.Date("2019-10-01"))+
  geom_vline(xintercept = as.Date("2023-06-23"))

dev.off()

ERCQ_BH <- ERCQ %>%
  mutate(date=as.Date(date)) %>%
  mutate(bulkhead_time = case_when(
    date < as.Date("2018-10-01")~"prebulkhead",
    date > as.Date("2018-10-01") & date < as.Date("2019-10-01")~"bulkhead open",
    date > as.Date("2019-10-01") & date < as.Date("2023-06-23")~"bulkhead partially open",
    date < as.Date("2023-06-23")~"bulkhead closed",
    .default = NA
  )) %>%
  filter(!is.na(bulkhead_time))

ERCQ_BH_solute <- ERCQ_BH %>%
  filter(Element=="Cd")

anova<-aov(value~bulkhead_time, data=ERCQ_BH_solute)

tuk<-TukeyHSD(anova)

tuk_df <- as.data.frame(tuk$bulkhead_time) %>%
  rownames_to_column("comparison") %>%
  separate(comparison, into = c("Class1", "Class2"), sep = "-") %>%
  mutate(
    sig = `p adj` < 0.05
  )

classes <- sort(unique(c(tuk_df$Class1, tuk_df$Class2)))

mat <- expand.grid(
  Class1 = classes,
  Class2 = classes
) %>%
  left_join(tuk_df, by = c("Class1", "Class2")) %>%
  left_join(tuk_df, by = c("Class1" = "Class2",
                           "Class2" = "Class1"),
            suffix = c("", "_rev")) %>%
  mutate(
    sig_final = case_when(
      Class1 == Class2 ~ NA,
      !is.na(sig) ~ sig,
      !is.na(sig_rev) ~ sig_rev,
      TRUE ~ FALSE
    )
  )

p2<-ggplot(mat, aes(Class1, Class2, fill = sig_final)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c(`TRUE` = "blue", `FALSE` = "grey"),
    na.value = "black"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(
    fill = "Tukey HSD\np < 0.05",
    x = NULL,
    y = NULL
  )+ggtitle("Cd Bulkhead Comparison")

pdf("Bulkhead_ANOVA.pdf", width = 10, height = 6)

ggarrange(p1, p2)

dev.off()

  