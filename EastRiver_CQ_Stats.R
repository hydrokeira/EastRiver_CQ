#read in data, cleam#
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

####compare CQ slope for each solute across timescale####

geo_df<-ercq_allscales %>%
  filter(Class=="geogenic")

anove_geo<-aov(slope~scale, data=geo_df)

tuk<-TukeyHSD(anove_geo)

tuk_df <- as.data.frame(tuk$scale) %>%
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

mat<-mat %>%
  mutate(Class1 = factor(Class1, levels=c("overall", "annual", "monthly")),
         Class2 = factor(Class2, levels=c("overall", "annual", "monthly")))

p1<-ggplot(mat, aes(Class1, Class2, fill = sig_final)) +
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
  )+ggtitle("Geogenic CQ Slope Comparison")
p1
#### metal ####
geo_df<-ercq_allscales %>%
  filter(Class=="metal")

anove_geo<-aov(slope~scale, data=geo_df)

tuk<-TukeyHSD(anove_geo)

tuk_df <- as.data.frame(tuk$scale) %>%
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

mat<-mat %>%
  mutate(Class1 = factor(Class1, levels=c("overall", "annual", "monthly")),
         Class2 = factor(Class2, levels=c("overall", "annual", "monthly")))

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
  )+ggtitle("Metal CQ Slope Comparison")

#### biogenic ####
geo_df<-ercq_allscales %>%
  filter(Class=="biogenic")

anove_geo<-aov(slope~scale, data=geo_df)

tuk<-TukeyHSD(anove_geo)

tuk_df <- as.data.frame(tuk$scale) %>%
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

mat<-mat %>%
  mutate(Class1 = factor(Class1, levels=c("overall", "annual", "monthly")),
         Class2 = factor(Class2, levels=c("overall", "annual", "monthly")))

p3<-ggplot(mat, aes(Class1, Class2, fill = sig_final)) +
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
  )+ggtitle("Biogenic CQ Slope Comparison")

p_all<-ggarrange(p1, p2, p3, nrow = 1)

#### now compare variance for each solute across scales ####
#define pairwise levene test#
pairwise_levene <- function(df, response, group) {
  
  groups <- unique(df[[group]])
  combs  <- combn(groups, 2, simplify = FALSE)
  
  map_dfr(combs, function(g) {
    
    sub <- df %>% filter(.data[[group]] %in% g)
    
    lv <- leveneTest(
      reformulate(group, response),
      data = sub,
      center = median
    )
    
    tibble(
      group1 = g[1],
      group2 = g[2],
      p_value = lv$`Pr(>F)`[1]
    )
  })
}
#### geogenic ####
geo_df<-ercq_allscales %>%
  filter(Class=="geogenic")

lev_df <- pairwise_levene(
  df       = geo_df,
  response = "slope",
  group    = "scale"
) %>%
  mutate(sig = p_value < 0.05)

classes <- sort(unique(c(lev_df$group1, lev_df$group2)))

mat <- expand.grid(
  Class1 = classes,
  Class2 = classes
) %>%
  left_join(lev_df, by = c("Class1" = "group1",
                           "Class2" = "group2")) %>%
  left_join(lev_df, by = c("Class1" = "group2",
                           "Class2" = "group1"),
            suffix = c("", "_rev")) %>%
  mutate(
    sig_final = case_when(
      Class1 == Class2 ~ NA,
      !is.na(sig) ~ sig,
      !is.na(sig_rev) ~ sig_rev,
      TRUE ~ FALSE
    )
  )

k1<-ggplot(mat, aes(Class1, Class2, fill = sig_final)) +
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
    fill = "BF Variance diff\np < 0.05",
    x = NULL,
    y = NULL
  )+ggtitle("Geogenic CQ Slope Variance Comparison")

#### metal ####
geo_df<-ercq_allscales %>%
  filter(Class=="metal")

lev_df <- pairwise_levene(
  df       = geo_df,
  response = "slope",
  group    = "scale"
) %>%
  mutate(sig = p_value < 0.05)

classes <- sort(unique(c(lev_df$group1, lev_df$group2)))

mat <- expand.grid(
  Class1 = classes,
  Class2 = classes
) %>%
  left_join(lev_df, by = c("Class1" = "group1",
                           "Class2" = "group2")) %>%
  left_join(lev_df, by = c("Class1" = "group2",
                           "Class2" = "group1"),
            suffix = c("", "_rev")) %>%
  mutate(
    sig_final = case_when(
      Class1 == Class2 ~ NA,
      !is.na(sig) ~ sig,
      !is.na(sig_rev) ~ sig_rev,
      TRUE ~ FALSE
    )
  )

k2<-ggplot(mat, aes(Class1, Class2, fill = sig_final)) +
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
    fill = "BF Variance diff\np < 0.05",
    x = NULL,
    y = NULL
  )+ggtitle("Metal CQ Slope Variance Comparison")

#### biogenic ####
geo_df<-ercq_allscales %>%
  filter(Class=="biogenic")

lev_df <- pairwise_levene(
  df       = geo_df,
  response = "slope",
  group    = "scale"
) %>%
  mutate(sig = p_value < 0.05)

classes <- sort(unique(c(lev_df$group1, lev_df$group2)))

mat <- expand.grid(
  Class1 = classes,
  Class2 = classes
) %>%
  left_join(lev_df, by = c("Class1" = "group1",
                           "Class2" = "group2")) %>%
  left_join(lev_df, by = c("Class1" = "group2",
                           "Class2" = "group1"),
            suffix = c("", "_rev")) %>%
  mutate(
    sig_final = case_when(
      Class1 == Class2 ~ NA,
      !is.na(sig) ~ sig,
      !is.na(sig_rev) ~ sig_rev,
      TRUE ~ FALSE
    )
  )

k3<-ggplot(mat, aes(Class1, Class2, fill = sig_final)) +
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
    fill = "BF Variance diff\np < 0.05",
    x = NULL,
    y = NULL
  )+ggtitle("Biogenic CQ Slope Variance Comparison")

k_all<-ggarrange(k1, k2, k3, nrow = 1)

pdf("Stats_CQ_Slopes_EastRiver.pdf", width = 13.5, height = 6)

ggarrange(p_all, k_all, nrow = 2)

dev.off()
