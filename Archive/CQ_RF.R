setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/ER_CQ")

CC_bf<-read.csv("CC_bf_prop.csv")
CC_bf$date<-as.Date(CC_bf$date)

CC_bf$month<-month(CC_bf$date)

CC_bf_monthly<-CC_bf %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(mean_monthly_bf=mean(bf_percent))

CC_SM<-read.csv("snotelSM.csv")
CC_SM<-CC_SM[,c(1,4)]
colnames(CC_SM)[2]<-"soil_moisture"
CC_SM$Date<-as.Date(CC_SM$Date, "%m/%d/%y")

CC_SM_monthly<-CC_SM %>%
  mutate(month=month(Date)) %>%
  group_by(month) %>%
  summarise(mean_SM=mean(soil_moisture, na.rm = T))

CC_Q<-read.csv("Coal_Creek_wy15_22_daily.csv")
CC_Q$date<-as.Date(CC_Q$date, "%m/%d/%y")

CC_Q_monthly<-CC_Q %>%
  mutate(month=month(date)) %>%
  group_by(month) %>%
  summarise(mean_Q=mean(CC_Q_cms, na.rm = T)) 

CC_Q_monthly<-CC_Q_monthly[complete.cases(CC_Q_monthly),]

input_monthly<-left_join(CC_Q_monthly, CC_bf_monthly)

input_monthly<-left_join(input_monthly, CC_SM_monthly)

circular_rolling_mean <- function(x, n) {
  len <- length(x)
  result <- numeric(len)
  
  for (i in seq_along(x)) {
    # Correct: look at current and n previous values
    indices <- ((i - n):i) %% len
    indices[indices == 0] <- len  # replace 0 with len for wraparound
    result[i] <- mean(x[indices])
  }
  
  return(result)
}

lags <- c(1, 2, 6)

for (col in names(input_monthly)) {
  for (lag in lags) {
    new_col <- paste0(col, "_lag", lag)
    input_monthly[[new_col]] <- circular_rolling_mean(input_monthly[[col]], lag)
  }
}

input_monthly<-input_monthly[,-c(5:7)]

CC_CQ<-read.csv("CoalCreek_CQ.csv")

CC_CQ$date<-as.Date(CC_CQ$date)
CC_CQ$month<-month(CC_CQ$date)

CC_CQ_monthly<-CC_CQ %>%
  dplyr::group_by(month, Element) %>%
  dplyr::summarise(
    slope=coef(lm(log(value)~log(CC_Q_cms)))[2]
  )

CQ_drivers_monthly<-left_join(CC_CQ_monthly, input_monthly)

CQ_drivers_monthly_test<-subset(CQ_drivers_monthly, CQ_drivers_monthly$Element=="Ca")

CQ_drivers_monthly_test<-subset(CQ_drivers_monthly, CQ_drivers_monthly$month==2)

input_rf<-CQ_drivers_monthly[,c(2:15)]

set.seed(123)
rf_model1<-randomForest(slope~Element*(mean_Q+mean_monthly_bf+mean_SM+
                                         mean_Q_lag1+mean_monthly_bf_lag1+mean_SM_lag1), 
                        data = input_rf, 
                        importance=TRUE, proximity=TRUE, ntree=300)

#visualize output
rf_model1
plot(rf_model1)

set.seed(123)
rf_model1<-randomForest(slope~Element+mean_Q+mean_monthly_bf+mean_SM, 
                        data = input_rf, 
                        importance=TRUE, proximity=TRUE, ntree=300)


# Get variable importance
importance(rf_model1)

# Plot variable importance
varImpPlot(rf_model1, main = "Variable Importance")

obs_pred<-bind_cols(CQ_drivers_monthly[,c(1,2,3)], rf_model1$predicted)

colnames(obs_pred)[4]<-"model_pred"

obs_pred<-left_join(obs_pred, class)

ggplot(obs_pred, aes(model_pred, slope))+geom_point(aes(col=Class))+geom_abline(slope = 1)

rmse(obs_pred$model_pred, obs_pred$slope)/mean(obs_pred$slope)

summary(lm(model_pred~slope, obs_pred))




