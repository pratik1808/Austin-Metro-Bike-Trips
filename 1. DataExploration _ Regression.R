library("dplyr")
library("tidyr")
library("ggmap")
library(tibble)

## Inputting Files
train_set = read.csv("/Users/tanushreedevi/Desktop/Stats_R_project/Train & Test - Aggregated/Training.csv")
test_set = read.csv("/Users/tanushreedevi/Desktop/Stats_R_project/Train & Test - Aggregated/Test.csv")

## DATA EXPLORATION  

full_set = rbind(train_set,test_set)
full_set_agg = full_set %>% group_by(full_set[6]) %>% summarise(total_trips = sum(TripCount))

par(mfrow=c(1,3)) 
plot(full_set$Lat,full_set$TripCount)
plot(full_set$Long,full_set$TripCount)
plot(full_set$LongTrip,full_set$TripCount)

par(mfrow=c(1,2)) 
plot(full_set$Temp,full_set$TripCount)
plot(full_set_agg$Month,full_set_agg$total_trips)

# -------------------------------------------------------------------------------- #

###### DATA CLEANING 

## Adding a boolean column to assess temperature
train_set <- train_set %>% add_column(is_hot_day = if_else(.$Temp>median(train_set$Temp), 1, 0))
test_set <- test_set %>% add_column(is_hot_day = if_else(.$Temp>median(test_set$Temp), 1, 0))

## Aggregating Data at desired granularity for Regression
train_set_agg = train_set %>% group_by(train_set[2],train_set[3],train_set[4],
                                       train_set[9],train_set[10],train_set[11],train_set[12],
                                       train_set[13],train_set[14],train_set[15],train_set[16],
                                       train_set[17],train_set[18],train_set[19],train_set[20],train_set[21]) %>% summarise(number_trips = sum(TripCount))

test_set_agg = test_set %>% group_by(test_set[2],test_set[3],test_set[4],
                                     test_set[9],test_set[10],test_set[11],test_set[12],
                                     test_set[13],test_set[14],test_set[15],test_set[16],
                                     test_set[17],test_set[18],test_set[19],test_set[20],test_set[21]) %>% summarise(number_trips = sum(TripCount))

# -------------------------------------------------------------------------------- #

## MULTIPLE LINEAR REGRESSION 

df_forward <- train_set_agg[,-c(17)] # lose trip count
df_forward_test <- test_set_agg[,-c(17)] # lose trip count
n = dim(df_forward)[1]

#### Scaling Variables
numtrips = train_set_agg$number_trips
numtrips_log = log(train_set_agg$number_trips)
XXmulreg <- data.frame(numtrips,scale(df_forward)) # For Simple Multiple Reg

# -- Full Model
model_mulreg_full = lm(numtrips~., data=XXmulreg)
summary(model_mulreg_full)

# -- Tuned Model
model_mulreg = lm(numtrips~ Long+ Month_Mar+Month_May+Month_Dec, data=XXmulreg)
summary(model_mulreg)

# -- Log Model
model_mulreg_log = lm(log(numtrips)~ Lat+ Long+
                        Month_Mar+Month_May+Month_Jun+Month_Jul+
                        Month_Sep, data=XXmulreg)
summary(model_mulreg_log)

# -------------------------------------------------------------------------------- #

## SUB-SETTING AND STEP-WISE REGRESSION

# Incorporating geo information into the interaction elements
XXdf <- model.matrix(~.*Long*Lat, data=data.frame(scale(df_forward)))[,-1]

dfdata = data.frame(numtrips,XXdf)
dfdata_log = data.frame(numtrips_log,XXdf)

null = lm(numtrips~1, data=dfdata)
full = lm(numtrips~., data=dfdata)

regForward = step(null, scope=formula(full), direction="forward", k=log(length(dfdata)))
regBack = step(full, direction="backward", k=log(length(dfdata)))
regBoth = step(null, scope=formula(full), direction="both", k=log(length(dfdata)))

# Forward Model - copied from results of 'regForward'
model1 = lm(numtrips~Month_Mar + Long + LongTrip.Lat + Lat.Month_Mar + 
              Lat.Long.Month_Mar + Lat.Long + is_hot_day + LongTrip.Long + 
              LongTrip.Lat.Long + Month_Apr + Lat.Month_Sep + Month_Dec + 
              Month_Oct + Lat + Lat.is_hot_day + Month_May, data=dfdata)
summary(model1)


# Backward Model - copied from results of 'regBack'
model2 = lm(numtrips~Lat + Long + Month_Jan + Month_Feb + Month_Mar + Month_May + 
              Month_Jun + Month_Jul + Month_Aug + Month_Sep + Month_Oct + 
              Month_Nov + LongTrip.Long + Lat.Long + Long.Month_May + LongTrip.Lat + 
              Lat.Month_Mar + Lat.Month_May + Lat.Month_Sep + LongTrip.Lat.Long + 
              Lat.Long.Month_Mar, data=dfdata)
summary(model2)

# Stepwise Model - copied from results of 'regBoth'
model3 = lm(numtrips~Month_Mar + Long + LongTrip.Lat + Lat.Month_Mar + 
              Lat.Long.Month_Mar + Lat.Long + is_hot_day + LongTrip.Long + 
              LongTrip.Lat.Long + Month_Apr + Lat.Month_Sep + Month_Dec + 
              Month_Oct + Lat + Lat.is_hot_day + Month_May, data=dfdata)
summary(model3)

# Validating the Fit on Test Set
numtrips_test = test_set_agg$number_trips
numtrips_test_log = log(test_set_agg$number_trips)

XXdf_test <- model.matrix(~.*Long*Lat, data=data.frame(scale(df_forward_test)))[,-1]
dfdata_test = data.frame(numtrips_test,XXdf_test)

#Forward Model
testmodel_1 = lm(numtrips_test~Month_Mar + Long + LongTrip.Lat + Lat.Month_Mar + 
                   Lat.Long.Month_Mar + Lat.Long + is_hot_day + LongTrip.Long + 
                   LongTrip.Lat.Long + Month_Apr + Lat.Month_Sep + Month_Dec + 
                   Month_Oct + Lat + Lat.is_hot_day + Month_May, data=dfdata_test)
summary(testmodel_1)

pred = predict(model1, newdata = dfdata_test)
a=mean((numtrips_test - pred) ^ 2)
a

# Backward Model - copied from results of 'regBack'
testmodel_2 = lm(numtrips_test~Lat + Long  + Month_Feb + Month_Mar + Month_May + 
                   Month_Jul  + Month_Sep  + LongTrip.Long + Lat.Long + LongTrip.Lat + 
                   Lat.Month_Mar + LongTrip.Lat.Long + Lat.Long.Month_Mar, data=dfdata_test)
summary(testmodel_2)

# Stepwise Model - copied from results of 'regBoth'
testmodel_3 = lm(numtrips_test~Month_Mar + Long + LongTrip.Lat + Lat.Month_Mar + 
                   Lat.Long.Month_Mar + Lat.Long + is_hot_day + LongTrip.Long + 
                   LongTrip.Lat.Long + Month_Apr + Lat.Month_Sep + Month_Dec + 
                   Month_Oct + Lat + Lat.is_hot_day + Month_May, data=dfdata_test)
summary(testmodel_3)
# -------------------------------------------------------------------------------- #

# FORWARD REGRESSION WITH LOG 

numtrips_log = log(train_set_agg$number_trips)
dfdata_log = data.frame(numtrips_log,XXdf)
null_log = lm(numtrips_log~1, data=dfdata_log)
full_log = lm(numtrips_log~., data=dfdata_log)
regForward_log = step(null_log, scope=formula(full_log), direction="forward", k=log(length(dfdata)))
summary(regForward_log)

# -------------------------------------------------------------------------------- #

