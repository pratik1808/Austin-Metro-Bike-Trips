################################################################################
# INSTALL PACKAGES/Libraries


# install.packages("dplyr")
# install.packages("fastDummies")
# install.packages("xgboost")
# install.packages("Ckmeans.1d.dp")



################################################################################
# Load Packages/Libraries


library(dplyr)
library(fastDummies)
library(xgboost)
library(Ckmeans.1d.dp)



################################################################################
# Read and prepare the Training and Testing Data from CSV


# 1. Read the data from CSV
training_data = read.csv("Training.csv")
testing_data = read.csv("Test.csv")

# 2. Separate the Xs and Y for training, and standardize them by scaling
y_training = as.numeric(training_data$TripCount)
Xs_training = training_data %>% select(-c("TripCount", "Year", "Month"))

Xs_training$Lat = scale(Xs_training$Lat)[,1]
Xs_training$Long = scale(Xs_training$Long)[,1]
Xs_training$Temp = scale(Xs_training$Temp)[,1]

# 3. Convert "MembershipType" to dummy variables and remove the categorical
  #   column from training Xs
Xs_training = dummy_cols(Xs_training, remove_first_dummy = T)
Xs_training = Xs_training %>% select(-MembershipType)

# 4. Separate the Xs and Y for testing, and standardize them by scaling
y_testing = as.numeric(testing_data$TripCount)
Xs_testing = testing_data %>% select(-c("TripCount", "Year", "Month"))

Xs_testing$Lat = scale(Xs_testing$Lat)[,1]
Xs_testing$Long = scale(Xs_testing$Long)[,1]
Xs_testing$Temp = scale(Xs_testing$Temp)[,1]

# 5. Convert "MembershipType" to dummy variables and remove the categorical
  #   column from testing Xs
Xs_testing = dummy_cols(Xs_testing, remove_first_dummy = T)
Xs_testing = Xs_testing %>% select(-MembershipType)



################################################################################
# Try creating different models by changing the settings (Knobs)


# 6. Set Parameters for building the model
params = list(eval_metric = "rmse",
              objective = "reg:squarederror")

# 7. Vectors to capture the RMSEs for training and Testing data
training_rmse = c()
testing_rmse = c()

# 8. Create a Combination Matrix to train with different settings
Eta = c(0.01, 0.05, 0.2, 0.3, 0.4)
Max_d = c(2, 4, 6, 8, 10)
Num_trees = c(10, 50, 200, 500, 1000)
  
combinations = tidyr::crossing(
  Eta,         # Eta (Lambda)    
  Max_d,       # Max-depth
  Num_trees    # Number of trees
)

# 9. Build each model and calculate the Training and Test RMSEs
for(i in 1:nrow(combinations)) {
  row = combinations[i,]
  model = xgboost(data= as.matrix(Xs_training),
                  label= y_training,
                  params= params,
                  eta = row$Eta,
                  max_depth = row$Max_d,
                  nrounds= row$Num_trees,
                  verbose= 0)
  
  # Calculate Training RMSEs
  predicted <- predict(model, as.matrix(Xs_training))
  RMSE = sqrt(mean((predicted - y_training)^2))
  training_rmse = c(training_rmse, RMSE)
  
  # Calculate Testing RMSEs
  predicted <- predict(model, as.matrix(Xs_testing))
  RMSE = sqrt(mean((predicted - y_testing)^2))
  testing_rmse = c(testing_rmse, RMSE)
}

# 10. Add the Training and Testing RMSEs to the combinations table
combinations["Training_RMSE"] = training_rmse
combinations["Testing_RMSE"] = testing_rmse

# 11. Export/View the Combinations table
#write.csv(combinations, "Combinations_and_outputs.csv", row.names = FALSE)
View(combinations)

# 12. Get the Combinations of settings where the RMSE was minimum
print(combinations[which.min(combinations$Testing_RMSE),])

best_Eta = as.numeric(combinations[which.min(combinations$Testing_RMSE),"Eta"])
best_Max_d = as.numeric(combinations[which.min(combinations$Testing_RMSE),"Max_d"])
best_Num_trees = as.numeric(combinations[which.min(combinations$Testing_RMSE),"Num_trees"])



################################################################################
# Try creating finer models by changing the settings (Knobs)


# 13. Vectors to capture the RMSEs for training and Testing data
training_rmse = c()
testing_rmse = c()

# 14. Create a Combination Matrix to train with different settings (finer)
Eta = 
  c( best_Eta*0.6,
     best_Eta*0.8,
     best_Eta,
     best_Eta*1.2,
     best_Eta*1.4
    )
Max_d = 
  c( as.integer(round(best_Max_d*0.6, digits = 0)),
     as.integer(round(best_Max_d*0.8, digits = 0)),
     as.integer(round(best_Max_d, digits = 0)),
     as.integer(round(best_Max_d*1.2, digits = 0)),
     as.integer(round(best_Max_d*1.4, digits = 0))
    )
Num_trees = 
  c( as.integer(round(best_Num_trees*0.6, digits = 0)),
     as.integer(round(best_Num_trees*0.8, digits = 0)),
     as.integer(round(best_Num_trees, digits = 0)),
     as.integer(round(best_Num_trees*1.2, digits = 0)),
     as.integer(round(best_Num_trees*1.4, digits = 0))
    )

combinations = tidyr::crossing(
  Eta,         # Eta (Lambda)    
  Max_d,       # Max-depth
  Num_trees    # Number of trees
)

# 15. Build each model and calculate the Training and Test RMSEs
for(i in 1:nrow(combinations)) {
  row = combinations[i,]
  model = xgboost(data= as.matrix(Xs_training),
                  label= y_training,
                  params= params,
                  eta = row$Eta,
                  max_depth = row$Max_d,
                  nrounds= row$Num_trees,
                  verbose= 0)
  
  # Calculate Training RMSEs
  predicted <- predict(model, as.matrix(Xs_training))
  RMSE = sqrt(mean((predicted - y_training)^2))
  training_rmse = c(training_rmse, RMSE)
  
  # Calculate Testing RMSEs
  predicted <- predict(model, as.matrix(Xs_testing))
  RMSE = sqrt(mean((predicted - y_testing)^2))
  testing_rmse = c(testing_rmse, RMSE)
}

# 16. Add the Training and Testing RMSEs to the combinations table
combinations["Training_RMSE"] = training_rmse
combinations["Testing_RMSE"] = testing_rmse

# 17. Export/View the Combinations table
write.csv(combinations, "Combinations_and_outputs.csv", row.names = FALSE)
View(combinations)

# 18. Get the Combinations of settings where the RMSE was minimum
print(combinations[which.min(combinations$Testing_RMSE),])

best_Eta = as.numeric(combinations[which.min(combinations$Testing_RMSE),"Eta"])
best_Max_d = as.numeric(combinations[which.min(combinations$Testing_RMSE),"Max_d"])
best_Num_trees = as.numeric(combinations[which.min(combinations$Testing_RMSE),"Num_trees"])



################################################################################
# Build the Best model and visualize feature importance


# 19. Build the model with the best settings (minimum Test RMSE)
model = xgboost(data= as.matrix(Xs_training),
                label= y_training,
                params= params,
                eta = best_Eta,
                max_depth = best_Max_d,
                nrounds= best_Num_trees,
                verbose= 0)

# 20. Calculate feature importance of each of the X
feature_importance = xgb.importance(colnames(as.matrix(Xs_training)), model = model)

# 21. Plot feature importance
(gg <- xgb.ggplot.importance(feature_importance, measure = "Frequency", rel_to_first = TRUE))
gg + ggplot2::ylab("Frequency")

# 22. Remove all variables from workspace
rm(list=ls())