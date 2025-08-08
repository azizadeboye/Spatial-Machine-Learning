
rm(list = ls(all = TRUE))
graphics.off()
shell("cls")

library(tidyverse)
library(sp)
library(sf)
library(fastmap)
library(skimr)
library(tmap)
library(tmaptools)
library(spdep)
#library(rgeos)
library(rgeoda)
library(terra)
#library(maptools) #install.packages("maptools", repos="http://R-Forge.R-project.org")
#library(rgdal)
library(spdep)
library(classInt)
library(gstat)
library(dplyr)

nat<- st_read("Africa/Africa_Countries.shp")
nat.sp <- as(nat, "Spatial")
class(nat.sp)

# Import the excel data
library(readxl)
mdata <- read_excel("panc_prevalence.xlsx")

## Joining/ merging my data and shapefiles

mapdata <- inner_join(nat, mdata, by = "NAME")   ## OR mapdata <- left_join(nat, codata, by = "DISTRICT_N")
str(mapdata)

#Basic map with labels
# quantile map
p1 <- tm_shape(mapdata) + 
  tm_fill("prevalence", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Prevalence")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p2 <- tm_shape(mapdata) + 
  tm_fill("female", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Female")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p3 <- tm_shape(mapdata) + 
  tm_fill("male", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Male")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p4 <- tm_shape(mapdata) + 
  tm_fill("ageb", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Age:20-54yrs")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p5 <- tm_shape(mapdata) + 
  tm_fill("agec", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Age:55+yrs")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p6 <- tm_shape(mapdata) + 
  tm_fill("agea", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Age:<20yrs")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p7 <- tm_shape(mapdata) + 
  tm_fill("fageb", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Female:20-54yrs")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p8 <- tm_shape(mapdata) + 
  tm_fill("fagec", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Female:55+yrs")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p9 <- tm_shape(mapdata) + 
  tm_fill("fagea", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Female:<20yrs")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p10 <- tm_shape(mapdata) + 
  tm_fill("mageb", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Male:20-54yrs")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p11 <- tm_shape(mapdata) + 
  tm_fill("magec", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Male:55+yrs")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p12 <- tm_shape(mapdata) + 
  tm_fill("magea", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Male:<20yrs")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p13 <- tm_shape(mapdata) + 
  tm_fill("yra", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Year:2017")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p14 <- tm_shape(mapdata) + 
  tm_fill("yrb", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "year:2018")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p15 <- tm_shape(mapdata) + 
  tm_fill("yrc", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Year:2019")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p16 <- tm_shape(mapdata) + 
  tm_fill("yrd", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Year:2020")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)
p17 <- tm_shape(mapdata) + 
  tm_fill("yre", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Year: 2021")) + tm_borders(fill_alpha = .3) + tm_compass() + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), frame = TRUE, component.autoscale = FALSE)

current.mode <- tmap_mode("plot")
tmap_arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, 
             widths = c(.75, .75))
tmap_mode(current.mode)

#################################################################################################
## Model building

install.packages(c("randomForest", "xgboost", "e1071", "nnet", "caret"))
library(randomForest)
library(xgboost) 
library(e1071) 
library(nnet) 
library(caret)

# 1. Random Forest Regression
# Train Random Forest
set.seed(123)
rf_model <- randomForest(prevalence ~ female + male + agea + ageb + agec + fagea + fageb + fagec +
                           magea + mageb + magec + yrb + yrc + yrd + yre, data = mapdata, ntree = 500, 
                         importance = TRUE)

# View model results
print(rf_model)
varImpPlot(rf_model)

#Plot Variable Importance
library(ggplot2)
importance_df <- data.frame(
  Variable = rownames(importance(rf_model)),
  Importance = importance(rf_model)[, "IncNodePurity"])

ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance (IncNodePurity)", x = "Variable", y = "Importance")

# 2. Gradient Boosting Machine (XGBoost)
library(xgboost)
# Prepare the data
x_vars <- model.matrix(prevalence ~ female + male + agea + ageb + agec + fagea + fageb + fagec +
                         magea + mageb + magec + yrb + yrc + yrd + yre, data = mapdata)[,-1]
y <- mapdata$prevalence

# Convert to DMatrix
dtrain <- xgb.DMatrix(data = x_vars, label = y)

# Train model
xgb_model <- xgboost(data = dtrain, 
                     objective = "reg:squarederror", 
                     nrounds = 100,
                     max_depth = 4,
                     eta = 0.1,
                     verbose = 0)

# Feature importance
xgb.importance(model = xgb_model)

# 3. Support Vector Regression (SVR)
library(e1071)

# Train SVR model
svr_model <- svm(prevalence ~ female + male + agea + ageb + agec + fagea + fageb + fagec +
                   magea + mageb + magec + yrb + yrc + yrd + yre, data = mapdata, 
                 type = "eps-regression", 
                 kernel = "radial")

# Summary and predictions
summary(svr_model)
#mapdata$pred_svr <- predict(svr_model)

# Model Evaluation (predictions) using caret:
# evaluate each model step-by-step:
library(caret)
# 1. Random Forest Evaluation
rf_preds <- predict(rf_model, newdata = mapdata)
rf_metrics <- postResample(pred = rf_preds, obs = mapdata$prevalence)
print(rf_metrics)

# 2. XGBoost Evaluation
xgb_preds <- predict(xgb_model, newdata = x_vars)
xgb_metrics <- postResample(pred = xgb_preds, obs = mapdata$prevalence)
print(xgb_metrics)

# 3. SVR Evaluation
svr_preds <- predict(svr_model, newdata = mapdata)
svr_metrics <- postResample(pred = svr_preds, obs = mapdata$prevalence)
print(svr_metrics)

# Compare All Models
model_eval <- data.frame(
  Model = c("Random Forest", "XGBoost", "SVR"),
  RMSE = c(rf_metrics["RMSE"], xgb_metrics["RMSE"], svr_metrics["RMSE"]),
  MAE = c(rf_metrics["MAE"], xgb_metrics["MAE"], svr_metrics["MAE"]),
  Rsquared = c(rf_metrics["Rsquared"], xgb_metrics["Rsquared"], svr_metrics["Rsquared"]))

print(model_eval)

#Choosing the Best Model
#Lowest RMSE and MAE = most accurate predictions
#Highest R² = best variance explanation
#Sort and interpret:
model_eval[order(model_eval$RMSE), ]  # Best = Top row

#### Models Predicted plots
library(ggplot2)
library(tidyr)
library(dplyr)
# Create a data frame from your table
model_preds <- data.frame(rf_preds, xgb_preds, svr_preds)
# Add observation ID
model_preds$ID <- 1:nrow(model_preds)

# Reshape for plotting
model_long <- model_preds %>%
  pivot_longer(cols = c("rf_preds", "xgb_preds", "svr_preds"), names_to = "Model", values_to = "Predicted")

# Plot
ggplot(model_long, aes(x = ID, y = Predicted, color = Model)) +
  geom_line(linewidth = 0.5) +
  labs(title = "Model Predictions Over Observations",
       x = "Observation", y = "Predicted Prevalence") +
  theme_minimal()

## plot actual vs predicted 
par(mfrow = c(1, 3))  # 3 plots side-by-side

# Random Forest
plot(mapdata$prevalence, mapdata$rf_pred,
     xlab = "Observed", ylab = "Predicted",
     main = "Random Forest", pch = 19, col = "steelblue")
abline(0, 1, col = "red", lwd = 2)

# XGBoost
plot(mapdata$prevalence, mapdata$xgb_pred,
     xlab = "Observed", ylab = "Predicted",
     main = "XGBoost", pch = 19, col = "darkgreen")
abline(0, 1, col = "red", lwd = 2)

# SVR
plot(mapdata$prevalence, mapdata$svr_pred,
     xlab = "Observed", ylab = "Predicted",
     main = "SVR", pch = 19, col = "purple")
abline(0, 1, col = "red", lwd = 2)

par(mfrow = c(1, 1))  # Reset layout

## Actual vs predicted plot with correlations

library(ggplot2)
library(ggpubr)  # For stat_cor

mapdata$rf_pred <- predict(rf_model)
mapdata$xgb_pred <- predict(xgb_model, newdata = x_vars)
mapdata$svr_pred <- predict(svr_model, newdata = mapdata)

# Random Forest Plot
p1 <- ggplot(mapdata, aes(x = prevalence, y = rf_pred)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  stat_cor(method = "pearson", aes(label = paste0("R² = ")), label.x = 0) +
  labs(title = "Random Forest", x = "Observed Prevalence", y = "Predicted Prevalence") +
  theme_minimal()

# XGBoost Plot
p2 <- ggplot(mapdata, aes(x = prevalence, y = xgb_pred)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  stat_cor(method = "pearson", aes(label = paste0("R² = ")), label.x = 0) +
  labs(title = "XGBoost", x = "Observed Prevalance", y = "Predicted Prevalence") +
  theme_minimal()

# SVR Plot
p3 <- ggplot(mapdata, aes(x = prevalence, y = svr_pred)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  stat_cor(method = "pearson", aes(label = paste0("R² = ")), label.x = 0) +
  labs(title = "SVR", x = "Observed Prevalence", y = "Predicted Prevalence") +
  theme_minimal()

combined_plot <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1, common.legend = FALSE)

# Save to PDF
ggsave("Observed_vs_Predicted_All_Models.pdf", plot = combined_plot, width = 12, height = 4)

# Save to PNG
ggsave("Observed_vs_Predicted_All_Models.png", plot = combined_plot, width = 12, height = 4, dpi = 300)



################################################################################################

## CROSS-VALIDATION
#Step 1: Prepare common setup
# Set seed for reproducibility
set.seed(123)

# Define 5-fold cross-validation
cv_control <- trainControl(method = "cv", number = 5)

# 1. Random Forest
library(randomForest)

rf_cv <- train(
  prevalence ~ female + male + agea + ageb + agec + fagea + fageb + fagec +
    magea + mageb + magec + yrb + yrc + yrd + yre,
  data = mapdata,
  method = "rf",
  trControl = cv_control,
  tuneLength = 3,
  importance = TRUE
)
print(rf_cv)

# 2. XGBoost
library(xgboost)

xgb_cv <- train(
  prevalence ~ female + male + agea + ageb + agec + fagea + fageb + fagec +
    magea + mageb + magec + yrb + yrc + yrd + yre,
  data = mapdata,
  method = "xgbTree",
  trControl = cv_control,
  tuneLength = 3
)
print(xgb_cv)

# 3. Support Vector Regression (SVR)
library(e1071)
library(kernlab)

svr_cv <- train(
  prevalence ~ female + male + agea + ageb + agec + fagea + fageb + fagec +
    magea + mageb + magec + yrb + yrc + yrd + yre,
  data = mapdata,
  method = "svmRadial",
  trControl = cv_control,
  preProcess = c("center", "scale"),  # SVR often benefits from scaling
  tuneLength = 3
)
print(svr_cv)


# Compare All Models (from CV)
results <- resamples(list(
  RF = rf_cv,
  XGB = xgb_cv,
  SVR = svr_cv
))

# Summary of RMSE, MAE, Rsquared
summary(results)

# Boxplot of performance
bwplot(results)


############################################################################################
## Spatial maps of predicted values of each model

install.packages("tmap")
library(tmap)

# 1. Random Forest Spatial Map
mapdata$pred_rf <- predict(rf_model, newdata = mapdata)

#tm_shape(mapdata) + tm_fill("pred_rf", fill.scale =tm_scale_intervals(values = "brewer.yl_gn_bu", style = "quantile"), fill.legend = tm_legend(title = "Prev_pred_rf")) + tm_borders()
#tm_layout(title = "Random Forest: Predicted mortality Cases")

tm_shape(mapdata) + 
  tm_fill("pred_rf", fill.scale =tm_scale_intervals(values = "brewer.greens",  style = "quantile"), 
          fill.legend = tm_legend(title = "Prev_pred_rf")) + tm_borders(fill_alpha = .2) + 
  tm_compass() + tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), 
                           frame = TRUE, component.autoscale = FALSE)

# 2. XGBoost Spatial Map
# Ensure matrix used in training
mapdata$pred_xgb <- predict(xgb_model, newdata = x_vars)

#tm_shape(mapdata) + tm_fill("pred_xgb", fill.scale =tm_scale_intervals(values = "brewer.pu_bu", style = "quantile"),  fill.legend = tm_legend(title = "Prev_pred_xgb")) + tm_borders() #+
#tm_layout(title = "XGBoost: Predicted mortality Cases")

tm_shape(mapdata) + 
  tm_fill("pred_xgb", fill.scale =tm_scale_intervals(values = "brewer.purples", style = "quantile"), 
          fill.legend = tm_legend(title = "Prev_pred_xgb")) + tm_borders(fill_alpha = .2) + 
  tm_compass() + tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), 
                           frame = TRUE, component.autoscale = FALSE)

# 3. Support Vector Regression (SVR) Spatial Map
mapdata$pred_svr <- predict(svr_model, newdata = mapdata)

#tm_shape(mapdata) + tm_fill("pred_svr", fill.scale =tm_scale_intervals(values = "brewer.gn_bu", style = "quantile"), fill.legend = tm_legend(title = "Prev_pred_svr")) + tm_borders() #+
#tm_layout(title = "SVR: Predicted mortality Cases")

tm_shape(mapdata) + 
  tm_fill("pred_svr", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Prev_pred_svr")) + tm_borders(fill_alpha = .2) + 
  tm_compass() + tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), 
                           frame = TRUE, component.autoscale = FALSE)

# Compare Side by Side
tmap_arrange(
  tm_shape(mapdata) + 
    tm_fill("pred_rf", fill.scale =tm_scale_intervals(values = "brewer.greens",  style = "quantile"), 
            fill.legend = tm_legend(title = "Prev_pred_rf")) + tm_borders(fill_alpha = .2) + 
    tm_compass() + tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), 
                             frame = TRUE, component.autoscale = FALSE),
  tm_shape(mapdata) + 
    tm_fill("pred_xgb", fill.scale =tm_scale_intervals(values = "brewer.purples", style = "quantile"), 
            fill.legend = tm_legend(title = "Prev_pred_xgb")) + tm_borders(fill_alpha = .2) + 
    tm_compass() + tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), 
                             frame = TRUE, component.autoscale = FALSE),
  tm_shape(mapdata) + 
    tm_fill("pred_svr", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
            fill.legend = tm_legend(title = "Prev_pred_svr")) + tm_borders(fill_alpha = .2) + 
    tm_compass() + tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), 
                             frame = TRUE, component.autoscale = FALSE),
  nrow = 1
)


#############################################################################################

# Predicted Residuals

# we've already trained all 5 models and have `mapdata$mortality` as your actual values.

### Step 1: Generate predictions and residuals for each model
# Random Forest
rf_preds <- predict(rf_model, newdata = mapdata)
rf_resid <- mapdata$prevalence - rf_preds

# XGBoost
xgb_preds <- predict(xgb_model, newdata = x_vars)  # x_vars = model.matrix(...)
xgb_resid <- mapdata$prevalence - xgb_preds

# SVR
svr_preds <- predict(svr_model, newdata = mapdata)
svr_resid <- mapdata$prevalence - svr_preds

### Step 2: Combine into a single data frame
residuals_df <- data.frame(
  actual = mapdata$prevalence,
  rf_pred = rf_preds,
  rf_resid = rf_resid,
  xgb_pred = xgb_preds,
  xgb_resid = xgb_resid,
  svr_pred = svr_preds,
  svr_resid = svr_resid
)

# export
write.csv(residuals_df, "model_residuals.csv", row.names = FALSE)
library(writexl)
write_xlsx(residuals_df, "model_residuals.xlsx")

### Compare residual distributions

boxplot(residuals_df$rf_resid, residuals_df$xgb_resid, residuals_df$svr_resid,
        names = c("RF", "XGB", "SVR"),
        main = "Model Residuals",
        ylab = "Prediction Error (Residual)")

###########################################################################################
## Spatial maps of residual values from each model
#Step 1: Add residuals to mapdata
#You should already have these from the previous steps:
mapdata$rf_resid <- residuals_df$rf_resid
mapdata$xgb_resid <- residuals_df$xgb_resid
mapdata$svr_resid <- residuals_df$svr_resid

#Step 2: Load tmap and create the maps
library(tmap)

# Set tmap mode to plot (static map)
tmap_mode("plot")

# Create individual residual maps
map_rf <- tm_shape(mapdata) +
  tm_fill("rf_resid", fill.scale =tm_scale_intervals(values = "brewer.greens", style = "quantile", 
                                                     midpoint = 0), fill.legend = tm_legend(title = "Prev_rf_resid")) +
  tm_borders(fill_alpha = .2) + tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), 
                                          frame = TRUE, component.autoscale = FALSE)

map_xgb <- tm_shape(mapdata) + 
  tm_fill("xgb_resid", fill.scale =tm_scale_intervals(values = "brewer.purples", style = "quantile", 
                                                      midpoint = 0), fill.legend = tm_legend(title = "Prev_xgb_resid")) + tm_borders(fill_alpha = .2) + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), 
            frame = TRUE, component.autoscale = FALSE)
map_svr <- tm_shape(mapdata) + 
  tm_fill("svr_resid", fill.scale =tm_scale_intervals(values = "brewer.reds", style = "quantile"), 
          fill.legend = tm_legend(title = "Prev_svr_resid")) + tm_borders(fill_alpha = .2) + 
  tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"), 
            frame = TRUE, component.autoscale = FALSE)

#map_svr <- tm_shape(mapdata) +
# tm_fill("svr_resid", fill.scale =tm_scale_intervals(values = "brewer.rdbu", style = "quantile"), 
#        midpoint = 0, fill.legend = tm_legend(title = "SVR Residuals")) +
#tm_borders()

#Step 3: Combine maps in a grid
# Combine maps in a grid layout
tmap_arrange(map_rf, map_xgb, map_svr, nrow = 1)

#################################################################################################
##Barplot and Spatial maps for RMSE and MAE
#Step 1: Calculate RMSE and MAE for each model
library(caret)
# Random Forest
rf_metrics <- postResample(pred = rf_preds, obs = mapdata$prevalence)

# XGBoost
xgb_metrics <- postResample(pred = xgb_preds, obs = mapdata$prevalence)

# SVR
svr_metrics <- postResample(pred = svr_preds, obs = mapdata$prevalence)

#Step 2: Combine into a summary data frame
model_eval <- data.frame(
  Model = c("Random Forest", "XGBoost", "SVR"),
  RMSE = c(rf_metrics["RMSE"], xgb_metrics["RMSE"], svr_metrics["RMSE"]),
  MAE = c(rf_metrics["MAE"], xgb_metrics["MAE"], svr_metrics["MAE"]),
  Rsquared = c(rf_metrics["Rsquared"], xgb_metrics["Rsquared"], svr_metrics["Rsquared"])
)

print(model_eval)

#Visualize MAE and RMSE
par(mfrow = c(1, 3))
#Barplot of RMSE
barplot(model_eval$RMSE, names.arg = model_eval$Model, 
        col = "skyblue", las = 1, main = "Model RMSE", ylab = "RMSE")

#Barplot of MAE
barplot(model_eval$MAE, names.arg = model_eval$Model, 
        col = "lightgreen", las = 1, main = "Model MAE", ylab = "MAE")
#Barplot of MAE
barplot(model_eval$Rsquared, names.arg = model_eval$Model, 
        col = "grey", las = 1, main = "Model Rsquared", ylab = "MAE")
par(mfrow = c(1, 1))

#Add metrics to residual maps as captions
map_rf <- tm_shape(mapdata) +
  tm_fill("rf_resid", fill.scale =tm_scale_intervals(values = "brewer.greens", midpoint = 0),
          title = paste0("rf_resid\nRMSE: ", round(rf_metrics["RMSE"], 2),
                         "\nMAE: ", round(rf_metrics["MAE"], 2))) +
  tm_borders(fill_alpha = .2) + tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"))

map_xgb <- tm_shape(mapdata) +
  tm_fill("xgb_resid", fill.scale =tm_scale_intervals(values = "-RdBu", midpoint = 0),
          title = paste0("xgb_resid\nRMSE: ", round(xgb_metrics["RMSE"], 2),
                         "\nMAE: ", round(xgb_metrics["MAE"], 2))) +
  tm_borders(fill_alpha = .2) + tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"))

map_svr <- tm_shape(mapdata) +
  tm_fill("svr_resid", fill.scale =tm_scale_intervals(values = "-RdBu", midpoint = 0),
          title = paste0("svr_resid\nRMSE: ", round(svr_metrics["RMSE"], 2),
                         "\nMAE: ", round(svr_metrics["MAE"], 2))) +
  tm_borders(fill_alpha = .2) + tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"))

tmap_arrange(map_rf, map_xgb, map_svr, nrow = 1)

##########################################################################################

#Global Moran’s I, Local Moran’s I (LISA), Cluster categories (e.g., High-High, Low-Low), Maps: Moran’s I map, 
#LISA clusters, High-High clusters using the predicted values from the four machine learning models

#Assumptions: Your spatial data is in mapdata (an sf object).
#Predicted values for each model are already stored in: rf_preds, xgb_preds, svr_preds, mlp_preds

library(sf)
library(spdep)
library(dplyr)
library(tmap)

#STEP 1: Create Spatial Weights (if not done yet)
neighbors <- poly2nb(mapdata) #if this gives warning, use the below codes
mapdata <- st_as_sf(mapdata)  # If it's not already sf
mapdata <- st_make_valid(mapdata)  # Fix any invalid geometries
neighbors <- poly2nb(mapdata, snap = 1e-15) ## Try 1e-6, 1e-5, or higher if needed. You can adjust snap upward incrementally until the warnings disappear or are reduced

listw <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

#STEP 2: Define a function to compute Moran’s I and cluster categories
analyze_spatial_autocorrelation <- function(mapdata, values, listw, model_name, signif_level = 0.05) {
  # Standardize predicted values
  mapdata$val_st <- scale(values)[, 1]
  # Compute lag
  mapdata$lag_val <- lag.listw(listw, mapdata$val_st, zero.policy = TRUE)
  # Global Moran's I
  global_moran <- moran.test(values, listw, zero.policy = TRUE)
  # Local Moran's I (LISA)
  lisa <- localmoran(values, listw, zero.policy = TRUE)
  lisa_df <- as.data.frame(lisa)
  #rename p-value column
  colnames(lisa_df)[5] <- "Pr_z"
  # Add to mapdata
  mapdata$Ii <- lisa_df$Ii
  mapdata$Z_Ii <- lisa_df$Z.I
  mapdata$Pr_z <- lisa_df$Pr_z
  #mapdata$Pr_z <- lisa_df[, "Pr(z > 0)"]
  # Classify clusters
  mapdata <- mapdata %>%
    mutate(
      cluster = case_when(
        val_st > 0 & lag_val > 0 & Pr_z <= signif_level ~ "High-High",
        val_st < 0 & lag_val < 0 & Pr_z <= signif_level ~ "Low-Low",
        val_st < 0 & lag_val > 0 & Pr_z <= signif_level ~ "Low-High",
        val_st > 0 & lag_val < 0 & Pr_z <= signif_level ~ "High-Low",
        TRUE ~ "Not Significant"
      )
    )
  return(list(
    map = mapdata,
    global_moran = global_moran
  ))
}

#STEP 3: Run the function for each model
rf_result <- analyze_spatial_autocorrelation(mapdata, rf_preds, listw, "Random Forest")
xgb_result <- analyze_spatial_autocorrelation(mapdata, xgb_preds, listw, "XGBoost")
svr_result <- analyze_spatial_autocorrelation(mapdata, svr_preds, listw, "SVR")

#STEP 4: Mapping LISA Clusters and High-High Areas for Random Forest
tmap_mode("plot")
# LISA Cluster Map.  fill.scale =tm_scale_intervals(values = "-RdBu")
tm_rf <- tm_shape(rf_result$map) +
  tm_fill(
    "cluster",
    fill.scale = tm_scale(values = c(
      "High-High" = "red",
      "Low-Low" = "blue",
      "Low-High" = "lightblue",
      "High-Low" = "pink",
      "Not Significant" = "gray90")),
    fill.legend = tm_legend(title = "LISA Clusters - RF")) +
  tm_borders(fill_alpha = .2) + tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom")) 

# Mapping LISA Clusters and High-High Areas for XGBoost
tmap_mode("plot")
# LISA Cluster Map
tm_xgb <- tm_shape(xgb_result$map) +
  tm_fill("cluster", 
          fill.scale = tm_scale(values = c(
            "High-High" = "red", 
            "Low-Low" = "blue",
            "Low-High" = "lightblue", 
            "High-Low" = "pink",
            "Not Significant" = "gray90")),
          fill.legend = tm_legend(title = "LISA Clusters - XGBoost")) +
  tm_borders(fill_alpha = .2) + tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom")) 

#Mapping LISA Clusters and High-High Areas for SVR
tmap_mode("plot")
# LISA Cluster Map
tm_svr <- tm_shape(svr_result$map) +
  tm_fill("cluster", 
          fill.scale = tm_scale(values = c(
            "High-High" = "red", 
            "Low-Low" = "blue",
            "Low-High" = "lightblue", 
            "High-Low" = "pink",
            "Not Significant" = "gray90")),
          fill.legend = tm_legend(title = "LISA Clusters - SVR")) +
  tm_borders(fill_alpha = .2) + tm_layout(legend.text.size = 0.5, legend.position = c("left", "bottom"))

tmap_arrange(tm_rf, tm_xgb, tm_svr, nrow = 1)
#You can also arrange maps side-by-side using tmap_arrange().

#View Global Moran’s I Results
#These print the test statistic and p-values for global spatial autocorrelation of predictions.
rf_result$global_moran
xgb_result$global_moran
svr_result$global_moran

##################################################################################################


