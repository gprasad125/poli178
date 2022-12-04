
rm(list=ls(all=TRUE))

###Packages

#Require Necessary Packages
require(tidyverse)
require(caret) 
require(recipes) 
require(rsample) 
require(rattle) 
require(yardstick) 
require(ggplot2)
require(ranger)
require(haven)
require(vip)
require(zoo)
require(grid)

###Datasets and Seed

#Introduce Quality of Government 21 (qog) Dataset

qog <- read_dta("C:/Users/jryan/Desktop/Poli 178 Project/Poli 178 Project/qog_bas_ts_jan21.dta")

#Introduce China Global Investment Tracker (cgit) Dataset

cgit <- readxl::read_xlsx("C:/Users/jryan/Desktop/Poli 178 Project/Poli 178 Project/China-Global-Investment-Tracker-2022-SPRING-final-1.xlsx")

#Set seed
set.seed(1)

###Data Cleaning

#Remove unnecessary nations and years from qog

qog1 <- filter(qog, year >= 2005)

qog1.1 <- subset(qog1, cname!="USSR")
qog1.2 <- subset(qog1.1, cname!="Vietnam, South")
qog1.3 <- subset(qog1.2, cname!="Vietnam, North")
qog1.4 <- subset(qog1.3, cname!="Ethiopia (-1992)")
qog1.5 <- subset(qog1.4, cname!="Germany, West")
qog1.6 <- subset(qog1.5, cname!="Germany, East")
qog1.7 <- subset(qog1.6, cname!="Yugoslavia")
qog1.8 <- subset(qog1.7, cname!="France (-1962)")
qog1.9 <- subset(qog1.8, cname!="Pakistan (-1970)")
qog1.11 <- subset(qog1.9, cname!="Cyprus (-1974)")
qog1.12 <- subset(qog1.11, cname!="Malaysia (-1965)")
qog1.13 <- subset(qog1.12, cname!="Yemen, North")
qog1.14 <- subset(qog1.13, cname!="Yemen, South")

#Remove unnecessary rows from cgit

cgit1 <- cgit %>% filter(!row_number() %in% c(1:5))

names(cgit1) <- c("year", "Month", "Investor", "invested", "Share Size", "Transaction Party", "Sector", "Subsector", "cname", "Region", "BRI", "Greenfield")

#Remove unnecessary columns from qog1
qog2 <- subset(qog1.14, select = c(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert))

#Remove unnecessary columns from cgit1
cgit2 <- subset(cgit1, select = c("year", "invested", "Sector", "cname"))

#Rename countries in qog2
qog2[qog2=="United States"] <- "USA"

qog2[qog2=="United Arab Emirates"] <- "UAE"

qog2[qog2=="Congo, Democratic Republic"] <- "Democratic Republic of the Congo"

qog2[qog2=="Cyprus (1975-)"] <- "Cyprus"

qog2[qog2=="Ethiopia (1993-)"] <- "Ethiopia"

qog2[qog2=="France (1963-)"] <- "France"

qog2[qog2=="Malaysia (1966-)"] <- "Malaysia"

qog2[qog2=="Pakistan (1971-)"] <- "Pakistan"

qog2[qog2=="Sudan (2012-)"] <- "Sudan"

qog2[qog2=="Sudan (-2011)"] <- "Sudan"

qog2[qog2=="Serbia and Montenegro"] <- "Serbia"

#Mutate cgit2 year column type to numeric
cgit2 <- cgit2 %>% mutate(year = as.numeric(year))
cgit2 <- cgit2 %>% mutate(invested = as.numeric(invested))

###Data set combination

#Join qog2 and cgit2
join0 <- left_join(qog2,cgit2)

#Fill NA values with data from previous year
join1.1 <- join0 %>% arrange(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, invested, Sector) %>%
  group_by(cname) %>% fill(Sector, .direction = "up") %>%
  ungroup

join1.2 <- join1.1 %>% arrange(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, invested, Sector) %>%
  group_by(cname) %>% fill(Sector, .direction = "down") %>%
  ungroup

join1.3 <- join1.2 %>% arrange(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, invested, Sector) %>%
  group_by(cname) %>% fill(wdi_spr, .direction = "up") %>%
  ungroup

join1.4 <- join1.3 %>% arrange(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, invested, Sector) %>%
  group_by(cname) %>% fill(wdi_spr, .direction = "down") %>%
  ungroup

join1.5 <- join1.4 %>% arrange(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, invested, Sector) %>%
  group_by(cname) %>% fill(dr_eg, .direction = "up") %>%
  ungroup

join1.6 <- join1.5 %>% arrange(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, invested, Sector) %>%
  group_by(cname) %>% fill(dr_eg, .direction = "down") %>%
  ungroup

join1.7 <- join1.6 %>% arrange(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, invested, Sector) %>%
  group_by(cname) %>% fill(wdi_expedut, .direction = "up") %>%
  ungroup

join1.8 <- join1.7 %>% arrange(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, invested, Sector) %>%
  group_by(cname) %>% fill(wdi_expedut, .direction = "down") %>%
  ungroup

join1.9 <- join1.8 %>% arrange(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, invested, Sector) %>%
  group_by(cname) %>% fill(wdi_gert, .direction = "up") %>%
  ungroup

join1.10 <- join1.9 %>% arrange(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, invested, Sector) %>%
  group_by(cname) %>% fill(wdi_gert, .direction = "down") %>%
  ungroup

join1.11 <- join1.10 %>% arrange(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, invested, Sector) %>%
  group_by(cname) %>% fill(invested, .direction = "up") %>%
  ungroup

join1.12 <- join1.11 %>% arrange(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, invested, Sector) %>%
  group_by(cname) %>% fill(invested, .direction = "down") %>%
  ungroup

join1.13 <- join1.12 %>% arrange(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, invested, Sector) %>%
  group_by(cname) %>% fill(Sector, .direction = "up") %>%
  ungroup

join1.14 <- join1.13 %>% arrange(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, invested, Sector) %>%
  group_by(cname) %>% fill(Sector, .direction = "down") %>%
  ungroup

#Apply extrapolated mean data to NA's in dataset
join1.14$wdi_spr[is.na(join1.14$wdi_spr)] <- mean(join1.14$wdi_spr, na.rm = T)

join1.14$dr_eg[is.na(join1.14$dr_eg)] <- mean(join1.14$dr_eg, na.rm = T)

join1.14$wdi_expedut[is.na(join1.14$wdi_expedut)] <- mean(join1.14$wdi_expedut, na.rm = T)

join1.14$wdi_gert[is.na(join1.14$wdi_gert)] <- mean(join1.14$wdi_gert, na.rm = T)

#Omit rows with NA values
joinNA <- na.omit(join1.14)

#Create prediction dataset without country names
join20.pro <- filter(joinNA, year == 2020)

join20 <- subset(join20.pro, select = c(year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, Sector))

join20 <- join20[!(row.names(join20) %in% c(6,7,8,9,10,11,12,16,24,25,35,44,45,46,48,49,59,60,61,62,63,70,71,83,100,109,115,118,125,126,130,135,143,145,146,147,148,149,153,154)),]

#Create prediction dataset with country names
join20cname <- subset(join20.pro, select = c(cname, year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, Sector))

join20cname <- join20cname[!(row.names(join20cname) %in% c(6,7,8,9,10,11,12,16,24,25,35,44,45,46,48,49,59,60,61,62,63,70,71,83,100,109,115,118,125,126,130,135,143,145,146,147,148,149,153,154)),]

#Create training dataset
jointrain <- filter(joinNA, year < 2020)

jointrain <- subset(jointrain, select = c(year, wdi_spr, dr_eg, wdi_expedut, wdi_gert, invested, Sector))

###ML Prep

#Split joined data into training and testing
splits <- initial_split(jointrain, prop = .75)
train_data <- training(splits) 
test_data <- testing(splits) 

#Create Recipe
rcp <- 
  recipe(invested~.,data = train_data) %>% 
  step_dummy(all_nominal(),-invested) %>% 
  step_log(invested) %>% 
  step_range(all_numeric(),-invested) %>% 
  prep()  

#Bake training and testing data
train_data2 <- bake(rcp,train_data)
test_data2 <- bake(rcp,test_data) 

#Fold Data
folds <- createFolds(train_data2$invested, k = 10)
sapply(folds,length)

#Apply control conditions
control_conditions <-
  trainControl(method='cv', 
               index = folds)

###KNN

#KNN Tuning
knn_tune = expand.grid(k = c(1,3,4)) #Setting knn model with tuning parameters 1,3,4

#KNN Model
mod_knn <- 
  train(invested ~ .,          
        data=train_data2, 
        method = "knn",    
        metric = "RMSE",   
        trControl = control_conditions, 
        tuneGrid = knn_tune)

###Random Forrest

#Model Forrest
mod_rf <- 
  train(invested ~ ., 
        data=train_data2, 
        method = "ranger", 
        metric = "RMSE",    
        trControl = control_conditions)


###Mod List

#Mod List
mod_list <-
  list(
    knn = mod_knn,
    rf = mod_rf)

#Resample
resamples(mod_list)

#Check Rsquared dotplot
dotplot(resamples(mod_list), metric = "Rsquared")

#Check RMSE dotplot
dotplot(resamples(mod_list), metric = "RMSE")


###Testing the Model
pred <- predict(mod_rf, newdata = test_data2) 
mse = sum((test_data2$invested-pred)^2)/nrow(test_data2)
rmse_score = sqrt(mse)
rmse_score

#Testing the model against the test data
pred <- predict(mod_rf, newdata = test_data2)
performance = tibble(truth = test_data2$invested, estimate = pred)

#Create a comparative table
comparative_table <- bind_rows(performance %>% rmse(truth,estimate), 
                               performance %>% rsq(truth,estimate))  

comparative_table_dat <- tableGrob(comparative_table, 
                               theme = ttheme_default(core = list(bg_params = list(fill = "grey99")))
                               )

grid.draw(comparative_table_dat)

###Ranger Importance Visualization
r1 <- ranger(invested ~ ., 
             data=train_data2, 
             importance = 'impurity')

v1 <- vip(r1)

###Prep for Prediction

#Create Recipe
rcp_pred <- 
  recipe(invested~.,data = jointrain) %>% 
  step_dummy(all_nominal(),-invested) %>% 
  step_log(invested) %>% 
  step_range(all_numeric(),-invested) %>% 
  prep()  

#Fold Data
folds_pred <- createFolds(jointrain$invested, k = 10)


#Apply control conditions
control_conditions_pred <-
  trainControl(method='cv', 
               index = folds_pred)

#Model Forrest
mod_rf_pred <- 
  train(invested ~ ., 
        data=jointrain, 
        method = "ranger", 
        metric = "RMSE",    
        trControl = control_conditions_pred)

###Ranger Prediction

#Predict using mod_rf
predict(
  mod_rf_pred,  
  join20,  
  predict.all = T
)


