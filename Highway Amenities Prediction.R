library(ggplot2)
library(dplyr)

highway_data= read.table(file = 'E:/Data Analytics for Process Improvement/DAPI Project 2/trainingData.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE)

str(highway_data)

View(highway_data)

#Coupon Status
highway_data$Coupon_Status = ifelse(highway_data$Y == 1, "Accepted", "Rejected")

ggplot(highway_data, aes(x = Coupon_Status, fill = Coupon_Status)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.25, color="black")+
  labs(title = "Count of Coupon Acceptances and Rejections",
       x = "Coupon Status",
       y = "Count") +
  scale_fill_manual(values = c("Accepted" = "lightgreen", "Rejected" = "orange")) +
  theme_minimal()

# Convert empty strings or strings that are only spaces to NA
highway_data$CarryAway[highway_data$CarryAway == "" | highway_data$CarryAway == " "] <- NA

sum(is.na(highway_data$CarryAway))

# Convert empty strings or strings that are only spaces to NA
highway_data$Bar[highway_data$Bar == "" | highway_data$Bar == " "] <- NA

# Convert empty strings or strings that are only spaces to NA
highway_data$CoffeeHouse[highway_data$CoffeeHouse == "" | highway_data$CoffeeHouse == " "] <- NA

# Convert empty strings or strings that are only spaces to NA
highway_data$RestaurantLessThan20[highway_data$RestaurantLessThan20 == "" | highway_data$RestaurantLessThan20 == " "] <- NA

# Convert empty strings or strings that are only spaces to NA
highway_data$Restaurant20To50[highway_data$Restaurant20To50 == "" | highway_data$Restaurant20To50 == " "] <- NA

#Occupation

ggplot(highway_data, aes(x = occupation, fill = factor(Y))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "orange", "1" = "lightgreen"), 
                    labels = c("0" = "Rejected", "1" = "Accepted")) +
  labs(title = "Bar Plot of Outcome Y by Occupation",
       x = "Field of Work",
       y = "Count",
       fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.5)))

#Education

ggplot(highway_data, aes(x = education, fill = factor(Y))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "orange", "1" = "lightgreen"), 
                    labels = c("0" = "Rejected", "1" = "Accepted")) +
  labs(title = "Bar Plot of Outcome Y by Education",
       x = "Degree Level",
       y = "Count",
       fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.8)))

#Income Level
ggplot(highway_data, aes(x = income, fill = factor(Y))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "orange", "1" = "lightgreen"), 
                    labels = c("0" = "Rejected", "1" = "Accepted")) +
  labs(title = "Bar Plot of Outcome Y by Income",
       x = "Income Bin",
       y = "Count",
       fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Feature Engineering 1
# Add "Unknown" to levels and then replace NA values
#CarryAway
highway_data$CarryAway <- addNA(highway_data$CarryAway, ifany = TRUE)
levels(highway_data$CarryAway) <- c(levels(highway_data$CarryAway), "Unknown")
highway_data$CarryAway[is.na(highway_data$CarryAway)] <- "Unknown"
ggplot(highway_data, aes(x = CarryAway, fill = factor(Y))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "orange", "1" = "lightgreen"),
                    labels = c("0" = "Rejected", "1" = "Accepted")) +
  labs(title = "Bar Plot of Outcome Y by CarryAway",
       x = "Frequency of Visit",
       y = "Count",
       fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Bar
highway_data$Bar <- addNA(highway_data$Bar, ifany = TRUE)
levels(highway_data$Bar) <- c(levels(highway_data$Bar), "Unknown")
highway_data$Bar[is.na(highway_data$Bar)] <- "Unknown"
ggplot(highway_data, aes(x = Bar, fill = factor(Y))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "orange", "1" = "lightgreen"),
                    labels = c("0" = "Rejected", "1" = "Accepted")) +
  labs(title = "Bar Plot of Outcome Y by Bar",
       x = "Frequency of Visit",
       y = "Count",
       fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#CoffeeHouse
highway_data$CoffeeHouse <- addNA(highway_data$CoffeeHouse, ifany = TRUE)
levels(highway_data$CoffeeHouse) <- c(levels(highway_data$CoffeeHouse), "Unknown")
highway_data$CoffeeHouse[is.na(highway_data$CoffeeHouse)] <- "Unknown"
ggplot(highway_data, aes(x = CoffeeHouse, fill = factor(Y))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "orange", "1" = "lightgreen"),
                    labels = c("0" = "Rejected", "1" = "Accepted")) +
  labs(title = "Bar Plot of Outcome Y by CoffeeHouse",
       x = "Frequency of Visit",
       y = "Count",
       fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#RestaurantLessThan20
highway_data$RestaurantLessThan20 <- addNA(highway_data$RestaurantLessThan20, ifany = TRUE)
levels(highway_data$RestaurantLessThan20) <- c(levels(highway_data$RestaurantLessThan20), "Unknown")
highway_data$RestaurantLessThan20[is.na(highway_data$RestaurantLessThan20)] <- "Unknown"
ggplot(highway_data, aes(x = RestaurantLessThan20, fill = factor(Y))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "orange", "1" = "lightgreen"),
                    labels = c("0" = "Rejected", "1" = "Accepted")) +
  labs(title = "Bar Plot of Outcome Y by RestaurantLessThan20",
       x = "Frequency of Visit",
       y = "Count",
       fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Restaurant20To50
highway_data$Restaurant20To50 <- addNA(highway_data$Restaurant20To50, ifany = TRUE)
levels(highway_data$Restaurant20To50) <- c(levels(highway_data$Restaurant20To50), "Unknown")
highway_data$Restaurant20To50[is.na(highway_data$Restaurant20To50)] <- "Unknown"
ggplot(highway_data, aes(x = Restaurant20To50, fill = factor(Y))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "orange", "1" = "lightgreen"),
                    labels = c("0" = "Rejected", "1" = "Accepted")) +
  labs(title = "Bar Plot of Outcome Y by Restaurant20To50",
       x = "Frequency of Visit",
       y = "Count",
       fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

View(highway_data)


# Feature Engineering 2
#Age conversion to Age Group Above 50 and Below 50
highway_data$Age_Group <- ifelse(highway_data$age %in% c(21, 26, 31, 36, 41, 46) | highway_data$age == "below21", "50 Below", "50 Above")
View(highway_data)

ggplot(highway_data, aes(x = Age_Group, fill = factor(Y))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "orange", "1" = "lightgreen"),
                    labels = c("0" = "Rejected", "1" = "Accepted")) +
  labs(title = "Bar Plot of Outcome Y by Age Group",
       x = "Age Group",
       y = "Count",
       fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Feature Engineering 3 
# Convert Time to categorical time of day
highway_data <- highway_data %>%
  mutate(Time_of_Day = case_when(
    time %in% c("7AM", "10AM") ~ "Morning",
    time == "2PM" ~ "Afternoon",
    time == "6PM" ~ "Evening",
    time == "10PM" ~ "Night"))

ggplot(highway_data, aes(x = Time_of_Day, fill = factor(Y))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "orange", "1" = "lightgreen"), 
                    labels = c("0" = "Rejected", "1" = "Accepted")) +
  labs(title = "Bar Plot of Outcome Y by Time of Day",
       x = "Time of Day",
       y = "Count",
       fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


names(highway_data)

str(highway_data)

highway_data$Age_Group= factor(highway_data$Age_Group)
highway_data$Time_of_Day=factor(highway_data$Time_of_Day)
highway_data$Coupon_Status=factor(highway_data$Coupon_Status)

table(highway_data$Coupon_Status)

# create a logical field of target variable
#highway_data$Accepted <- as.logical(0)
#highway_data$Rejected <- as.logical(0)

#for(i in 1:nrow(highway_data)) {
#  if (highway_data$Coupon_Status[i]=="Accepted")
#    highway_data$Accepted[i] <- as.logical(1)
#  else
#    highway_data$Rejected[i] <- as.logical(1)
#}

View(highway_data)

names(highway_data)

highway.df <- select(highway_data, 
                    destination,
                    passanger,
                    weather,
                    temperature,
                    Time_of_Day,
                    coupon,
                    expiration,
                    gender,
                    Age_Group,
                    maritalStatus,
                    has_children,
                    education,
                    occupation,
                    income,              
                    Bar,
                    CoffeeHouse,         
                    CarryAway,
                    RestaurantLessThan20,
                    Restaurant20To50,    
                    toCoupon_GEQ15min,
                    toCoupon_GEQ25min,   
                    direction_same,
                    direction_opp,
                    Y)

View(highway.df)
outcomeName = "Y"
predictorNames <- names(highway.df)[names(highway.df) != "Y"]
predictorNames

str(highway.df)

set.seed(1234)  # setting seed to reproduce results of random sampling
split<-(.80)
library(caret) # Run this command in console install.packages("caret")
index <- createDataPartition(highway.df$Y, p=split, list=FALSE) # row indices for training data

train.df <- highway.df[ index,]  # model training data
test.df<- highway.df[ -index,]   # test data

train.df[, outcomeName] <- factor(train.df[, outcomeName], levels = c(0, 1))
test.df[, outcomeName] <- factor(test.df[, outcomeName], levels = c(0, 1))

modelLookup(model='rf')  # To find the parameters of a model that can be tuned
modelLookup(model='gbm')

fitControl <- trainControl(method = "none")   # control parameters for training

### RF Model
rf <- train(x = train.df[, predictorNames], 
            y = train.df[, outcomeName],
            method = 'rf', 
            trControl = fitControl)


rfImp<-varImp(rf)  # computes variable importance for regression and classification models
rfImp
plot(rfImp, cex.lab=0.2,las=2)


### GBM Model
gbm <- train(train.df[, predictorNames], train.df[, outcomeName],
             method = 'gbm',
             trControl = fitControl)

gbmImp<-summary(gbm)
gbmImp

gbm.predict<-predict(gbm,test.df[,predictorNames],type="raw")
gbm_confusionMatrix = confusionMatrix(gbm.predict,test.df[,outcomeName], positive = "1")
gbm_accuracy <- gbm_confusionMatrix$overall['Accuracy']
gbm_recall <- gbm_confusionMatrix$byClass['Sensitivity']
gbm_precision <- gbm_confusionMatrix$byClass['Pos Pred Value']
gbm_F1 <- 2 * (gbm_precision * gbm_recall) / (gbm_precision + gbm_recall)
gbm_confusionMatrix
gbm_accuracy
gbm_recall
gbm_precision
gbm_F1

### GBM Model Tuning 1
fitControl.gbm <- trainControl(method = "cv",
                               number = 20,
                               sampling = "up")   # control parameters for training
# see help(trainControl) for details

gbm.tuned<-train(train.df[,predictorNames],train.df[,outcomeName],   #model retraining
                 method='gbm',
                 trControl=fitControl.gbm)

gbm.tuned.predict<-predict(gbm.tuned,test.df[,predictorNames],type="raw")
gbm_tuned_1 = confusionMatrix(gbm.tuned.predict,test.df[,outcomeName], positive = "1")
gbm_tuned_1
gbm_accuracy_1 <- gbm_tuned_1$overall['Accuracy']
gbm_recall_1 <- gbm_tuned_1$byClass['Sensitivity']
gbm_precision_1 <- gbm_tuned_1$byClass['Pos Pred Value']
gbm_F1_1 <- 2 * (gbm_precision_1 * gbm_recall_1) / (gbm_precision_1 + gbm_recall_1)
gbm_accuracy_1
gbm_recall_1
gbm_precision_1
gbm_F1_1

### GBM Model Tuning 2
fitControl.gbm2 <- trainControl(method = "repeatedcv",
                                number = 20,
                                repeats = 5,
                                sampling = "up")

gbm2.tuned<-train(train.df[,predictorNames],train.df[,outcomeName],
                  method='gbm',
                  trControl=fitControl.gbm2)

gbm2.tuned.predict<-predict(gbm2.tuned,test.df[,predictorNames],type="raw")
gbm_tuned_2 = confusionMatrix(gbm2.tuned.predict,test.df[,outcomeName], positive = "1")
gbm_tuned_2
gbm_accuracy_2 <- gbm_tuned_2$overall['Accuracy']
gbm_recall_2 <- gbm_tuned_2$byClass['Sensitivity']
gbm_precision_2 <- gbm_tuned_2$byClass['Pos Pred Value']
gbm_F1_2 <- 2 * (gbm_precision_2 * gbm_recall_2) / (gbm_precision_2 + gbm_recall_2)
gbm_accuracy_2
gbm_recall_2
gbm_precision_2
gbm_F1_2

#Computing Probabilities
gbm.probs <- predict(gbm,test.df[,predictorNames],type="prob")    
rf.probs <- predict(rf,test.df[,predictorNames],type="prob")
gbm.tuned.probs <- predict(gbm.tuned,test.df[,predictorNames],type="prob")
gbm2.tuned.probs <- predict(gbm2.tuned,test.df[,predictorNames],type="prob")

# draw ROC curve and perform visual check for better accuracy/performance
library(pROC)
#Comparison Plot
gbm.plot<-plot(roc(test.df$Y,gbm.probs[,2]))
rf.plot<-lines(roc(test.df$Y,rf.probs[,2]), col="blue")
gbm.tuned.plot<-lines(roc(test.df$Y,gbm.tuned.probs[,2]), col="red")
gbm2.tuned.plot<-lines(roc(test.df$Y,gbm2.tuned.probs[,2]), col="green")
legend("bottomright", legend=c("rf", "gbm", "gbm.tuned", "gbm2.tuned"), col=c("blue", "black", "red", "green"), lwd=2)

auc(test.df$Y,gbm.probs[,2])
auc(test.df$Y,rf.probs[,2])
auc(test.df$Y,gbm.tuned.probs[,2])
auc(test.df$Y,gbm2.tuned.probs[,2])

# Example of storing metrics
model_metrics <- data.frame(
  Model = c('randomForest', 'GBM', 'GBM Tuned I','GBM Tuned II'),
  Accuracy = c(rf_accuracy, gbm_accuracy, gbm_accuracy_1, gbm_accuracy_2),
  Recall = c(rf_recall, gbm_recall, gbm_recall_1, gbm_recall_2),
  F1 = c(rf_F1, gbm_F1, gbm_F1_1, gbm_F1_2)
)
View(model_metrics)

#Scoring Data

scoring_data= read.table(file = 'E:/Data Analytics for Process Improvement/DAPI Project 2/scoringData.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE)

str(scoring_data)

View(scoring_data)

# Convert empty strings or strings that are only spaces to NA
scoring_data$CarryAway[scoring_data$CarryAway == "" | scoring_data$CarryAway == " "] <- NA


# Convert empty strings or strings that are only spaces to NA
scoring_data$Bar[scoring_data$Bar == "" | scoring_data$Bar == " "] <- NA


# Convert empty strings or strings that are only spaces to NA
scoring_data$CoffeeHouse[scoring_data$CoffeeHouse == "" | scoring_data$CoffeeHouse == " "] <- NA

sum(is.na(scoring_data$CoffeeHouse))


# Convert empty strings or strings that are only spaces to NA
scoring_data$RestaurantLessThan20[scoring_data$RestaurantLessThan20 == "" | scoring_data$RestaurantLessThan20 == " "] <- NA


# Convert empty strings or strings that are only spaces to NA
scoring_data$Restaurant20To50[scoring_data$Restaurant20To50 == "" | scoring_data$Restaurant20To50 == " "] <- NA


# Feature Engineering 1
# Add "Unknown" to levels and then replace NA values
scoring_data$CarryAway <- addNA(scoring_data$CarryAway, ifany = TRUE)
levels(scoring_data$CarryAway) <- c(levels(scoring_data$CarryAway), "Unknown")
scoring_data$CarryAway[is.na(scoring_data$CarryAway)] <- "Unknown"

scoring_data$Bar <- addNA(scoring_data$Bar, ifany = TRUE)
levels(scoring_data$Bar) <- c(levels(scoring_data$Bar), "Unknown")
scoring_data$Bar[is.na(scoring_data$Bar)] <- "Unknown"

scoring_data$CoffeeHouse <- addNA(scoring_data$CoffeeHouse, ifany = TRUE)
levels(scoring_data$CoffeeHouse) <- c(levels(scoring_data$CoffeeHouse), "Unknown")
scoring_data$CoffeeHouse[is.na(scoring_data$CoffeeHouse)] <- "Unknown"

scoring_data$RestaurantLessThan20 <- addNA(scoring_data$RestaurantLessThan20, ifany = TRUE)
levels(scoring_data$RestaurantLessThan20) <- c(levels(scoring_data$RestaurantLessThan20), "Unknown")
scoring_data$RestaurantLessThan20[is.na(scoring_data$RestaurantLessThan20)] <- "Unknown"

scoring_data$Restaurant20To50 <- addNA(scoring_data$Restaurant20To50, ifany = TRUE)
levels(scoring_data$Restaurant20To50) <- c(levels(scoring_data$Restaurant20To50), "Unknown")
scoring_data$Restaurant20To50[is.na(scoring_data$Restaurant20To50)] <- "Unknown"


View(scoring_data)

# Feature Engineering 2
#Age conversion to Age Group Above 50 and Below 50
scoring_data$Age_Group <- ifelse(scoring_data$age %in% c(21, 26, 31, 36, 41, 46) | scoring_data$age == "below21", "50 Below", "50 Above")
View(scoring_data)

# Feature Engineering 3 
# Convert Time to categorical time of day
scoring_data <- scoring_data %>%
  mutate(Time_of_Day = case_when(
    time %in% c("7AM", "10AM") ~ "Morning",
    time == "2PM" ~ "Afternoon",
    time == "6PM" ~ "Evening",
    time == "10PM" ~ "Night"))

#Feature Engineering 4
scoring_data$Coupon_Status = ifelse(scoring_data$Y == 1, "Accepted", "Rejected")


names(scoring_data)

str(scoring_data)

scoring_data$Age_Group= factor(scoring_data$Age_Group)
scoring_data$Time_of_Day=factor(scoring_data$Time_of_Day)
scoring_data$Coupon_Status=factor(scoring_data$Coupon_Status)

scoring.df <- scoring_data[, predictorNames]
predictions <- predict(rf, newdata = scoring.df, type = "raw")

scoring_data$Y <- predictions

write.csv(scoring_data, "E:/Data Analytics for Process Improvement/DAPI Project 2/Team1_Scoring.csv", row.names = FALSE)
