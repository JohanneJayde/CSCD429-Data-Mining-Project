#nstall.packages("caret")
library("caret")
#install.packages("FSinR")
library("FSinR")
#install.packages("corpcor")
library(corpcor)

training_set <- read.csv("laptop_data_training_set.csv")
test_set <- read.csv("laptop_data_test_set.csv")

#fix issue with Inspiron and Insprion and IdeaPad and Ideapad
training_set$model[training_set$model == "Insprion"] <- "Inspiron"
training_set$model[training_set$model == "Ideapad"] <- "IdeaPad"

#removing latest_price_range to stop it from influencing regression model
training_set <- subset(training_set, select = -latest_price_range)
training_set <- training_set[training_set$old_price > 0,]

#remove these two lines if you want test the regressions with "outliers"
test_set <- test_set[test_set$latest_price < 1515.55,]
training_set <- training_set[training_set$latest_price < 1619.53,]


#fix issue with Inspiron and Insprion and IdeaPad and Ideapad
test_set$model[test_set$model == "Insprion"] <- "Inspiron"
test_set$model[test_set$model == "Ideapad"] <- "IdeaPad"
test_set$model[test_set$model == "OMEN"] <- "Omen"
test_set$processor_name[test_set$processor_name == "GEFORCE RTX"] <- "GeForce RTX"
#removing latest_price_range to stop it from influencing regression model
test_set <- subset(test_set, select = -latest_price_range)
test_set <- test_set[test_set$old_price > 0,]

test_set <- test_set[test_set$latest_price < 3000,]
training_set <- training_set[training_set$latest_price < 4000,]

test_set_remove_model_prc_brand <- subset(test_set, select= -c(model, processor_brand))
training_set_remove_model_prc_brand <- subset(training_set, select= -c(model, processor_brand))

#create vectors for nominal values that don't exists in training set
processor_brand_vec <- c("Qualcomm")
brand_vec <- c("samsung, alienware")
model_vec <- c("15-ec1105AX", "430", "Athlon", "Bravo", "Cosmos", "G7", "Galaxy", "Summit", "VivoBook14", "WF65")
processor_name_vec <- c("Snapdragon 7c", "GeForce RTX")

#remove rows with nomial values but in feature set without model or processor_name
test_set_remove_model_prc_brand <- test_set_remove_model_prc_brand[!(test_set_remove_model_prc_brand$brand %in% brand_vec),]
test_set_remove_model_prc_brand <- test_set_remove_model_prc_brand[!(test_set_remove_model_prc_brand$processor_name %in% processor_name_vec),]

test_set_remove_colinearity <-subset(test_set_remove_model_prc_brand, select= -c(graphic_card_gb))
training_set_remove_colinearity <-subset(training_set_remove_model_prc_brand, select= -c(graphic_card_gb))

#remove rows with nominal values not pressent in the training set, this is done to make the model working and may be changed
#swapping to target encoding may help but that led to NaN issues
test_set_new <- test_set[!(test_set$brand %in% brand_vec),]
test_set_new <- test_set_new[!(test_set_new$model %in% model_vec),]
test_set_new <- test_set_new[!(test_set_new$processor_brand %in% processor_brand_vec),]
test_set_new <- test_set_new[!(test_set_new$processor_name %in% processor_name_vec),]

#linear regression function for a given dataset
linear_regression_with_one_hot_encoding <- function(dataset, featureNum){
  
  selectKFeatues <- selectKBest(k=featureNum)
  #select k features from laptop_data
  features <- selectKFeatues(dataset, 'latest_price', determinationCoefficient())$featuresSelect
  #grab the ten best features and insert them into a data frame
  k_feature_set <- dataset[features];
  latest_price <- dataset$latest_price
  k_feature_set <- cbind(k_feature_set, latest_price)
  
  #encode categorical features using one hot encoding
  dummy <- dummyVars(" ~ .", data=k_feature_set)
  encoded_dataset <- data.frame(predict(dummy, newdata=k_feature_set))
  
  #create forumla to be passed into linear regression function
  vec <- as.vector(colnames(subset(encoded_dataset, select=-latest_price)))
  b <- paste(vec, sep ="+")
  formulaB <- paste(b,collapse = " + ")
  formula <- as.formula(paste("latest_price ~ ", formulaB))

  #print out summary of model performance
  linear_model <- lm(formula, data= encoded_dataset)
  return(linear_model)
 # wt <- 1 / lm(abs(linear_model$residuals) ~ linear_model$fitted.values)$fitted.values^2
  #weighted_linear_model <- lm(formula,encoded_dataset, wt)

  #summary(weighted_linear_model)
  
}

create_linear_regression_model <- function(dataset, featureNum){
  
  selectKFeatures <- selectKBest(k=featureNum)
  #select k features from laptop_data
  features <- selectKFeatures(dataset, 'latest_price', determinationCoefficient())$featuresSelect
  print(features)
  #grab the ten best features and insert them into a data frame
  k_feature_set <- dataset[features];
  latest_price <- dataset$latest_price
  k_feature_set <- cbind(k_feature_set, latest_price)
  
  #create forumla to be passed into linear regression function
  vec <- as.vector(colnames(subset(k_feature_set, select=-latest_price)))
  b <- paste(vec, sep ="+")
  formulaB <- paste(b,collapse = " + ")
  formula <- as.formula(paste("latest_price ~ ", formulaB))
  linear_model <- lm(formula, data= k_feature_set)
  print(summary(linear_model))
  
  #print out summary of model performance
  return(linear_model)

  
}

predict_linear_regression <- function(test_set, training_set, num){
  linear_model <- create_linear_regression_model(training_set, num)
  results <- predict(linear_model, test_set)
  
  Root_Mean_Squared_Error <- RMSE(results, test_set$latest_price);
  R2 <- R2(results, test_set$latest_price);
  
  print(paste("Root Mean Squared Error: ", Root_Mean_Squared_Error))
  print(paste("R2: ", R2))
  
}

#testing with all variables
predict_linear_regression(test_set_new[test_set_new$brand != "alienware",], training_set, 22)
predict_linear_regression(test_set_new[test_set_new$brand != "alienware",], training_set, 10)

#testing with two variables, this removes issues with independent variables that correlate to each other
predict_linear_regression(test_set_new[test_set_new$brand != "alienware",], training_set, 2)

#chi squared test shows processor_brand and processor_name are heavily correlated
#chi squared test shows model and processor_name are heavily correlated
chisq.test(training_set$processor_brand, training_set$processor_name)
chisq.test(training_set$model, training_set$processor_name)
chisq.test(training_set$model, training_set$processor_brand)

#testing without model or processor_brand with 4 features
#four features result in the highest values
predict_linear_regression(test_set_remove_model_prc_brand, training_set_remove_model_prc_brand, 4)




