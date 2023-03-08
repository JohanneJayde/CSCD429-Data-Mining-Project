#install package
#install.packages(c("naivebayes", "dplyr", "ggplot2", "psych", "e1071"))

# Loading package
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(e1071)

#set test and training sets
laptopTest <- read.csv("laptop_data_test_set.csv", header =T)
laptopTraining <- read.csv("laptop_data_training_set.csv", header =T)

processor_brand_vec <- c("Qualcomm")
brand_vec <- c("samsung, alienware")
model_vec <- c("15-ec1105AX", "430", "Athlon", "Bravo", "Cosmos", "G7", "Galaxy", "Summit", "VivoBook14", "WF65")
processor_name_vec <- c("Snapdragon 7c", "GeForce RTX")

#laptopTest <- laptopTest[!(laptopTest$brand %in% brand_vec),]
#laptopTest <- laptopTest[!(laptopTest$processor_name %in% processor_name_vec),]
#laptopTest <- laptopTest[!(laptopTest$model_vec %in% model_vec),]
#laptopTest <- laptopTest[!(laptopTest$processor_brand %in% processor_brand_vec),]


#test_set_remove_model_prc_brand <- subset(laptopTest, select= -c(model, processor_brand, processor_name, brand))
#training_set_remove_model_prc_brand <- subset(laptopTraining, select= -c(model, processor_brand, processor_name, brand))

#create the naive bayes model based on the training data
model <- naive_bayes(latest_price_range ~ processor_gnrtn, data = laptopTraining, laplace = 1)

#predicting on the test set using the training model
p <- predict(model, laptopTest)

#getting the confusion matrix for latest_price_range from the prediction
tab <- table(p, laptopTest$latest_price_range)

#finding the MISCLASSIFICATION percent
misclass <- 1-sum(diag(tab))/sum(tab)

#finding the correct classification percent
class <- 1-misclass
class
