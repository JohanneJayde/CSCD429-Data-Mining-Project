#install package
install.packages(c("naivebayes", "dplyr", "ggplot2", "psych", "e1071"))

# Loading package
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(e1071)

#set test and training sets
laptopTest <- read.csv("C:/Users/12086/Documents/RStudio/laptopSets/laptop_data_test_set.csv", header =T)
laptopTraining <- read.csv("C:/Users/12086/Documents/RStudio/laptopSets/laptop_data_0s_replaced_by_avg_of_nonzeros.csv", header =T)

#convert all types into factor variables or something
laptopTraining$ram_gb <- as.factor(laptopTraining$ram_gb)
laptopTraining$ssd <- as.factor(laptopTraining$ssd)
laptopTraining$hdd <- as.factor(laptopTraining$hdd)
laptopTraining$graphic_card_gb <- as.factor(laptopTraining$graphic_card_gb)
laptopTraining$display_size <- as.factor(laptopTraining$display_size)
laptopTraining$warranty <- as.factor(laptopTraining$warranty)
laptopTraining$latest_price <- as.factor(laptopTraining$latest_price)
laptopTraining$old_price <- as.factor(laptopTraining$old_price)
laptopTraining$discount <- as.factor(laptopTraining$discount)
laptopTraining$star_rating <- as.factor(laptopTraining$star_rating)
laptopTraining$ratings <- as.factor(laptopTraining$ratings)
laptopTraining$reviews <- as.factor(laptopTraining$reviews)
laptopTest$ram_gb <- as.factor(laptopTest$ram_gb)
laptopTest$ssd <- as.factor(laptopTest$ssd)
laptopTest$hdd <- as.factor(laptopTest$hdd)
laptopTest$graphic_card_gb <- as.factor(laptopTest$graphic_card_gb)
laptopTest$display_size <- as.factor(laptopTest$display_size)
laptopTest$warranty <- as.factor(laptopTest$warranty)
laptopTest$latest_price <- as.factor(laptopTest$latest_price)
laptopTest$old_price <- as.factor(laptopTest$old_price)
laptopTest$discount <- as.factor(laptopTest$discount)
laptopTest$star_rating <- as.factor(laptopTest$star_rating)
laptopTest$ratings <- as.factor(laptopTest$ratings)
laptopTest$reviews <- as.factor(laptopTest$reviews)
laptopTest$ram_gb <- factor(laptopTest$ram_gb, levels = levels(laptopTraining$ram_gb))
laptopTest$ssd <- factor(laptopTest$ssd, levels = levels(laptopTraining$ssd))
laptopTest$hdd <- factor(laptopTest$hdd, levels = levels(laptopTraining$hdd))
laptopTest$graphic_card_gb <- factor(laptopTest$graphic_card_gb, levels = levels(laptopTraining$graphic_card_gb))
laptopTest$display_size <- factor(laptopTest$display_size, levels = levels(laptopTraining$display_size))
laptopTest$warranty <- factor(laptopTest$warranty, levels = levels(laptopTraining$warranty))
laptopTest$latest_price <- factor(laptopTest$latest_price, levels = levels(laptopTraining$latest_price))
laptopTest$old_price <- factor(laptopTest$old_price, levels = levels(laptopTraining$old_price))
laptopTest$discount <- factor(laptopTest$discount, levels = levels(laptopTraining$discount))
laptopTest$star_rating <- factor(laptopTest$star_rating, levels = levels(laptopTraining$star_rating))
laptopTest$ratings <- factor(laptopTest$ratings, levels = levels(laptopTraining$ratings))
laptopTest$reviews <- factor(laptopTest$reviews, levels = levels(laptopTraining$reviews))

#create the naive bayes model based on the training data
model <- naive_bayes(latest_price_range ~ ., data = laptopTraining, usekernel = T, laplace = 1)

#predicting on the test set using the training model
p <- predict(model, laptopTest)

#getting the confusion matrix for latest_price_range from the prediction
tab <- table(p, laptopTest$latest_price_range)

#finding the MISCLASSIFICATION percent
misclass <- 1-sum(diag(tab))/sum(tab)

#finding the correct classification percent
class <- 1-misclass
class