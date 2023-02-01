#make sure to set the working directory in order to properly read in CSV

laptop_data <- read.csv("laptop_data.csv");

#summary shows NA data
summary(data);

#remove missing values in r via na.omit() method
#it is important to note that the values must be "NA" r type in order to properly seen as NA values
laptop_data_omit_NA <- na.omit(laptop_data);

#summary of data after it has been  cleaned
summary(laptop_data_omit_NA);

#correlation test between model and processor generation 
cor.test(laptop_data_omit_NA$model, laptop_data_omit_NA$processor_gnrtn, method="pearson");
#correlation test between model and display size 
cor.test(laptop_data_omit_NA$model, laptop_data_omit_NA$display_size, method="pearson");

chisq.test(dataSub$firstColumn, dataSub$secondColumn, correct=FALSE)



