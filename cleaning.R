#loading stringr in order to utilize str_replace method
library(stringr)
#reading in last cleaned data
fullClean <- read.csv("laptop_data_cleaned.csv");
#turning ssd into a numerical data type
fullClean$ssd <- str_replace(fullClean$ssd, " GB", "");
fullClean$ssd <- as.numeric(fullClean$ssd);

#turning hdd into a numerical data type
fullClean$hdd <- str_replace(fullClean$hdd, " GB", "");
fullClean$hdd <- as.numeric(fullClean$hdd);

#converting brand strings into lower case and changing it into a factor data type
fullClean$brand <- tolower(fullClean$brand);
fullClean$brand <- as.factor(fullClean$brand);

#converting latest price from rupees to USD
fullClean$latest_price <- fullClean$latest_price / 82.57;
fullClean$latest_price <- round(fullClean$latest_price, 2);

#converting latest price from rupees to USD
fullClean$old_price <- fullClean$old_price / 82.57;
fullClean$old_price <- round(fullClean$old_price, 2);

#changing attributes as factors as necessary
fullClean$msoffice <- as.factor(fullClean$msoffice);
fullClean$os_bit <- as.factor(fullClean$os_bit);
fullClean$weight <- as.factor(fullClean$weight);
fullClean$os <- as.factor(fullClean$os);
fullClean$processor_brand <- as.factor(fullClean$processor_brand);
fullClean$processor_gnrtn <- as.factor(fullClean$processor_gnrtn);
fullClean$Touchscreen <- as.factor(fullClean$Touchscreen);

write.csv(fullClean, "C:\\Users\\johan\\Desktop\\clean_laptop_data.csv",  row.names=FALSE);

