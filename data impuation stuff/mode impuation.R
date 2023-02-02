
#read in dataset with KNN generated processor_gntrn and display_size attributes
laptop_data <- read.csv("LD_knn_processor_gntrn_display_size.csv");

#collect records that include missing laptop models
missing_models <- laptop_data[laptop_data$model == '',]

#get the all the brands that have records missing the 'model' attribute
brands <- unique(missing_models$brand)

#for each brand missing model, grab there individual records and store the for finding out the mean
acer_brand<- laptop_data[laptop_data$brand == "acer",]
alienware_brand <- laptop_data[laptop_data$brand == "ALIENWARE",]
asus_brand <- laptop_data[laptop_data$brand == "ASUS",]
dell_brand <- laptop_data[laptop_data$brand =="DELL",]
hp_brand <- laptop_data[laptop_data$brand == "HP",]
lenovo_brand <- laptop_data[laptop_data$brand == "Lenovo",]
msi_brand <- laptop_data[laptop_data$brand =="MSI",]

#use find_mode on the model column of each brand in order to find the mode that will be used to imput the values for missing data
find_mode(acer_brand$model)
find_mode(alienware_brand$model)
find_mode(asus_brand$model)
find_mode(dell_brand$model)
find_mode(hp_brand$model)
find_mode(lenovo_brand$model)
find_mode(msi_brand$model)

#using these values, we can replace them using rapid miner or using spreadsheet application

#finds mode for a given vector
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}


