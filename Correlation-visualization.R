laptop_data <- read.csv("clean_laptop_data.csv")

ram_gb_coeff <- (cor.test(laptop_data$ram_gb, laptop_data$latest_price, method = "pearson"))$estimate
graphic_card_gb_coeff <- (cor.test(laptop_data$graphic_card_gb, laptop_data$latest_price, method = "pearson"))$estimate
display_size_coeff <- (cor.test(laptop_data$display_size, laptop_data$latest_price, method = "pearson"))$estimate
warranty_coeff <- (cor.test(laptop_data$warranty, laptop_data$latest_price, method = "pearson"))$estimate
old_price_coeff <- (cor.test(laptop_data$old_price, laptop_data$latest_price, method = "pearson"))$estimate
discount_coeff <- (cor.test(laptop_data$discount, laptop_data$latest_price, method = "pearson"))$estimate
star_rating_coeff <- (cor.test(laptop_data$star_rating, laptop_data$latest_price, method = "pearson"))$estimate
ratings_coeff <- (cor.test(laptop_data$ratings, laptop_data$latest_price, method = "pearson"))$estimate
reviews_coeff <- (cor.test(laptop_data$reviews, laptop_data$latest_price, method = "pearson"))$estimate
ssd_coeff <- (cor.test(laptop_data$ssd, laptop_data$latest_price, method = "pearson"))$estimate
hdd_coeff <- (cor.test(laptop_data$hdd, laptop_data$latest_price, method = "pearson"))$estimate

chistats <- c(0.400, 0.333, 0.272, 0.262, 0.096, 0.084, 0.083, 0.065, 0.044, 0.033, 0.031, 0.023, 0.018, 0.016, 0.009, 0.007, 0.005, 0.005, 0.002, 0.001, 0.001, 0)
chi_names <- c("old_price", "model", "processor_name", "ssd", "graphic_card_gb", "display_size", "brand", "ram_gb", "processor_brand", "os", "ram_type", "star_rating", "weight", "hdd", "processor_gntrn", "discount", "Touchscreen", "reviews", "ratings", "warranty", "msoffice", "os_bit")
barplot(chistats, names.arg = chi_names, ylab = "Chi Squared Statistic", xlab = "Features", ylim = c(0, 1))

coeffs <- c(old_price_coeff, ssd_coeff, graphic_card_gb_coeff, ram_gb_coeff, display_size_coeff, warranty_coeff, star_rating_coeff, ratings_coeff, reviews_coeff, hdd_coeff, discount_coeff)

names <- c("old_price", "ssd", "graphic_card_gb", "ram_gb", "display_size", "warranty", "star_rating", "ratings", "reviews", "hdd", "discount")


barplot(coeffs, names.arg = names, ylab = "Correlation Coefficient", xlab = "Attributes", ylim = c(-1, 1))
