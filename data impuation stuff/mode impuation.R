
laptop_data <- read.csv("LD_knn_processor_gntrn_display_size.csv");

lenovo_laptop <- laptop_data[laptop_data$brand == "Lenovo",];

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

find_mode(lenovo_laptop$model)
