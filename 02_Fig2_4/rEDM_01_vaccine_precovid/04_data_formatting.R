#############################
# Data Formatting Functions #
#############################


data.clean <- function(dataset, i){
  
  # Load the data and create the main dataframe df
  file_name <- paste("01_data/", dataset, ".csv", sep="")
  data <- read.csv(file_name, header = TRUE)
  # Extract country name and subset data for the specific country
  country_name <- unique(data$country)
  data_country <- subset(data, data$country == country_name[i])
  #data_country <- subset(data_country, year >= 2016 & year <= 2019)
  # Print country information for verification
  print(paste0(i, "_", country_name[i]))
  return(data_country)
}

