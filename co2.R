# Load the CO2 dataset
data(co2)

# Convert the time series object to a data frame
co2_df <- data.frame(Time = time(co2), CO2 = as.numeric(co2))

# Specify the file path where you want to save the CSV
file_path <- "co2_data.csv"

# Save the data frame as a CSV file
write.csv(co2_df, file = file_path, row.names = FALSE)

# Confirm the file has been saved
cat("CO2 data has been saved to", file_path)

