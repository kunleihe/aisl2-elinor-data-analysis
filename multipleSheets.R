# Load necessary library
library(readxl)

# Specify the path to the Excel file
excel_path <- "raw_data/CR_score.xlsx"

# Get the names of all sheets in the Excel file
sheet_names <- excel_sheets(excel_path)

# Loop through each sheet name
for(sheet in sheet_names) {
  # Read the sheet
  data <- read_excel(excel_path, sheet = sheet)
  
  # Create a CSV file name
  csv_file_name <- paste0(sheet, ".csv")
  
  # Write data to CSV file
  write.csv(data, csv_file_name, row.names = FALSE)
}

