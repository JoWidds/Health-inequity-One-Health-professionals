setwd ("C:/Users/jw01626/OneDrive - University of Surrey/Documents/Elicitation/WOHC/Analysis and code/Arthur Stata analysis Sept 24")
rm(list=ls())  #Erase workspace

# library(rstudioapi)
# setwd(dirname(getActiveDocumentContext()$path)) #set file location as working directory

if (!requireNamespace("haven", quietly = TRUE)) {
  # If not installed, install it
  install.packages("haven")
}

library(haven)

# List all files in directory
files <- list.files("C:/Users/jw01626/OneDrive - University of Surrey/Documents/Elicitation/WOHC/Analysis and code/Arthur Stata analysis Sept 24")

# Look for Stata files using .dta extension within 'files' 
stata_files <- files[grep("\\.dta$", files)]

# Check the filtered list of Stata files
print(stata_files)

if (length(stata_files) > 0) {
  
      data <- read_dta(stata_files[1])
      write.csv(data, "long_data_equity_including_new_respondents.csv", row.names = FALSE)# Write the data to a CSV file
      
    } else {
      print("No Stata files found in the specified directory.")
          }


