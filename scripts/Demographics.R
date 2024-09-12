"
Author: Niranjana Hegde BS
First Checked On: 30/07/2024, Saarbruecken
Reference documentation from R: https://tidyr.tidyverse.org/reference/pivot_wider.html

Below script will extract the data from CSV, select the relevant columns and extract all the rows that are marked as Demogrpahics.
Later, this is transformed into a new dataframe with column names being the unique row names of demographics.
"

# Load necessary libraries
library(dplyr)
library(tidyr)

# Read the CSV file
current_dir <- getwd()
parent_dir <- dirname(current_dir)
file_path_data <- file.path(parent_dir, "False-Friends/csv", "cleaned.csv")
data <- read.csv(file_path_data)

# Filter rows with demographic information and select relevant columns
demographics <- data %>%
  filter(PennElementName == "demographics") %>%
  select(MD5.hash.of.participant.s.IP.address, Parameter, Value)

# Reshape the data
demographics_wide <- demographics %>%
  pivot_wider(names_from = Parameter, values_from = Value)

# Rename MD5.hash.of.participant.s.IP.address to ParticipantId
demographics_wide <- demographics_wide %>%
  rename(ParticipantId = MD5.hash.of.participant.s.IP.address)

# Write the result to a new CSV file
output_file_path <- file.path(parent_dir, "False-Friends/csv", "demographics_output.csv")
write.csv(demographics_wide, output_file_path, row.names = FALSE)
