"
Author: Niranjana Hegde BS
First Checked On: 30/07/2024, Saarbruecken
Reference documentation and online blogs referred:
 - https://tidyr.tidyverse.org/reference/pivot_wider.html,
 - https://cran.r-project.org/web/packages/stringr/vignettes/stringr.html
 - https://r-coder.com/sapply-function-r/

Below script will extract the data from CSV, select the relevant columns and extract all the rows that are marked as VST.
Later, this is transformed into a new dataframe with column names being the unique row names of VST. A new column `Matches`
is introduced to capture the compare the participant's choice versus the actual answer. This can be later used for accuracy calculation.
"

# Load necessary libraries
library(stringr)
library(dplyr)
library(tidyr)

# Read the CSV file
current_dir <- getwd()
parent_dir <- dirname(current_dir)
file_path_data <- file.path(parent_dir, "False-Friends/csv", "cleaned.csv")
data <- read.csv(file_path_data)

# Filter rows with demographic information and select relevant columns
vst_original <- data %>%
  filter(Label == "vst") %>%
  select(MD5.hash.of.participant.s.IP.address, Parameter, Value, ItemID, Target.Word.Position)

# Reshape the data
vst_wide <- vst_original %>%
  pivot_wider(names_from = Parameter, values_from = Value)

# Rename MD5.hash.of.participant.s.IP.address to ParticipantId
vst_wide <- vst_wide %>%
  rename(ParticipantId = MD5.hash.of.participant.s.IP.address, Answer = Target.Word.Position)

# Remove unwanted columns in list
remove_cols <- c("_Trial_", "_Header_")
vst_cleaned <- subset(vst_wide, select = !(names(vst_wide) %in% remove_cols))
vst_cleaned$Choice <- sapply(vst_cleaned$Choice, unlist)

# Create new column 'Matches' to count the accuracy for later
vst_cleaned$Matches <- as.numeric(
  str_trim(str_to_lower(vst_cleaned$Choice)) == str_trim(str_to_lower(vst_cleaned$Answer))
)

# Write the result to a new CSV file
output_file_path <- file.path(parent_dir, "False-Friends/csv", "vst_output.csv")
write.csv(vst_cleaned, output_file_path, row.names = FALSE)
