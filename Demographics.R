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
data <- read.csv("my_results.csv")

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
write.csv(demographics_wide, "demographics_output.csv", row.names = FALSE)
