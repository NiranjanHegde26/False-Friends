"
Author: Niranjana Hegde BS
Written on: 02/08/2024, Saarbruecken
Reference documentation and online blogs referred: 
 - https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/join, 
 - https://www.rdocumentation.org/packages/dplyr/versions/1.0.10

Below script will extract the data from Main stimuli CSV, select the relevant columns and extract all the rows that are marked as main.
Also, another operation is executed where all the keypresses are captured. Both are joined together later.
"

# Load required library
library(dplyr)
library(tidyr)

data <- read.csv("my_results.csv")

# Filter all the main stimuli for all participants where only the target word related rows are considered
dashed_sentences <- data %>%
  filter(Label == "main") %>%
  filter(PennElementName == "DashedSentence",
         Parameter == Target.Word.Position)

# Separeately filter all the key presses related row based on Participant ID, Order no. of items, create a new column called PressedKey
# This ensures that only the data related to each key press of each participant for the questions are selected. 
pressed_keys <- data %>%
filter(Label == "main") %>%
  filter(Parameter == "PressedKey") %>%
  select(PressedKey = Value, 
         Order.number.of.item, 
         MD5.hash.of.participant.s.IP.address) %>%
  group_by(MD5.hash.of.participant.s.IP.address, Order.number.of.item) %>%
  slice(1) %>%  # Keep only the first PressedKey for each participant and Order.number.of.item
  ungroup()

# Join DashedSentences with PressedKeys and create a new dataframe.
result <- dashed_sentences %>%
  left_join(pressed_keys, 
            by = c("Order.number.of.item", "MD5.hash.of.participant.s.IP.address")) %>%
  arrange(MD5.hash.of.participant.s.IP.address, Order.number.of.item)

# Remove unnecessary columns from the dataframe by listing all the required columns.
columns_to_keep <- c("MD5.hash.of.participant.s.IP.address", "Parameter", "Value", "Reading.time","Target.Word.Position", "ItemID", "PressedKey", "SPR_Answer")
result <- result %>%
  select(all_of(columns_to_keep))

# Remane the Participant ID
result <- result %>%
            rename(ParticipantId = MD5.hash.of.participant.s.IP.address)

# Write to new CSV for future usage.
write.csv(result, "main_output.csv", row.names = FALSE)