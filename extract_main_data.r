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
stimuli_file <- read.csv("Stimuli.csv")
stimuli_file$Type <- tolower(trimws(stimuli_file$Type))
stimuli_file$Word <- tolower(trimws(stimuli_file$Word))

# Filter all the main stimuli for all participants where only the target word related rows are considered.
# This will also remove any fillers we have in the main stimuli file.
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

# Append the type of word to each target word. This was missing in PCIbex coding.
get_type <- function(word) {
  word <- tolower(trimws(word))
  match <- stimuli_file$Type[stimuli_file$Word == word]
  if (length(match) > 0) {
    return(match[1])
  } else {
    return(NA)
  }
}

# There are some strings with ',' character. Remove the character and save it back.
result$Value <- sapply(result$Value, function(x) gsub("%2C", "", x))
result$Value <- sapply(result$Value, function(x) gsub("PROFESSOR'S", "PROFESSOR", x))
result$Type <- sapply(result$Value, get_type)

# Write to new CSV for future usage.
write.csv(result, "main_output.csv", row.names = FALSE)