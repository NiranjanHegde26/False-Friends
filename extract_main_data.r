"
Author: Niranjana Hegde BS
First Checked On: 02/08/2024, Saarbruecken
Reference documentation and online blogs referred:
 - https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/join,
 - https://www.rdocumentation.org/packages/dplyr/versions/1.0.10

Below script will extract the data from Main stimuli CSV, select the relevant columns and extract all the rows that are marked as main.
Also, another operation is executed where all the keypresses are captured. Both are joined together later.
"

# Load required library
library(dplyr)
library(tidyr)
library(stringr)

data <- read.csv("my_results.csv")
stimuli_file <- read.csv("Stimuli.csv")
stimuli_file$Type <- tolower(trimws(stimuli_file$Type))
stimuli_file$Word <- tolower(trimws(stimuli_file$Word))


# Filter all the main stimuli for all participants where only the target word related rows are considered.
# This will also remove any fillers we have in the main stimuli file.
dashed_sentences <- data %>%
  filter(Label == "main") %>%
  filter(
    PennElementName == "DashedSentence",
    Parameter == Target.Word.Position
  )

# Also save the length of the stimuli sentence
dashed_sentences$SentenceLength <- str_count(dashed_sentences$Sentence..or.sentence.MD5., "\\w+")

# Separeately filter all the key presses related row based on Participant ID, Order no. of items, create a new column called PressedKey
# This ensures that only the data related to each key press of each participant for the questions are selected.
pressed_keys <- data %>%
  filter(Label == "main") %>%
  filter(Parameter == "PressedKey") %>%
  select(
    PressedKey = Value,
    Order.number.of.item,
    MD5.hash.of.participant.s.IP.address
  ) %>%
  group_by(MD5.hash.of.participant.s.IP.address, Order.number.of.item) %>%
  slice(1) %>% # Keep only the first PressedKey for each participant and Order.number.of.item
  ungroup()


# Join DashedSentences with PressedKeys and create a new dataframe.
result <- dashed_sentences %>%
  left_join(pressed_keys,
    by = c("Order.number.of.item", "MD5.hash.of.participant.s.IP.address")
  ) %>%
  filter(!is.na(PressedKey)) %>% # Remove any sentences whose comprehension answer is not present
  arrange(MD5.hash.of.participant.s.IP.address, Order.number.of.item)

result <- result %>%
  mutate(
    PressedKey = case_when(
      PressedKey == "D" ~ "Yes",
      PressedKey == "K" ~ "No",
      TRUE ~ PressedKey
    ),
    Matches = case_when(
      PressedKey == SPR_Answer ~ 1,
      TRUE ~ 0
    )
  )

# Remove unnecessary columns from the dataframe by listing all the required columns.
columns_to_keep <- c("MD5.hash.of.participant.s.IP.address", "Parameter", "Value", "Reading.time", "Target.Word.Position", "ItemID", "PressedKey", "Matches", "SPR_Answer", "SentenceLength")
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

# From Larson, Kevin. (2005). The Science of Word Recognition; or how I learned to stop worrying and love the bouma. 13. 2-11.
# we can ignore any reading time less than 200ms and more than 300ms. We become a bit more lenient due to possible key press delays
# Our new window is thus, 200ms-300ms. So, we filter out any rows that do not fit this window.
result$Reading.time <- as.numeric(result$Reading.time)
count <- sum(result$Reading.time < 200)
print(paste("Number of rows with reading time < 200:", count))
result <- result %>% filter(Reading.time >= 200)
print(paste("Number of rows with reading time in required range:", nrow(result)))

# Write to new CSV for future usage.
write.csv(result, "cleaned.csv", row.names = FALSE)
