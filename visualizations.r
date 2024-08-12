"
Author: Niranjana Hegde BS
Written on: 03/08/2024, Saarbruecken
Reference documentation from R:
    - https://rkabacoff.github.io/datavis/index.html
    - https://ggplot2.tidyverse.org/
"

library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Basic Analysis on the Demographics
data <- read.csv("demographics_output.csv")
mean_age <- mean(data$Age, na.rm = TRUE)
std_dev_age <- sd(data$Age, na.rm = TRUE)

print(paste("Mean:", mean_age))
print(paste("Standard Deviation:", std_dev_age))

"
  Reading time vs the participant's self reported proficiency
"
# Basic Analysis on the main stimuli
stimuli_data <- read.csv("main_output.csv")
results <- stimuli_data %>%
  group_by(Type) %>%
  summarize(
    Mean = mean(Reading.time, na.rm = TRUE),
    SD = sd(Reading.time, na.rm = TRUE),
    Count = n(),
  )

print(results)

# Plot the the reading time as simple bar graph
ggplot(results, aes(x = Type, y = Mean, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.1) +
  labs(
    title = "Mean Reading Time by Type",
    y = "Reading Time",
    x = "Type"
  ) +
  theme_minimal()

# Now, take the self-reported data from the participants, and use them to plot the proficiency vs Reading time for each type.
demographics_data <- read.csv("demographics_output.csv")
proficiency_df <- data.frame(
  ParticipantId = demographics_data$ParticipantId,
  Proficiency = demographics_data$L1_level
)

# Calculate mean Reading Time for each participant and Type
mean_reading_time <- stimuli_data %>%
  group_by(ParticipantId, Type) %>%
  summarize(Mean_Reading_Time = mean(Reading.time, na.rm = TRUE), .groups = "drop")

mean_reading_time

# Merge with proficiency data
final_data <- mean_reading_time %>%
  left_join(proficiency_df, by = "ParticipantId")

ggplot(final_data, aes(x = Type, y = Mean_Reading_Time, color = Proficiency, group = Proficiency)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(
    title = "Mean Reading Time by Type and Self Reported L2 Proficiency",
    x = "Type", y = "Mean Reading Time"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

"
  Comprehension Question score based on self-reported proficiency
"
comprehension_score <- stimuli_data %>%
  group_by(ParticipantId, Type) %>%
  summarize(Mean_Comprehension_Score = mean(sum(Matches, na.rm = TRUE)), .groups = "drop")

final_comprehension_score_data <- comprehension_score %>%
  left_join(proficiency_df, by = "ParticipantId")

ggplot(final_comprehension_score_data, aes(x = Type, y = Mean_Comprehension_Score, color = Proficiency, group = Proficiency)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(
    title = "Mean Comprehension Score by Type and Self Reported L2  Proficiency",
    x = "Type", y = "Mean Comprehension Score"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")


"
  Reading time vs the participant's VST Score
"
vst_data <- read.csv("vst_output.csv")
participant_score <- vst_data %>%
  group_by(ParticipantId) %>%
  summarize(
    Score = sum(Matches, na.rm = TRUE),
    Accuracy = mean(Matches, na.rm = TRUE),
    Total_Samples = n()
  ) %>%
  ungroup()

# View the result
print(participant_score)

# Calculate overall mean accuracy
overall_mean_accuracy <- mean(participant_score$Accuracy, na.rm = TRUE)

# Find highest accuracy
highest_accuracy <- max(participant_score$Accuracy, na.rm = TRUE)

# Find lowest accuracy
lowest_accuracy <- min(participant_score$Accuracy, na.rm = TRUE)

# Print the results
cat("Overall mean accuracy:", round(overall_mean_accuracy, 2), "\n")
cat("Highest accuracy:", round(highest_accuracy, 2), "\n")
cat("Lowest accuracy:", round(lowest_accuracy, 2), "\n")

# Optionally, print the user(s) with the highest and lowest accuracy
user_highest <- participant_score$ParticipantId[which.max(participant_score$Accuracy)]
user_lowest <- participant_score$ParticipantId[which.min(participant_score$Accuracy)]

cat("User(s) with highest accuracy:", user_highest, "\n")
cat("User(s) with lowest accuracy:", user_lowest, "\n")

# Merge the VST score with reading time
vst_final_data <- mean_reading_time %>%
  left_join(participant_score, by = "ParticipantId")

# Plot the results
ggplot(vst_final_data, aes(x = Score, y = Mean_Reading_Time, color = Type)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(x = "Score", y = "Mean Reading Time", title = "Mean Reading Time by Condition and VST Score") +
  theme_classic()


"
  Comprehension Question score based on VST Score
"
vst_comprehension_data <- comprehension_score %>%
  left_join(participant_score, by = "ParticipantId")

ggplot(vst_comprehension_data, aes(x = Score, y = Mean_Comprehension_Score, color = Type)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(x = "Score", y = "Mean Comprehension Score", title = "Mean Comprehension Score by Condition and VST Score") +
  theme_classic()

"
  Comprehension Question score based on the length of the sentence
"
comprehension_score_sentence_length <- aggregate(Matches ~ Type + SentenceLength, data = stimuli_data, FUN = mean)

# Create the plot
ggplot(comprehension_score_sentence_length, aes(x = SentenceLength, y = Matches, color = Type, group = Type)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Sentence Length", y = "Accuracy",
    title = "Interaction between Sentence Length and Accuracy by Type"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)
