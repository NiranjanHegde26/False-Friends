"
Author: Niranjana Hegde BS
First Checked On: 03/08/2024, Saarbruecken
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
data$L1_age <- as.numeric(data$L1_age)
mean_age_l2 <- mean(data$L1_age, na.rm = TRUE)
std_dev_age_l2 <- sd(data$L1_age, na.rm = TRUE)

print(paste("Mean Age:", mean_age))
print(paste("Standard Deviation Age:", std_dev_age))
print(paste("Mean Age of English Exposure:", mean_age_l2))
print(paste("Standard Deviation Age of English Exposure:", std_dev_age_l2))

"
  Reading time vs the participant's self reported proficiency
"
# Basic Analysis on the main stimuli
stimuli_data <- read.csv("spr.csv")
results <- stimuli_data %>%
  group_by(Type) %>%
  summarize(
    Mean = mean(Reading.time, na.rm = TRUE),
    SD = sd(Reading.time, na.rm = TRUE),
    Count = n(),
  )

print(results)
# Plot the the reading time as simple bar graph
rt_type <- ggplot(results, aes(x = Type, y = Mean, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.1) +
  labs(
    title = "Mean Reading Time by Type",
    y = "Reading Time",
    x = "Type"
  ) +
  theme_minimal()
ggsave(filename = "rt_type.jpg", plot = rt_type, width = 8, height = 6, dpi = 800)

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

# Merge with proficiency data
spr_data <- mean_reading_time %>%
  left_join(proficiency_df, by = "ParticipantId")

rt_srp <- ggplot(spr_data, aes(x = Type, y = Mean_Reading_Time, color = Proficiency, group = Proficiency)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(
    title = "Mean Reading Time by Type and Self Reported L2 Proficiency",
    x = "Type", y = "Mean Reading Time"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")
ggsave(filename = "rt_srp.jpg", plot = rt_srp, width = 8, height = 6, dpi = 800)

"
  Comprehension Question score based on self-reported proficiency
"
comprehension_score <- stimuli_data %>%
  group_by(ParticipantId, Type) %>%
  summarize(Mean_Comprehension_Score = mean(sum(Matches, na.rm = TRUE)), .groups = "drop")

final_comprehension_score_data <- comprehension_score %>%
  left_join(proficiency_df, by = "ParticipantId")

cs_srp <- ggplot(final_comprehension_score_data, aes(x = Type, y = Mean_Comprehension_Score, color = Proficiency, group = Proficiency)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(
    title = "Mean Comprehension Score by Type and Self Reported L2  Proficiency",
    x = "Type", y = "Mean Comprehension Score"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")
ggsave(filename = "cs_srp.jpg", plot = cs_srp, width = 8, height = 6, dpi = 800)

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
hist(participant_score$Score)

# Calculate overall mean score
overall_mean_score <- mean(participant_score$Score, na.rm = TRUE)
overall_score_sd <- sd(participant_score$Score)
# Find highest score
highest_score <- max(participant_score$Score, na.rm = TRUE)

# Find lowest score
lowest_score <- min(participant_score$Score, na.rm = TRUE)

# Print the results
cat("Overall mean score:", round(overall_mean_score, 2), "\n")
cat("Highest score:", round(highest_score, 2), "\n")
cat("Lowest score:", round(overall_score_sd, 2), "\n")

# Optionally, print the user(s) with the highest and lowest score
user_highest <- participant_score$ParticipantId[which.max(participant_score$Score)]
user_lowest <- participant_score$ParticipantId[which.min(participant_score$Score)]

cat("User(s) with highest score:", user_highest, "\n")
cat("User(s) with lowest score:", user_lowest, "\n")

# Merge the VST score with reading time. The boundaries are defined by IQR
Q1 <- quantile(participant_score$Score, 0.25) # First quartile (25th percentile)
Q3 <- quantile(participant_score$Score, 0.75) # Third quartile (75th percentile)

# Calculate IQR
IQR <- Q3 - Q1

# Calculate the Lower Bound
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q1 + 1.5 * IQR

# Filter the data
filtered_participant_score <- participant_score %>%
  filter(Score >= lower_bound & Score <= upper_bound)

# Remove reading time data of all those participants that didn't score in the upper and lower bound
vst_spr_data <- mean_reading_time %>%
  left_join(filtered_participant_score, by = "ParticipantId") %>%
  filter(!is.na(Score))

# Plot the results
rt_vst <- ggplot(vst_spr_data, aes(x = Score, y = Mean_Reading_Time, color = Type)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(x = "Score", y = "Mean Reading Time", title = "Mean Reading Time by Condition and VST Score") +
  theme_classic()

ggsave(filename = "rt_vst.jpg", plot = rt_vst, width = 8, height = 6, dpi = 800)

"
  Comprehension Question score based on VST Score
"
vst_comprehension_data <- comprehension_score %>%
  left_join(filtered_participant_score, by = "ParticipantId") %>%
  filter(!is.na(Score))

cs_vst <- ggplot(vst_comprehension_data, aes(x = Score, y = Mean_Comprehension_Score, color = Type)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(x = "Score", y = "Mean Comprehension Score", title = "Mean Comprehension Score by Condition and VST Score") +
  theme_classic()

ggsave(filename = "cs_vst.jpg", plot = cs_vst, width = 8, height = 6, dpi = 800)

"
  Comprehension Question score based on the length of the sentence
"
comprehension_score_sentence_length <- aggregate(Matches ~ Type + SentenceLength, data = stimuli_data, FUN = mean)

# Create the plot
ggplot(comprehension_score_sentence_length, aes(x = SentenceLength, y = Matches, color = Type, group = Type)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Sentence Length", y = "Score",
    title = "Interaction between Sentence Length and Accuracy by Type"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)
