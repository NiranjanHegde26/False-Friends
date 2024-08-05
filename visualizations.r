"
Author: Niranjana Hegde BS
Written on: 03/08/2024, Saarbruecken
Reference documentation from R: 
    - https://rkabacoff.github.io/datavis/index.html
    - https://ggplot2.tidyverse.org/
"

library(dplyr)
library(tidyr)
install.packages("ggplot2")
library(ggplot2)

# Step 1: Basic Analysis on the Demographics
data <- read.csv("demographics_output.csv")
mean_age <- mean(data$Age, na.rm = TRUE)
std_dev_age <- sd(data$Age, na.rm = TRUE)

print(paste("Mean:", mean_age))
print(paste("Standard Deviation:", std_dev_age))

# Step 2: Basic Analysis on the main stimuli
stimuli_data <- read.csv("main_output.csv")
results <- stimuli_data %>%
  group_by(Type) %>%
  summarize(
    Mean = mean(Reading.time, na.rm = TRUE),
    SD = sd(Reading.time, na.rm = TRUE), 
    Count = n()
  )

print(results)

# Plot the the reading time as simple bar graph
ggplot(results, aes(x = Type, y = Mean, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.1) +
  labs(title = "Mean Reading Time by Type", 
       y = "Reading Time", 
       x = "Type") +
  theme_minimal()

# Now, take the self-reported data from the participants, and use them to plot the proficiency vs Reading time for each type.
demographics_data <- read.csv("demographics_output.csv")
proficiency_df <- data.frame(
  ParticipantId = demographics_data$ParticipantId,
  Proficiency = demographics_data$L1_level
)

# Calculate mean Reading Time for each participant and Type
mean_reading_time <-  stimuli_data %>%
  group_by(ParticipantId, Type) %>%
  summarize(Mean_Reading_Time = mean(Reading.time, na.rm = TRUE), .groups = "drop")

# Merge with proficiency data
final_data <- mean_reading_time %>%
  left_join(proficiency_df, by = "ParticipantId")

ggplot(final_data, aes(x = Type, y = Mean_Reading_Time, color = Proficiency, group = Proficiency)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(title = "Interaction Plot: Mean Reading Time by Type and Proficiency",
       x = "Type", y = "Mean Reading Time") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")