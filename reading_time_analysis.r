"
Author: Niranjana Hegde BS
Written on: 12/08/2024, Saarbruecken
Reference documentation from R:
    - https://www.rdocumentation.org/packages/lme4/versions/1.1-35.5/topics/lmer
    - https://www.rdocumentation.org/packages/lme4/versions/1.1-35.5/topics/glmer
    This file contains the model fit and analysis of Reading time of target words based on both self reported proficiency and VST score.
"

library(dplyr)
library(tidyr)
library(lme4)
stimuli_data <- read.csv("main_output.csv")
demographics_data <- read.csv("demographics_output.csv")
vst_data <- read.csv("vst_output.csv")

proficiency_df <- data.frame(
    ParticipantId = demographics_data$ParticipantId,
    Proficiency = demographics_data$L1_level
)

participant_score <- vst_data %>%
    group_by(ParticipantId) %>%
    summarize(
        Score = sum(Matches, na.rm = TRUE),
        Accuracy = mean(Matches, na.rm = TRUE),
        Total_Samples = n()
    ) %>%
    ungroup()

# Add both proficiency and VST score to stimuli data
reading_time_data <- stimuli_data %>%
    left_join(proficiency_df, by = "ParticipantId") %>%
    left_join(participant_score, by = "ParticipantId")

self_reported_proficiency_levels <- c("intermediate", "advanced") # Since we didn't have any beginner English L2 speakers
word_types <- c("false friend", "cognate", "unrelated")

reading_time_data$Proficiency <- factor(reading_time_data$Proficiency, levels = self_reported_proficiency_levels)
reading_time_data$Type <- factor(reading_time_data$Type, levels = word_types)

contrasts(reading_time_data$Proficiency) <- contr.helmert(2)
word_type_contrasts <- matrix(c(
    2 / 3, -1 / 3, -1 / 3,
    0, 1 / 2, -1 / 2
), ncol = 2)
contrasts(reading_time_data$Type) <- word_type_contrasts
model_rt_srp <- lmer(Reading.time ~ Proficiency * Type + (1 | ParticipantId), data = reading_time_data)
summary(model_rt_srp)


model_rt_vst <- glmer(Reading.time ~ Accuracy * Type + (1 | ParticipantId),
    data = reading_time_data,
    family = binomial
)
summary(model_rt_vst)
