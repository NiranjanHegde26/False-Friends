"
Author: Niranjana Hegde BS
First Checked On: 12/08/2024, Saarbruecken
Reference documentation from R:
    - https://www.rdocumentation.org/packages/lme4/versions/1.1-35.5/topics/lmer
    - https://www.rdocumentation.org/packages/lme4/versions/1.1-35.5/topics/glmer
    This file contains the model fit and analysis of Reading time of target words based on both self reported proficiency and VST score.
"

library(dplyr)
library(tidyr)
library(lme4)
library(emmeans)
library(lmerTest)
library(ggplot2)

# Read CSV files
current_dir <- getwd()
parent_dir <- dirname(current_dir)
file_path_data <- file.path(parent_dir, "False-Friends/csv", "spr.csv")
file_path_demographics <- file.path(parent_dir, "False-Friends/csv", "demographics_output.csv")
file_path_vst_data <- file.path(parent_dir, "False-Friends/csv", "vst_output.csv")

stimuli_data <- read.csv(file_path_data)
demographics_data <- read.csv(file_path_demographics)
vst_data <- read.csv(file_path_vst_data)
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

# Calculate overall mean score and standard deviation of score.
# Use them to exclude any data that are above and below some bound defined by these metrics.
overall_mean_score <- mean(participant_score$Score, na.rm = TRUE)
overall_score_sd <- sd(participant_score$Score)

# Look at this later.
Q1 <- quantile(participant_score$Score, 0.25) # First quartile (25th percentile)
Q3 <- quantile(participant_score$Score, 0.75) # Third quartile (75th percentile)

# Calculate IQR
IQR <- Q3 - Q1

# Calculate the Lower Bound
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q1 + 1.5 * IQR

# Filter the data
participant_score <- participant_score %>%
    filter(Score >= lower_bound & Score <= upper_bound)

# Add both proficiency and VST score to stimuli data
reading_time_data <- stimuli_data %>%
    left_join(proficiency_df, by = "ParticipantId")

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

residuals <- resid(model_rt_srp)
fitted <- fitted(model_rt_srp)

residuals_df <- data.frame(Residuals = residuals, Fitted = fitted)
ggplot(residuals_df, aes(x = Fitted, y = Residuals)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, color = "#db890d") + # Add a smooth line
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Add a horizontal line at y = 0
    labs(title = "Residuals vs Fitted Plot - Readting time vs Self Reported Proficiency", x = "Fitted Values", y = "Residuals") +
    theme_minimal()

# QQ Plot
residuals_qq_df <- data.frame(Residuals = residuals)
ggplot(residuals_qq_df, aes(sample = Residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") + # Add a Q-Q line
    labs(title = "Q-Q Plot of Residuals - Readting time vs Self Reported Proficiency", x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()

# Post-hoc comparisons for reading time model
emmeans(model_rt_srp, pairwise ~ Type | Proficiency)

# Model fit using VST
reading_time_data_vst <- stimuli_data %>%
    left_join(proficiency_df, by = "ParticipantId") %>%
    left_join(participant_score, by = "ParticipantId") %>%
    filter(!is.na(Score))
reading_time_data_vst$Score_Centered <- scale(reading_time_data_vst$Score, center = TRUE, scale = FALSE)
model_rt_vst <- lmer(Reading.time ~ Score_Centered * Type + (1 | ParticipantId),
    data = reading_time_data_vst,
)
summary(model_rt_vst)

residuals <- resid(model_rt_vst)
fitted <- fitted(model_rt_vst)

residuals_df <- data.frame(Residuals = residuals, Fitted = fitted)
ggplot(residuals_df, aes(x = Fitted, y = Residuals)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, color = "#db890d") + # Add a smooth line
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Add a horizontal line at y = 0
    labs(title = "Residuals vs Fitted Plot - Readting time vs VST", x = "Fitted Values", y = "Residuals") +
    theme_minimal()

# QQ Plot
residuals_qq_df <- data.frame(Residuals = residuals)
ggplot(residuals_qq_df, aes(sample = Residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") + # Add a Q-Q line
    labs(title = "Q-Q Plot of Residuals - Readting time vs VST", x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
emmeans(model_rt_vst, pairwise ~ Type | Score_Centered)
# Compare 2 models based on AIC and BIC
models <- list(model_rt_srp, model_rt_vst)
aic_values <- sapply(models, AIC)
bic_values <- sapply(models, BIC)

# Create a data frame for easy comparison
comparison <- data.frame(
    Model = paste0("Model", 1:2),
    AIC = aic_values,
    BIC = bic_values
)
print(comparison)
