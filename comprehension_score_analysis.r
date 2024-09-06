"
Author: Niranjana Hegde BS
First Checked On: 12/08/2024, Saarbruecken
Reference documentation from R:
    - https://www.rdocumentation.org/packages/lme4/versions/1.1-35.5/topics/lmer
    - https://www.rdocumentation.org/packages/lme4/versions/1.1-35.5/topics/glmer
    This file contains the model fit and analysis of Comprehension score based on both self reported proficiency and VST score.
"

library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(lmerTest)
library(emmeans)

stimuli_data <- read.csv("spr.csv")
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

# Calculate overall mean score and standard deviation of score.
# Use them to exclude any data that are above and below some bound defined by these metrics.
overall_mean_score <- mean(participant_score$Score, na.rm = TRUE)
overall_score_sd <- sd(participant_score$Score)
overall_mean_score
overall_score_sd
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
comprehension_score_data <- stimuli_data %>%
    left_join(proficiency_df, by = "ParticipantId")

self_reported_proficiency_levels <- c("intermediate", "advanced") # Since we didn't have any beginner English L2 speakers
word_types <- c("false friend", "cognate", "unrelated")


comprehension_score_data$Proficiency <- factor(comprehension_score_data$Proficiency, levels = self_reported_proficiency_levels)
comprehension_score_data$Type <- factor(comprehension_score_data$Type, levels = word_types)

contrasts(comprehension_score_data$Proficiency) <- contr.helmert(2)
word_type_contrasts <- matrix(c(
    2 / 3, -1 / 3, -1 / 3,
    0, 1 / 2, -1 / 2
), ncol = 2)
contrasts(comprehension_score_data$Type) <- word_type_contrasts


# Model fit for Self Reported Proficiency
model_cs_srp <- glmer(Matches ~ Proficiency * Type + (1 | ParticipantId), data = comprehension_score_data, family = binomial)
summary(model_cs_srp)
emmeans(model_cs_srp, pairwise ~ Type | Proficiency)

# Some post-model fit analysis
# Residuals vs Fitted Plot
residuals <- resid(model_cs_srp)
fitted <- fitted(model_cs_srp)

residuals_df <- data.frame(Residuals = residuals, Fitted = fitted)
ggplot(residuals_df, aes(x = Fitted, y = Residuals)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, color = "#db890d") + # Add a smooth line
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Add a horizontal line at y = 0
    labs(title = "Residuals vs Fitted Plot - Comprehension Score vs Self Reported Proficiency", x = "Fitted Values", y = "Residuals") +
    theme_minimal()

# QQ Plot
residuals_qq_df <- data.frame(Residuals = residuals)
ggplot(residuals_qq_df, aes(sample = Residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") + # Add a Q-Q line
    labs(title = "Q-Q Plot of Residuals - Comprehension Score vs Self Reported Proficiency", x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()


# Model fit for VST Score
comprehension_score_data_vst <- stimuli_data %>%
    left_join(proficiency_df, by = "ParticipantId") %>%
    left_join(participant_score, by = "ParticipantId") %>%
    filter(!is.na(Score))
comprehension_score_data_vst$Score_centered <- scale(comprehension_score_data_vst$Score, center = TRUE, scale = FALSE)
model_cs_vst <- glmer(Matches ~ Score_centered * Type + (1 | ParticipantId),
    data = comprehension_score_data_vst,
    family = binomial
)
summary(model_cs_vst)

# Residuals vs Fitted Plot
residuals <- resid(model_cs_vst)
fitted <- fitted(model_cs_vst)
residuals_df <- data.frame(Residuals = residuals, Fitted = fitted)

ggplot(residuals_df, aes(x = Fitted, y = Residuals)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, color = "#db890d") + # Add a smooth line
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Add a horizontal line at y = 0
    labs(title = "Residuals vs Fitted Plot Comprehension Score vs VST", x = "Fitted Values", y = "Residuals") +
    theme_minimal()

# Q-Q plot
residuals_qq_df <- data.frame(Residuals = residuals)
ggplot(residuals_qq_df, aes(sample = Residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") + # Add a Q-Q line
    labs(title = "Q-Q Plot of Residuals Comprehension Score vs VST", x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
emmeans(model_cs_vst, pairwise ~ Type | Score_centered)

# Add sentence length as a fixed effect to see if it can give a better fit.
# The assumption here is that maybe the sentence length has an effect on the participants' ability to answer.
# Longer sentences might lead to working memory overload and hence has an influence on the answering.

summary(comprehension_score_data$SentenceLength)
hist(comprehension_score_data$SentenceLength)

# Howeveer, the basic summary and histogram says that the sentence length is not normally distributed.
# We scale the sentence length to fit the model.

comprehension_score_data$SentenceLengthScaled <- scale(comprehension_score_data$SentenceLength)
hist(comprehension_score_data$SentenceLengthScaled)

model_cs_sl_srp <- glmer(Matches ~ Proficiency * Type + SentenceLengthScaled + (1 | ParticipantId), data = comprehension_score_data, family = binomial)
summary(model_cs_sl_srp)

model_cs_sl_vst <- glmer(Matches ~ Accuracy * Type + SentenceLengthScaled + (1 | ParticipantId),
    data = comprehension_score_data,
    family = binomial
)
summary(model_cs_sl_vst)
# There is no conervenge for GLMER model with sentence length.


# Compare 3 models based on AIC and BIC
models <- list(model_cs_srp, model_cs_vst, model_cs_sl_srp)
aic_values <- sapply(models, AIC)
bic_values <- sapply(models, BIC)

# Create a data frame for easy comparison
comparison <- data.frame(
    Model = paste0("Model", 1:3),
    AIC = aic_values,
    BIC = bic_values
)
print(comparison)
