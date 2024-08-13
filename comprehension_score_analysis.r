"
Author: Niranjana Hegde BS
Written on: 12/08/2024, Saarbruecken
Reference documentation from R:
    - https://www.rdocumentation.org/packages/lme4/versions/1.1-35.5/topics/lmer
    - https://www.rdocumentation.org/packages/lme4/versions/1.1-35.5/topics/glmer
    This file contains the model fit and analysis of Comprehension score based on both self reported proficiency and VST score.
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
comprehension_score_data <- stimuli_data %>%
    left_join(proficiency_df, by = "ParticipantId") %>%
    left_join(participant_score, by = "ParticipantId")

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

model_cs_srp <- glmer(Matches ~ Proficiency * Type + (1 | ParticipantId), data = comprehension_score_data, family = binomial)
summary(model_cs_srp)


model_cs_vst <- glmer(Matches ~ Accuracy * Type + (1 | ParticipantId),
    data = comprehension_score_data,
    family = binomial
)
summary(model_cs_vst)

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
