# About

This repository contains the code for the data analysis for the research on effects of proficiency of L2 on disambiguating the meanings of false friends.

# Abstract

This study investigates nonselective lexical access in German L2 English speakers by examining the impact of orthographic form of false friends on sentence processing on varying English proficiency of participants. Participants read English sentences and answered comprehension questions in German. Results strongly support nonselective access: false friends increased reading times regardless of L2 proficiency, indicating simultaneous activation of both languages. Comprehension accuracy was lower for false friends compared to cognates, but higher L2 proficiency was associated with improved disambiguation. These findings align with the bilingual interactive activation plus (BIA+) model, demonstrating that while lexical access remains nonselective, proficient L2 speakers can better suppress non-target language activation.

# Prerequisites

R - No hard dependency on the IDE. Compatible with VS Code, R Studio.

# Packages Used

1. dplyr
2. tidyr
3. lme4
4. ggplot2
5. lmerTest
6. emmeans
7. stringr

In case these packages are not installed, it is highly recommended for you to install them. Alternatively, the scripts will also install them in case they are not available.

# Steps to use

1. Clone/Copy the repo contents into your desired directory.
2. Create the subdirectories called "csv" and "images" and place the FalseFriendsData.csv into this newly created "csv" subdirectory.
3. Install any missing packages.
4. Execute the files in the below order.
   1. extract_from_pcibex.r (To clean the PCIbex raw data into cleaner dataframe into new CSV)
   2. extract_main_data.r (To extract SPR data from cleaned CSV)
   3. demographics.r (To extarct the demographics data from cleaned CSV)
   4. vst_extarcted.r (To extarct the VST data from cleaned CSV)
   5. visualizations.r (To generate the visualization images and save the images to local subdirectory "images")
   6. reading_time_analysis.r (To fit the models for Reading time)
   7. comprehension_score_analysis.r (To fit the models for Comprehension score and Matches).
