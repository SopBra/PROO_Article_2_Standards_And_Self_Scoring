############################################################################  --
##                       Data Analysis Study 1 and 2:
##                  Effects of Self-Scoring & Standards on
##                Monitoring Accuracy and Text Comprehension
##                           Sophia Braumann                                  
## -------------------------------------------------------------------------- --
##
##           !! COLLAPSE ALL LINES TO BETTER NAVIGATE THIS SCRIPT !!
##            (Edit->Folding->Collapse All / alt+cmd+o/ Ctrl+Alt+o)
##
## -------------------------------------------------------------------------- --
## 0. Set the paths to the required data and load files                       ----
## -------------------------------------------------------------------------- ----
##    Set path to relevant data for running the script                        ----
# To the directory of the real data:
rel_path_processed_data <- "Data/Processed_Data_For_Analyses/"
rel_path_artificial_data <- "Data/Artificial_Data/Artificial_Peformance_Data/"

##    (Install if necessary and) load relevant libraries                      ----
#  Check and perform installation of relevant packages if needed
list_of_packages <- c(
  "tidyverse",  # distinct, filter, %>%, and many more
  "data.table", # for fread (loading .txt files)
  "Hmisc",      # for computing gamma correlations
  "ggrepel",    # for ggplots with labelled points
  "lme4",       # for ML models
  "lmerTest",   # for p values with lmer
  "sjstats",    # partial eta squared and cohens effect size in repeated masures ML
  "sjPlot",     # for printing lmer HTML tables
  "rsq",        # for retrieving conditional R squared of lmer models
  "emmeans",    # for pairwise comparisons and effect sizes etc.
  "pbkrtest",   # for "kenward-roger" mode when using emmeans
  "BayesFactor",# for Baysian analysis
  "DescTools",  # for GoodmanKruskalGamma correlations
  "stringi",    # for self-scoring evaluation
  "kableExtra"  # for formatted table outout with kable
) 

# List of relevant packages
new_packages <- list_of_packages[ # Check whether all packages are installed
  !(list_of_packages %in% installed.packages()[, "Package"])]
# Install them if that is not the case
if (length(new_packages)) install.packages(new_packages, repos = list(CRAN="http://cran.rstudio.com/")) 
# Load the relevant libraries
sapply(list_of_packages, require, character.only = TRUE)

##    Relevant functions for script                                           ----
# A function that loads all files in a given directory into the global environment
load_all_txt_files <- function(incoming_path, ...) {
  incoming_path <- rel_path_processed_data
  # Save all required file names in each directory
  filenames_in_directory <- list.files(paste0("./", incoming_path))
  # Only select the txt files
  selected_txt_filenames <- filenames_in_directory[grepl(".txt", filenames_in_directory)]
  # Load all files together into a list with data.table::fread
  txt_file_list <- sapply(paste0(incoming_path, selected_txt_filenames), fread, fill = TRUE)
  # Exclude the path from entry name and .txt from the file name
  names(txt_file_list) <- str_extract(names(txt_file_list), selected_txt_filenames)
  names(txt_file_list) <- gsub(".txt", "", names(txt_file_list))
  txt_file_list <- txt_file_list %>% type.convert(as.is = TRUE)
}

# A function that can compute and retrieve the gamma correlation coefficient
#   for relative monitoring accuracy from the long summary output of rcorr.cens
calculate_rel_MA <- function(incoming_participant_ID, incoming_data) {
  # Filter the data of the one participant
  incoming_data <- incoming_data %>% filter(Participant_Private_ID == unique(incoming_participant_ID))
  # Generate gamma statistic only
  gamma <- rcorr.cens(incoming_data$JoL, incoming_data$Test_Total_Correct, outx = TRUE)[2]
  gamma <- round(gamma, 4)
  gamma
}

# A function that can compute and retrieve the gamma correlation coefficient
#   for restudy decisions from the long summary output of rcorr.cens

calculate_rel_RA_MA <- function(incoming_participant_ID, incoming_data) {
  # Filter the data of the one participant
  incoming_data <- incoming_data %>% filter(Participant_Private_ID == unique(incoming_participant_ID))
  # Generate gamma statistic only
  gamma <- GoodmanKruskalGamma(incoming_data$JoL, incoming_data$RestudyDecision)
  gamma <- round(gamma, 4)
  gamma
}

# A function that can compute and retrieve the gamma correlation coefficient
#   for restudy decisions from the long summary output of rcorr.cens
calculate_rel_RA_TC <- function(incoming_participant_ID, incoming_data) {
  # Filter the data of the one participant
  incoming_data <- incoming_data %>% filter(Participant_Private_ID == unique(incoming_participant_ID))
  # Generate gamma statistic only
  gamma <- GoodmanKruskalGamma(incoming_data$Test_Total_Correct, incoming_data$RestudyDecision)
  gamma <- round(gamma, 4)
  gamma
}

# A function that calculates descriptives
calculate_descriptives <- function(incoming_data, outcome_of_interest) {
  desc_outcome <- incoming_data %>%
    dplyr::summarize(
      Mean = round(mean({{ outcome_of_interest }}, na.rm = TRUE), 2),
      SD = round(sd({{ outcome_of_interest }}, na.rm = TRUE), 2),
      Median = median({{ outcome_of_interest }}, na.rm = TRUE),
      Min = round(min({{ outcome_of_interest }}, na.rm = TRUE),2),
      Max = round(max({{ outcome_of_interest }}, na.rm = TRUE),2)
    )
  desc_outcome <- desc_outcome %>% distinct(Mean, SD, Median, Min, Max, .keep_all = TRUE)
  desc_outcome
}

# A function that computes and merges standardized effects of pairwise comparisons
# and generates a ready_to_print kable object
return_print_ready_emmeans <- function(incomming_emmeans, fitted_Model) {
  ## Retrieve the effect sizes of the pairwise comparisons with emmeans package
  suppressMessages( # Suppress the unhelpful message of eff_size function
    # Call eff_size functions with emmeans and model objects
    eff_size_frame <- eff_size(incomming_emmeans,
      sigma = sigma(fitted_Model),
      edf = df.residual(fitted_Model)
    )
  )
  eff_size_frame <- data.frame(eff_size_frame) # Save as data frame
  # Only keep the contrast specification and the effect sizes
  eff_sizes <- eff_size_frame %>% select(contrast, effect.size)

  ## Merge the standardized effects to the incomming_emmeans
  # Save the pairwise comparisons as data frame
  contrast_frame <- summary(incomming_emmeans, as.df = TRUE)
  # summary output comes as list, transform to data frame
  contrast_frame <- enframe(contrast_frame) %>%
    unnest(cols = c(value)) %>%
    rename("contrast" = `1`) %>% # Rename the first column and
    select(c(contrast, estimate, SE, df, t.ratio, p.value)) %>% # select relevant columns
    filter(!is.na(contrast)) %>%
    # Join the the created data frame with the standardized effects
    full_join(., eff_sizes, by = "contrast") %>%
    # Remove the brackets from the contrasts
    mutate(contrast = str_remove_all(contrast, "[()]"))
  # Create the kable output
  contrast_frame <- contrast_frame %>%
    kbl(., digits = c(0, 2, 2, 2, 2, 3, 2)) %>%
    kable_styling(full_width = FALSE) %>%
    row_spec(return_rows_4_bold_print(contrast_frame), bold = T)
  contrast_frame
}

## Retrieve p-values from a model summary
retrieve_p_value <- function(model_object, predictor_name) {
  model_summary <- summary(model_object)
  output <- as.data.frame(model_summary$coefficients)
  p_Value <- output$`Pr(>|t|)`[rownames(output) == predictor_name]
  p_Value
}

# A function that retrieves model rows with p values <= 0.05 to print in bold
return_rows_4_bold_print <- function(incoming_data_frame) {
  returning_Row_Vec <- which(incoming_data_frame$p.value <= 0.05)
  returning_Row_Vec
}

# A function that retrieves the p-value from an anova model comparision
# (and prints whether this was significant)
retrieve_anova_model_comparison_output <- function(anova_model_frame) {
  # The anova comparison for ML models prints Pr(>Chisq) in the summary,
  # the non-ML model summary Pr(>F). Adapt: If "Pr(>Chisq)" in incoming frame
  if ("Pr(>Chisq)" %in% names(anova_model_frame)) {
    return_Obj <- data.frame(
      "Interpretation" = ifelse(anova_model_frame$`Pr(>Chisq)`[2] <= 0.05,
        "explained", "did not explain"
      ),
      "Stat" = "_Chi<sup>2</sup>_",
      "Estimate" = round(anova_model_frame$Chisq[2], 2),
      "df" = paste0("(", anova_model_frame$Df[2], ")"),
      "sign" = ifelse(anova_model_frame$Chisq[2] <= 0.05, "<", "="),
      "p" = format(round(anova_model_frame$`Pr(>Chisq)`[2], 3), nsmall = 3)
    )
  } else {
    return_Obj <- data.frame(
      "Interpretation" = ifelse(anova_model_frame$`F`[2] <= 0.05,
        "explained", "did not explain"
      ),
      "Stat" = "_F_",
      "Estimate" = round(anova_model_frame$`F`[2], 2),
      "df" = paste0("(", anova_model_frame$Df[2], ")"),
      "sign" = ifelse(anova_model_frame$`F`[2] <= 0.05, "<", "="),
      "p" = format(round(anova_model_frame$`F`[2], 3), nsmall = 3)
    )
  }

  final_return <- c(
    return_Obj$Interpretation,
    paste0(return_Obj$Stat, return_Obj$df, " = ", return_Obj$Estimate, " _p_ ", return_Obj$sign, " ", return_Obj$p)
  )
}


# A function that can compute cue diagnosticities
#   (i.e., person correlations between Cue Value and Test Scores)
calculate_Pearson_Corrs <- function(Diagram_Cue, Test_Or_JOL) {
  outgoing_data <- cor(Diagram_Cue, Test_Or_JOL, use = "everything", method = "pearson")
  outgoing_data
}

##    Load all files from the specified directory                             ----
# (Note, this only works with multiple files that are not identical)
processed_data_list <- load_all_txt_files(rel_path_processed_data)
# Load all files from the list into the global environment/ current work space
list2env(processed_data_list, .GlobalEnv)
rm(processed_data_list) # remove the list as it's not needed anymore

## -------------------------------------------------------------------------- ----
## 1. Sample stats                                                            ----
## -------------------------------------------------------------------------- ----
##    Total sample overview before exclusion                                  ----
# Count the number of participants before exclusion
total_participants_before_excl <- reaction_time_data %>%
  ungroup() %>%
  distinct(Participant_Private_ID, .keep_all = TRUE) %>%
  count(Experiment, Condition) %>% # Participants
  rename(Participants = n)
# And the number of trials before exclusion
total_trials_before_excl <- reaction_time_data %>%
  distinct(Experiment,  Condition, Participant_Private_ID, Trial) %>%
  count(Experiment, Condition) %>% # Trials
  rename(Trials = n)

## Save the original data file with all observations
row_per_trial_data_all <- row_per_trial_data
##    Drop-out & data exclusion                                               ----
# Check how many participants took over 40min to complete the experimental tasks
exclusion_by_overtime <- row_per_trial_data_all %>%
  ungroup() %>%
  distinct(Participant_Private_ID, .keep_all = TRUE) %>%
  group_by(Experiment, Condition) %>%
  count(Time_On_Task >= 40)

## Exclude those participants from the data
row_per_trial_data <- row_per_trial_data_all %>% ungroup() %>% filter(!Time_On_Task >= 40)

##    Establish cut-off for min. reading times and exclude participants       ----
## (See Participant section https://www.tandfonline.com/doi/pdf/10.1080/20445911.2021.1890092)
# Minimal number words of reading materials: 158
# 0.17s per words (based on 60s/350 words; Trauzettel-Klosinski & Dietz, 2012)
## Write a function that can take a trial name and then computes the minimal required milliseconds per text
establish_minimum_reading_time <- function(incoming_trial) {
  Trial <- as.vector(unique(incoming_trial))
  # Depending on the incoming trial name, check how many milliseconds were min. required based on text length
  required_milliseconds <- switch(Trial, 
    "Geld" = 182 * (0.17 * 1000), # Geld text has 182 words, multiply the factor and make it milliseconds
    "Suez" = 163 * (0.17 * 1000),
    "Beton" = 170 * (0.17 * 1000),
    "Metro" = 162 * (0.17 * 1000),
    "Botox" = 189 * (0.17 * 1000),
    "Muziek" = 170 * (0.17 * 1000)
  )
  required_milliseconds
}
# Apply the function to the data
reaction_time_data <- reaction_time_data %>%
  group_by(Trial) %>%
  # Create a new variable by proving the trial names to the function establish_minimum_reading_time()
  mutate(minimum_reading_time = establish_minimum_reading_time(Trial))

# Check which reading times were shorter than the minimally required reading time
trials_to_kick_reading_times <- reaction_time_data %>%
  filter(Screen_Name == "Text" & minimum_reading_time > Reaction_Time)

# Check how many trials were affected by too short reading times
trials_to_kick_reading_times %>%
  group_by(Experiment, Condition) %>%
  count()

# Save the ID, trial and phase of each participant that read a text too quickly
ids_trials_to_kick_reading_times <- trials_to_kick_reading_times %>%
  distinct(Experiment, Condition, Phase, Participant_Private_ID, Trial)

## Remove those trials from the data
row_per_trial_data_red <- anti_join(row_per_trial_data, ids_trials_to_kick_reading_times)

# Merge included reaction times to data set
reading_times <- reaction_time_data %>% 
  filter(Screen_Name == "Text" & minimum_reading_time < Reaction_Time) %>%
  select(Experiment, Condition:Trial, Participant_Private_ID, Reaction_Time) %>%
  rename(Reading_Time_ms = Reaction_Time) %>%
  mutate(Reading_Time_sec = round(Reading_Time_ms/1000,3))

## Descripitves for reading times 
ids_trials_to_kick_reading_times %>% group_by(Experiment, Condition, Phase) %>% count()
# Save for output
desc_trials_to_kick_condition <- ids_trials_to_kick_reading_times %>% group_by(Experiment, Condition) %>% count()
desc_trials_to_kick_exp <- ids_trials_to_kick_reading_times %>% group_by(Experiment, Condition) %>% count()

## Participants to kick because of minimal reading in Phase two twice below cut-off
# Check if more trials per Phase:
participants_to_kick_reading_times <- trials_to_kick_reading_times %>% 
  group_by(Experiment, Condition, Participant_Private_ID, Phase) %>% count() %>% 
  filter(n >= 2) %>% ungroup() %>%
  distinct(Experiment, Participant_Private_ID, .keep_all = TRUE)
ids_participants_to_kick_min_reading <- participants_to_kick_reading_times %>% select(Experiment, Condition, Participant_Private_ID)
ids_participants_to_kick_min_reading %>% group_by(Experiment, Condition) %>% count()
leftover_trials_to_kick <- anti_join(ids_trials_to_kick_reading_times, ids_participants_to_kick_min_reading)

## Remove participants that have two or more texts they read under minimal reading time
row_per_trial_data_red <- anti_join(row_per_trial_data_red, ids_participants_to_kick_min_reading)
# Retrieve the IDs and trials to keep
ids_and_trials_to_keep <- row_per_trial_data_red %>% 
  distinct(Participant_Private_ID, Trial) %>% ungroup()

## Exclude the excluded reading times
reading_times <- left_join(ids_and_trials_to_keep, reading_times)

## Calculate the descriptive statistics for the reading times per...
# ...Experiment, Condition, and Phase
desc_reading_times_sec_condition_phase <- reading_times %>% 
  group_by(Experiment, Condition, Phase) %>%
  summarise(Mean = round(mean(Reading_Time_sec, na.rm = TRUE),2),
            SD = round(sd(Reading_Time_sec, na.rm = TRUE),2))
# ...Experiment and Condition
desc_reading_times_sec_condition <- reading_times %>% 
  group_by(Experiment, Condition) %>%
  summarise(Mean = round(mean(Reading_Time_sec, na.rm = TRUE),2),
            SD = round(sd(Reading_Time_sec, na.rm = TRUE),2))

## Create an overview of the number of included participants
total_participants_final <- row_per_trial_data_red %>%
  ungroup() %>%
  distinct(Participant_Private_ID, .keep_all = TRUE) %>%
  count(Experiment, Condition) %>% # Participants
  rename(Participants = n)
## And of the number of included trials
total_trials_final <- row_per_trial_data_red %>%
  distinct(Experiment, Condition, Participant_Private_ID, Trial) %>%
  count(Experiment, Condition) %>% # Trials
  rename(Trials = n)

##    Exclude those participants from all data sets                           ----
age_gender_to_report <- left_join(ids_and_trials_to_keep, age_gender_exp_1_and_2) %>% 
  type.convert(as.is=TRUE) 
coded_test_data_red <- left_join(ids_and_trials_to_keep, coded_test_data)
diagram_data_red <- left_join(ids_and_trials_to_keep, diagram_data_both_exp)
reaction_time_data_red <- left_join(ids_and_trials_to_keep, reaction_time_data)

##    Descriptives Final Sample: Age and Gender                               ----
age_gender_frame <- age_gender_to_report %>% 
  type.convert(as.is=TRUE) %>% 
  group_by(Experiment) %>%  
  summarise(Mean = round(mean(Age, na.rm = TRUE),2), 
            SD = round(sd(Age, na.rm = TRUE),2))
female_ratio_to_merge <- age_gender_to_report %>%
  mutate(Gender = case_match(Gender,
                             c("Dat zeg ik liever niet.", "Andere. Dat is voor mij...") ~ "Not specified",
                             "Man/jonge" ~ "Male",
                             "Vrouw/meisje" ~ "Female")) %>%
  distinct(Experiment, Participant_Private_ID, .keep_all = TRUE) %>%
  group_by(Experiment, Gender) %>%
  count() %>%
  group_by(Experiment) %>%
  mutate('%' = round((n/sum(n))*100,2))

##    Descriptives Final Sample: Time on Task                                 ----
desc_trial_completion_times <- row_per_trial_data_red %>%
  distinct(Experiment, Participant_Private_ID, Time_On_Task, Condition, Phase, Time_On_Trial)
## Compute the descriptive statistics of the completion times for each experiment, ...
desc_time_on_task_exp <- desc_trial_completion_times %>% 
  group_by(Experiment) %>% 
  summarise(Mean = round(mean(Time_On_Task, na.rm = TRUE),2),
            SD = round(sd(Time_On_Task, na.rm = TRUE),2),
            Min = round(min(Time_On_Task, na.rm = TRUE),2),
            Max = round(max(Time_On_Task, na.rm = TRUE),2)) %>%
  mutate(Condition = paste0('Total Exp.', Experiment)) %>%
  relocate(Condition, .after = Experiment)
# ...and for each Condition in Experiment
desc_time_on_task_exp_condition <- desc_trial_completion_times %>% 
  group_by(Experiment, Condition) %>%
  summarise(Mean = round(mean(Time_On_Task, na.rm = TRUE),2),
            SD = round(sd(Time_On_Task, na.rm = TRUE),2),
            Min = round(min(Time_On_Task, na.rm = TRUE),2),
            Max = round(max(Time_On_Task, na.rm = TRUE),2))

## -------------------------------------------------------------------------- ----
## 2. Compute Monitoring Accuracy                                             ----
## -------------------------------------------------------------------------- ----
##    Monitoring Accuracy Bias: JoL2 - Test Score of each trial (row)         ----
row_per_trial_data_red <- row_per_trial_data_red %>%
  group_by(Participant_Private_ID) %>%
  mutate(Mon_Acc_Bias = JoL - Test_Total_Correct)
# Then Absolute Monitoring Accuracy as the unsigned difference JoL2-Test
row_per_trial_data_red <- row_per_trial_data_red %>%
  group_by(Participant_Private_ID) %>%
  mutate(Abs_Mon_Acc = abs(JoL - Test_Total_Correct))

##    Relative Monitoring Accuracy                                            ----
# Calculate gamma correlations between JOL2 and Test across the six trials

# Apply function to compute gamma correlations to the trials of each participant
row_per_trial_data_red <- row_per_trial_data_red %>%
  group_by(Participant_Private_ID) %>% # For every participant,
  mutate(
    Rel_Mon_Acc = # call the gamma calculating function from above,
      calculate_rel_MA(Participant_Private_ID, .)
  ) %>% # providing participant ID and participants' data
  relocate(Rel_Mon_Acc, .after = Abs_Mon_Acc)

##    Compute Means per Participants and Phase for descripitives              ----
row_per_phase_data <- row_per_trial_data_red %>%
  # For each Participant and Phase, ..
  group_by(Participant_Private_ID, Phase) %>% 
  # ..compute the mean absolute monitoring accuracy
  mutate(Mean_Abs_Mon_Acc = mean(Abs_Mon_Acc, na.rm = TRUE)) %>% 
  # Again, for each Participant and Phase, ..
  group_by(Participant_Private_ID, Phase) %>% 
  # ..compute the mean text comprehenion
  mutate(Mean_Test_Scores = mean(Test_Total_Correct, na.rm = TRUE)) %>%
  group_by(Participant_Private_ID, Phase) %>% 
  # ...and the mean JOLs
  mutate(Mean_JOLs = mean(JoL, na.rm = TRUE)) %>%
  select(Experiment:Phase, Mean_Abs_Mon_Acc, Mean_Test_Scores, Mean_JOLs) %>% 
  distinct()

## Do this again on participant level to reformat the data to one row per participant
row_per_participant_data <- row_per_trial_data_red %>%
  group_by(Participant_Private_ID) %>% 
  mutate(Mean_Abs_Mon_Acc = mean(Abs_Mon_Acc, na.rm = TRUE)) %>% 
  group_by(Participant_Private_ID) %>% 
  mutate(Mean_Test_Scores = mean(Test_Total_Correct, na.rm = TRUE)) %>%
  group_by(Participant_Private_ID) %>% 
  mutate(Mean_JOLs = mean(JoL, na.rm = TRUE)) %>%
  select(Experiment, Condition, Participant_Private_ID, Mean_Abs_Mon_Acc, Rel_Mon_Acc, Mean_Test_Scores, Mean_JOLs) %>% 
  distinct()
## -------------------------------------------------------------------------- ----
## 3. Descriptive statistics all relevant outcomes (Experiment 1 and 2)       ----
## -------------------------------------------------------------------------- ----
## Reorder Condition levels for descriptives
row_per_trial_for_desc <- row_per_trial_data_red %>%
  mutate(Condition = factor(Condition)) %>%
  mutate(Condition = fct_relevel(Condition, "SSI", "No-SSI","Diagramming-Only"))
row_per_phase_for_desc <- row_per_phase_data %>%
  mutate(Condition = factor(Condition)) %>%
  mutate(Condition = fct_relevel(Condition, "SSI", "No-SSI","Diagramming-Only"))
row_per_participant_for_desc <- row_per_participant_data %>%
  mutate(Condition = factor(Condition)) %>%
  mutate(Condition = fct_relevel(Condition, "SSI", "No-SSI","Diagramming-Only"))
##    Monitoring Accuracy                                                     ----
## Per Experiment
desc_abs_MA_experiment <- row_per_participant_for_desc %>%
  group_by(Experiment) %>%
  reframe(calculate_descriptives(., Mean_Abs_Mon_Acc)) %>%
  distinct() 
##    Per condition
desc_abs_MA_condition <- row_per_participant_for_desc %>%
  group_by(Experiment, Condition) %>%
  reframe(calculate_descriptives(., Mean_Abs_Mon_Acc)) %>%
  distinct() 
##    Per Phase
desc_abs_MA_phase <- row_per_phase_for_desc %>%
  group_by(Experiment, Phase) %>%
  reframe(calculate_descriptives(., Mean_Abs_Mon_Acc)) %>%
  distinct()
##    Per Phase and Condition
desc_abs_MA_condition_phase <- row_per_phase_for_desc %>%
  group_by(Experiment, Condition, Phase) %>%
  reframe(calculate_descriptives(., Mean_Abs_Mon_Acc)) %>%
  distinct()
## Merge all frames computed in the lines above
desc_MA_all <- full_join(desc_abs_MA_experiment, desc_abs_MA_condition, 
                         by = join_by(Experiment, Mean, SD, Median, Min, Max)) %>%
  full_join(.,desc_abs_MA_phase, 
            by = join_by(Experiment, Mean, SD, Median, Min, Max)) %>%
  full_join(.,desc_abs_MA_condition_phase, 
            by = join_by(Experiment, Mean, SD, Median, Min, Max, Condition, Phase)) %>%
  arrange(Experiment, Phase, Condition) %>%
  relocate(Condition, .after = Experiment) %>%
  relocate(Phase, .after = Condition)

##    Create interaction plots of these descriptives
ma_condition_plot_exp1 <- desc_abs_MA_condition_phase %>%
  filter(Experiment == 1) %>%
  select(Experiment:SD) %>%
  ggplot(aes(
    x = Phase,
    y = Mean,
    group = Condition,
    color = Condition,
    ymin = 0,
    ymax = 2.5
  )) +
  geom_errorbar(
    aes(
      ymin = Mean - SD,
      ymax = Mean + SD
    ),
    width = .1
  ) +
  geom_line(linewidth = 1, aes(linetype = Condition)) + # Add different line types
  # Choose line types manually
  scale_linetype_manual(values = c("solid", "dashed")) +
  # Show them in legend respectively (and without the stripe)
  guides(color = guide_legend(override.aes = list(linetype = c(1, 3)))) +
  scale_color_brewer(palette = "Paired") +
  ylab("Monitoring Accuracy Exp. 1") +
  xlab("Phase") +
  theme_bw() + # Remove grey background
  theme(axis.text.x = element_blank(),
    legend.key.width = unit(2, "cm"), # Change length of lines in legend
    #axis.title.x = element_blank(), # Remove x-axis label
    text = element_text(family = "Times New Roman", size = 18)
  ) # Times New Roman, 12pt

# Also fit one with different labels and lines for the manuscript
ma_condition_plot_exp1_lab <- desc_abs_MA_condition_phase %>%
  filter(Experiment == 1) %>%
  select(Experiment:SD) %>%
  ggplot(aes(
    x = Phase,
    y = Mean,
    group = Condition,
    color = Condition,
    ymin = 0,
    ymax = 2.2
  )) +
  geom_label_repel(aes(label = paste0(Mean," (",SD,")")), nudge_x = 0.35, size = 8) +
  geom_errorbar(
    aes(
      ymin = Mean - SD,
      ymax = Mean + SD
    ),
    width = .1
  ) +
  geom_line(linewidth = 1, aes(linetype = Condition)) + # Add different line types
  # Choose line types manually
  scale_linetype_manual(values = c("solid", "dashed")) +
  # Show them in legend respectively (and without the stripe)
  guides(color = guide_legend(override.aes = list(label = "", linetype = c(1, 2)))) +
  scale_color_brewer(palette = "Paired") +
  ylab("Monitoring Accuracy Exp. 1") +
  xlab("Phase") +
  theme_bw() + # Remove grey background
  theme(axis.text.x = element_blank(),
        legend.key.width = unit(2, "cm"), # Change length of lines in legend
        #axis.title.x = element_blank(), # Remove x-axis label
        text = element_text(family = "Times New Roman", size = 24)
  )  # Times New Roman, 12pt

## Do this again for experiment 2
ma_condition_plot_exp2 <- desc_abs_MA_condition_phase %>%
  filter(Experiment == 2) %>%
  select(Experiment:SD) %>%
  ggplot(aes(
    x = Phase,
    y = Mean,
    group = Condition,
    color = Condition,
    ymin = 0,
    ymax = 2.2
  )) +
  geom_label_repel(aes(label = paste0(Mean," (",SD,")")), nudge_x = 0.35, size = 4) +
  geom_errorbar(
    aes(
      ymin = Mean - SD,
      ymax = Mean + SD
    ),
    width = .1
  ) +
  geom_line(linewidth = 1, aes(linetype = Condition)) + # Add different line types
  # Choose line types manually
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) + 
  # Show them in legend respectively (and without the stripe)
  guides(color = guide_legend(override.aes = list(linetype = c(1,2,3)))) +
  scale_color_brewer(palette = "Paired") +
  ylab("Monitoring Accuracy Exp.2") +
  xlab("Phase") +
  theme_bw() + # Remove grey background
  theme(
    legend.key.width = unit(2, "cm"), # Change length of lines in legend
    axis.title.x = element_blank(), # Remove x-axis label
    text = element_text(family = "Times New Roman", size = 18)
  ) # Times New Roman, 12pt

# And the alternative plot again
ma_condition_plot_exp2_lab <- desc_abs_MA_condition_phase %>%
  filter(Experiment == 2) %>%
  select(Experiment:SD) %>%
  ggplot(aes(
    x = Phase,
    y = Mean,
    group = Condition,
    color = Condition,
    ymin = 0,
    ymax = 2.2
  )) +
  geom_label_repel(aes(label = paste0(Mean," (",SD,")")), nudge_x = 0.35, size = 8) +
  geom_errorbar(
    aes(
      ymin = Mean - SD,
      ymax = Mean + SD
    ),
    width = .1
  ) +
  geom_line(linewidth = 1, aes(linetype = Condition)) + # Add different line types
  # Choose line types manually
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) + # , "dotdash")) +
  # Show them in legend respectively (and without the stripe)
  guides(color = guide_legend(override.aes = list(label = "", 
                                                  linetype = c(1,2,3)))) +
  scale_color_brewer(palette = "Paired") +
  ylab("Monitoring Accuracy Exp.2") +
  xlab("Phase") +
  theme_bw() + # Remove grey background
  theme(axis.text.x = element_blank(),
    legend.key.width = unit(2, "cm"), # Change length of lines in legend
    #axis.title.x = element_blank(), # Remove x-axis label
    text = element_text(family = "Times New Roman", size = 24)
  ) # Times New Roman, 12pt


##    Text Comprehension                                                      ----
##    Per Experiment
desc_TC_experiment <- row_per_participant_for_desc %>%
  group_by(Experiment) %>%
  reframe(calculate_descriptives(., Mean_Test_Scores)) %>%
  distinct()
##    Per condition
desc_TC_condition <- row_per_participant_for_desc %>%
  group_by(Experiment, Condition) %>%
  reframe(calculate_descriptives(., Mean_Test_Scores)) %>%
  distinct()
##    Per Phase
# For diagramming as factor
desc_TC_phase <- row_per_phase_for_desc %>%
  group_by(Experiment, Phase) %>%
  reframe(calculate_descriptives(., Mean_Test_Scores)) %>%
  distinct()
##    Per Phase and Condition
# For diagramming as factor
desc_TC_condition_phase <- row_per_phase_for_desc %>%
  group_by(Experiment, Condition, Phase) %>%
  reframe(calculate_descriptives(., Mean_Test_Scores)) %>%
  distinct()

## Merge all those frames again
desc_TC_all <- full_join(desc_TC_experiment, desc_TC_condition) %>%
  full_join(.,desc_TC_phase) %>%
  full_join(.,desc_TC_condition_phase) %>%
  arrange(Experiment, Phase, Condition) %>%
  relocate(Condition, .after = Experiment) %>%
  relocate(Phase, .after = Condition)

##    Create interaction plots of these descriptives
tc_condition_plot_exp1 <- desc_TC_condition_phase %>%
  filter(Experiment == 1) %>%
  select(Experiment:SD) %>%
  ggplot(aes(
    x = Phase,
    y = Mean,
    group = Condition,
    color = Condition,
    ymin = 0,
    ymax = 2.5
  )) +
  geom_errorbar(
    aes(
      ymin = Mean - SD,
      ymax = Mean + SD
    ),
    width = .1
  ) +
  geom_line(linewidth = 1, aes(linetype = Condition)) + # Add different line types
  # Choose line types manually
  scale_linetype_manual(values = c("solid", "dashed")) +
  # Show them in legend respectively (and without the stripe)
  guides(color = guide_legend(override.aes = list(linetype = c(1, 2)))) +
  scale_color_brewer(palette = "Paired") +
  ylab("Test Scores Exp.1") +
  xlab("Phase") +
  theme_bw() + # Remove grey background
  theme(
    legend.key.width = unit(2, "cm"), # Change length of lines in legend
    axis.title.x = element_blank(), # Remove x-axis label
    text = element_text(family = "Times New Roman", size = 18)
  ) # Times New Roman, 12pt

## Again the same plat with adapted layout 
tc_condition_plot_exp1_lab <- desc_TC_condition_phase %>%
  filter(Experiment == 1) %>%
  select(Experiment:SD) %>%
  ggplot(aes(
    x = Phase,
    y = Mean,
    group = Condition,
    color = Condition,
    ymin = 0,
    ymax = 4
  )) +
  geom_label_repel(aes(label = paste0(Mean," (",SD,")")), nudge_x = 0.35, size = 8) +
  geom_errorbar(
    aes(
      ymin = Mean - SD,
      ymax = Mean + SD
    ),
    width = .1
  ) +
  geom_line(linewidth = 1, aes(linetype = Condition)) + # Add different line types
  # Choose line types manually
  scale_linetype_manual(values = c("solid", "dashed")) +
  # Show them in legend respectively (and without the stripe)
  guides(color = guide_legend(override.aes = list(label = "", linetype = c(1, 2)))) +
  scale_color_brewer(palette = "Paired") +
  ylab("Test Scores Exp.1") +
  xlab("Phase") +
  theme_bw() + # Remove grey background
  theme(axis.text.x = element_blank(),
    legend.key.width = unit(2, "cm"), # Change length of lines in legend
   # axis.title.x = element_blank(), # Remove x-axis label
    text = element_text(family = "Times New Roman", size = 24)
  ) # Times New Roman, 12pt


##    Create interaction plots for experiment 2
tc_condition_plot_exp2 <- desc_TC_condition_phase %>%
  filter(Experiment == 2) %>%
  select(Experiment:SD) %>%
  ggplot(aes(
    x = Phase,
    y = Mean,
    group = Condition,
    color = Condition,
    ymin = 0,
    ymax = 4
  )) +
  geom_errorbar(
    aes(
      ymin = Mean - SD,
      ymax = Mean + SD
    ),
    width = .1
  ) +
  geom_line(linewidth = 1, aes(linetype = Condition)) + # Add different line types
  # Choose line types manually
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) + # , "dotdash")) +
  # Show them in legend respectively (and without the stripe)
  guides(color = guide_legend(override.aes = list(linetype = c(1,2,3)))) + # , 4)))) +
  scale_color_brewer(palette = "Paired") +
  ylab("Test Scores Exp.2") +
  xlab("Phase") +
  theme_bw() + # Remove grey background
  theme(
    legend.key.width = unit(2, "cm"), # Change length of lines in legend
    axis.title.x = element_blank(), # Remove x-axis label
    text = element_text(family = "Times New Roman", size = 18)
  ) # Times New Roman, 12pt

## Again the same plat with adapted layout 
tc_condition_plot_exp2_lab <- desc_TC_condition_phase %>%
  filter(Experiment == 2) %>%
  select(Experiment:SD) %>%
  ggplot(aes(
    x = Phase,
    y = Mean,
    group = Condition,
    color = Condition,
    ymin = 0,
    ymax = 4
  )) +
  geom_label_repel(aes(label = paste0(Mean," (",SD,")")), nudge_x = 0.35, size = 8) +
  geom_errorbar(
    aes(
      ymin = Mean - SD,
      ymax = Mean + SD
    ),
    width = .1
  ) +
  geom_line(linewidth = 1, aes(linetype = Condition)) + # Add different line types
  # Choose line types manually
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) + # , "dotdash")) +
  # Show them in legend respectively (and without the stripe)
  guides(color = guide_legend(override.aes = list(label = "", linetype = c(1,2,3)))) + # , 4)))) +
  scale_color_brewer(palette = "Paired") +
  ylab("Test Scores Exp.2") +
  xlab("Phase") +
  theme_bw() + # Remove grey background
  theme(axis.text.x = element_blank(),
    legend.key.width = unit(2, "cm"), # Change length of lines in legend
    #axis.title.x = element_blank(), # Remove x-axis label
    text = element_text(family = "Times New Roman", size = 24)
  ) # Times New Roman, 12pt

##    JOL                                                                     ----
## Per Experiment
desc_JOL_experiment <- row_per_participant_for_desc %>%
  group_by(Experiment) %>%
  reframe(calculate_descriptives(., Mean_JOLs)) %>%
  distinct()
## Per Experiment and Condition
desc_JOL_condition <- row_per_participant_for_desc %>%
  group_by(Experiment, Condition) %>%
  reframe(calculate_descriptives(., Mean_JOLs)) %>%
  distinct()

##    Per Phase and Condition
desc_JOL_condition_phase <- row_per_phase_for_desc %>%
  group_by(Experiment, Condition, Phase) %>%
  reframe(calculate_descriptives(., Mean_JOLs)) %>%
  distinct()

##    Per Experiment and Phase
desc_JOL_phase <- row_per_phase_for_desc %>%
  group_by(Experiment, Phase) %>%
  reframe(calculate_descriptives(., Mean_JOLs)) %>%
  distinct()

## All together
desc_JOL_all <- full_join(desc_JOL_experiment, desc_JOL_condition) %>%
  full_join(.,desc_JOL_phase) %>%
  full_join(.,desc_JOL_condition_phase) %>%
  arrange(Experiment, Phase, Condition) %>%
  relocate(Condition, .after = Experiment) %>%
  relocate(Phase, .after = Condition)

## Merge all those frames
jol_condition_plot_exp1_lab <- desc_JOL_condition_phase %>%
  filter(Experiment == 1) %>%
  select(Experiment:SD) %>%
  ggplot(aes(
    x = Phase,
    y = Mean,
    group = Condition,
    color = Condition,
    ymin = 0,
    ymax = 4
  )) +
  geom_label_repel(aes(label = paste0(Mean," (",SD,")")), nudge_x = 0.35, size = 8) +
  geom_errorbar(
    aes(
      ymin = Mean - SD,
      ymax = Mean + SD
    ),
    width = .1
  ) +
  geom_line(linewidth = 1, aes(linetype = Condition)) + # Add different line types
  # Choose line types manually
  scale_linetype_manual(values = c("solid", "dashed")) +
  # Show them in legend respectively (and without the stripe)
  guides(color = guide_legend(override.aes = list(label = "", linetype = c(1, 2)))) +
  scale_color_brewer(palette = "Paired") +
  ylab("JOLs Exp.1") +
  xlab("Phase") +
  theme_bw() + # Remove grey background
  theme(axis.text.x = element_blank(),
        legend.key.width = unit(2, "cm"), # Change length of lines in legend
        # axis.title.x = element_blank(), # Remove x-axis label
        text = element_text(family = "Times New Roman", size = 24)
  ) # Times New Roman, 12pt

## Adapt the lines and lables again
jol_condition_plot_exp2_lab <- desc_JOL_condition_phase %>%
  filter(Experiment == 2) %>%
  select(Experiment:SD) %>%
  ggplot(aes(
    x = Phase,
    y = Mean,
    group = Condition,
    color = Condition,
    ymin = 0,
    ymax = 4
  )) +
  geom_label_repel(aes(label = paste0(Mean," (",SD,")")), nudge_x = 0.35, size = 8) +
  geom_errorbar(
    aes(
      ymin = Mean - SD,
      ymax = Mean + SD
    ),
    width = .1
  ) +
  geom_line(linewidth = 1, aes(linetype = Condition)) + # Add different line types
  # Choose line types manually
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) + # , "dotdash")) +
  # Show them in legend respectively (and without the stripe)
  guides(color = guide_legend(override.aes = list(label = "", linetype = c(1,2,3)))) + # , 4)))) +
  scale_color_brewer(palette = "Paired") +
  ylab("JOLs Exp.2") +
  xlab("Phase") +
  theme_bw() + # Remove grey background
  theme(axis.text.x = element_blank(),
        legend.key.width = unit(2, "cm"), # Change length of lines in legend
        #axis.title.x = element_blank(), # Remove x-axis label
        text = element_text(family = "Times New Roman", size = 24)
  ) # Times New Roman, 12pt

##    All together                                                            ----
## Per Condition
# Remove the median stats
desc_MA_all  <- desc_MA_all %>% select(-Median)
desc_TC_all  <- desc_TC_all %>% select(-Median)
desc_JOL_all <- desc_JOL_all %>% select(-Median)

# Merge all single descriptive outputs per variable to one output frame
desc_all_outcomes <- full_join(desc_MA_all, desc_TC_all, by = join_by(Experiment, Condition, Phase)) %>%
  full_join(., desc_JOL_all,  by = join_by(Experiment, Condition, Phase)) %>%
  relocate(Phase, .before = Condition) %>%
  # Remove the .x and .y from the column names
  kbl(col.names = gsub("\\.x|\\.y", "", names(.)), 
      # Add a title to the table
      caption = "Descriptive Statistics of Monitoring Accuracy, Text Comprehension and JOLs for Exp. 1 & 2",
      # Edit the alignment of each column: c = center, l = left (r = right)
      align = "cclcccccccccccc") %>%
  # Change the layout of the table to be striped
  kable_paper("striped", full_width = TRUE) %>%
  # Add column borders to distinguish the variables better:
  column_spec(3, border_right=T) %>%  # On the right side of column 3
  column_spec(7, border_right=T) %>%  # On the right side of column 7
  column_spec(11, border_right=T) %>% # On the right side of column 11
  # Add sub-headings for each variable (first 3 columns without heading, Abs. Monitoring Acc. across 4 columns, etc.)
  add_header_above(c(" " = 3, "Abs. Monitoring Acc." = 4, "Text Comprehension" = 4, "JOLs" = 4)) %>% 
  # If the table is very long, add the option to scroll within the table
  #scroll_box(width = "100%", height = "300px") %>%
  # Let the table span through the whole page
  kable_styling(full_width = TRUE)

##    Bias Frequency Plot per condition and Experiment                        ----
## Prepare all data to plot
## Filter Data of Experiment 1 
# SSI Condition
bias_data_exp_1_ssi <- row_per_trial_for_desc %>%
  filter(Experiment == 1 & Condition == "SSI")
# No-SSI Condition
bias_data_exp_1_no_ssi <- row_per_trial_for_desc %>%
  filter(Experiment == 1 & Condition == "No-SSI")
## Filter Data of Experiment 2
bias_data_exp_2_ssi <- row_per_trial_for_desc %>%
  filter(Experiment == 2 & Condition == "SSI")
bias_data_exp_2_no_ssi <- row_per_trial_for_desc %>%
  filter(Experiment == 2 & Condition == "No-SSI")
bias_data_exp_2_diagramming_only <- row_per_trial_for_desc %>%
  filter(Experiment == 2 & Condition == "Diagramming-Only")

##    Plots for Experiment 1                                                
hist_bias_data_exp_1_ssi <- hist(bias_data_exp_1_ssi$Mon_Acc_Bias, breaks=8 ,xlim=c(-4,4) , xlab="Bias Scores Experiment 1 - SSI" , ylab="Frequency of Score" , main="SSI Condition")
hist_bias_data_exp_1_no_ssi <- hist(bias_data_exp_1_no_ssi$Mon_Acc_Bias, breaks=8 , xlim=c(-4,4) , xlab="Bias Scores Experiment 1 - No-SSI" , ylab="Frequency of Score" , main="No-SSI Condition")

##   Plots for Experiment 2                                                         
hist_bias_data_exp_2_ssi <- hist(bias_data_exp_2_ssi$Mon_Acc_Bias, breaks=8 , xlim=c(-4,4) , xlab="Bias Scores Experiment 2 - SSI" , ylab="Frequency of Score" , main="SSI Condition")
hist_bias_data_exp_2_no_ssi <- hist(bias_data_exp_2_no_ssi$Mon_Acc_Bias, breaks=8 , xlim=c(-4,4) , xlab="Bias Scores Experiment 2 - No-SSI" , ylab="Frequency of Score" , main="No-SSI Condition")
hist_bias_data_exp_2_diagramming_only <- hist(bias_data_exp_2_diagramming_only$Mon_Acc_Bias, breaks=8 , xlim=c(-4,4) , xlab="Bias Scores Experiment 2 - Diagramming-Only" , ylab="Frequency of Score" , main="Diagramming-Only Condition")


## -------------------------------------------------------------------------- ----
## 4. ML Analyses                                                             ----
## -------------------------------------------------------------------------- ----
## Reorder factor so that reference level is diagramming only - No-SSI - SSI
row_per_trial_data_red <- row_per_trial_data_red %>%
  mutate(Condition = factor(Condition)) %>%
  mutate(Condition = fct_relevel(Condition, "Diagramming-Only", "No-SSI", "SSI"))
##    MA                                                                      ----
##      For Experiment 1                                                      ----
# Filter out data of experiment 1
row_per_trial_data_red_exp1 <- row_per_trial_data_red %>% filter(Experiment == 1)

## Fit the model with all possible effects of experiment, condition and phase
abs_Mon_Acc_Mod_full_exp1 <- lmer(Abs_Mon_Acc ~ Condition * Phase +
  (1 | Class_ID / Participant_Private_ID), data = row_per_trial_data_red_exp1)
abs_Mon_Acc_full_exp1_tab <- tab_model(abs_Mon_Acc_Mod_full_exp1, show.se = TRUE, show.std = TRUE)

## Fit for main effects only
abs_Mon_Acc_Mod_main_exp1 <- lmer(Abs_Mon_Acc ~ Condition + Phase +
  (1 | Class_ID / Participant_Private_ID), data = row_per_trial_data_red_exp1)
abs_Mon_Acc_main_exp1_tab <- tab_model(abs_Mon_Acc_Mod_main_exp1, show.se = TRUE, show.std = TRUE)

all_MA_exp1_tab_models <- tab_model(abs_Mon_Acc_Mod_full_exp1,
  abs_Mon_Acc_Mod_main_exp1,
  show.se = TRUE, show.std = TRUE
)

## Retrieve the interpretation sentence for the model comparisons
ma_exp_1_model_comparison <- anova(abs_Mon_Acc_Mod_full_exp1, abs_Mon_Acc_Mod_main_exp1)
ma_exp_1_model_comparison_interpretation <- retrieve_anova_model_comparison_output(ma_exp_1_model_comparison)

##      For Experiment 2                                                      ----
# Filter out data of experiment 1
row_per_trial_data_red_exp2 <- row_per_trial_data_red %>% filter(Experiment == 2)

## Fit the model with all possible effects of experiment, condition and phase
abs_Mon_Acc_Mod_full_exp2 <- lmer(Abs_Mon_Acc ~ Condition * Phase +
  (1 | Participant_Private_ID), data = row_per_trial_data_red_exp2) # with class resulted in singular fit
abs_Mon_Acc_full_exp2_tab <- tab_model(abs_Mon_Acc_Mod_full_exp2, show.se = TRUE, show.std = TRUE)

## Fit for main effects only
abs_Mon_Acc_Mod_main_exp2 <- lmer(Abs_Mon_Acc ~ Condition + Phase +
  (1 | Participant_Private_ID), data = row_per_trial_data_red_exp2)
abs_Mon_Acc_main_exp2_tab <- tab_model(abs_Mon_Acc_Mod_main_exp2, show.se = TRUE, show.std = TRUE)

## Condition
abs_Mon_Acc_Mod_condition_exp2 <- lmer(Abs_Mon_Acc ~ Condition +
  (1 | Participant_Private_ID), data = row_per_trial_data_red_exp2)
abs_Mon_Acc_condition_exp2_tab <- tab_model(abs_Mon_Acc_Mod_condition_exp2, show.se = TRUE, show.std = TRUE)

all_MA_exp2_tab_models <- tab_model(abs_Mon_Acc_Mod_full_exp2,
  abs_Mon_Acc_Mod_main_exp2,
  show.se = TRUE, show.std = TRUE
)

## Retrieve the interpretation sentence for the model comparisons
ma_exp_2_model_comparison <- anova(abs_Mon_Acc_Mod_full_exp2, abs_Mon_Acc_Mod_main_exp2)
ma_exp_2_model_comparison_interpretation <- retrieve_anova_model_comparison_output(ma_exp_2_model_comparison)

##      Compare experiments                                                   ----
## Kick out the diagramming-only condition of experiment 2
row_per_trial_data_red_ssi_only <- row_per_trial_data_red %>% filter(Condition != "Diagramming-Only")
abs_Mon_Acc_Mod_full <- lmer(Abs_Mon_Acc ~ Experiment * Condition * Phase +
  (1 | Class_ID / Participant_Private_ID), data = row_per_trial_data_red_ssi_only)
# Print the output
abs_Mon_Acc_full_tab <- tab_model(abs_Mon_Acc_Mod_full, show.se = TRUE, show.std = TRUE)

## Only with main effects
abs_Mon_Acc_Mod_main <- lmer(Abs_Mon_Acc ~ Experiment + Condition + Phase +
  (1 | Class_ID / Participant_Private_ID), data = row_per_trial_data_red_ssi_only)
abs_Mon_Acc_main_tab <- tab_model(abs_Mon_Acc_Mod_main, show.se = TRUE, show.std = TRUE)

## Only with experiment and phase
abs_Mon_Acc_Mod_experiment_phase <- lmer(Abs_Mon_Acc ~ Experiment + Phase +
  (1 | Class_ID / Participant_Private_ID), data = row_per_trial_data_red_ssi_only)
abs_Mon_Acc_experiment_phase_tab <- tab_model(abs_Mon_Acc_Mod_experiment_phase, show.se = TRUE, show.std = TRUE)

## Only with experiment
abs_Mon_Acc_Mod_experiment <- lmer(Abs_Mon_Acc ~ Experiment +
  (1 | Class_ID / Participant_Private_ID), data = row_per_trial_data_red_ssi_only)
abs_Mon_Acc_experiment_tab <- tab_model(abs_Mon_Acc_Mod_experiment, show.se = TRUE, show.std = TRUE)

all_MA_tab_models <- tab_model(abs_Mon_Acc_Mod_full,
  abs_Mon_Acc_Mod_main,
  show.se = TRUE, show.std = TRUE
)

## Retrieve the interpretation sentence for the model comparisons
ma_both_exp_model_comparison <- anova(abs_Mon_Acc_Mod_full, abs_Mon_Acc_Mod_main)
ma_both_exp_model_comparison_interpretation <- retrieve_anova_model_comparison_output(ma_both_exp_model_comparison)

##      Run Bayesian t-tests                                                  ----
## Calculate Bayes based on this:
## https://www.r-bloggers.com/2014/02/bayes-factor-t-tests-part-2-two-sample-tests/
## Example:
# bf = BayesFactor::ttestBF(formula = logFuseTime ~ condition, data = randDotStereo)
# bf
## Bayes factor analysis

## [1] Alt., r=0.707 : 2.322 ±0.06%
## 
## Against denominator:
##   Null, mu1-mu2 = 0 
## ---
## Bayes factor type: BFindepSample, JZS
# The Bayes factor analysis with the default prior settings yields a Bayes factor of 2.3224 in favor of the alternative. What does this mean?
# - The data are 2.3224 times as probable under the alternative as under the null.
# - The relative odds in favor of the alternative, against the null, must change by a factor of 2.3224 in light of the data. 
#   For instance, if we held even odds before seeing the data, then our posterior odds would be 2.3224:1 for the alternative. 
#   This is regarded as weak evidence, though it does slightly favor the alternative.

## Filter data so that we only look at the SSI group
row_per_participant_SSI_Exp_1 <- row_per_phase_data %>% 
  filter(Experiment == 1 &  Condition == "SSI") %>% as.data.frame()
bayes_abs_MA_Phase_Exp_1 <- BayesFactor::ttestBF(formula = Mean_Abs_Mon_Acc ~ Phase, data = row_per_participant_SSI_Exp_1)
bayes_abs_MA_Phase_Exp_1
bayes_abs_MA_Phase_Exp_1_df <- as.data.frame(bayes_abs_MA_Phase_Exp_1)
# - The data are 0.781197 times as probable under the alternative as under the null.
# - The relative odds in favor of the alternative, against the null, must change by a factor of 0.781197 in light of the data. 
#   For instance, if we held even odds before seeing the data, then our posterior odds would be 0.781197:1 for the alternative. 

row_per_participant_SSI_Exp_2 <- row_per_phase_data %>% 
  filter(Experiment == 2 &  Condition == "SSI") %>% as.data.frame()
bayes_abs_MA_Phase_Exp_2 <- BayesFactor::ttestBF(formula = Mean_Abs_Mon_Acc ~ Phase, data = row_per_participant_SSI_Exp_2)
bayes_abs_MA_Phase_Exp_2
bayes_abs_MA_Phase_Exp_2_df <- as.data.frame(bayes_abs_MA_Phase_Exp_2)
# - The data are 0.208288 times as probable under the alternative as under the null.
# - The relative odds in favor of the alternative, against the null, must change by a factor of 0.208288 in light of the data. 
#   For instance, if we held even odds before seeing the data, then our posterior odds would be 0.208288:1 for the alternative. 

##    TC                                                                      ----
##      For Experiment 1                                                      ----

## Fit the model with all possible effects of experiment, condition and phase
text_comp_full_exp1 <- lmer(Test_Total_Correct ~ Condition * Phase +
  (1 | Class_ID / Participant_Private_ID), data = row_per_trial_data_red_exp1)
text_comp_full_exp1_tab <- tab_model(text_comp_full_exp1, show.se = TRUE, show.std = TRUE)

## Fit for main effects only
text_comp_main_exp1 <- lmer(Test_Total_Correct ~ Condition + Phase +
  (1 | Class_ID / Participant_Private_ID), data = row_per_trial_data_red_exp1)
text_comp_main_exp1_tab <- tab_model(text_comp_main_exp1, show.se = TRUE, show.std = TRUE)

## Print both models
text_comp_all_models_exp1_tab <- tab_model(text_comp_full_exp1, text_comp_main_exp1, show.se = TRUE, show.std = TRUE)

## Retrieve the interpretation sentence for the model comparisons
tc_exp_1_model_comparison <- anova(text_comp_full_exp1, text_comp_main_exp1)
tc_exp_1_model_comparison_interpretation <- retrieve_anova_model_comparison_output(tc_exp_1_model_comparison)


##      For Experiment 2                                                      ----
# Filter out data of experiment 1
## Fit the model with all possible effects of experiment, condition and phase
text_comp_full_exp2 <- lmer(Test_Total_Correct ~ Condition * Phase +
  (1 | Class_ID / Participant_Private_ID), data = row_per_trial_data_red_exp2)
text_comp_full_exp2_tab <- tab_model(text_comp_full_exp2, show.se = TRUE, show.std = TRUE)

## Fit for main effects only
text_comp_main_exp2 <- lmer(Test_Total_Correct ~ Condition + Phase +
  (1 | Class_ID / Participant_Private_ID), data = row_per_trial_data_red_exp2)
text_comp_main_exp2_tab <- tab_model(text_comp_main_exp2, show.se = TRUE, show.std = TRUE)

## Print both models together
all_text_comp_model_exp_2 <- tab_model(text_comp_full_exp2, text_comp_main_exp2, show.se = TRUE, show.std = TRUE)

## Retrieve the interpretation sentence for the model comparisons
tc_exp_2_model_comparison <- anova(text_comp_full_exp2, text_comp_main_exp2)
tc_exp_2_model_comparison_interpretation <- retrieve_anova_model_comparison_output(tc_exp_2_model_comparison)

## Pairwise comparisons
##    Pairwise comparisons
emm_TC_exp2 <- emmeans(text_comp_main_exp2, list(pairwise ~ Condition), adjust = "Bonferroni")
# Generate formatted output with standardized effects again (see function above)
emm_TC_exp2 <- return_print_ready_emmeans(emm_TC_exp2, text_comp_main_exp2)
# Add a scroll_box to the output as it's a long table
emm_TC_exp2 <- emm_TC_exp2 %>%
  scroll_box(width = "100%", height = "400px")

##      Compare experiments                                                   ----
## Kick out the diagramming-only condition of experiment 2
row_per_trial_data_red_ssi_only <- row_per_trial_data_red %>% filter(Condition != "Diagramming-Only")
text_comp_full_model <- lmer(Test_Total_Correct ~ Experiment * Condition * Phase +
  (1 | Class_ID / Participant_Private_ID), data = row_per_trial_data_red_ssi_only)
text_comp_full_tab <- tab_model(text_comp_full_model, show.se = TRUE, show.std = TRUE)

## Only with main effects
text_comp_main <- lmer(Test_Total_Correct ~ Experiment + Condition + Phase +
  (1 | Class_ID / Participant_Private_ID), data = row_per_trial_data_red_ssi_only)
text_comp_main_tab <- tab_model(text_comp_main, show.se = TRUE, show.std = TRUE)

## Print full and main effects together
text_comp_both_exp_all_models_tab <- tab_model(text_comp_full_model, text_comp_main, show.se = TRUE, show.std = TRUE)

## Retrieve the interpretation sentence for the model comparisons
tc_both_exp_model_comparison <- anova(text_comp_full_model, text_comp_main)
tc_both_exp_model_comparison_interpretation <- retrieve_anova_model_comparison_output(tc_both_exp_model_comparison)

## -------------------------------------------------------------------------- ----
## 5. Time on Task Analysis                                                   ----
## -------------------------------------------------------------------------- ----
##   Check processing times for each text reading screen                      ----
## Prepare the data for processing time analyses 
reaction_time_data <- left_join(ids_and_trials_to_keep, reaction_time_data) %>%
  mutate(Trial_Number = as.factor(Trial_Number))
# Filter data of experiment 1
reading_time_data_exp1 <- reaction_time_data %>%
  filter(Experiment == 1 & Screen_Name == "Text") %>%
  mutate(Reaction_Time_min = Reaction_Time / 60000) 
##   Check processing times for Experiment 1                                  ----
# Compute the sample sizes for each trial number
sample_size_exp1 <- reading_time_data_exp1 %>%
  group_by(Trial_Number) %>%
  dplyr::summarise(num = n(), .groups = "keep")
# Then plot a violin plot with myVar and the incoming title of the plot
reading_time_plot_exp1 <- reading_time_data_exp1 %>%
  left_join(sample_size_exp1) %>%
  mutate(my_axis = paste0(Trial_Number, "\n", "obs=", num)) %>%
  ggplot(aes(x = my_axis, y = Reaction_Time_min, fill = Trial_Number)) +
  geom_boxplot() + # width=1.4 inside the brackets
  stat_summary(
    fun = mean, colour = "darkred", geom = "point",
    shape = 18, size = 3, show.legend = FALSE, position = position_dodge(0.9)
  ) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.2) +
  scale_fill_brewer(palette = "Paired") + # "#E69F00","#D55E00"
  theme(plot.title = element_text(size = 10)) +
  # ggtitle("A boxplot with jitter") +
  xlab("") +
  ylab("Reading Times Experiment 1")

##   Check processing times for Experiment 2                                  ----
# Do the same for Experiment 2
reading_time_data_exp2 <- reaction_time_data %>%
  filter(Experiment == 2 & Screen_Name == "Text") %>%
  mutate(Reaction_Time_min = Reaction_Time / 60000) 
sample_size_exp2 <- reading_time_data_exp2 %>%
  group_by(Trial_Number) %>%
  dplyr::summarise(num = n(), .groups = "keep")
# Then plot a violin plot with myVar and the incoming title of the plot
reading_time_plot_exp2 <- reading_time_data_exp2 %>%
  left_join(sample_size_exp2) %>%
  mutate(my_axis = paste0(Trial_Number, "\n", "obs=", num)) %>%
  ggplot(aes(x = my_axis, y = Reaction_Time_min, fill = Trial_Number)) +
  geom_boxplot() + # width=1.4 inside the brackets
  stat_summary(
    fun = mean, colour = "darkred", geom = "point",
    shape = 18, size = 3, show.legend = FALSE, position = position_dodge(0.9)
  ) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.2) +
  scale_fill_brewer(palette = "Paired") + # "#E69F00","#D55E00"
  theme(plot.title = element_text(size = 10)) +
  # ggtitle("A boxplot with jitter") +
  xlab("") +
  ylab("Reading Times Experiment 2")

# Time on Trial
time_on_trial_exp2 <- reaction_time_data %>%
  filter(Experiment == 2) %>%
  distinct(Participant_Private_ID, Trial, .keep_all = TRUE) %>%
  mutate(Time_On_Trial_min = Time_On_Trial / 60)
# Create plot for experiment 2
time_on_trial_plot_exp2 <- time_on_trial_exp2 %>%
  left_join(sample_size_exp2) %>%
  mutate(my_axis = paste0(Trial_Number, "\n", "obs=", num)) %>%
  ggplot(aes(x = my_axis, y = Time_On_Trial_min, fill = Trial_Number)) +
  geom_boxplot() + # width=1.4 inside the brackets
  stat_summary(
    fun = mean, colour = "darkred", geom = "point",
    shape = 18, size = 3, show.legend = FALSE, position = position_dodge(0.9)
  ) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.2) +
  scale_fill_brewer(palette = "Paired") + # "#E69F00","#D55E00"
  theme(plot.title = element_text(size = 10)) +
  # ggtitle("A boxplot with jitter") +
  xlab("") +
  ylab("Minutes on Trial Experiment 2")

# Compare time on trial for Experiment 2
comparison_time_on_trial_exp2 <- time_on_trial_exp2 %>% filter(Trial_Number == 1 | Trial_Number == 6)
time_on_trial_model <- lm(Time_On_Trial_min ~ Trial_Number, data = comparison_time_on_trial_exp2)
tab_model(time_on_trial_model)
time_on_trial_first_last <- t.test(Time_On_Trial_min ~ Trial_Number, data = comparison_time_on_trial_exp2)
# Compare reading time for Experiment 2
comparison_reading_time_exp2 <- reading_time_data_exp2 %>% 
  filter(Trial_Number == 1 | Trial_Number == 6)
reading_time_model_exp_2 <- lm(Reaction_Time_min ~ Trial_Number, data = comparison_reading_time_exp2)
tab_model(reading_time_model_exp_2)
reading_time_first_last <- t.test(Reaction_Time_min  ~ Trial_Number, data = comparison_reading_time_exp2)
# Compare diagram study for Experiment 2
comparison_digram_study_time_exp2 <- reaction_time_data %>% 
  filter(Experiment == 2 & grepl('DiagramStudy', Screen_Name) & grepl('SSI', Condition)) %>%
  mutate(Reaction_Time_min = Reaction_Time / 60000) %>% 
  filter(Trial_Number == 1 | Trial_Number == 6)
digram_study_time_exp2 <- lm(Reaction_Time_min ~ Trial_Number, data = comparison_digram_study_time_exp2)
tab_model(digram_study_time_exp2)
digram_study_time_first_last_exp2 <- t.test(Reaction_Time_min  ~ Trial_Number, data = comparison_digram_study_time_exp2)
# Compare test time difference first and last trial
comparison_test_time_exp2 <- reaction_time_data %>%
  filter(Experiment == 2 & Screen_Name == "Test") %>%
  mutate(Reaction_Time_min = Reaction_Time / 60000) %>%
  filter(Trial_Number == 1 | Trial_Number == 6)
test_time_model_exp_2 <- lm(Reaction_Time_min ~ Trial_Number, data = comparison_test_time_exp2)
tab_model(test_time_model_exp_2)
test_time_first_last <- t.test(Reaction_Time_min  ~ Trial_Number, data = comparison_test_time_exp2)

## -------------------------------------------------------------------------- ----
## 6. Restudy Decisions                                                       -----
## -------------------------------------------------------------------------- ----
##    (Relative) monitoring-based & comprehension-based regulation accuracy   ----
#  computed as the gamma correlation coefficient between JOL-scores and restudy decisions across the six trials. 
# Apply function to compute gamma correlations to the trials of each participant
row_per_trial_data_red <- row_per_trial_data_red %>% ungroup() %>%
  group_by(Participant_Private_ID) %>% # For every participant,
  mutate(
    Rel_Restudy_Acc_MA = # call the gamma calculating function from above,
      calculate_rel_RA_MA(Participant_Private_ID, .)
  ) %>% # providing participant ID and participants' data
  relocate(Rel_Restudy_Acc_MA, .after = RestudyDecision)

##    Comprehension-based regulation accuracy                                 ----
# computed as gamma correlation coefficient between test scores and restudy decisions across the six trials.
row_per_trial_data_red <- row_per_trial_data_red %>% ungroup() %>%
  group_by(Participant_Private_ID) %>% # For every participant,
  mutate(
    Rel_Restudy_Acc_TC = # call the gamma calculating function from above,
      calculate_rel_RA_TC(Participant_Private_ID, .)
  ) %>% # providing participant ID and participants' data
  relocate(Rel_Restudy_Acc_TC, .after = Rel_Restudy_Acc_MA)

# How many NA's?
row_per_participant_data <- row_per_trial_data_red %>% 
  select(Experiment:Rel_Restudy_Acc_TC, -c(starts_with("Trial"), "Phase"), Rel_Mon_Acc) %>%
  distinct(Participant_Private_ID, .keep_all = TRUE)
# Restudy Accuracy MA based
table(row_per_participant_data$Rel_Restudy_Acc_MA, useNA = "ifany")
# Restudy Accuracy TC based
table(row_per_participant_data$Rel_Restudy_Acc_TC, useNA = "ifany")

## Count the NAs
row_per_participant_data %>% group_by(Experiment, Condition) %>%
  count(is.na(Rel_Restudy_Acc_MA))
row_per_participant_data %>% group_by(Experiment) %>%
  count(is.na(Rel_Restudy_Acc_MA))
row_per_participant_data %>% group_by(Experiment, Condition) %>%
  count(is.na(Rel_Restudy_Acc_TC))
row_per_participant_data %>% group_by(Experiment) %>%
  count(is.na(Rel_Restudy_Acc_TC))

## Check per experiment: Rel_Restudy_Acc_MA
row_per_participant_data %>% group_by(Experiment, Condition) %>%
  summarise(Mean = round(mean(Rel_Restudy_Acc_MA, na.rm = TRUE),2),
            SD = round(sd(Rel_Restudy_Acc_MA, na.rm = TRUE),2))

row_per_participant_data %>% group_by(Experiment, Condition) %>%
  summarise(Mean = round(mean(Rel_Restudy_Acc_TC, na.rm = TRUE),2),
            SD = round(sd(Rel_Restudy_Acc_TC, na.rm = TRUE),2))

# Experiment 1
row_per_participant_data_exp1 <- row_per_participant_data %>% filter(Experiment == 1)
restudy_MA_mod_exp1 <- lm(Rel_Restudy_Acc_MA ~ Condition, data = row_per_participant_data_exp1)
restudy_MA_mod_tab_exp1 <- tab_model(restudy_MA_mod_exp1)
summary(restudy_MA_mod_exp1)

## Experiment 2
row_per_participant_data_exp2 <- row_per_participant_data %>% filter(Experiment == 2)
restudy_MA_mod_exp2 <- lm(Rel_Restudy_Acc_MA ~ Condition, data = row_per_participant_data_exp2)
restudy_MA_mod_tab_exp2 <- tab_model(restudy_MA_mod_exp2)
summary(restudy_MA_mod_exp2)

## Check per experiment: Rel_Restudy_Acc_TC
# Experiment 1
row_per_participant_data_exp1 <- row_per_participant_data %>% filter(Experiment == 1)
restudy_TC_mod_exp1 <- lm(Rel_Restudy_Acc_TC ~ Condition, data = row_per_participant_data_exp1)
restudy_TC_mod_tab_exp1 <- tab_model(restudy_TC_mod_exp1)
summary(restudy_TC_mod_exp1)

## Experiment 2
row_per_participant_data_exp2 <- row_per_participant_data %>% filter(Experiment == 2)
restudy_TC_mod_exp2 <- lm(Rel_Restudy_Acc_TC ~ Condition, data = row_per_participant_data_exp2)
restudy_TC_mod_tab_exp2 <- tab_model(restudy_TC_mod_exp2)
summary(restudy_TC_mod_exp2)

## -------------------------------------------------------------------------- ----
## 7. Compute the Diagram Cues etc.                                           ----
## -------------------------------------------------------------------------- ----
## Remove participants due to exclusion criteria
# Then remove all IDs and trials from the diagram data that are not included in there
diagram_data_red <- diagram_data_red %>%
  select(Experiment, Phase, Participant_Private_ID, Trial, Zone_Name:Word_Count)

##    Compute Cue Values                                                      ----
# Correct Boxes
diagram_data_red <- diagram_data_red %>% group_by(Participant_Private_ID, Trial) %>%
  mutate(Cue_Value_Correct = sum(Correct_Points, na.rm = TRUE))
# Commissions
diagram_data_red <- diagram_data_red %>% group_by(Participant_Private_ID, Trial) %>%
  mutate(Cue_Value_Commission = sum(Commission, na.rm = TRUE))
# Omissions: first create omission errors based on other cue columns
diagram_data_red <- diagram_data_red %>% ungroup() %>% 
  mutate(Omission = ifelse(Correct_Points == 0 & (Element_Number == "NA"|is.na(Element_Number)) & Commission == 0 & (Word_Count == 0|is.na(Word_Count)),
                           1,
                           0))
# Then compute the cue value omissions
diagram_data_red <- diagram_data_red %>% group_by(Participant_Private_ID, Trial) %>%
  mutate(Cue_Value_Omission = sum(Omission, na.rm = TRUE))

# Reformat the data to one row per trial
diagram_data_row_per_trial <- diagram_data_red %>% 
  distinct(Participant_Private_ID, Trial, .keep_all = TRUE) %>% 
  select(-c(Zone_Name, Diagramming, 
            Correct_Points, Element_Number, Commission, Omission, Word_Count))

##    Merge Diagram Data and Performance data                                 ----
row_per_trial_data_red$Simple_Date <- as.Date(row_per_trial_data_red$Simple_Date)
all_row_per_trial_data <- left_join(row_per_trial_data_red, diagram_data_row_per_trial,
                                    by = join_by(Experiment, Phase, Participant_Private_ID, Trial))

##    Cue Diagnosticity                                                       ----
## Calculate the cue diagnosticity by correlating the cue values with the test scores for each participant
# Correct Boxes
all_row_per_trial_data <- all_row_per_trial_data %>% group_by(Participant_Private_ID) %>% 
  mutate(Cue_Diag_Correct = calculate_Pearson_Corrs(Cue_Value_Correct, Test_Total_Correct))
# Commission Errors
all_row_per_trial_data <- all_row_per_trial_data %>% group_by(Participant_Private_ID) %>% 
  mutate(Cue_Diag_Commission = calculate_Pearson_Corrs(Cue_Value_Commission, Test_Total_Correct))
# Omission Errors
all_row_per_trial_data <- all_row_per_trial_data %>% group_by(Participant_Private_ID) %>% 
  mutate(Cue_Diag_Omission = calculate_Pearson_Corrs(Cue_Value_Omission, Test_Total_Correct))

##    Cue Utilization                                                         ----
## Calculate the cue utilization by correlating the cue values with the JOLs of each participant
# Correct Boxes
all_row_per_trial_data <- all_row_per_trial_data %>% group_by(Participant_Private_ID) %>% 
  mutate(Cue_Util_Correct = calculate_Pearson_Corrs(Cue_Value_Correct, JoL))
# Commission Errors
all_row_per_trial_data <- all_row_per_trial_data %>% group_by(Participant_Private_ID) %>% 
  mutate(Cue_Util_Commission = calculate_Pearson_Corrs(Cue_Value_Commission, JoL))
# Omission Errors
all_row_per_trial_data <- all_row_per_trial_data %>% group_by(Participant_Private_ID) %>% 
  mutate(Cue_Util_Omission =   calculate_Pearson_Corrs(Cue_Value_Omission,JoL))

# Reformat data to one row per phase for each participant
diagram_cue_value_data_per_trial <- all_row_per_trial_data %>%
  select(Experiment, Condition, Phase, Participant_Private_ID, starts_with('Cue')) 

##    Calculate the descriptive statistics                                    ----
# Another one that rounds min and max in case of the correlations
calculate_desc_of_corrs <- function(incoming_data, outcome_of_interest){
  desc_outcome <- incoming_data %>% 
    dplyr::summarize(Mean = round(mean(x = {{outcome_of_interest}}, na.rm = TRUE),2),
                     SD = round(sd(x = {{outcome_of_interest}}, na.rm = TRUE),2),
                     Min = round(min(x = {{outcome_of_interest}}, na.rm = TRUE),2),
                     Max = round(max(x = {{outcome_of_interest}}, na.rm = TRUE),2),
                     NAs = sum(is.na({{outcome_of_interest}})))
  desc_outcome <- desc_outcome %>% distinct(Mean, SD, Min, Max, .keep_all = TRUE)
}

# Reformat the grouping structure of the data frame:
# Retrieve the descriptives per Experiment and Condition
diagram_cue_value_data_per_trial <- diagram_cue_value_data_per_trial %>% group_by(Experiment, Condition)

## For all Cue Values, Diagnosticities and Utilizations of Correct, Commission and Omission boxes
desc_Cue_Value_Correct    <- calculate_desc_of_corrs(diagram_cue_value_data_per_trial, Cue_Value_Correct) %>%
  filter(!is.na(Condition))
desc_Cue_Value_Commission <- calculate_desc_of_corrs(diagram_cue_value_data_per_trial, Cue_Value_Commission) %>%
  filter(!is.na(Condition))
desc_Cue_Value_Omission   <- calculate_desc_of_corrs(diagram_cue_value_data_per_trial, Cue_Value_Omission) %>%
  filter(!is.na(Condition))

## Reduce the diagram data to row per participant for the correlation scores
diagram_cue_diag_util_participant <- diagram_cue_value_data_per_trial %>% ungroup() %>%
  select(-c(Phase, Cue_Value_Commission, Cue_Value_Correct, Cue_Value_Omission)) %>%
  distinct(Participant_Private_ID, .keep_all = TRUE)
  
# Reformat the grouping structure of the data frame:
# Retrieve the descriptives per Experiment and Condition
diagram_cue_diag_util_participant <- diagram_cue_diag_util_participant %>% group_by(Experiment, Condition)
## Cue Diagnosticities 
desc_Cue_Diag_Correct    <- calculate_desc_of_corrs(diagram_cue_diag_util_participant, Cue_Diag_Correct) %>%
  filter(!is.na(Condition))
desc_Cue_Diag_Commission <- calculate_desc_of_corrs(diagram_cue_diag_util_participant, Cue_Diag_Commission) %>%
  filter(!is.na(Condition))
desc_Cue_Diag_Omission   <- calculate_desc_of_corrs(diagram_cue_diag_util_participant, Cue_Diag_Omission) %>%
  filter(!is.na(Condition))
## Cue_Utilizations
desc_Cue_Util_Correct    <- calculate_desc_of_corrs(diagram_cue_diag_util_participant, Cue_Util_Correct) %>%
  filter(!is.na(Condition))
desc_Cue_Util_Commission <- calculate_desc_of_corrs(diagram_cue_diag_util_participant, Cue_Util_Commission) %>%
  filter(!is.na(Condition))
desc_Cue_Util_Omission   <- calculate_desc_of_corrs(diagram_cue_diag_util_participant, Cue_Util_Omission) %>%
  filter(!is.na(Condition))

##    Descriptives Cue Values, Cue Diagnosticity and Cue Utilization          ----
## Create the html output for the output file
desc_Cues <- rbind(
  cbind(desc_Cue_Value_Correct,desc_Cue_Value_Commission, desc_Cue_Value_Omission),  
  cbind(desc_Cue_Diag_Correct, desc_Cue_Diag_Commission, desc_Cue_Diag_Omission),
  cbind(desc_Cue_Util_Correct, desc_Cue_Util_Commission, desc_Cue_Util_Omission)) %>% 
  kbl(col.names = gsub("\\...\\d+", "", names(.)),
      caption = "Descriptive Statistics Cue Values, Cue Diagnosticity and Cue Utilization",
      align = "clccccccccccccccccc") %>%
  remove_column(c(8,9,15,16)) %>%
  kable_paper("striped", full_width = F) %>%
  add_header_above(c(" " = 2, "Correct" = 5, "Commission" = 5, "Omission" = 5)) %>% 
  column_spec(2, border_right=T) %>%
  column_spec(7, border_right=T) %>%
  column_spec(12, border_right=T) %>%
  pack_rows("Cue Value", 1, 5) %>%
  pack_rows("Cue Diagnosticity", 6, 10) %>%
  pack_rows("Cue Utilization", 11, 15) %>%
  scroll_box(width = "100%", height = "300px") 

## -------------------------------------------------------------------------- ----
## 8. Evaluate Self-Scoring                                                   ----
## -------------------------------------------------------------------------- ----
##    Merge the Self-Scoring Responses to the diagram data                    ----
row_per_diagram_box <- full_join(self_scoring_responses, diagram_data_red, 
                                 by = join_by(Experiment, Participant_Private_ID, Trial, Zone_Name))
row_per_diagram_box <- row_per_diagram_box %>%
  mutate(Box_Position = sub('.*(?=.{1}$)', '', Zone_Name, perl=T)) %>%
  # stri_detect_coll finds Box_Position in Element_Number
  mutate(Matching_Position_Box_Answer = stringi::stri_detect_coll(Box_Position, Element_Number))

diagram_and_self_scoring <- full_join(diagram_data_red, self_scoring_responses) %>%
  filter(Phase == 1 & Condition == "SSI") %>%
  mutate(Omission = ifelse(Correct_Points == 0 & (Element_Number == "NA"|is.na(Element_Number)) & Commission == 0 & (Word_Count == 0|is.na(Word_Count)),
                           1,
                           0))

##    Count general matches of coding and self-scoring                        ----
desc_match_ssi_coding <- diagram_and_self_scoring %>% group_by(Experiment) %>%
  count(Match = 
          (Correct_Points == 1 & DiagramStudySS == "Juist") |
          (Commission == 1 & DiagramStudySS == "Ingevuld maar onjuist") |
          (Omission == 1 & DiagramStudySS == "Niet ingevuld")) %>%
  filter(!is.na(Match)) %>%
  mutate("%" = round(n/sum(n)*100, 2))

# Count Correct == 1 DiagramStudySS == "Juist"
desc_correct_ss_coding <- diagram_and_self_scoring %>% 
  filter(Correct_Points == 1) %>% 
  group_by(Experiment) %>%
  count(Value = Correct_Points == 1 & DiagramStudySS == "Juist") %>% 
  filter(!is.na(Value)) %>%
  mutate("%" = round(n/sum(n)*100, 2))
# Count Correct == 0 DiagramStudySS == "Juist"
diagram_and_self_scoring %>% group_by(Experiment) %>%
  count(Value = Correct_Points == 0 & DiagramStudySS == "Juist") %>% 
  filter(!is.na(Value)) %>%
  mutate("%" = round(n/sum(n)*100, 2))
# Count Commission == 1 DiagramStudySS == "Juist"
desc_commission_ss_coding <- diagram_and_self_scoring %>% 
  filter(Commission == 1) %>%
  group_by(Experiment) %>%
  count(Value = Commission == 1 & DiagramStudySS == "Ingevuld maar onjuist") %>% 
  filter(!is.na(Value)) %>%
  mutate("%" = round(n/sum(n)*100, 2))
# Count Ommission == 1 DiagramStudySS == "Juist"
desc_omission_ss_coding <- diagram_and_self_scoring %>% 
  filter(Omission == 1) %>%
  group_by(Experiment) %>% 
  count(Value = Omission == 1 & DiagramStudySS == "Niet ingevuld") %>% 
  filter(!is.na(Value)) %>%
  mutate("%" = round(n/sum(n)*100, 2))

##    Format to kable output                                                  ----
all_self_scoring_output <- rbind(cbind(desc_match_ssi_coding,
                                       desc_correct_ss_coding[,-c(1,2)],
                                       desc_commission_ss_coding[,-c(1,2)],
                                       desc_omission_ss_coding[,-c(1,2)])) %>% 
  mutate(Match = case_match(Match, 
                           FALSE ~ 'Incorrect Self-Scores',
                           TRUE ~ 'Correct Self-Scores')) %>% 
  rename(' ' = Match) %>%
  arrange(Experiment, 'n...3') %>%
  kbl(col.names = gsub("\\...\\d+", "", names(.)),
      caption = "Self-Scoring Evalutaion - Students Scoring Matching Official Coding",
      align = "clcccccccc") %>%
  kable_paper("striped", full_width = T) %>%
  add_header_above(c(" " = 2, "Overall" = 2, "Correctly Completed" = 2, "Commission" = 2, "Omission" = 2))
## -------------------------------------------------------------------------- ----
## 9. More formatting for the RMarkdown output overview                       ----
## -------------------------------------------------------------------------- ----
##    Reading Times as HTML output                                            ----
## Reading Times
desc_reading_times_sec <- full_join(desc_reading_times_sec_condition_phase, 
                                    desc_reading_times_sec_condition) %>%
  arrange(Experiment, Condition) %>% 
  ungroup() %>%
  mutate(Condition = ifelse(is.na(Phase),
                            paste0('Total ', Condition),
                            Condition))

##    Before Exclusion Overview as HTML output                                ----
overview_before_exlusion <- full_join(total_participants_before_excl, total_trials_before_excl)
# Exclusion Overtime
overview_exclusion_by_overtime <- exclusion_by_overtime %>% 
  filter(`Time_On_Task >= 40` == TRUE) %>% 
  select(Experiment, Condition, n) %>%
  rename(Participants = n) %>%
  mutate(Trials = Participants*6)
# Minimal Reading cut-off 
desc_participants_to_exclude_min_reading <- ids_participants_to_kick_min_reading %>% 
  group_by(Experiment, Condition) %>% count() %>%
  rename(Participants = n)
desc_additional_trials_excluded_min_reading <- leftover_trials_to_kick %>% 
  group_by(Experiment, Condition) %>% count() %>% 
  rename(Trials = n)
overview_excluded_based_on_minimal_reading <- full_join(desc_participants_to_exclude_min_reading, 
                                                        desc_additional_trials_excluded_min_reading) %>% 
  mutate(Trials = paste0(Participants*6, ' + ', Trials))


##    After Exclusion Overview as HTML output                                 ----
overview_analysed_participants_and_trials <- full_join(total_participants_final, total_trials_final)

# Merge and format the Table
overview_n <- full_join(overview_before_exlusion, 
                        overview_exclusion_by_overtime, 
                        by = join_by(Experiment, Condition)) %>% 
  full_join(.,  
            overview_excluded_based_on_minimal_reading, 
            by = join_by(Experiment, Condition)) %>% 
  full_join(.,  
            overview_analysed_participants_and_trials, 
            by = join_by(Experiment, Condition)) %>% 
  rowwise() %>% 
  mutate(Trials.x.x = paste0(eval(parse(text = Trials.x.x)), ' (', Trials.x.x, ')'))
names(overview_n) <- gsub("\\.x|\\.y", "", names(overview_n))


##    Time on Task                                                            ----
overview_time_on_task <- full_join(desc_time_on_task_exp_condition, desc_time_on_task_exp) %>% 
  arrange(Experiment)
## -------------------------------------------------------------------------- ----
## 10.Subgroup analysis: effects of standards for groups of commission errors ----
diagram_data_row_per_trial <- diagram_data_row_per_trial %>% 
  group_by(Participant_Private_ID) %>%
  mutate(Sum_Commission_Errors_All_Trials = sum(Cue_Value_Commission, na.rm = TRUE)) #%>%
  
diagram_data_row_per_trial %>% group_by(Sum_Commission_Errors_All_Trials) %>% count()

diagram_data_row_per_trial <- diagram_data_row_per_trial %>% 
  ungroup() %>% 
  mutate(Commission_Freq_Groups = case_when(
    Sum_Commission_Errors_All_Trials <= 3 ~ 'Few Commissions',
    Sum_Commission_Errors_All_Trials >= 4 ~ 'Many Commissions'))

diagram_data_row_per_trial %>% 
  filter(Experiment == 2) %>%
  distinct(Participant_Private_ID, .keep_all = TRUE) %>% group_by(Commission_Freq_Groups) %>% count()

quick_ids_for_condition_name <- row_per_trial_data_red %>% distinct(Condition, Participant_Private_ID)
right_join(quick_ids_for_condition_name, diagram_data_row_per_trial) %>%
  filter(Experiment == 2) %>%
  group_by(Condition, Commission_Freq_Groups) %>% distinct(Participant_Private_ID, Condition, Commission_Freq_Groups) %>% count()
right_join(quick_ids_for_condition_name, diagram_data_row_per_trial) %>%
  filter(Experiment == 2) %>%
  group_by(Condition) %>% distinct(Participant_Private_ID, Condition) %>% count()

ids_commission_groups <- diagram_data_row_per_trial %>%
  distinct(Participant_Private_ID, Commission_Freq_Groups)

row_per_trial_data_red <- right_join(ids_commission_groups, row_per_trial_data_red,
                                     by = join_by(Participant_Private_ID))

# Filter out data of experiment 2
row_per_trial_data_red_exp_2 <- row_per_trial_data_red %>% filter(Experiment == 2)

row_per_trial_data_red_exp_2 %>% 
  distinct(Participant_Private_ID, .keep_all = TRUE) %>%group_by(Condition, Commission_Freq_Groups) %>% count()

## Fit the model with all possible effects of experiment, condition and phase
abs_Mon_Acc_Mod_full_exp_2_commission_analysis <- lmer(Abs_Mon_Acc ~ Commission_Freq_Groups*Condition +
                                    (1 | Participant_Private_ID), data = row_per_trial_data_red_exp_2) # with class resulted in singular fit
abs_Mon_Acc_full_exp_2_commission_tab <- tab_model(abs_Mon_Acc_Mod_full_exp_2_commission_analysis, show.se = TRUE, show.std = TRUE)
summary(abs_Mon_Acc_Mod_full_exp_2_commission_analysis)

desc_MA_commission_groups <- row_per_trial_data_red_exp_2 %>%
  group_by(Commission_Freq_Groups) %>%
  dplyr::summarise(Mean = round(mean(Abs_Mon_Acc, na.rm = TRUE), 2),
                   SD = round(sd(Abs_Mon_Acc, na.rm = TRUE), 2),
                   Min = min(Abs_Mon_Acc, na.rm = TRUE),
                   Max = max(Abs_Mon_Acc, na.rm = TRUE))

desc_MA_condition_commission_groups <- row_per_trial_data_red_exp_2 %>%
  group_by(Condition, Commission_Freq_Groups) %>%
  dplyr::summarise(Mean = round(mean(Abs_Mon_Acc, na.rm = TRUE), 2),
                  SD = round(sd(Abs_Mon_Acc, na.rm = TRUE), 2),
                  Min = min(Abs_Mon_Acc, na.rm = TRUE),
                  Max = max(Abs_Mon_Acc, na.rm = TRUE))

desc_MA_condition_for_commission <- row_per_trial_data_red_exp_2 %>%
  group_by(Condition) %>%
  dplyr::summarise(Mean = round(mean(Abs_Mon_Acc, na.rm = TRUE), 2),
                   SD = round(sd(Abs_Mon_Acc, na.rm = TRUE), 2),
                   Min = min(Abs_Mon_Acc, na.rm = TRUE),
                   Max = max(Abs_Mon_Acc, na.rm = TRUE))

desc_MA_total_for_commission <- row_per_trial_data_red_exp_2 %>%
  ungroup() %>%
  dplyr::summarise(Mean = round(mean(Abs_Mon_Acc, na.rm = TRUE), 2),
                   SD = round(sd(Abs_Mon_Acc, na.rm = TRUE), 2),
                   Min = min(Abs_Mon_Acc, na.rm = TRUE),
                   Max = max(Abs_Mon_Acc, na.rm = TRUE))


############################################################################  ----