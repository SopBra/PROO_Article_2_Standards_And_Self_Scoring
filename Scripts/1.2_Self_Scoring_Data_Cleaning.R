############################################################################  --
##                       Data Cleaning Study 1 and 2:
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
## Save paths to the different directories where the data is stored
# To the directory of the data of Experiment 1 and 2:
rel_path_raw_data_exp1 <- "Data/Raw_Data_For_Cleaning/Experiment_1/"
rel_path_raw_data_exp2 <- "Data/Raw_Data_For_Cleaning/Experiment_2/"
rel_path_coded_data <- "Data/Raw_Data_For_Cleaning/Other_Required_Data/"
rel_path_coded_irr_data <- "Data/Raw_Data_For_Cleaning/Other_Required_Data/IRR_Data/"

output_path <- "Output/Data_Cleaning/"
output_for_analysis <- "Data/Processed_Data_For_Analyses/"
## -------------------------------------------------------------------------- ----
##    (Install if necessary and) load relevant libraries                      ----
#  Check and perform installation of relevant packages if needed
list_of_packages <- c("tidyverse", # distinct, filter, %>%, and many more
                      "data.table", # to read in all raw data --> alternative to vroom
                      "lubridate",  # for converting times
                      "readxl", # for reading excel files for the inter-rater reliabilies
                      "stringi", # for random string generator
                      "hms",     # for converting strings to time
                      "irr",
                      "xlsx" # for writing excel files
                      )  # for advanced string processing

# List of relevant packages
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])] # Check whether all packages are installed
if(length(new_packages)) install.packages(new_packages) # Install them if that is not the case
## Load the relevant libraries
sapply(list_of_packages, require, character.only = TRUE)



##    Relevant functions used in this script                                  ----
# A function that returns all files in a directory by taking 
#   the path to the directory, a pattern of relevant filenames and a file type
load_all_raw_data_files <- function(incomingPath, relevantPattern, fileType) {
  # Collect all file names that should be loaded
  filenames <- list.files(path = incomingPath, 
                          pattern = paste0(relevantPattern,"(.)*.",fileType,"$")) 
  # Load the files by adding the path to the directory to the file names,
  file_frame <- sapply(filenames, function(x) paste0(incomingPath, x), USE.NAMES = FALSE) %>%
    # reading in all columns as characters (to avoid issues with different types) 
    # and collapse all loaded data frames into one
    map_df(~fread(., fill = TRUE, colClasses = 'character')) %>% 
    type.convert(as.is=TRUE) # Convert the columns types back to what they contain
}

# Check which separator was used and call a respective read.csv function
# !!This function was created with the help of ChatGPT --> i.e., extract the separator
check_csv_separator <- function(incoming_paths_and_file_names) {
  # Read in the first line
  first_line <- readLines(incoming_paths_and_file_names, n = 1)
  extract_separator_indices <- as.vector(gregexpr(";|,|\t",first_line)[[1]])
  sepatator <- substr(first_line, extract_separator_indices, extract_separator_indices)
  sepatator
}

## Load all other file types with grepl specifications
load_set_of_specific_csv_files <- function(incoming_path, chr_pattern_of_interest) {
  # List all csv files names with the specified pattern in the specified directory
  relevant_file_names <- list.files(path = incoming_path,
                                    pattern = paste0(chr_pattern_of_interest, "(.)*.", "csv", "$"))
  # Save the connections to the files to check their separators
  all_paths_and_file_names <- paste0(incoming_path, relevant_file_names)
  
  # Check which separator was used by sampling one of the files
  csv_separator <- check_csv_separator(sample(all_paths_and_file_names, size = 1))
  
  # Load those csv files with the found separator
  relevant_files <- lapply(all_paths_and_file_names, read.csv, sep = csv_separator)
  
  names(relevant_files) <- paste0("data_", seq(1:length(relevant_files)))
  relevant_files
  ## For the purpose of data cleaning, stop here and do not merge those files ##
  # Run through the list and transform all columns to characters
  # (so that same column names that contain different data types can be merged)
  raw_data_ls <- lapply(relevant_files, function(x) {
    x[] <- lapply(x, as.character)
    x
  })
  
   ## Merge 
  all_raw_data <- raw_data_ls %>% 
    reduce(full_join) %>% 
    type.convert(as.is = TRUE) 
}

# A function to remove white spaces from the variable names
remove_white_spaces <- function(incomingData) {
  # Remove all white spaces from the variable names and replace them with a _
  names(incomingData) <- gsub(" ", "_", names(incomingData), fixed = TRUE)
  # Do the same for the - separator 
  names(incomingData) <- gsub("-", "_", names(incomingData), fixed = TRUE)
  # Do the same for . in the column names 
  names(incomingData) <- gsub(".", "_", names(incomingData), fixed = TRUE)
  
  incomingData
}

# A function to clean both data sets of Experiment 1 and 2
clean_gorilla_data <- function(incomingData) {
  # Remove the white spaces with the remove_white_spaces function above
  incomingData <- remove_white_spaces(incomingData)
  # Exclude introduction video files and practice trial files
  incomingData <- incomingData %>% filter(!grepl('Alice', Spreadsheet))
  incomingData <- incomingData %>% filter(!grepl('Sporten', Spreadsheet))
  incomingData <- incomingData %>% filter(!grepl('Sporten', display))
  # Also remove EventIndex == 999 (END OF FILE)
  incomingData <- incomingData %>% filter(Event_Index != "END OF FILE" & Event_Index != 999)
  # Also remove BEGIN TASK and END TASK 
  incomingData <- incomingData %>% filter(Trial_Number != "BEGIN TASK" | Trial_Number != "END TASK")
  # Remove all empty columns/variables (variables Gorilla added automatically)
  incomingData <- incomingData %>% ungroup(.) %>% discard(~all(is.na(.) | . ==""))
  # Remove "Attempt" as it seems to be coded differently across experiments by Gorilla and is not relevant
  incomingData <- incomingData %>% select(-Attempt)
  incomingData <- incomingData %>% type.convert(as.is=TRUE)
  # Transform Local_Date into a year-month-day variable
  incomingData <- incomingData %>% mutate(Local_Date = dmy_hms(Local_Date)) 
  # Create a Time on Task variable based on this new time variable
  incomingData <- incomingData %>% group_by(Participant_Private_ID) %>% 
    mutate(Time_On_Task = max(Local_Date) - min(Local_Date)) %>% relocate(Time_On_Task, .after = Local_Date)
}

# Rename levels of Condition (preferably adapt in data cleaning)
rename_Conditions <- function(Condition){
  # (And add more spaces where necessary)
  Condition <- gsub(Condition, pattern = "DiagramFeedback", replacement = "Diagramming+Standard")
  Condition <- gsub(Condition, pattern = "PictureMatching", replacement = "No-Diagram-Control")
  Condition <- gsub(Condition, pattern = "PictureMapping", replacement = "No-Diagram-Control")
  Condition <- gsub(Condition, pattern = "DiagramNoStandard", replacement = "Diagramming-Only")
  Condition <- gsub(Condition, pattern = "StandardOnly", replacement = "Standard-Only")
  Condition <- gsub(Condition, pattern = "DiagramNoFeedback", replacement = "Diagramming-Only")
  Condition <- gsub(Condition, pattern = "FeedbackOnly", replacement = "Standard-Only")
  Condition <- gsub(Condition, pattern = "DiagramStandard", replacement = "Diagramming+Standard")
}

# A function that can retrieve digits from stings
retrieve_digits_of_strings <- function(incoming_column) {
  my_digit <- as.integer(str_extract(incoming_column, "\\d+"))
  my_digit
}

# A function that calculates the test total scores
calculate_test_total_scores <- function(incoming_coded_tests){
## Create one variable that unites all strings of the relevant six columns that were coded
incoming_coded_tests <- incoming_coded_tests %>% 
  unite("all_test_element_codings_as_one", 
        as.symbol(str_subset(names(incoming_coded_tests),"Element1")):
          as.symbol(str_subset(names(incoming_coded_tests),"Element6")), 
        remove = FALSE, sep = " ", na.rm = TRUE)
## Now count how many instances of correct Elements there are in each string (i.e., correct if coded with 1,2,3,4,or NA)
incoming_coded_tests <- incoming_coded_tests %>% group_by(Participant_Private_ID, Trial) %>% 
  mutate(Test_Total_Correct = sum(str_count(all_test_element_codings_as_one, c("1|2|3|4|NA")))) %>% 
  relocate(Test_Total_Correct,.after = all_test_element_codings_as_one)
incoming_coded_tests
}

# Remove excel formatting
remove_excel_formatting <- function(incoming_column) {
  ## Remove potential excel formatting from Response for merging
  incoming_column <- gsub("[\r\n]", "", incoming_column)
  incoming_column
}
##    Load the raw data files for the experiments in the defined directories  ----
# Use the function "loadFileByName" above to read-in the different task and questionnaire files
all_data_exp1 <- load_all_raw_data_files(rel_path_raw_data_exp1, "task", "csv")
questionnaires_exp1 <- load_set_of_specific_csv_files(rel_path_raw_data_exp1, "questionnaire") %>% 
  remove_white_spaces(.)

# Data of Experiment 2
all_data_exp2 <- load_all_raw_data_files(rel_path_raw_data_exp2, "task", "csv")
questionnaires_exp2 <-  load_all_raw_data_files(rel_path_raw_data_exp2, "questionnaire", "csv") %>% 
  remove_white_spaces(.)

## Also the coded data after raw data cleaning 
coded_test_data <- fread(paste0(rel_path_coded_data, "coded_test_data.txt")) 
gender_exp_1 <- fread(paste0(rel_path_coded_data, "gender_merge_file_exp_1.txt"))
coded_diagrams_without_irr_exp2 <- read_xlsx(paste0(rel_path_coded_data, "coded_row_per_diagram_box_without_irr_exp2.xlsx")) 
coded_diagram_data_exp1 <- read_xlsx(paste0(rel_path_coded_data, "coded_data_sheets_exp1.xlsx"), sheet = 1)

irr_diagram_data_coder1 <- read_xlsx(paste0(rel_path_coded_irr_data, "irr_diagram_data_Sophia.xlsx"), sheet = 2)
irr_diagram_data_coder2 <- read_xlsx(paste0(rel_path_coded_irr_data, "irr_diagram_data_Linda.xlsx"), sheet = 2)
irr_test_data_coder1 <- read_xlsx(paste0(rel_path_coded_irr_data, "irr_test_data_Sophia.xlsx"), sheet = 2)
irr_test_data_coder2 <- read_xlsx(paste0(rel_path_coded_irr_data, "irr_test_data_Linda.xlsx"), sheet = 2)

## -------------------------------------------------------------------------- ----
## 1. Extract age and gender of the participants who gave their consent       ----
## -------------------------------------------------------------------------- ----
##    Save number of students who did not provide consent                     ----
# Experiment 1
participants_to_kick_exp1 <- questionnaires_exp1 %>% 
  filter(Question_Key == "Consent-2") %>% 
  select(Participant_Private_ID)
# Experiment 2
participants_to_kick_exp2 <- questionnaires_exp2 %>% 
  filter(Question_Key == "Consent-2") %>% 
  select(Participant_Private_ID)
participants_to_kick <- full_join(participants_to_kick_exp1, participants_to_kick_exp2)
##    Process Age of the participants                                         ----
age_gender_exp1 <- questionnaires_exp1 %>% 
  filter(grepl("Consent-1|response-5|Age", Question_Key) & !grepl("quantised|text", Question_Key)) %>% 
  select(Participant_Private_ID, Question_Key, Response) %>% mutate(Experiment = 1) %>% relocate(Experiment)
age_gender_exp1$Question_Key <- gsub('[[:digit:]]+', '', age_gender_exp1$Question_Key) 

age_gender_exp2 <- questionnaires_exp2 %>% 
  filter(grepl("Consent-1|response-5|Age", Question_Key) & !grepl("quantised|text", Question_Key)) %>% 
  select(Participant_Private_ID, Question_Key, Response) %>% mutate(Experiment = 2) %>% relocate(Experiment)
age_gender_exp2$Question_Key <- gsub('[[:digit:]]+', '', age_gender_exp2$Question_Key) 

age_gender <- full_join(age_gender_exp1, age_gender_exp2) %>% 
  pivot_wider(id_cols = Experiment:Participant_Private_ID, names_from = Question_Key, values_from = Response) %>% 
  rename(Consent = `Consent-`, Gender = `response-`)

##    Fix Gender Exp 1                                                        ----

age_gender <- full_join(age_gender, gender_exp_1, by = "Participant_Private_ID") %>%
  ungroup() %>%
  mutate(Gender.x = ifelse(Experiment == 1,
                           Gender.y,
                           Gender.x)) %>%
  select(-Gender.y) %>% rename(Gender = Gender.x) %>%
  filter(!is.na(Consent))

#write_delim(age_gender, 'age_gender_exp_1_and_2.txt')

## -------------------------------------------------------------------------- ----
## 2. Preliminary Data Cleaning                                               ----
## -------------------------------------------------------------------------- ----
##    (Remove white spaces, introduction videos, practice trials, etc.)       ----
# With the clean_gorilla_data function written to clean both data sets 
all_data_exp1 <- clean_gorilla_data(all_data_exp1)
all_data_exp2 <- clean_gorilla_data(all_data_exp2)

##    Remove participants who did not provide consent                         ----
data_exp1 <- all_data_exp1 %>% 
  filter(!(Participant_Private_ID %in% participants_to_kick_exp1$Participant_Private_ID))
data_exp2 <- all_data_exp2 %>% 
  filter(!(Participant_Private_ID %in% participants_to_kick_exp2$Participant_Private_ID))

##    Add Experiment 1 and 2 to the two data sets and merge them              ----  
data_exp1 <- data_exp1 %>% mutate(Experiment = 1) %>% relocate(Experiment)
data_exp2 <- data_exp2 %>% mutate(Experiment = 2) %>% relocate(Experiment)
all_data <- full_join(data_exp1, data_exp2)
dim(data_exp1)[1] + dim(data_exp2)[1] == dim(all_data)[1]
## -------------------------------------------------------------------------- ----
## 3. Add some new variables (Trial, Phase, Time/Class level, etc.)           ----
## -------------------------------------------------------------------------- ----
##    Add new variables: condition, trial names, trial and phase numbers      ----
assign_relevant_variables <- function(incoming_data){
  ## Assign condition names
  # First rename the randomiser column to be condition names
  incoming_data <- incoming_data %>% rename(Condition = randomiser_81rl)
  # Then recode the levels
  incoming_data$Condition <- recode(incoming_data$Condition, 
                                    DiagrammingOnly = "Diagramming-Only", 
                                    NoSelfScoringInstruction = "No-SSI",
                                    SelfScoringInstruction = "SSI") 
  ## Assign the trial names
  trial_names <- c("Beton|Metro|Botox|Muziek|Suez|Geld") 
  outgoing_data <- incoming_data %>% mutate(Trial = str_extract(display, trial_names)) %>% 
    relocate(Trial, .after = Condition)
  
  ## Assign Trial and Phase numbers
  data_subset <- outgoing_data %>% filter(Trial != "character(0)") %>% 
    distinct(Participant_Private_ID, Trial, .keep_all = TRUE) %>% 
    select(Event_Index, Participant_Private_ID, Trial) 
  data_subset <- data_subset %>% group_by(Participant_Private_ID) %>% 
    arrange(Event_Index, .by_group = TRUE) %>%
    mutate(New_Trial_Number = row_number()) %>% select(-Event_Index)
  data_subset <- data_subset %>% rowwise() %>% mutate(Phase = ifelse(New_Trial_Number <= 3, 1, 2))
  
  # Retrieve an overview of all participant ids with trial names and numbers + phase numbers to merge
  trial_frame <- data_subset %>% distinct(Participant_Private_ID, New_Trial_Number, Trial, Phase)
  
  ## Save trial names to restudy decisions
  only_restudy_decisions <- outgoing_data %>% 
    # Filter out the relevant rows (that were kicked out when filtering out all rows without trial names)
    filter(grepl("RestudyDecisionBlock|RestudyDecisionText", display) & !grepl("Intro", Screen_Name) & Zone_Name != "script")
  # Also assign the trial sequence to the restudy decisions (then used for merging the trial names)
  only_restudy_decisions <- only_restudy_decisions %>% ungroup() %>% 
    mutate(New_Trial_Number = ifelse(Experiment == 1, retrieve_digits_of_strings(Zone_Name),
                                 retrieve_digits_of_strings(display))) %>% select(-c(Trial))
  # Merge trial names based on New_Trial_Number
  only_restudy_decisions <- full_join(trial_frame, only_restudy_decisions)
  
  # Merge the two subsets with the main data and return in
  outgoing_data <- full_join(data_subset, outgoing_data, multiple = "all")
  outgoing_data <- full_join(outgoing_data, only_restudy_decisions, multiple = "all")
  outgoing_data <- outgoing_data %>% arrange(Experiment, Participant_Private_ID, Event_Index)
  
  # Remove all data where no trial name was assigned
  outgoing_data <- outgoing_data %>% filter(!is.na(Trial))
}
all_data <- assign_relevant_variables(all_data)

##    Create Simple Time and Simple Date                                      ----
transform_time_variables <- function(incoming_data) {
  # Simple Time
  incoming_data <- incoming_data %>% mutate(Simple_Time = as_hms(Local_Date)) %>%
    relocate(Simple_Time, .after = Local_Date)
  # Simple Date
  incoming_data <- incoming_data %>% mutate(Simple_Date = date(Local_Date)) %>%
    relocate(Simple_Date, .after = Local_Date)
  # Add a time on trial variable
  incoming_data <- incoming_data %>% group_by(Participant_Private_ID, Trial) %>% 
    mutate(Time_On_Trial = max(Simple_Time) - min(Simple_Time)) %>% 
    relocate(Time_On_Trial, .after = Trial)
}
# Apply function
all_data <- transform_time_variables(all_data)
##    Create Class_Date_Frame                                                 ----
# First save all the dates of the data collection (multiple if > 1 class per day)
dates <- c("2022-06-03", "2022-06-07", "2022-06-08", "2022-06-09", "2022-06-13", "2022-06-15", # Experiment 1
           # Experiment 2:
           "2022-12-12", "2022-12-12", "2022-12-12", "2022-12-13", "2022-12-13", 
           "2023-01-10", "2023-01-10", "2023-01-10", "2023-01-12", "2023-02-03", "2023-02-03")
# Create random strings for all classes (based on length of dates vector)
class <- stri_rand_strings(length(dates), 3)
# Save start and end date of each lesson (for cut-off later)
start_lesson <- c("12:40:00", "10:45:00", "10:45:00", "10:55:00", "13:00:00", "11:45:00", # Experiment 1
                  # Experiment 2:
                  "09:00:00", "10:30:00", "12:00:00", "10:50:00", "13:20:00", 
                  "08:20:00","09:50:00","10:50:00","09:50:00","11:10:00","12:00:00")
end_lesson   <- c("14:10:00", "12:15:00", "12:15:00", "12:10:00", "14:20:00", "12:50:00", # Experiment 1
                  # Experiment 2:
                  "10:00:00", "11:45:00", "13:15:00", "11:50:00", "14:20:00", 
                  "09:20:00","10:49:59","11:50:00","10:50:00","12:00:00","12:50:00")
# Create a frame merging all this information
date_class_frame <- as_tibble(cbind(dates, class, start_lesson, end_lesson))

date_class_frame <- date_class_frame %>% mutate(dates = date(dates)) 
date_class_frame <- date_class_frame %>% mutate(start_lesson = hms::as_hms(start_lesson)) 
date_class_frame <- date_class_frame %>% mutate(end_lesson = hms::as_hms(end_lesson)) 
assign_class_id_exp2 <- function(simple_date, simple_time) {
  return_class_id <- date_class_frame$class[date_class_frame$dates == simple_date &
                                           simple_time <= date_class_frame$end_lesson &
                                           simple_time >= date_class_frame$start_lesson]

  # Check if empty
  return_class_id <- ifelse(!is_empty(return_class_id), return_class_id, 'Time out of bounce')
}

# Apply the function
all_data <- all_data %>% ungroup() %>% rowwise() %>%
  dplyr::mutate(Class_ID = assign_class_id_exp2(Simple_Date, Simple_Time)) %>% 
  relocate(Class_ID, .before = Condition)

## -------------------------------------------------------------------------- ----
## 4. Reduce and Reformat data                                                ----
## -------------------------------------------------------------------------- ----
##    Reduce to relevant data and do some more cleaning                       ----
relevant_data <- all_data %>% 
  select(Experiment, Class_ID, Local_Date, Simple_Date, Simple_Time, Participant_Private_ID, 
         Time_On_Task, Condition, Phase, Trial, Time_On_Trial, Event_Index, 
         New_Trial_Number, Screen_Name, Zone_Name, Zone_Type, Reaction_Time, Response) %>%
  rename(Trial_Number = New_Trial_Number)

## Filter out irrelevant rows
relevant_data <- relevant_data %>% filter(!(Screen_Name == "Test" & Zone_Name == "buttonB"))
relevant_data <- relevant_data %>% filter(!(Screen_Name == "Diagramming" & Zone_Name == "Zone1"))
relevant_data <- relevant_data %>% filter(!(Screen_Name == "DiagramStudySS" & Zone_Name == "script"))
##    Reformat relevant data to one row per trial                             ----
relevant_data$Screen_Name[grepl("RestudyDecision", relevant_data$Screen_Name)] <-  "RestudyDecision"
row_per_trial_data <- relevant_data %>% filter(!grepl("Diagram", Screen_Name)) %>% 
  select(Experiment, Simple_Date, Class_ID, Participant_Private_ID, Time_On_Task,
         Condition, Phase, Trial, Time_On_Trial, Trial_Number, Screen_Name, Response) %>% 
  ungroup() %>% filter(Response != "" ) %>%
  pivot_wider(., id_cols = Experiment:Trial_Number, 
              names_from = Screen_Name, 
              values_from = Response,
              values_fn = list) %>% unnest(cols = c(RestudyDecision, JoL, Test))
##    Reformat relevant data to one row per diagram box                       ----
relevant_data$Zone_Name <- gsub("CueSelection", "DiagBox", relevant_data$Zone_Name)
row_per_diagram_box <- relevant_data %>% filter(grepl("Diagram", Screen_Name)) %>% 
  select(Experiment, Simple_Date, Class_ID, Participant_Private_ID, Time_On_Task,
         Condition, Phase, Trial, Time_On_Trial, Trial_Number, Screen_Name, Zone_Name, Response) %>% 
  relocate(Zone_Name, .before = Screen_Name) %>%
  ungroup() %>% filter(Response != "" ) %>%
  pivot_wider(., id_cols = Experiment:Zone_Name, 
              names_from = Screen_Name, 
              values_from = Response,
              values_fn = list) %>% unnest(cols = c(Diagramming, DiagramStudySS))

## -------------------------------------------------------------------------- ----
## 5. Inter-Rater-Reliability (IRR) and Data Coding                           ----
## -------------------------------------------------------------------------- ----
##    Extract 20% of surely usable data for inter-rater-reliability           ----
extract_twenty_percent <- function(incoming_data){
  set.seed(070492)
  n_trials <- incoming_data %>% ungroup() %>% distinct(Participant_Private_ID, Trial)
  twenty_percent <- (dim(n_trials)[1]*20)/100 # IRR based on 1035 total trials
  selected_participants <- n_trials %>% slice_sample(n = round(twenty_percent))
  twenty_perc_incoming_data <- left_join(selected_participants, incoming_data, multiple = "all") 
}

# Apply function to randomly extract data
diagram_data_exp2_20_perc <- extract_twenty_percent(row_per_diagram_box) %>% 
  mutate(Correct_Points = NA) %>% mutate(Element_Number = NA) %>% mutate(Commission = NA) %>% mutate(Opmerking = NA)
test_data_exp2_20_perc <- extract_twenty_percent(row_per_trial_data) 

## Add new columns for test data
new_col_coding_tests <- c(paste0("Element",1:6), paste0("Commission", 1:5))
test_data_exp2_20_perc[new_col_coding_tests] <- NA

# Write the files
#write.xlsx(diagram_data_exp2_20_perc, file=paste0(output_path, "irr_diagram_data_to_code.xlsx"))
#write.xlsx(test_data_exp2_20_perc, file=paste0(output_path, "irr_test_data_to_code.xlsx"))


##    Calculate IRR                                                           ----
##    Calculate test total scores in each IRR data set                        ----
calculate_test_total_scores <- function(incoming_coded_tests){
  ## Create one variable that unites all strings of the relevant six columns that were coded
  incoming_coded_tests <- incoming_coded_tests %>% 
    unite("all_correct_codings_as_one", 
          as.symbol(str_subset(names(incoming_coded_tests),"Element1")):
            as.symbol(str_subset(names(incoming_coded_tests),"Element6")), 
          remove = FALSE, sep = " ", na.rm = TRUE)
  ## Now count how many instances of correct Elements there are in each string (i.e., correct if coded with 1,2,3,4,or NA)
  incoming_coded_tests <- incoming_coded_tests %>% group_by(Participant_Private_ID, Trial) %>% 
    mutate(Test_Total_Correct = sum(str_count(all_correct_codings_as_one, c("1|2|3|4|NA")))) %>% 
    relocate(Test_Total_Correct,.after = all_correct_codings_as_one)
  incoming_coded_tests
}

## Calculate the total correct diagram scores, as those will be used for the analysis
calculate_diagram_total_scores <- function(incoming_data) {
  # Recode string NAs as actual NAs, and convert variable into integer
  incoming_data$Correct_Points <- type.convert(incoming_data$Correct_Points, as.is=TRUE)
  incoming_data <- incoming_data %>% group_by(Participant_Private_ID, Trial) %>% 
    mutate(Diagram_Total_Correct = sum(Correct_Points, na.rm = TRUE))
  incoming_data
}

## Add Coder ID for merging the dataset and remove excel formatting
reformat_irr_data <- function(incoming_data, incoming_coder_id){
  incoming_data <- incoming_data %>% 
    rename_with(.cols = !c(Event_Index:Response), ~paste0(.,incoming_coder_id)) %>% 
    mutate(Response =  remove_excel_formatting(Response)) 
}

## Apply functions to data sets
# To the test data
irr_test_data_coder1 <- calculate_test_total_scores(irr_test_data_coder1) %>% 
  reformat_irr_data(., "_Coder1")
irr_test_data_coder2 <- calculate_test_total_scores(irr_test_data_coder2) %>% 
  reformat_irr_data(., "_Coder2")
# To the diagram data
irr_diagram_data_coder1 <- calculate_diagram_total_scores(irr_diagram_data_coder1) %>% 
  reformat_irr_data(., "_Coder1")
irr_diagram_data_coder2 <- calculate_diagram_total_scores(irr_diagram_data_coder2) %>% 
  reformat_irr_data(., "_Coder2")

## Merge the data sets of the two coders
# The diagram data
irr_diagram_data_merged <- full_join(irr_diagram_data_coder1, irr_diagram_data_coder2) 
anti_join(irr_diagram_data_coder2, irr_diagram_data_coder1) ## FIX THIS NEXT TIME!!
# The test data
irr_test_data_merged <- full_join(irr_test_data_coder1, irr_test_data_coder2) 

## Calculate Kappa's
# Based on: https://www.datanovia.com/en/lessons/inter-rater-reliability-analyses-quick-r-codes/
# Cohen’s Kappa: It can be used for either two nominal or two ordinal variables. It accounts for strict agreements between observers. 
#                It is most appropriate for two nominal variables.
# Weighted Kappa: It should be considered for two ordinal variables only. It allows partial agreement.

## Calculate the irr for the total test scores
irr_test_total_correct <- kappa2(cbind(irr_test_data_merged$Test_Total_Correct_Coder1, 
                                       irr_test_data_merged$Test_Total_Correct_Coder2), weight = "squared") 

irr_diagram_data_row_per_trial <- irr_diagram_data_merged %>% 
  distinct(Participant_Private_ID, Trial, Diagram_Total_Correct_Coder1, Diagram_Total_Correct_Coder2) 

## IRR for the...
# ...total correct boxes per diagram
irr_diagram_total_correct <- kappa2(cbind(irr_diagram_data_row_per_trial$Diagram_Total_Correct_Coder1, 
                                          irr_diagram_data_row_per_trial$Diagram_Total_Correct_Coder2), weight = "squared") 
# "squared": disagreements are weighted according to their squared distance from perfect agreement
# The weighted Kappa coefficient with '"squared"' weights equals the product moment correlation under certain conditions. 
#   Own weights could be specified by supplying the function with a numeric vector of weights, 
#   starting from perfect agreement to worst disagreement. 
#   The length of this vector must equal the number of rating categories.

# ...all single correct boxes
irr_diagram_correct_points <- kappa2(cbind(irr_diagram_data_merged$Correct_Points_Coder1, 
                                           irr_diagram_data_merged$Correct_Points_Coder2), weight = "squared") 
# ...all element numbers
irr_diagram_element_number <- kappa2(cbind(irr_diagram_data_merged$Element_Number_Coder1, 
                                           irr_diagram_data_merged$Element_Number_Coder2), weight = "squared") 
# ...all commission errors
irr_diagram_commission <- kappa2(cbind(irr_diagram_data_merged$Commission_Coder1, 
                                       irr_diagram_data_merged$Commission_Coder2), weight = "squared") 

##    Output formatting: Inter-Rater-Reliabilities                            ----
## Create a formatted table for the IRRs
irr_diagram_total_correct <- c('Total Correct Scores', round(irr_diagram_total_correct$value,2))
irr_diagram_correct_points <- c('Correct Box Scores', round(irr_diagram_correct_points$value,2))
irr_diagram_element_number <- c('Element Numbers', round(irr_diagram_element_number$value,2))
irr_diagram_commission <- c('Commission Errors', round(irr_diagram_commission$value,2))
irr_test_total_correct <- c('Test Total Scores', round(irr_test_total_correct$value,2))

all_IRRs <- kbl(rbind(irr_diagram_total_correct,
                      irr_diagram_correct_points,
                      irr_diagram_element_number,
                      irr_diagram_commission,
                      irr_diagram_total_correct ,
                      deparse.level = 0),
                caption = "Inter-Rater-Reliabilities of Diagram and Test Responses (squared kappa's)") %>%
  kable_paper("striped", full_width = F) %>%
  kable_styling(full_width = FALSE, position = "left") %>%
  pack_rows("Diagram Responses", start_row = 1, end_row = 4) %>%
  pack_rows("Test Responses", start_row = 5, end_row = 5) 

## Save the ids and trials that were used for the inter rater reliability to exclude them later
id_irr_tests <- irr_test_data_merged %>% distinct(Participant_Private_ID, Trial)
id_irr_diagrams <- irr_diagram_data_merged %>% distinct(Participant_Private_ID, Trial)

## FIX THIS LATER ----
all_diagram_data_to_code <- row_per_diagram_box %>% 
  filter(grepl("DiagBox",Zone_Name)) %>% 
  select(Participant_Private_ID, Trial, Zone_Name, Diagramming) %>%
  mutate(Correct_Points = NA) %>% 
  mutate(Element_Number = NA) %>% 
  mutate(Commission = NA) %>% 
  mutate(Opmerking = NA)  

## Prepare the test data
all_test_data_to_code <- row_per_trial_data %>% 
  select(Participant_Private_ID, Trial, Test)

## Add the columns to the test data
new_col_coding_tests <- c(paste0("Element",1:6), paste0("Commission", 1:5))
all_test_data_to_code[new_col_coding_tests] <- NA

## Filter out data that was coded for IRR
diagram_data_left_to_code <- anti_join(all_diagram_data_to_code, id_irr_diagrams) # anti_join() return all rows from x without a match in y.
test_data_left_to_code <- anti_join(all_test_data_to_code, id_irr_tests) # anti_join() return all rows from x without a match in y.

## Already pre code all omission errors
# Add a wordcount of the response to the data
diagram_data_left_to_code <- diagram_data_left_to_code %>% rowwise() %>% 
  mutate(Word_Count = str_count(Diagramming, '\\w+')) %>% relocate(Word_Count, .after = Diagramming)
# Whereever WordCount == 0: CorrectPoints == 0	ElementNumber == NA	Commission == 0
diagram_data_left_to_code$Correct_Points[diagram_data_left_to_code$Word_Count == 0] <- 0
diagram_data_left_to_code$Element_Number[diagram_data_left_to_code$Word_Count == 0] <- "NA"
diagram_data_left_to_code$Commission[diagram_data_left_to_code$Word_Count == 0] <- 0

test_data_left_to_code <- test_data_left_to_code %>% rowwise() %>% 
  mutate(Word_Count = str_count(Test, '\\w+')) %>% relocate(Word_Count, .after = Test)
test_data_left_to_code$Element1[test_data_left_to_code$Word_Count == 0] <- 0

# Write the files
#write.xlsx(diagram_data_left_to_code, file=paste0(output_path, "remaining_diagram_data_to_code.xlsx"))
#write.xlsx(test_data_left_to_code, file=paste0(output_path, "remaining_test_data_to_code.xlsx"))

## -------------------------------------------------------------------------- ----
## 6. Merge coded data with cleaned raw data and compute total scores         ----
## -------------------------------------------------------------------------- ----
##    Merge coded test data to row per trial data                             ----
coded_test_data <- coded_test_data %>% filter(!(Participant_Private_ID %in% participants_to_kick_exp2$Participant_Private_ID))
##    Compute Test Total Scores                                               ----
coded_test_data <- calculate_test_total_scores(coded_test_data) %>% select(Experiment:Test_Total_Correct)

## Merge the cleaned raw data with the 
row_per_trial_data <- left_join(row_per_trial_data, coded_test_data) %>% select(-Response)
##    Save sub sample for Reactions Times only                                ----
reaction_time_data <- relevant_data %>% 
  distinct(Participant_Private_ID, Trial, Screen_Name, Reaction_Time, .keep_all = TRUE) %>%
  select(-c(Zone_Name, Zone_Type, Response)) %>% 
  distinct(Participant_Private_ID, Trial, Screen_Name, Reaction_Time, .keep_all = TRUE)
 
##    Merge coded diagram data to row per diagram box data                    ----
coded_diagram_irr_data <- irr_diagram_data_coder1 
colnames(coded_diagram_irr_data) <- gsub('_Coder1', '',colnames(coded_diagram_irr_data))
coded_diagram_irr_data <- coded_diagram_irr_data %>% select(-c(Event_Index, Opmerking, Remove))
coded_diagram_irr_data$Correct_Points[coded_diagram_irr_data$Correct_Points == "NA"] <- NA
coded_diagram_irr_data$Correct_Points <- as.numeric(coded_diagram_irr_data$Correct_Points)
diagram_data_exp2 <- left_join(coded_diagrams_without_irr_exp2, coded_diagram_irr_data) %>% 
  mutate(Experiment = 2)

## Load diagram data of experiment 1 and merge
coded_diagram_data_exp1 <- coded_diagram_data_exp1 %>%
  rename(Participant_Private_ID = ParticipantID,
         Correct_Points = CorrectPoints,
         Element_Number = ElementNumber,
         Word_Count = WordCount) %>% mutate(Experiment = 1)
# Merge all diagram data 
diagram_data <- full_join(coded_diagram_data_exp1,diagram_data_exp2)
diagram_data$Correct_Points[is.na(diagram_data$Correct_Points)] <- "NA"
##    Save those data files to output and analysis folder                     ----
today <- Sys.Date()
#write_delim(row_per_trial_data, paste0(output_path, "row_per_trial_data_",today,".txt" ))
#write_delim(row_per_trial_data, paste0(output_for_analysis, "row_per_trial_data_",today, ".txt"))
#write_delim(reaction_time_data, paste0(output_path, "reaction_time_data_",today, ".txt"))
#write_delim(reaction_time_data, paste0(output_for_analysis, "reaction_time_data_",today, ".txt"))

############################################################################# ----
