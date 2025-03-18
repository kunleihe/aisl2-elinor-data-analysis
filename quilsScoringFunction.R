### Packages ###
library(readxl)
library(openxlsx)
library(dplyr)
library(stringr)
library(tidyverse)

###########################
### Directory & Pathing ###
###########################

# User's personal directory
username <- Sys.info()[["user"]]

if (username == "jasetran") {
  user.dir <- "~/Documents/uci-digital-learning-lab/c2l-analysis/aisl2-analysis/"
} else if (username == "[PLACEHOLDER]") {
  user.dir <- "[PLACEHOLDER]"
} else {
  stop("Add your directory above.")
}

# Creating directory path variables
answers.dir <- paste0(user.dir, "answer-keys/")
raw.dir <- paste0(user.dir, "/data/raw/")
clean.dir <- paste0(user.dir, "/data/cleaned/")

######################
### Loading data #####
######################

# QUILS answer key
# Based on the answer key from QUILS user manual (pg. 97)
quils.answers <- read.csv(paste0(answers.dir, "quils-answer-key.csv"))

# Raw QUILS data files 
quils.p1.raw <- read.csv(paste0(raw.dir, "raw_quils_pt1.csv"))
quils.p2.raw <- read.csv(paste0(raw.dir, "raw_quils_pt2.csv"))

# Combining the dataframes
combined.df <- rbind(quils.p1.raw, quils.p2.raw) 

#################
### Functions ###
#################

## processQUILSData(df)
##### Description: This function takes in a dataframe of raw data from QUILS. The data is then processed 
##### and returned in dataframe. If there are duplicate runs then the higher score is taken. 
##### Output: 
#####     - Student.ID (char): Unique participant ID
#####     - First.Name (char)
#####     - Last.Name (char)
#####     - Age (num)
#####     - Last.Access (char)
#####     - rawScore (num): QUILS raw total score. Possible range is 0 to 48.
processQUILSData <- function(df) {

  long.df <- df %>% 
    filter(Status == "Completed" & 
             !grepl("test", First.Name, ignore.case = TRUE) &
             !grepl("test", Last.Name, ignore.case = TRUE)) %>%
    select(-User.Name, -Days.Since.Screening, 
           -Bookmark, -Status, -starts_with("PR")) %>%
    pivot_longer(!c(Student.ID, First.Name, Last.Name, Age, First.Access),
                 names_to = "question", values_to = "answer") %>%
    left_join(quils.answers, by="question") %>%
    mutate(correct = ifelse(answer == correctAnswer, 1, 0))
  
  scores.df <- long.df %>%
    group_by(questionID, Student.ID, First.Name, Last.Name, Age, First.Access) %>%
    mutate(sumCorrect = cumsum(correct),
           correctNew = ifelse(learningQuestion == "y" & sumCorrect == 2, 1, 0),
           correctNew = ifelse(learningQuestion == "n", sumCorrect, correctNew)) %>%
    group_by(Student.ID, First.Name, Last.Name, Age, First.Access) %>%
    dplyr::summarize(rawScore = cumsum(correctNew)) %>%
    slice_max(rawScore) %>%
    unique() %>%
    group_by(Student.ID, First.Name, Last.Name, Age) %>%
    slice_max(rawScore)
  
  print("QUILS scores processed! For duplicate runs, the higher score was selected.")
  return(scores.df)
  
}

##############################
### Saving Cleaned Dataset ###
##############################

# Applying the function
final.df <- processQUILSData(combined.df)

# Saving the dataframe
write.csv(final.df, paste0(clean.dir, "aisl2_quils_scores.csv"), row.names = FALSE)



