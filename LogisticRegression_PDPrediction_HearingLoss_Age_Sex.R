# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------

#Import libraries
library("readxl")
library("tidyr")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("readxl")
library("boot")
library("table1")
library("Hmisc")
library("sjmisc")


# The final result of this script is a logistic regression model that predicts the likelihood of having Parkinson’s Disease or Prodromal PD based on:
#     Whether the participant had hearing loss diagnosed at least one year prior to PD diagnosis
#     Their enrollment age
#     Their sex


#Import data
Participant <- read_csv("data/ParticipantStatus.csv")
PDDiagHistory <- read.csv("data/PDDiagHistory.csv")
Demo <- read_csv("data/Demographics.csv")

### Hearing Loss ###
#Import and mutate hearing loss data
MedConditions <- read_csv("data/MedConditions.csv")
hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

#Merge the two together
cohToTest1 <- merge(Demo,hloss,by="PATNO",all.x=T)
cohToTest2 <- merge(cohToTest1,Participant,by="PATNO",all.x=T)

#Keep only the ones where they were diagnosed with hearing loss at least a year prior to diagnosis of Parkinson's disease
cohToTest2$EnrollDate <- as.Date(paste0("01/",cohToTest2$ENROLL_DATE),format="%d/%m/%Y")
cohToTest2$HLDate <- as.Date(paste0("01/",cohToTest2$MHDIAGDT),format="%d/%m/%Y")
cohToTest2$PDdate <- as.Date(paste0("01/",cohToTest2$PDDXDT),format="%d/%m/%Y")
cohToTest2$hlDiff <- ifelse(is.na(cohToTest2$PDdate),cohToTest2$EnrollDate - cohToTest2$HLDate,cohToTest2$PDdate - cohToTest2$HLDate)
cohToTest2$hlBefore <- ifelse(!is.na(cohToTest2$MHTERM),ifelse(cohToTest2$hlDiff > (-365 * 1) ,T,F),F)

#Keep certain columns
Tes1 <- select(cohToTest2, c(PATNO, SEX, COHORT_DEFINITION, ENROLL_AGE, hlBefore))

#Mutate Parkinson's status
Tes2 <- Tes1 %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ TRUE,
    COHORT_DEFINITION == "Prodromal" ~ TRUE,
    COHORT_DEFINITION == "Healthy Control" ~ FALSE,
    COHORT_DEFINITION == "SWEDD" ~ FALSE
  ))
### end ###

#This is a logistic regression model that tries to predict whether someone has Parkinson's disease based on hearing loss, age, and sex
Tes2 <- Tes2[!is.na(Tes2$hlBefore),]
test <- glm(ID ~ as.numeric(hlBefore) + ENROLL_AGE + SEX, data = Tes2, family = "binomial")
summary(test)
exp(coef(test))