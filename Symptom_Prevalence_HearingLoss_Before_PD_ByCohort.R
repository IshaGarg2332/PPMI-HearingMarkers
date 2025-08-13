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
library("table1")


#The table shows how many people with a symptom had hearing loss before getting Parkinson’s
#It compares people who had hearing loss at least a year before Parkinson’s to people who didn’t have hearing loss
#It includes three groups: Healthy Controls, Prodromal, and Parkinson’s Disease
#A number greater than 1 means the symptom was more common in those with hearing loss; less than 1 means it was less likely


#Import data
Participant <- read_csv("data/ParticipantStatus.csv")
PDDiagHistory <- read.csv("data/PDDiagHistory.csv")
ParticipantStatus <- read_csv("data/ParticipantStatus.csv")
MedConditions <- read_csv("data/MedConditions.csv")

#Combine ParticipantStatus and DiagHistory
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)


#For each of these dataframes, the following is done:
#     Prepare participant data and merge diagnosis dates
#     Filter and rename data
#     Merge data with participant data
#     Filter and clean hearing loss
#     Keep only the ones where they were diagnosed with hearing loss at least a year prior to diagnosis of Parkinson's disease
#     Each one has a specific symptom and hearing loss data

#### Head Injury ####
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

HeadInjury <- read_csv("data/HeadInjury.csv")
HeadInjury <- read_csv("data/MedConditions.csv")
HeadInjury <- HeadInjury[grepl("(Concussion|concussion|concussions|skull fracture|Skull fracture|Head Injury|head injury|Head injury)",HeadInjury$MHTERM,ignore.case = F),]
tmp <- HeadInjury
colnames(tmp)[colnames(tmp) == "MHTERM"] <- "hiq1"
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

tmp2 <- merge(cohToTest,tmp,by="PATNO",all.x=T)

MedConditions <- read_csv("data/MedConditions.csv")
hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")
cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),F)
cohToTest$COHORT_DEFINITION <- as.factor(cohToTest$COHORT_DEFINITION)
HeadInjury <- cohToTest
rm("cohToTest")

#### Hypertension ####
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Hypertension <- read_csv("data/MedConditions.csv")
Hypertension <- Hypertension[grepl("(Hypertension|hypertension|High Blood Pressure|High blood pressure|high blood pressure|Elevated blood pressure)",Hypertension$MHTERM,ignore.case = F),]
tmp <- Hypertension
colnames(tmp)[colnames(tmp) == "MHTERM"] <- "hiq1"
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

tmp2 <- merge(cohToTest,tmp,by="PATNO",all.x=T)

MedConditions <- read_csv("data/MedConditions.csv")
hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")
cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),F)
cohToTest$COHORT_DEFINITION <- as.factor(cohToTest$COHORT_DEFINITION)
Hypertension <- cohToTest
rm("cohToTest")


#### Hypotension ####
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Hypotension <- read_csv("data/MedConditions.csv")
Hypotension <- Hypotension[grepl("(Orthostatis Hypotension|ORTHOSTATIC HYPOTENSION|orthostatic hypotension|Neurogenic Orthostatic Hypotension|Neurogenic orthostatic hypotension|Low Blood Pressure\\b|Syncope\\b)",Hypotension$MHTERM,ignore.case = F),]
tmp <- Hypotension
colnames(tmp)[colnames(tmp) == "MHTERM"] <- "hiq1"
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

tmp2 <- merge(cohToTest,tmp,by="PATNO",all.x=T)

MedConditions <- read_csv("data/MedConditions.csv")
hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")
cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),F)
cohToTest$COHORT_DEFINITION <- as.factor(cohToTest$COHORT_DEFINITION)
Hypotension <- cohToTest
rm("cohToTest")


#### Constipation ####
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Constipation <- read_csv("data/MedConditions.csv")
Constipation <- Constipation[grepl("(Constipation|Chronic Constipation|Occasional constipation|chronic constipation|constipation|CONSTIPATION|Intermittent Constipation|intermittent constipation|Chronic Constipation|Chronic Idiopathic Constipation|Mild Constipation|Chronic constipation|Intermittent constipation|severe constipation|Stomach cramps and dysmotility\\b)",Constipation$MHTERM,ignore.case = F),]
tmp <- Constipation
colnames(tmp)[colnames(tmp) == "MHTERM"] <- "hiq1"
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

tmp2 <- merge(cohToTest,tmp,by="PATNO",all.x=T)

MedConditions <- read_csv("data/MedConditions.csv")
hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")
cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),F)
cohToTest$COHORT_DEFINITION <- as.factor(cohToTest$COHORT_DEFINITION)
Constipation <- cohToTest
rm("cohToTest")


#### Urinary Problems ####
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Urinary <- read_csv("data/MedConditions.csv")
Urinary <- Urinary[grepl("(Urinary\\b|uria\\b|urination\\b|prostatic hypertrophy\\b|hyperplasia\\b)",Urinary$MHTERM,ignore.case = F),]
tmp <- Urinary
colnames(tmp)[colnames(tmp) == "MHTERM"] <- "hiq1"
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

tmp2 <- merge(cohToTest,tmp,by="PATNO",all.x=T)

MedConditions <- read_csv("data/MedConditions.csv")
hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")
cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),F)
cohToTest$COHORT_DEFINITION <- as.factor(cohToTest$COHORT_DEFINITION)
Urinary <- cohToTest
rm("cohToTest")


#### Sleep Problems ####
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Sleep <- read_csv("data/MedConditions.csv")
Sleep <- Sleep[grepl("(sleep\\b|sleeping|somnia\\b|restless leg\\b|dream\\b|nightmare\\b)",Sleep$MHTERM,ignore.case = T),]
tmp <- Sleep
colnames(tmp)[colnames(tmp) == "MHTERM"] <- "hiq1"
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

tmp2 <- merge(cohToTest,tmp,by="PATNO",all.x=T)

MedConditions <- read_csv("data/MedConditions.csv")
hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")
cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),F)
cohToTest$COHORT_DEFINITION <- as.factor(cohToTest$COHORT_DEFINITION)
Sleep <- cohToTest
rm("cohToTest")


#### Loss of Smell ####
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Smell <- read_csv("data/MedConditions.csv")
Smell <- Smell[grepl("(smell\\b|osmia\\b)",Smell$MHTERM,ignore.case = T),]
tmp <- Smell
colnames(tmp)[colnames(tmp) == "MHTERM"] <- "hiq1"
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

tmp2 <- merge(cohToTest,tmp,by="PATNO",all.x=T)

MedConditions <- read_csv("data/MedConditions.csv")
hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")
cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),F)
cohToTest$COHORT_DEFINITION <- as.factor(cohToTest$COHORT_DEFINITION)
Smell <- cohToTest
rm("cohToTest")


#### Cognitive Problems ####
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Cognitive <- read_csv("data/MedConditions.csv")
Cognitive <- Cognitive[grepl("(Cognitive\\b|Memory\\b|Alzheimer\\b|dementia\\b|forget\\b|)",Cognitive$MHTERM,ignore.case = T),]
tmp <- Cognitive
colnames(tmp)[colnames(tmp) == "MHTERM"] <- "hiq1"
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

tmp2 <- merge(cohToTest,tmp,by="PATNO",all.x=T)

MedConditions <- read_csv("data/MedConditions.csv")
hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")
cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),F)
cohToTest$COHORT_DEFINITION <- as.factor(cohToTest$COHORT_DEFINITION)
Cognitive <- cohToTest
rm("cohToTest")


#### Tremors ####
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Tremor <- read_csv("data/MedConditions.csv")
Tremor <- Tremor[grepl("(Tremor\\b|shaking\\b|twitch\\b|jerk\\b)",Tremor$MHTERM,ignore.case = T),]
tmp <- Tremor
colnames(tmp)[colnames(tmp) == "MHTERM"] <- "hiq1"
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

tmp2 <- merge(cohToTest,tmp,by="PATNO",all.x=T)

MedConditions <- read_csv("data/MedConditions.csv")
hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")
cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),F)
cohToTest$COHORT_DEFINITION <- as.factor(cohToTest$COHORT_DEFINITION)
Tremor <- cohToTest
rm("cohToTest")


#### Falls ####
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Falls <- read_csv("data/MedConditions.csv")
Falls <- Falls[grepl("(fall\\b|falling\\b|falls\\b)",Falls$MHTERM,ignore.case = T),]
tmp <- Falls
colnames(tmp)[colnames(tmp) == "MHTERM"] <- "hiq1"
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

tmp2 <- merge(cohToTest,tmp,by="PATNO",all.x=T)

MedConditions <- read_csv("data/MedConditions.csv")
hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")
cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),F)
cohToTest$COHORT_DEFINITION <- as.factor(cohToTest$COHORT_DEFINITION)
Falls <- cohToTest
rm("cohToTest")


#### Balance ####
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Balance <- read_csv("data/MedConditions.csv")
Balance <- Balance[grepl("(dizziness\\b|balance\\b|instability\\b|Unstable|Posture)",Balance$MHTERM,ignore.case = T),]
tmp <- Balance
colnames(tmp)[colnames(tmp) == "MHTERM"] <- "hiq1"
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

tmp2 <- merge(cohToTest,tmp,by="PATNO",all.x=T)

MedConditions <- read_csv("data/MedConditions.csv")
hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")
cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),F)
cohToTest$COHORT_DEFINITION <- as.factor(cohToTest$COHORT_DEFINITION)
Balance <- cohToTest
rm("cohToTest")


#### PTSD ####
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

PTSD <- read_csv("data/MedConditions.csv")
PTSD <- PTSD[grepl("(Post traumatic stress disorder\\b|PTSD\\b|TBI\\b|traumatic brain injury\\b)",PTSD$MHTERM,ignore.case = T),]
tmp <- PTSD
colnames(tmp)[colnames(tmp) == "MHTERM"] <- "hiq1"
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

tmp2 <- merge(cohToTest,tmp,by="PATNO",all.x=T)

MedConditions <- read_csv("data/MedConditions.csv")
hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")
cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),F)
cohToTest$COHORT_DEFINITION <- as.factor(cohToTest$COHORT_DEFINITION)
PTSD <- cohToTest
rm("cohToTest")


#### Diabetes ####
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Diabetes <- read_csv("data/MedConditions.csv")
Diabetes <- Diabetes[grepl("(Diabetes\\b|Diabetic\\b)",Diabetes$MHTERM,ignore.case = T),]
tmp <- Diabetes
colnames(tmp)[colnames(tmp) == "MHTERM"] <- "hiq1"
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

tmp2 <- merge(cohToTest,tmp,by="PATNO",all.x=T)

MedConditions <- read_csv("data/MedConditions.csv")
hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")
cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),F)
cohToTest$COHORT_DEFINITION <- as.factor(cohToTest$COHORT_DEFINITION)
Diabetes <- cohToTest
rm("cohToTest")


df <- data.frame(matrix(ncol = 4, nrow = 2))


#The function that will make the proportion table
graphAgainstHL <- function(hldf,colname) {
  cols_to_keep <- c("PATNO", "COHORT_DEFINITION", "EnrollDate", "HLDate", "PDdate", "hlDiff", "hlBefore",colname,"MHTERM")
  hldf <- hldf[,cols_to_keep]
  
  Par <- hldf
  Par1 <- hldf
  Par2 <- hldf
  Par3 <- hldf
  
  Par <- hldf[hldf$COHORT_DEFINITION %in% c("Parkinson's Disease"),]
  Par1 <- hldf[hldf$COHORT_DEFINITION %in% c("Healthy Control"),]
  Par2 <- hldf[hldf$COHORT_DEFINITION %in% c("Prodromal"),]
  Par3 <- hldf[hldf$COHORT_DEFINITION %in% c("SWEDD"),]
  
  names(Par)[names(Par) == "COHORT_DEFINITION"] <- "definition1"
  names(Par1)[names(Par1) == "COHORT_DEFINITION"] <- "definition2"
  names(Par2)[names(Par2) == "COHORT_DEFINITION"] <- "definition3"
  names(Par3)[names(Par3) == "COHORT_DEFINITION"] <- "definition4"
  
  #  rm("Participant", "HeadInjury", "MedConditions")
  
  Parkinson <- Par
  Healthy <- Par1
  SWEDD <- Par3
  Prodromal <- Par2
  
  Healthy$consecutive<-1:nrow(Healthy)
  Parkinson$consecutive<-1:nrow(Parkinson)
  Prodromal$consecutive<-1:nrow(Prodromal)
  
  
  names(Healthy)[names(Healthy) == "PATNO"] <- "patient1"
  names(Parkinson)[names(Parkinson) == "PATNO"] <- "patient2"
  names(Prodromal)[names(Prodromal) == "PATNO"] <- "patient3"
  
  names(Healthy)[names(Healthy) == colname] <- "Head1"
  names(Parkinson)[names(Parkinson) == colname] <- "Head2"
  names(Prodromal)[names(Prodromal) == colname] <- "Head3"
  
  names(Healthy)[names(Healthy) == "MHTERM"] <- "term1"
  names(Parkinson)[names(Parkinson) == "MHTERM"] <- "term2"
  names(Prodromal)[names(Prodromal) == "MHTERM"] <- "term3"
  
  
  Healthy <- Healthy %>%
    mutate(hlBefore=case_when(
      hlBefore == FALSE ~ "False",
      hlBefore == TRUE ~ "True",
    ))
  Healthy <- Healthy %>%
    mutate(Head1=case_when(
      is.na(Head1) ~ "No",
      !is.na(Head1) ~ "Yes"
    ))
  
  Parkinson <- Parkinson %>%
    mutate(hlBefore=case_when(
      hlBefore == FALSE ~ "False",
      hlBefore == TRUE ~ "True",
    ))
  Parkinson <- Parkinson %>%
    mutate(Head2=case_when(
      is.na(Head2) ~ "No",
      !is.na(Head2) ~ "Yes"
    ))
  
  Prodromal <- Prodromal %>%
    mutate(hlBefore=case_when(
      hlBefore == FALSE ~ "False",
      hlBefore == TRUE ~ "True",
    ))
  Prodromal <- Prodromal %>%
    mutate(Head3=case_when(
      is.na(Head3) ~ "No",
      !is.na(Head3) ~ "Yes"
    ))
  
  df <- data.frame(matrix(ncol = 4, nrow = 2))
  
  healthyRatio <- prop.table(table(Healthy$Head1,Healthy$hlBefore),2)["Yes","True"] / prop.table(table(Healthy$Head1,Healthy$hlBefore),2)["Yes","False"]
  ParkinsonRatio <- prop.table(table(Parkinson$Head2,Parkinson$hlBefore),2)["Yes","True"] / prop.table(table(Parkinson$Head2,Parkinson$hlBefore),2)["Yes","False"]
  ProdromalRatio <- prop.table(table(Prodromal$Head3,Prodromal$hlBefore),2)["Yes","True"] / prop.table(table(Prodromal$Head3,Prodromal$hlBefore),2)["Yes","False"]
  
  df[1,1] = "Yes"
  df[1, 2] = healthyRatio
  df[1, 3] = ProdromalRatio
  df[1, 4] = ParkinsonRatio
  
  healthyRatio1 <- prop.table(table(Healthy$Head1,Healthy$hlBefore),2)["No","True"] / prop.table(table(Healthy$Head1,Healthy$hlBefore),2)["No","False"]
  ParkinsonRatio1 <- prop.table(table(Parkinson$Head2,Parkinson$hlBefore),2)["No","True"] / prop.table(table(Parkinson$Head2,Parkinson$hlBefore),2)["No","False"]
  ProdromalRatio1 <- prop.table(table(Prodromal$Head3,Prodromal$hlBefore),2)["No","True"] / prop.table(table(Prodromal$Head3,Prodromal$hlBefore),2)["No","False"]
  
  df[2,1] = "No"
  df[2, 2] = healthyRatio1
  df[2, 3] = ProdromalRatio1
  df[2, 4] = ParkinsonRatio1
  
  rm("healthyRatio", "ParkinsonRatio", "ProdromalRatio", "healthyRatio1", "ParkinsonRatio1", "ProdromalRatio1")
  rm("Healthy", "Parkinson", "Prodromal", "SWEDD")
  
  names(df)[names(df) == "X1"] <- "Symptom"
  names(df)[names(df) == "X2"] <- "Healthy Control"
  names(df)[names(df) == "X3"] <- "Prodromal"
  names(df)[names(df) == "X4"] <- "Parkinson"
  
  library(gt)
  
  df %>%
    gt() %>%
    tab_header(
      title = "Proportion Table",
      subtitle = "Proportions of Hearing Loss to No Hearing Loss in Patients"
    ) %>%
    tab_style(
      style = cell_borders(
        sides = c("right"),
        weight = px(0.9)),
      locations = cells_body(
        columns = c(Parkinson)
      )
    )%>%
    tab_style(
      style = cell_borders(
        sides = c("all"),
        weight = px(0.9)),
      locations = cells_body(
        columns = c("Symptom")
      ))
}

graphAgainstHL(Constipation, "hiq1")