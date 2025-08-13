# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------

#Import libraries
library("dplyr")
library("tidyverse")
library("ggplot2")
library("survival")
library("readxl")

# Gives a chart of the total count of hearing loss and no hearing loss patients filtered into if they have hypotension, constipation, urinary problems, sleeping problems, smell loss, cognitive problems, tremors, falls, unstability, head injuries, PTSD, diabetes, or hypertension


#Load in the csvs
ParticipantStatus <- read_csv("data/ParticipantStatus.csv")
MedConditions <- read_csv("data/MedConditions.csv")
PDDiagHistory <- read.csv("data/PDDiagHistory.csv")
Demo <- read_csv("data/Demographics.csv")

#Combine ParticipantStatus and DiagHistory
cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=TRUE)

#Make datasets with only the relevant information and with only the medical conditions as listed
#### Hearing Loss ####
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=TRUE)

hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

cohToTest1 <- merge(cohToTest1,hloss,by="PATNO",all.x=TRUE)
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$hlBefore <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

cohToTest1$bilat <- ifelse(grepl("(bilateral|b\\/l|both ears)",cohToTest1$MHTERM,ignore.case=T),T,F)
cohToTest1$unilat <- ifelse(grepl("(asymm|left|right|unilat)",cohToTest1$MHTERM,ignore.case=T),T,F)
cohToTest1$loss <- ifelse(grepl("(loss)",cohToTest1$MHTERM,ignore.case=T),T,F)
cohToTest1$impair <- ifelse(grepl("(impair|defici)",cohToTest1$MHTERM,ignore.case=T),T,F)
cohToTest1$decr <- ifelse(grepl("(decreas|reduc|hardness)",cohToTest1$MHTERM,ignore.case=T),T,F)
cohToTest1$anyHL <- ifelse(cohToTest1$bilat | cohToTest1$unilat | cohToTest1$loss | cohToTest1$impair | cohToTest1$decr,T,F)
cohToTest1$aid <- ifelse(grepl("(aid|device)",cohToTest1$MHTERM,ignore.case=T),T,F)
cohToTest1$sensori <- ifelse(grepl("(sensory|sensori)",cohToTest1$MHTERM,ignore.case=T),T,F)
cohToTest1$slight <- ifelse(grepl("(slight|mild|partial)",cohToTest1$MHTERM,ignore.case=T),T,F)
cohToTest1$moder <- ifelse(grepl("(moderate|chronic)",cohToTest1$MHTERM,ignore.case=T),T,F)
cohToTest1$tinnitus <- ifelse(grepl("(tinitis|tinnitus)",cohToTest1$MHTERM,ignore.case=T),T,F)

cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=TRUE)
HearingLoss <- select(cohToTest1, c(SEX, ENROLL_AGE, hlBefore, COHORT_DEFINITION, PATNO, HLDate, PDdate, hlDiff))
HearingLoss <- HearingLoss[!is.na(HearingLoss$hlBefore),]
rm("hloss")

HearingLoss$consecutive<-1:nrow(HearingLoss)
HearingLoss <- HearingLoss[-c(13), ]
dim(HearingLoss[duplicated(HearingLoss$PATNO),])[1]


#### Head Injury ####
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

HeadInjury <- MedConditions[grepl("(Concussion\\b|Skull fracture\\b|head injury\\b|loss of consciousness\\b)",MedConditions$MHTERM,ignore.case = T),]
HeadInjury <- HeadInjury[!is.na(HeadInjury$MHDIAGDT),]
HeadInjury$MHTERM <- tolower(HeadInjury$MHTERM)
HeadInjury$MHTERM <- str_squish(HeadInjury$MHTERM)

cohToTest1 <- merge(cohToTest1,HeadInjury,by="PATNO",all.x=T)
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$HeadBefore <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=T)
HeadInjury <- select(cohToTest1, c(HLDate, PDdate, hlDiff, SEX, ENROLL_AGE, HeadBefore, COHORT_DEFINITION, PATNO))
HeadInjury <- HeadInjury[!is.na(HeadInjury$HeadBefore),]

HeadInjury$consecutive<-1:nrow(HeadInjury)
HeadInjury = HeadInjury[!duplicated(HeadInjury$PATNO),]
dim(HeadInjury[duplicated(HeadInjury$PATNO),])[1]


#### Hypertension ####
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Hypertension <- read_csv("data/MedConditions.csv")
Hypertension <- Hypertension[grepl("(Hypertension|hypertension|High Blood Pressure|High blood pressure|high blood pressure|Elevated blood pressure)",Hypertension$MHTERM,ignore.case = F),]
Hypertension <- Hypertension[!is.na(Hypertension$MHDIAGDT),]
Hypertension$MHTERM <- tolower(Hypertension$MHTERM)
Hypertension$MHTERM <- str_squish(Hypertension$MHTERM)

cohToTest1 <- merge(cohToTest1,Hypertension,by="PATNO",all.x=T)
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$HyperBefore <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=T)
Hypertension <- select(cohToTest1, c(HLDate, PDdate, hlDiff, SEX, ENROLL_AGE, HyperBefore, COHORT_DEFINITION, PATNO))
Hypertension <- Hypertension[!is.na(Hypertension$HyperBefore),]

Hypertension$consecutive<-1:nrow(Hypertension)
Hypertension = Hypertension[!duplicated(Hypertension$PATNO),]
dim(Hypertension[duplicated(Hypertension$PATNO),])[1]


#### Hypotension ####
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Hypotension <- read_csv("data/MedConditions.csv")
Hypotension <- Hypotension[grepl("(Orthostatis Hypotension|ORTHOSTATIC HYPOTENSION|orthostatic hypotension|Neurogenic Orthostatic Hypotension|Neurogenic orthostatic hypotension|Low Blood Pressure\\b|Syncope\\b)",Hypotension$MHTERM,ignore.case = F),]
Hypotension <- Hypotension[!is.na(Hypotension$MHDIAGDT),]
Hypotension$MHTERM <- tolower(Hypotension$MHTERM)
Hypotension$MHTERM <- str_squish(Hypotension$MHTERM)

cohToTest1 <- merge(cohToTest1,Hypotension,by="PATNO",all.x=T)
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$Hypo <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=T)
Hypotension <- select(cohToTest1, c(HLDate, PDdate, hlDiff, SEX, ENROLL_AGE, Hypo, COHORT_DEFINITION, PATNO))
Hypotension <- Hypotension[!is.na(Hypotension$Hypo),]

Hypotension$consecutive<-1:nrow(Hypotension)
Hypotension = Hypotension[!duplicated(Hypotension$PATNO),]
dim(Hypotension[duplicated(Hypotension$PATNO),])[1]


#### Constipation ####
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Constipation <- read_csv("data/MedConditions.csv")
Constipation <- Constipation[grepl("(Constipation|Chronic Constipation|Occasional constipation|chronic constipation|constipation|CONSTIPATION|Intermittent Constipation|intermittent constipation|Chronic Constipation|Chronic Idiopathic Constipation|Mild Constipation|Chronic constipation|Intermittent constipation|severe constipation|Stomach cramps and dysmotility\\b)",Constipation$MHTERM,ignore.case = F),]
Constipation <- Constipation[!is.na(Constipation$MHDIAGDT),]
Constipation$MHTERM <- tolower(Constipation$MHTERM)
Constipation$MHTERM <- str_squish(Constipation$MHTERM)

cohToTest1 <- merge(cohToTest1,Constipation,by="PATNO",all.x=T)
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$Consti <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=T)
Constipation <- select(cohToTest1, c(HLDate, PDdate, hlDiff, SEX, ENROLL_AGE, Consti, COHORT_DEFINITION, PATNO))
Constipation <- Constipation[!is.na(Constipation$Consti),]

Constipation$consecutive<-1:nrow(Constipation)
Constipation = Constipation[!duplicated(Constipation$PATNO),]
dim(Constipation[duplicated(Constipation$PATNO),])[1]


#### Urinary Problems ####
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Urinary <- read_csv("data/MedConditions.csv")
Urinary <- Urinary[grepl("(Urinary\\b|uria\\b|urination\\b|prostatic hypertrophy\\b|hyperplasia\\b)",Urinary$MHTERM,ignore.case = F),]
Urinary <- Urinary[!is.na(Urinary$MHDIAGDT),]
Urinary$MHTERM <- tolower(Urinary$MHTERM)
Urinary$MHTERM <- str_squish(Urinary$MHTERM)

cohToTest1 <- merge(cohToTest1,Urinary,by="PATNO",all.x=T)
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$Uri <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=T)
Urinary <- select(cohToTest1, c(HLDate, PDdate, hlDiff, SEX, ENROLL_AGE, Uri, COHORT_DEFINITION, PATNO))
Urinary <- Urinary[!is.na(Urinary$Uri),]

Urinary$consecutive<-1:nrow(Urinary)
Urinary = Urinary[!duplicated(Urinary$PATNO),]
dim(Urinary[duplicated(Urinary$PATNO),])[1]


#### Sleep Problems ####
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Sleep <- read_csv("data/MedConditions.csv")
Sleep <- Sleep[grepl("(sleep\\b|sleeping|somnia\\b|restless leg\\b|dream\\b|nightmare\\b)",Sleep$MHTERM,ignore.case = T),]
Sleep <- Sleep[!is.na(Sleep$MHDIAGDT),]
Sleep$MHTERM <- tolower(Sleep$MHTERM)
Sleep$MHTERM <- str_squish(Sleep$MHTERM)

cohToTest1 <- merge(cohToTest1,Sleep,by="PATNO",all.x=T)
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$sleep <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=T)
Sleep <- select(cohToTest1, c(HLDate, PDdate, hlDiff, SEX, ENROLL_AGE, sleep, COHORT_DEFINITION, PATNO))
Sleep <- Sleep[!is.na(Sleep$sleep),]

Sleep$consecutive<-1:nrow(Sleep)
Sleep = Sleep[!duplicated(Sleep$PATNO),]
dim(Sleep[duplicated(Sleep$PATNO),])[1]


#### Loss of Smell ####
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Smell <- read_csv("data/MedConditions.csv")
Smell <- Smell[grepl("(smell\\b|osmia\\b)",Smell$MHTERM,ignore.case = T),]
Smell <- Smell[!is.na(Smell$MHDIAGDT),]
Smell$MHTERM <- tolower(Smell$MHTERM)
Smell$MHTERM <- str_squish(Smell$MHTERM)

cohToTest1 <- merge(cohToTest1,Smell,by="PATNO",all.x=T)
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$smell <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=T)
Smell <- select(cohToTest1, c(HLDate, PDdate, hlDiff, SEX, ENROLL_AGE, smell, COHORT_DEFINITION, PATNO))
Smell <- Smell[!is.na(Smell$smell),]

Smell$Smell<-1:nrow(Smell)
Smell = Smell[!duplicated(Smell$PATNO),]
dim(Smell[duplicated(Smell$PATNO),])[1]

#### Cognitive Problems ####
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Cognitive <- read_csv("data/MedConditions.csv")
Cognitive <- Cognitive[grepl("(Cognitive\\b|Memory\\b|Alzheimer\\b|dementia\\b|forget\\b)",Cognitive$MHTERM,ignore.case = T),]
Cognitive <- Cognitive[!is.na(Cognitive$MHDIAGDT),]
Cognitive$MHTERM <- tolower(Cognitive$MHTERM)
Cognitive$MHTERM <- str_squish(Cognitive$MHTERM)

cohToTest1 <- merge(cohToTest1,Cognitive,by="PATNO",all.x=T)
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$cog <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=T)
Cognitive <- select(cohToTest1, c(HLDate, PDdate, hlDiff, SEX, ENROLL_AGE, cog, COHORT_DEFINITION, PATNO))
Cognitive <- Cognitive[!is.na(Cognitive$cog),]

Cognitive$consecutive<-1:nrow(Cognitive)
Cognitive <- Cognitive[-c(15), ]
dim(Cognitive[duplicated(Cognitive$PATNO),])[1]


#### Tremors ####
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Tremor <- read_csv("data/MedConditions.csv")
Tremor <- Tremor[grepl("(Tremor\\b|shaking\\b|twitch\\b|jerk\\b)",Tremor$MHTERM,ignore.case = T),]
Tremor <- Tremor[!is.na(Tremor$MHDIAGDT),]
Tremor$MHTERM <- tolower(Tremor$MHTERM)
Tremor$MHTERM <- str_squish(Tremor$MHTERM)

cohToTest1 <- merge(cohToTest1,Tremor,by="PATNO",all.x=T)
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$tremors <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=T)
Tremor <- select(cohToTest1, c(HLDate, PDdate, hlDiff, SEX, ENROLL_AGE, tremors, COHORT_DEFINITION, PATNO))
Tremor <- Tremor[!is.na(Tremor$tremors),]

Tremor$Tremor<-1:nrow(Tremor)
Tremor = Tremor[!duplicated(Tremor$PATNO),]
dim(Tremor[duplicated(Tremor$PATNO),])[1]


#### Falls ####
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Falls <- read_csv("data/MedConditions.csv")
Falls <- Falls[grepl("(fall\\b|falling\\b|falls\\b)",Falls$MHTERM,ignore.case = T),]
Falls <- Falls[!is.na(Falls$MHDIAGDT),]
Falls$MHTERM <- tolower(Falls$MHTERM)
Falls$MHTERM <- str_squish(Falls$MHTERM)

cohToTest1 <- merge(cohToTest1,Falls,by="PATNO",all.x=T)
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$fall <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=T)
Falls <- select(cohToTest1, c(HLDate, PDdate, hlDiff, SEX, ENROLL_AGE, fall, COHORT_DEFINITION, PATNO))
Falls <- Falls[!is.na(Falls$fall),]

Falls$consecutive<-1:nrow(Falls)
Falls = Falls[!duplicated(Falls$PATNO),]
dim(Falls[duplicated(Falls$PATNO),])[1]


#### Balance ####
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Balance <- read_csv("data/MedConditions.csv")
Balance <- Balance[grepl("(dizziness\\b|balance\\b|instability\\b|Unstable|Posture)",Balance$MHTERM,ignore.case = T),]
Balance <- Balance[!is.na(Balance$MHDIAGDT),]
Balance$MHTERM <- tolower(Balance$MHTERM)
Balance$MHTERM <- str_squish(Balance$MHTERM)

cohToTest1 <- merge(cohToTest1,Balance,by="PATNO",all.x=T)
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$dizzy <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=T)
Balance <- select(cohToTest1, c(HLDate, PDdate, hlDiff, SEX, ENROLL_AGE, dizzy, COHORT_DEFINITION, PATNO))
Balance <- Balance[!is.na(Balance$dizzy),]

Balance$consecutive<-1:nrow(Balance)
Balance = Balance[!duplicated(Balance$PATNO),]
dim(Balance[duplicated(Balance$PATNO),])[1]


#### PTSD ####
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

PTSD <- read_csv("data/MedConditions.csv")
PTSD <- PTSD[grepl("(Post traumatic stress disorder\\b|PTSD\\b|TBI\\b|traumatic brain injury\\b)",PTSD$MHTERM,ignore.case = T),]
PTSD <- PTSD[!is.na(PTSD$MHDIAGDT),]
PTSD$MHTERM <- tolower(PTSD$MHTERM)
PTSD$MHTERM <- str_squish(PTSD$MHTERM)

cohToTest1 <- merge(cohToTest1,PTSD,by="PATNO",all=T)
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$trauma <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=T)
PTSD <- select(cohToTest1, c(HLDate, PDdate, hlDiff, SEX, ENROLL_AGE, trauma, COHORT_DEFINITION, PATNO))
PTSD <- PTSD[!is.na(PTSD$trauma),]

PTSD$consecutive<-1:nrow(PTSD)
PTSD = PTSD[!duplicated(PTSD$PATNO),]
dim(PTSD[duplicated(PTSD$PATNO),])[1]


#### Diabetes ####
cohToTest1 <- ParticipantStatus
cohToTest1 <- cohToTest1[!is.na(cohToTest1$ENROLL_AGE),]
cohToTest1$EnrollDate <- as.Date(paste0("01/",cohToTest1$ENROLL_DATE),format="%d/%m/%Y")
cohToTest1 <- merge(cohToTest1,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Diabetes <- read_csv("data/MedConditions.csv")
Diabetes <- Diabetes[grepl("(Diabetes\\b|Diabetic\\b)",Diabetes$MHTERM,ignore.case = T),]
Diabetes <- Diabetes[!is.na(Diabetes$MHDIAGDT),]
Diabetes$MHTERM <- tolower(Diabetes$MHTERM)
Diabetes$MHTERM <- str_squish(Diabetes$MHTERM)

cohToTest1 <- merge(cohToTest1,Diabetes,by="PATNO",all.x=T)
cohToTest1$HLDate <- as.Date(paste0("01/",cohToTest1$MHDIAGDT),format="%d/%m/%Y")
cohToTest1$PDdate <- as.Date(paste0("01/",cohToTest1$PDDXDT),format="%d/%m/%Y")
cohToTest1$hlDiff <- ifelse(is.na(cohToTest1$PDdate),cohToTest1$EnrollDate - cohToTest1$HLDate,cohToTest1$PDdate - cohToTest1$HLDate)
cohToTest1$diabetic <- ifelse(!is.na(cohToTest1$MHTERM),ifelse(cohToTest1$hlDiff > (-365 * 1) ,T,F),NA)

cohToTest1$AgeCut <- cut(cohToTest1$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest1$HLBfCut <- cut(as.numeric(cohToTest1$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest1$ID <- ifelse(grepl("Park|prodro",cohToTest1$COHORT_DEFINITION,ignore.case=T),T,F)
cohToTest1 <- merge(cohToTest1,Demo,by="PATNO",all.x=T)
Diabetes <- select(cohToTest1, c(HLDate, PDdate, hlDiff, SEX, ENROLL_AGE, diabetic, COHORT_DEFINITION, PATNO))
Diabetes <- Diabetes[!is.na(Diabetes$diabetic),]

Diabetes$consecutive<-1:nrow(Diabetes)
Diabetes = Diabetes[!duplicated(Diabetes$PATNO),]
dim(Diabetes[duplicated(Diabetes$PATNO),])[1]


#### Main Code ####
#Remove unnecessary dataframes
rm("ParticipantStatus", "MedConditions", "PDDiagHistory", "Demo", "cohToTest1")

#Select only certain columns in the medical condition specific datasets
PTSD <- select(PTSD, c(trauma, PATNO, COHORT_DEFINITION))
Constipation <- select(Constipation, c(Consti, PATNO, COHORT_DEFINITION))
Diabetes <- select(Diabetes, c(diabetic, PATNO, COHORT_DEFINITION))
Falls <- select(Falls, c(fall, PATNO, COHORT_DEFINITION))
HeadInjury <- select(HeadInjury, c(HeadBefore, PATNO, COHORT_DEFINITION))
HearingLoss <- select(HearingLoss, c(hlBefore, PATNO, COHORT_DEFINITION))
Hypertension <- select(Hypertension, c(HyperBefore, PATNO, COHORT_DEFINITION))
Hypotension <- select(Hypotension, c(Hypo, PATNO, COHORT_DEFINITION))
Cognitive <- select(Cognitive, c(cog, PATNO, COHORT_DEFINITION))
Sleep <- select(Sleep, c(sleep, PATNO, COHORT_DEFINITION))
Smell <- select(Smell, c(smell, PATNO, COHORT_DEFINITION))
Tremor <- select(Tremor, c(tremors, PATNO, COHORT_DEFINITION))
Urinary <- select(Urinary, c(Uri, PATNO, COHORT_DEFINITION))
Balance <- select(Balance, c(dizzy, PATNO, COHORT_DEFINITION))

#Merge all dataasets with hearing loss dataset
HeadInjury <- merge(HearingLoss, HeadInjury, by=c("PATNO", "COHORT_DEFINITION"),all.x=TRUE, all.y=TRUE)
Diabetes <- merge(HearingLoss,Diabetes,by=c("PATNO", "COHORT_DEFINITION"),all=T)
Constipation <- merge(HearingLoss,Constipation,by=c("PATNO", "COHORT_DEFINITION"),all=T)
Falls <- merge(HearingLoss,Falls,by=c("PATNO", "COHORT_DEFINITION"),all=T)
Hypertension <- merge(HearingLoss,Hypertension,by=c("PATNO", "COHORT_DEFINITION"),all=T)
Hypotension <- merge(HearingLoss,Hypotension,by=c("PATNO", "COHORT_DEFINITION"),all=T)
Cognitive <- merge(HearingLoss,Cognitive,by=c("PATNO", "COHORT_DEFINITION"),all=T)
Sleep <- merge(HearingLoss,Sleep,by=c("PATNO", "COHORT_DEFINITION"),all=T)
Smell <- merge(HearingLoss,Smell,by=c("PATNO", "COHORT_DEFINITION"),all=T)
PTSD <- merge(HearingLoss,PTSD,by=c("PATNO", "COHORT_DEFINITION"),all=T)
Tremor <- merge(HearingLoss,Tremor,by=c("PATNO", "COHORT_DEFINITION"),all=T)
Urinary <- merge(HearingLoss,Urinary,by=c("PATNO", "COHORT_DEFINITION"),all=T)
Balance <- merge(HearingLoss,Balance,by=c("PATNO", "COHORT_DEFINITION"),all=T)

#### Remove Healthy Control and SWEDD ####
Constipation <- Constipation[grepl("Parkinson's Disease|Prodromal", Constipation$COHORT_DEFINITION),]
Diabetes <- Diabetes[grepl("Parkinson's Disease|Prodromal", Diabetes$COHORT_DEFINITION),]
Falls <- Falls[grepl("Parkinson's Disease|Prodromal", Falls$COHORT_DEFINITION),]
Hypertension <- Hypertension[grepl("Parkinson's Disease|Prodromal", Hypertension$COHORT_DEFINITION),]
Hypotension <- Hypotension[grepl("Parkinson's Disease|Prodromal", Hypotension$COHORT_DEFINITION),]
Cognitive <- Cognitive[grepl("Parkinson's Disease|Prodromal", Cognitive$COHORT_DEFINITION),]
Sleep <- Sleep[grepl("Parkinson's Disease|Prodromal", Sleep$COHORT_DEFINITION),]
Smell <- Smell[grepl("Parkinson's Disease|Prodromal", Smell$COHORT_DEFINITION),]
PTSD <- PTSD[grepl("Parkinson's Disease|Prodromal", PTSD$COHORT_DEFINITION),]
Tremor <- Tremor[grepl("Parkinson's Disease|Prodromal", Tremor$COHORT_DEFINITION),]
Urinary <- Urinary[grepl("Parkinson's Disease|Prodromal", Urinary$COHORT_DEFINITION),]
Balance <- Balance[grepl("Parkinson's Disease|Prodromal", Balance$COHORT_DEFINITION),]
HeadInjury <- HeadInjury[grepl("Parkinson's Disease|Prodromal", HeadInjury$COHORT_DEFINITION),]

#### Counting up the Columns ####
#Making a column for each factor:
#      Medical condition is true and hearing loss is true
#      Medical condition is true and hearing loss is false
#      Medical condition is false and hearing loss is true
#      Medical condition is false and hearing loss is false


HeadInjury <- HeadInjury %>%
  mutate(TrueTrue = case_when(
    hlBefore == TRUE & HeadBefore == TRUE ~ 1
  )) %>%
  mutate(FalseTrue = case_when(
    hlBefore == FALSE & HeadBefore == TRUE ~ 1
  )) %>%
  mutate(TrueFalse = case_when(
    hlBefore == TRUE & HeadBefore == FALSE ~ 1
  )) %>%
  mutate(FalseFalse = case_when(
    hlBefore == FALSE & HeadBefore == FALSE ~ 1
  ))
HeadInjury[is.na(HeadInjury)] <- 0

Balance <- Balance %>%
  mutate(TrueTrue = case_when(
    hlBefore == TRUE & dizzy == TRUE ~ 1
  )) %>%
  mutate(FalseTrue = case_when(
    hlBefore == FALSE & dizzy == TRUE ~ 1
  )) %>%
  mutate(TrueFalse = case_when(
    hlBefore == TRUE & dizzy == FALSE ~ 1
  )) %>%
  mutate(FalseFalse = case_when(
    hlBefore == FALSE & dizzy == FALSE ~ 1
  ))
Balance[is.na(Balance)] <- 0

Cognitive <- Cognitive %>%
  mutate(TrueTrue = case_when(
    hlBefore == TRUE & cog == TRUE ~ 1
  )) %>%
  mutate(FalseTrue = case_when(
    hlBefore == FALSE & cog == TRUE ~ 1
  )) %>%
  mutate(TrueFalse = case_when(
    hlBefore == TRUE & cog == FALSE ~ 1
  )) %>%
  mutate(FalseFalse = case_when(
    hlBefore == FALSE & cog == FALSE ~ 1
  ))
Cognitive[is.na(Cognitive)] <- 0

Constipation <- Constipation %>%
  mutate(TrueTrue = case_when(
    hlBefore == TRUE & Consti == TRUE ~ 1
  )) %>%
  mutate(FalseTrue = case_when(
    hlBefore == FALSE & Consti == TRUE ~ 1
  )) %>%
  mutate(TrueFalse = case_when(
    hlBefore == TRUE & Consti == FALSE ~ 1
  )) %>%
  mutate(FalseFalse = case_when(
    hlBefore == FALSE & Consti == FALSE ~ 1
  ))
Constipation[is.na(Constipation)] <- 0

Diabetes <- Diabetes %>%
  mutate(TrueTrue = case_when(
    hlBefore == TRUE & diabetic == TRUE ~ 1
  )) %>%
  mutate(FalseTrue = case_when(
    hlBefore == FALSE & diabetic == TRUE ~ 1
  )) %>%
  mutate(TrueFalse = case_when(
    hlBefore == TRUE & diabetic == FALSE ~ 1
  )) %>%
  mutate(FalseFalse = case_when(
    hlBefore == FALSE & diabetic == FALSE ~ 1
  ))
Diabetes[is.na(Diabetes)] <- 0

Falls <- Falls %>%
  mutate(TrueTrue = case_when(
    hlBefore == TRUE & fall == TRUE ~ 1
  )) %>%
  mutate(FalseTrue = case_when(
    hlBefore == FALSE & fall == TRUE ~ 1
  )) %>%
  mutate(TrueFalse = case_when(
    hlBefore == TRUE & fall == FALSE ~ 1
  )) %>%
  mutate(FalseFalse = case_when(
    hlBefore == FALSE & fall == FALSE ~ 1
  ))
Falls[is.na(Falls)] <- 0

Hypertension <- Hypertension %>%
  mutate(TrueTrue = case_when(
    hlBefore == TRUE & HyperBefore == TRUE ~ 1
  )) %>%
  mutate(FalseTrue = case_when(
    hlBefore == FALSE & HyperBefore == TRUE ~ 1
  )) %>%
  mutate(TrueFalse = case_when(
    hlBefore == TRUE & HyperBefore == FALSE ~ 1
  )) %>%
  mutate(FalseFalse = case_when(
    hlBefore == FALSE & HyperBefore == FALSE ~ 1
  ))
Hypertension[is.na(Hypertension)] <- 0

Hypotension <- Hypotension %>%
  mutate(TrueTrue = case_when(
    hlBefore == TRUE & Hypo == TRUE ~ 1
  )) %>%
  mutate(FalseTrue = case_when(
    hlBefore == FALSE & Hypo == TRUE ~ 1
  )) %>%
  mutate(TrueFalse = case_when(
    hlBefore == TRUE & Hypo == FALSE ~ 1
  )) %>%
  mutate(FalseFalse = case_when(
    hlBefore == FALSE & Hypo == FALSE ~ 1
  ))
Hypotension[is.na(Hypotension)] <- 0

PTSD <- PTSD %>%
  mutate(TrueTrue = case_when(
    hlBefore == TRUE & trauma == TRUE ~ 1
  )) %>%
  mutate(FalseTrue = case_when(
    hlBefore == FALSE & trauma == TRUE ~ 1
  )) %>%
  mutate(TrueFalse = case_when(
    hlBefore == TRUE & trauma == FALSE ~ 1
  )) %>%
  mutate(FalseFalse = case_when(
    hlBefore == FALSE & trauma == FALSE ~ 1
  ))
PTSD[is.na(PTSD)] <- 0

Sleep <- Sleep %>%
  mutate(TrueTrue = case_when(
    hlBefore == TRUE & sleep == TRUE ~ 1
  )) %>%
  mutate(FalseTrue = case_when(
    hlBefore == FALSE & sleep == TRUE ~ 1
  )) %>%
  mutate(TrueFalse = case_when(
    hlBefore == TRUE & sleep == FALSE ~ 1
  )) %>%
  mutate(FalseFalse = case_when(
    hlBefore == FALSE & sleep == FALSE ~ 1
  ))
Sleep[is.na(Sleep)] <- 0

Smell <- Smell %>%
  mutate(TrueTrue = case_when(
    hlBefore == TRUE & smell == TRUE ~ 1
  )) %>%
  mutate(FalseTrue = case_when(
    hlBefore == FALSE & smell == TRUE ~ 1
  )) %>%
  mutate(TrueFalse = case_when(
    hlBefore == TRUE & smell == FALSE ~ 1
  )) %>%
  mutate(FalseFalse = case_when(
    hlBefore == FALSE & smell == FALSE ~ 1
  ))
Smell[is.na(Smell)] <- 0

Tremor <- Tremor %>%
  mutate(TrueTrue = case_when(
    hlBefore == TRUE & tremors == TRUE ~ 1
  )) %>%
  mutate(FalseTrue = case_when(
    hlBefore == FALSE & tremors == TRUE ~ 1
  )) %>%
  mutate(TrueFalse = case_when(
    hlBefore == TRUE & tremors == FALSE ~ 1
  )) %>%
  mutate(FalseFalse = case_when(
    hlBefore == FALSE & tremors == FALSE ~ 1
  ))
Tremor[is.na(Tremor)] <- 0

Urinary <- Urinary %>%
  mutate(TrueTrue = case_when(
    hlBefore == TRUE & Uri == TRUE ~ 1
  )) %>%
  mutate(FalseTrue = case_when(
    hlBefore == FALSE & Uri == TRUE ~ 1
  )) %>%
  mutate(TrueFalse = case_when(
    hlBefore == TRUE & Uri == FALSE ~ 1
  )) %>%
  mutate(FalseFalse = case_when(
    hlBefore == FALSE & Uri == FALSE ~ 1
  ))
Urinary[is.na(Urinary)] <- 0

#### Add them together ####
#Add up the values in each column per dataset and making a new dataset out of them
HeadInjury1 <- data.frame(
  TrueTrue = sum(HeadInjury$TrueTrue),
  TrueFalse = sum(HeadInjury$TrueFalse),
  FalseTrue = sum(HeadInjury$FalseTrue),
  FalseFalse =sum(HeadInjury$FalseFalse),
  Identity = "HeadInjury"
)
Balance1 <- data.frame(
  TrueTrue = sum(Balance$TrueTrue),
  TrueFalse = sum(Balance$TrueFalse),
  FalseTrue = sum(Balance$FalseTrue),
  FalseFalse =sum(Balance$FalseFalse),
  Identity = "Balance"
)
Cognitive1 <- data.frame(
  TrueTrue = sum(Cognitive$TrueTrue),
  TrueFalse = sum(Cognitive$TrueFalse),
  FalseTrue = sum(Cognitive$FalseTrue),
  FalseFalse =sum(Cognitive$FalseFalse),
  Identity = "Cognitive"
)
Constipation1 <- data.frame(
  TrueTrue = sum(Constipation$TrueTrue),
  TrueFalse = sum(Constipation$TrueFalse),
  FalseTrue = sum(Constipation$FalseTrue),
  FalseFalse =sum(Constipation$FalseFalse),
  Identity = "Constipation"
)
Diabetes1 <- data.frame(
  TrueTrue = sum(Diabetes$TrueTrue),
  TrueFalse = sum(Diabetes$TrueFalse),
  FalseTrue = sum(Diabetes$FalseTrue),
  FalseFalse =sum(Diabetes$FalseFalse),
  Identity = "Diabetes"
)
Falls1 <- data.frame(
  TrueTrue = sum(Falls$TrueTrue),
  TrueFalse = sum(Falls$TrueFalse),
  FalseTrue = sum(Falls$FalseTrue),
  FalseFalse =sum(Falls$FalseFalse),
  Identity = "Falls"
)
Hypertension1 <- data.frame(
  TrueTrue = sum(Hypertension$TrueTrue),
  TrueFalse = sum(Hypertension$TrueFalse),
  FalseTrue = sum(Hypertension$FalseTrue),
  FalseFalse =sum(Hypertension$FalseFalse),
  Identity = "Hypertension"
)
Hypotension1 <- data.frame(
  TrueTrue = sum(Hypotension$TrueTrue),
  TrueFalse = sum(Hypotension$TrueFalse),
  FalseTrue = sum(Hypotension$FalseTrue),
  FalseFalse =sum(Hypotension$FalseFalse),
  Identity = "Hypotension"
)
PTSD1 <- data.frame(
  TrueTrue = sum(PTSD$TrueTrue),
  TrueFalse = sum(PTSD$TrueFalse),
  FalseTrue = sum(PTSD$FalseTrue),
  FalseFalse =sum(PTSD$FalseFalse),
  Identity = "PTSD"
)
Sleep1 <- data.frame(
  TrueTrue = sum(Sleep$TrueTrue),
  TrueFalse = sum(Sleep$TrueFalse),
  FalseTrue = sum(Sleep$FalseTrue),
  FalseFalse =sum(Sleep$FalseFalse),
  Identity = "Sleep"
)
Smell1 <- data.frame(
  TrueTrue = sum(Smell$TrueTrue),
  TrueFalse = sum(Smell$TrueFalse),
  FalseTrue = sum(Smell$FalseTrue),
  FalseFalse =sum(Smell$FalseFalse),
  Identity = "Smell"
)
Tremor1 <- data.frame(
  TrueTrue = sum(Tremor$TrueTrue),
  TrueFalse = sum(Tremor$TrueFalse),
  FalseTrue = sum(Tremor$FalseTrue),
  FalseFalse =sum(Tremor$FalseFalse),
  Identity = "Tremor"
)
Urinary1 <- data.frame(
  TrueTrue = sum(Urinary$TrueTrue),
  TrueFalse = sum(Urinary$TrueFalse),
  FalseTrue = sum(Urinary$FalseTrue),
  FalseFalse =sum(Urinary$FalseFalse),
  Identity = "Urinary"
)
rm("Balance", "Cognitive", "Constipation", "Diabetes", "Falls", "HeadInjury",
   "HearingLoss", "Hypertension", "Hypotension", "PTSD", "Smell", "Sleep", "Tremor", "Urinary")
rm("cohToTest", "Demo")

together1 <- merge(Balance1, Cognitive1,by=c("TrueTrue", "TrueFalse", "FalseTrue", "FalseFalse", "Identity"),all=T)
together2 <- merge(together1, Constipation1,by=c("TrueTrue", "TrueFalse", "FalseTrue", "FalseFalse", "Identity"),all=T)
together3 <- merge(together2, Diabetes1,by=c("TrueTrue", "TrueFalse", "FalseTrue", "FalseFalse", "Identity"),all=T)
together4 <- merge(together3, Falls1,by=c("TrueTrue", "TrueFalse", "FalseTrue", "FalseFalse", "Identity"),all=T)
together5 <- merge(together4, HeadInjury1,by=c("TrueTrue", "TrueFalse", "FalseTrue", "FalseFalse", "Identity"),all=T)
together6 <- merge(together5, Hypertension1,by=c("TrueTrue", "TrueFalse", "FalseTrue", "FalseFalse", "Identity"),all=T)
together7 <- merge(together6, Hypotension1,by=c("TrueTrue", "TrueFalse", "FalseTrue", "FalseFalse", "Identity"),all=T)
together8 <- merge(together7, PTSD1,by=c("TrueTrue", "TrueFalse", "FalseTrue", "FalseFalse", "Identity"),all=T)
together9 <- merge(together8, Sleep1,by=c("TrueTrue", "TrueFalse", "FalseTrue", "FalseFalse", "Identity"),all=T)
together10 <- merge(together9, Smell1,by=c("TrueTrue", "TrueFalse", "FalseTrue", "FalseFalse", "Identity"),all=T)
together11 <- merge(together10, Tremor1,by=c("TrueTrue", "TrueFalse", "FalseTrue", "FalseFalse", "Identity"),all=T)
together12 <- merge(together11, Urinary1,by=c("TrueTrue", "TrueFalse", "FalseTrue", "FalseFalse", "Identity"),all=T)

rm("together1", "together2", "together3", "together4", "together5",
   "together6", "together7", "together8", "together9", "together10", "together11")
rm("Balance1", "Cognitive1", "Constipation1", "Diabetes1", "Falls1", "HeadInjury1",
   "Hypertension1", "Hypotension1", "PTSD1", "Smell1", "Sleep1", "Tremor1", "Urinary1")

together1 <- together12
together2 <- together12

together1 <- select(together1, c(TrueTrue, FalseTrue, Identity))
together2 <- select(together2, c(FalseFalse, TrueFalse, Identity))
together1 <- together1 %>%
  mutate(Identity = recode(Identity, "Hypotension" = "HypotensionTrue",
                           "Falls" = "FallsTrue",
                           "PTSD" = "PTSDTrue",
                           "HeadInjury" = "HeadInjuryTrue",
                           "Balance" = "BalanceTrue",
                           "Cognitive" = "CognitiveTrue",
                           "Tremor" = "TremorTrue",
                           "Smell" = "SmellTrue",
                           "Sleep" = "SleepTrue",
                           "Constipation" = "ConstipationTrue",
                           "Diabetes" = "DiabetesTrue",
                           "Urinary" = "UrinaryTrue",
                           "Hypertension" = "HypertensionTrue"
  ))
names(together1)[names(together1) == "TrueTrue"] <- "True"
names(together1)[names(together1) == "FalseTrue"] <- "False"

together2 <- together2 %>%
  mutate(Identity = recode(Identity, "Hypotension" = "HypotensionFalse",
                           "Falls" = "FallsFalse",
                           "PTSD" = "PTSDFalse",
                           "HeadInjury" = "HeadInjuryFalse",
                           "Balance" = "BalanceFalse",
                           "Cognitive" = "CognitiveFalse",
                           "Tremor" = "TremorFalse",
                           "Smell" = "SmellFalse",
                           "Sleep" = "SleepFalse",
                           "Constipation" = "ConstipationFalse",
                           "Diabetes" = "DiabetesFalse",
                           "Urinary" = "UrinaryFalse",
                           "Hypertension" = "HypertensionFalse"
  ))
names(together2)[names(together2) == "TrueFalse"] <- "True"
names(together2)[names(together2) == "FalseFalse"] <- "False"

together <- merge(together1, together2, by=c("True", "False", "Identity"), all=T)


#### Table ####
#Table of the final dataset
library(gtable)
library(gtsummary)
library(gt)

together <- together[c(3, 4, 14, 22, 8, 24, 13, 26, 6, 21, 11, 19, 7, 20, 15, 2, 1, 18, 10, 16, 5, 12, 9, 23, 17, 25),]
together <- together %>%
  mutate(Condition = recode(Identity, "HypotensionTrue" = "Yes",
                            "FallsTrue" = "Yes",
                            "PTSDTrue" = "Yes",
                            "HeadInjuryTrue" = "Yes",
                            "BalanceTrue" = "Yes",
                            "CognitiveTrue" = "Yes",
                            "TremorTrue" = "Yes",
                            "SmellTrue" = "Yes",
                            "SleepTrue" = "Yes",
                            "ConstipationTrue" = "Yes",
                            "DiabetesTrue" = "Yes",
                            "UrinaryTrue" = "Yes",
                            "HypertensionTrue" = "Yes",
                            "HypotensionFalse" = "No",
                            "FallsFalse" = "No",
                            "PTSDFalse" = "No",
                            "HeadInjuryFalse" = "No",
                            "BalanceFalse" = "No",
                            "CognitiveFalse" = "No",
                            "TremorFalse" = "No",
                            "SmellFalse" = "No",
                            "SleepFalse" = "No",
                            "ConstipationFalse" = "No",
                            "DiabetesFalse" = "No",
                            "UrinaryFalse" = "No",
                            "HypertensionFalse" = "No"
  ))
together <- select(together, c(Condition, True, False))

counting <- gt(together)
counting <-
  counting |>
  tab_header(
    title = "Summary Table",
    subtitle = "Amongst those with data, people with Parkinson's"
  )
#Titles for each medical condition
counting <-
  counting |>
  tab_row_group(
    label = "Hypotension",
    rows = 1:2
  ) |>
  tab_row_group(
    label = "Constipation",
    rows = 3:4
  ) |>
  tab_row_group(
    label = "Urinary Problems",
    rows = 5:6
  ) |>
  tab_row_group(
    label = "Sleeping Problems",
    rows = 7:8
  ) |>
  tab_row_group(
    label = "Smell Loss",
    rows = 9:10
  ) |>
  tab_row_group(
    label = "Cognitive Problems (Memory, Thinking)",
    rows = 11:12
  ) |>
  tab_row_group(
    label = "Tremors",
    rows = 13:14
  ) |>
  tab_row_group(
    label = "Falls",
    rows = 15:16
  ) |>
  tab_row_group(
    label = "Unstable",
    rows = 17:18
  ) |>
  tab_row_group(
    label = "Head Injury",
    rows = 19:20
  ) |>
  tab_row_group(
    label = "PTSD",
    rows = 21:22
  ) |>
  tab_row_group(
    label = "Diabetes",
    rows = 23:24
  ) |>
  tab_row_group(
    label = "Hypertension",
    rows = 25:26
  ) |>
  row_group_order(groups = c("Hypotension", "Constipation", "Urinary Problems", "Sleeping Problems",
                             "Smell Loss", "Cognitive Problems (Memory, Thinking)", "Tremors", "Falls",
                             "Unstable", "Head Injury", "PTSD", "Diabetes", "Hypertension"))
#Title for the whole table
counting <-
  counting %>%
  tab_spanner(
    label = "Hearing Loss",
    columns = c(True, False)
  )

counting