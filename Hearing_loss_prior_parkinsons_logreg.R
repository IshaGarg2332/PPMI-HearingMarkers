# --------------------------------------------------
# NOTE: This script requires private datasets that are not included due to confidentiality. 
# Replace the paths with your own files structured similarly to the expected data frames.
# --------------------------------------------------

#Import libraries
library("dplyr")
library("tidyverse")
library("ggplot2")
library("survival")
library("stats")


#Investigated whether hearing loss diagnosed at least one year before Parkinson’s disease or prodromal 
# diagnosis is associated with higher odds of being in the Parkinson’s-related group
#Used logistic regression controlling for age and sex, finding that prior hearing loss may be linked to 
# increased risk of Parkinson’s disease or prodromal status


#Load in the datasets
ParticipantStatus <- read_csv("data/ParticipantStatus.csv")
MedConditions <- read_csv("data/MedConditions.csv")
PDDiagHistory <- read.csv("data/PDDiagHistory.csv")
Demo <- read_csv("data/Demographics.csv")


#Combine ParticipantStatus and DiagHistory
cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

#Make the hearing loss data
hloss <- MedConditions[grepl("(hear\\b|hearing)",MedConditions$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
hloss$MHTERM <- tolower(hloss$MHTERM)
hloss$MHTERM <- str_squish(hloss$MHTERM)
hloss <- hloss[!grepl("(neuroma|tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant|eustachian|calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[hloss$MHTERM != "hearing",]
#hloss <- hloss[!grepl("(decreas)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(asymm|left|right|unilat| l | r )",hloss$MHTERM,ignore.case=T),]

#Merge the two together
cohToTest <- merge(cohToTest,hloss,by="PATNO",all.x=T)

#Keep only the ones where they were diagnosed with hearing loss at least a year prior to diagnosis of Parkinson's disease
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")
cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),NA)

#Remove the types of hearing loss
cohToTest$bilat <- ifelse(grepl("(bilateral|b\\/l|both ears)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$unilat <- ifelse(grepl("(asymm|left|right|unilat)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$loss <- ifelse(grepl("(loss)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$impair <- ifelse(grepl("(impair|defici)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$decr <- ifelse(grepl("(decreas|reduc|hardness)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$anyHL <- ifelse(cohToTest$bilat | cohToTest$unilat | cohToTest$loss | cohToTest$impair | cohToTest$decr,T,F)
cohToTest$aid <- ifelse(grepl("(aid|device)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$sensori <- ifelse(grepl("(sensory|sensori)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$slight <- ifelse(grepl("(slight|mild|partial)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$moder <- ifelse(grepl("(moderate|chronic)",cohToTest$MHTERM,ignore.case=T),T,F)
cohToTest$tinnitus <- ifelse(grepl("(tinitis|tinnitus)",cohToTest$MHTERM,ignore.case=T),T,F)

# Categorize enrollment age and hearing loss timing; flag Parkinson’s/prodromal cases
cohToTest$AgeCut <- cut(cohToTest$ENROLL_AGE,breaks=c(0,60,80,120))
cohToTest$HLBfCut <- cut(as.numeric(cohToTest$hlDiff),breaks=c(-99999,-365,0,365,99999),na.rm=T)
cohToTest$ID <- ifelse(grepl("Park|prodro",cohToTest$COHORT_DEFINITION,ignore.case=T),T,F)

#Merge datasets
cohToTest <- merge(cohToTest,Demo,by="PATNO",all.x=T)

# Logistic regression: hearing loss, age, sex predicting disease status
res <- glm(ID ~ hlBefore + ENROLL_AGE + SEX, data = cohToTest, family = "binomial")
summary(res)

# Create binary hearing loss variable, replacing NAs with FALSE
cohToTest$hl <- ifelse(is.na(cohToTest$hlBefore), FALSE, cohToTest$hlBefore)

# Subset to Parkinson’s Disease and Prodromal cohorts
cohPD <- cohToTest[cohToTest$COHORT_DEFINITION == "Parkinson\\'s Disease" | cohToTest$COHORT_DEFINITION == "Prodromal", ]

# Proportion tables: hearing loss by cohort
prop.table(table(cohToTest$COHORT_DEFINITION, cohToTest$hlBefore), 1)

# Proportion tables: disease status vs. hearing loss
prop.table(table(isPD = cohToTest$ID, isHearLoss = cohToTest$hl), 1)

# Proportion tables: hearing loss by cohort, age group, and hearing loss
prop.table(table(cohToTest$COHORT_DEFINITION, cohToTest$AgeCut, cohToTest$hl), 1)

# Proportion tables: GBA mutation status vs. hearing loss
prop.table(table(isGBA = cohToTest$CONGBA, isHearLoss = cohToTest$hl), 1)

# Proportion tables: LRRK2 mutation status vs. hearing loss
prop.table(table(isLRRK = cohToTest$CONLRRK2, isHearLoss = cohToTest$hl), 1)