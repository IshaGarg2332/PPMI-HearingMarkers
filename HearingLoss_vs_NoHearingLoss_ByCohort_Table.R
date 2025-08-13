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


# Gives a table of the total count of hearing loss and no hearing loss patients filtered into if they are healthy controls, prodromal, or Parkinson's


#Load the data
Participant <- read_csv("data/ParticipantStatus.csv")
HeadInjury <- read_csv("data/HeadInjury.csv")
PDDiagHistory <- read.csv("data/PDDiagHistory.csv")
ParticipantStatus <- read_csv("data/ParticipantStatus.csv")
MedConditions <- read_csv("data/MedConditions.csv")
Demographics <- read_csv("data/Demographics.csv")

HeadInjury <- HeadInjury
tmp <- HeadInjury
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"
tmp2 <- merge(Participant,tmp,by="PATNO",all.x=T)

#Mutate the hearing loss data
MedConditions <- read_csv("data/MedConditions.csv")
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
cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)

#Keep only the ones where they were diagnosed with hearing loss at least a year prior to diagnosis of Parkinson's disease
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
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

#Mutate the column values
cohToTest$hl <- ifelse(is.na(cohToTest$hlBefore),F,cohToTest$hlBefore)
cohToTest$hiq1 <- ifelse(is.na(cohToTest$hiq1),NA,ifelse(cohToTest$hiq1 == 9999,NA,cohToTest$hiq1))
cohToTest$hiqYes <- ifelse(cohToTest$hiq1 == 1,T,F)

cohToTest$ID <- ifelse(grepl("Park",cohToTest$COHORT_DEFINITION,ignore.case=T),T,F)

cohToTest <- merge(cohToTest,Demographics,by="PATNO",all.x=T)

cohToTest$testHiqAge <- ifelse(cohToTest$hiqa1_age <= 13,T,F)

cohToTest <- select(cohToTest, c(hlBefore, COHORT_DEFINITION))

cohToTest <- cohToTest[!is.na(cohToTest$COHORT_DEFINITION),]
cohToTest[is.na(cohToTest)] <- FALSE

df <- data.frame(matrix(ncol = 4, nrow = 2))

#Keep a column for each outcome with hearing loss (true or false) and Parkinson's status (healthy control, prodromal, or Parkinson's)
cohToTest <- cohToTest %>%
  mutate(One=case_when(
    hlBefore == TRUE & COHORT_DEFINITION == "Healthy Control" ~ 1,
    hlBefore == TRUE & COHORT_DEFINITION == "Prodromal" ~ 0,
    hlBefore == TRUE & COHORT_DEFINITION == "Parkinson's Disease" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "Healthy Control" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "Prodromal" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "Parkinson's Disease" ~ 0,
    hlBefore == TRUE & COHORT_DEFINITION == "SWEDD" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "SWEDD" ~ 0
  ))

cohToTest <- cohToTest %>%
  mutate(Two=case_when(
    hlBefore == TRUE & COHORT_DEFINITION == "Healthy Control" ~ 0,
    hlBefore == TRUE & COHORT_DEFINITION == "Prodromal" ~ 1,
    hlBefore == TRUE & COHORT_DEFINITION == "Parkinson's Disease" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "Healthy Control" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "Prodromal" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "Parkinson's Disease" ~ 0,
    hlBefore == TRUE & COHORT_DEFINITION == "SWEDD" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "SWEDD" ~ 0
  ))

cohToTest <- cohToTest %>%
  mutate(Three=case_when(
    hlBefore == TRUE & COHORT_DEFINITION == "Healthy Control" ~ 0,
    hlBefore == TRUE & COHORT_DEFINITION == "Prodromal" ~ 0,
    hlBefore == TRUE & COHORT_DEFINITION == "Parkinson's Disease" ~ 1,
    hlBefore == FALSE & COHORT_DEFINITION == "Healthy Control" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "Prodromal" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "Parkinson's Disease" ~ 0,
    hlBefore == TRUE & COHORT_DEFINITION == "SWEDD" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "SWEDD" ~ 0
  ))
cohToTest <- cohToTest %>%
  mutate(Four=case_when(
    hlBefore == TRUE & COHORT_DEFINITION == "Healthy Control" ~ 0,
    hlBefore == TRUE & COHORT_DEFINITION == "Prodromal" ~ 0,
    hlBefore == TRUE & COHORT_DEFINITION == "Parkinson's Disease" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "Healthy Control" ~ 1,
    hlBefore == FALSE & COHORT_DEFINITION == "Prodromal" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "Parkinson's Disease" ~ 0,
    hlBefore == TRUE & COHORT_DEFINITION == "SWEDD" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "SWEDD" ~ 0
  ))
cohToTest <- cohToTest %>%
  mutate(Five=case_when(
    hlBefore == TRUE & COHORT_DEFINITION == "Healthy Control" ~ 0,
    hlBefore == TRUE & COHORT_DEFINITION == "Prodromal" ~ 0,
    hlBefore == TRUE & COHORT_DEFINITION == "Parkinson's Disease" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "Healthy Control" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "Prodromal" ~ 1,
    hlBefore == FALSE & COHORT_DEFINITION == "Parkinson's Disease" ~ 0,
    hlBefore == TRUE & COHORT_DEFINITION == "SWEDD" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "SWEDD" ~ 0
  ))
cohToTest <- cohToTest %>%
  mutate(Six=case_when(
    hlBefore == TRUE & COHORT_DEFINITION == "Healthy Control" ~ 0,
    hlBefore == TRUE & COHORT_DEFINITION == "Prodromal" ~ 0,
    hlBefore == TRUE & COHORT_DEFINITION == "Parkinson's Disease" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "Healthy Control" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "Prodromal" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "Parkinson's Disease" ~ 1,
    hlBefore == TRUE & COHORT_DEFINITION == "SWEDD" ~ 0,
    hlBefore == FALSE & COHORT_DEFINITION == "SWEDD" ~ 0
  ))

#Sum up all the columns, one by one, and insert it into a dataframe
df[1,2] = sum(cohToTest$One)
df[1,3] = sum(cohToTest$Two)
df[1,4] = sum(cohToTest$Three)
df[2,2] = sum(cohToTest$Four)
df[2,3] = sum(cohToTest$Five)
df[2,4] = sum(cohToTest$Six)
df[1,1] = "Yes"
df[2,1] = "No"

#Rename column names
colnames(df)[which(names(df) == "X1")] <- "Hearing Loss"
colnames(df)[which(names(df) == "X2")] <- "Healthy Control"
colnames(df)[which(names(df) == "X3")] <- "Prodromal"
colnames(df)[which(names(df) == "X4")] <- "Parkinson's Disease"

library(gt)

#Create table
df %>%
  gt() %>%
  tab_header(
    title = "Hearing Loss and Parkinson's"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(0.9)),
    locations = cells_body(
      columns = c("Parkinson's Disease")
    )
  )%>%
  tab_style(
    style = cell_borders(
      sides = c("all"),
      weight = px(0.9)),
    locations = cells_body(
      columns = c("Hearing Loss")
    ))