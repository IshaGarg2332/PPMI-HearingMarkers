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
library("ggplot2")

# Gives the number of patients with hearing loss
# Gives males with hearing loss in one bar graph and females with hearing loss in another
# The bars are by age on the x-axis and there are three bars per age for healthy control, prodromal disease, and Parkinson's disease

#Import data
Participant <- read_csv("data/ParticipantStatus.csv")
PDDiagHistory <- read.csv("data/PDDiagHistory.csv")

#Combine ParticipantStatus and DiagHistory
cohToTest <- Participant
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

#Import All Medical Conditions data
Hypertension <- read_csv("data/MedConditions.csv")

#Sort out the rows with hypertension
Hypertension <- Hypertension[grepl("(Hypertension|hypertension|High Blood Pressure|High blood pressure|high blood pressure|Elevated blood pressure)",Hypertension$MHTERM,ignore.case = F),]
tmp1 <- Hypertension

#Rename columns
names(tmp1)[names(tmp1) == "MHTERM"] <- "hyper"
colnames(tmp1)[colnames(tmp1) == "patno"] <- "PATNO"

#Import All Medical Conditions data
Hypotension <- read_csv("data/MedConditions.csv")

#Sort out the rows with hypotension
Hypotension <- Hypotension[grepl("(Orthostatis Hypotension|ORTHOSTATIC HYPOTENSION|orthostatic hypotension|Neurogenic Orthostatic Hypotension|Neurogenic orthostatic hypotension)",Hypotension$MHTERM,ignore.case = F),]
tmp2 <- Hypotension

#Rename columns
names(tmp2)[names(tmp2) == "MHTERM"] <- "hypo"
colnames(tmp2)[colnames(tmp2) == "patno"] <- "PATNO"

#Merge ParticipantStatus, DiagHistory, hypertension, and hypotension data
Test1 <- merge(cohToTest,tmp1,by="PATNO",all.x=T)
Test2 <- merge(cohToTest,tmp1,by="PATNO",all.x=T)

#Import Medical Conditions and sort out hearing loss
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
Tes1 <- merge(Test1,hloss,by="PATNO",all.x=T)

#Sort out only hearing loss data where it was diagnosed at least a year prior to Parkinson's diagnosis
Tes1$HLDate <- as.Date(paste0("01/",Tes1$MHDIAGDT.y),format="%d/%m/%Y")
Tes1$PDdate <- as.Date(paste0("01/",Tes1$PDDXDT),format="%d/%m/%Y")
Tes1$hlDiff <- ifelse(is.na(Tes1$PDdate),Tes1$EnrollDate - Tes1$HLDate,Tes1$PDdate - Tes1$HLDate)
Tes1$hlBefore <- ifelse(!is.na(Tes1$MHTERM),ifelse(Tes1$hlDiff > (-365 * 1) ,T,F),F)

#Select certain columns
Tes1 <- select(Tes1, c(PATNO, COHORT_DEFINITION, EnrollDate, ENROLL_AGE, HLDate, PDdate, hlDiff, hlBefore, hyper, MHTERM))

#Make hypertension and hearing loss binary
Tes2 <- Tes1 %>%
  mutate(hyper=case_when(
    is.na(hyper) ~ 0,
    !is.na(hyper) ~ 1
  ))
Tes2 <- Tes2 %>%
  mutate(hlBefore=case_when(
    hlBefore == FALSE ~ 0,
    hlBefore == TRUE ~ 1
  ))

#Merge with Demographics data
Demo <- read_csv("data/Demographics.csv")
Test1 <- merge(Demo,Tes2,by="PATNO",all.x=T)

#Select certain columns
Tes3 <- select(Test1, c(PATNO, COHORT_DEFINITION, EnrollDate, ENROLL_AGE, HLDate, PDdate, hlDiff, hlBefore, hyper, MHTERM, SEX))

#Remove unnessesary dataframes
rm("cohToTest", "Demo", "HeadInjury", "hloss", "Hypertension", "Hypotension", "MedConditions", "Participant", "PDDiagHistory", "Tes1", "Tes2", "Test1", "Test2", "tmp", "tmp1", "tmp2")

#Keep only the rows where the sex of the patient is known
Female <- Tes3 %>%
  mutate(SEX=case_when(
    SEX == 0 ~ NA,
    SEX == 1 ~ 1
  ))
Male <- Tes3 %>%
  mutate(SEX=case_when(
    SEX == 0 ~ 0,
    SEX == 1 ~ NA
  ))
rm("Tes3")
Female <- Female[!is.na(Female$SEX),]
Male <- Male[!is.na(Male$SEX),]

#Make categorical variable out of age
Female1 <- Female %>%
  mutate(agecat=case_when(
    ENROLL_AGE >=18 & ENROLL_AGE <25 ~ "18-25",
    ENROLL_AGE >=25 & ENROLL_AGE <35 ~ "25-35",
    ENROLL_AGE >=35 & ENROLL_AGE <45 ~ "35-45",
    ENROLL_AGE >=45 & ENROLL_AGE <55 ~ "45-55",
    ENROLL_AGE >=55 & ENROLL_AGE <65 ~ "55-65",
    ENROLL_AGE >=65 & ENROLL_AGE <75 ~ "65-75",
    ENROLL_AGE >=75 & ENROLL_AGE <85 ~ "75-85",
    ENROLL_AGE >=85 ~ ">85",
  ))
Female1 <- Female1[!is.na(Female1$agecat),]
Male1 <- Male %>%
  mutate(agecat=case_when(
    ENROLL_AGE >=18 & ENROLL_AGE <25 ~ "18-25",
    ENROLL_AGE >=25 & ENROLL_AGE <35 ~ "25-35",
    ENROLL_AGE >=35 & ENROLL_AGE <45 ~ "35-45",
    ENROLL_AGE >=45 & ENROLL_AGE <55 ~ "45-55",
    ENROLL_AGE >=55 & ENROLL_AGE <65 ~ "55-65",
    ENROLL_AGE >=65 & ENROLL_AGE <75 ~ "65-75",
    ENROLL_AGE >=75 & ENROLL_AGE <85 ~ "75-85",
    ENROLL_AGE >=85 ~ ">85",
  ))
Male1 <- Male1[!is.na(Male1$agecat),]
rm("Female", "Male")

#Keep only rows where hearing loss is true
Female2 <- Female1 %>%
  mutate(hlBefore=case_when(
    hlBefore == 0 ~ NA,
    hlBefore == 1 ~ 1
  ))
Male2 <- Male1 %>%
  mutate(hlBefore=case_when(
    hlBefore == 0 ~ NA,
    hlBefore == 1 ~ 1
  ))
Female2 <- Female2[!is.na(Female2$hlBefore),]
Male2 <- Male2[!is.na(Male2$hlBefore),]

#Keep only certain columns
Female2 <- select(Female2, c(COHORT_DEFINITION, agecat, hlBefore))
Female2$COHORT_DEFINITION <- as.factor(Female2$COHORT_DEFINITION)
Female2$agecat <- factor(Female2$agecat, levels = c("45-55", "55-65", "65-75", "75-85", ">85"))
names(Female2)[names(Female2) == 'COHORT_DEFINITION'] <- "Parkinson's Disease"

Male2 <- select(Male2, c(COHORT_DEFINITION, agecat, hlBefore))
Male2$COHORT_DEFINITION <- as.factor(Male2$COHORT_DEFINITION)
Male2$agecat <- factor(Male2$agecat, levels = c("35-45", "55-65", "65-75", "75-85"))
names(Male2)[names(Male2) == 'COHORT_DEFINITION'] <- "Parkinson's Disease"

#Create barplots
Female <- Female2 %>%
  ggplot(aes(x = agecat, fill = `Parkinson's Disease`)) +
  geom_bar(position = position_dodge(preserve = "single"),
           width = 0.7) +
  scale_fill_manual(
    "Test",
    guide = "legend"
  )+
  scale_fill_brewer(palette = "Pastel1", drop = FALSE) +
  labs(x="Age",y="Number of Hearing Loss Patients",
       title="Females With Hearing Loss") +
  scale_y_continuous(breaks=c(0,2,4,6,8,10))

Male <- Male2 %>%
  ggplot(aes(x = agecat, fill = `Parkinson's Disease`)) +
  geom_bar(position = position_dodge(preserve = "single"),
           width = 0.7) +
  scale_fill_manual(
    "Test",
    guide = "legend"
  )+
  scale_fill_brewer(palette = "Pastel1", drop = FALSE) +
  labs(x="Age",y="Number of Hearing Loss Patients",
       title="Males With Hearing Loss") +
  scale_y_continuous(breaks=c(0,2,4,6,8,10, 12, 14, 16, 18, 20, 22))
rm("Male1", "Male2", "Female1", "Female2")

#Arrange the barplots, one on top of the other
library("ggpubr")
figure <- ggarrange(Male, Female,
                    ncol = 1, nrow = 2)
figure