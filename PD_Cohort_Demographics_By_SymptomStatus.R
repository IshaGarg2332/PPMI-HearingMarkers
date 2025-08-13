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
library("gt")


#The population includes individuals with Parkinson's Disease, Prodromal PD, Healthy Controls, and SWEDD patients
#For each subgroup defined by symptom status ("Symptom" vs. "No Symptom"), the table displays:
#     Sex: Categorical distribution (Male vs. Female)
#     Race/Ethnicity: White, Black/African American, Hispanic/Latino, Asian, American Indian/Alaska Native, Hawaiian/Other Pacific Islander, and Unknown
#     Age at Enrollment: Sorted by age (e.g., 24–36, 36–42, 42–56, etc.)
#     Status of Parkinson's disease: Parkinson's Disease, Prodromal, Healthy Control, and SWEDD


#Import data
ParticipantStatus <- read_csv("data/ParticipantStatus.csv")
PDDiagHistory <- read.csv("data/PDDiagHistory.csv")
Demographics <- read_csv("data/Demographics.csv")
MedConditions <- read.csv("data/MedConditions.csv")

#Combine ParticipantStatus and DiagHistory
cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all=T)


#### Hearing Loss ####
#Make the hearing loss data
#The presence or absence of hearing loss occurring at least one year prior to Parkinson's diagnosis 
#Table of hearing loss against race, sex, age, and Parkinson's status

hloss <- MedConditions[grepl("(hear|hearing|Hear|Hearing|hear loss|HEAR|HEARING|hearing loss|Hear loss|hearloss|Hearloss|Hearing loss|Hearing Loss|LOSS OF HEARING|hearing loss)",MedConditions$MHTERM,ignore.case = T),]

hloss <- hloss[-grep("Heart", hloss$MHTERM), ]
hloss <- hloss[-grep("heart", hloss$MHTERM), ]
hloss <- hloss[-grep("HEART", hloss$MHTERM), ]

hloss <- hloss[!grepl("(neuroma)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(tumor)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(implant)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(eustachian)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(calcification)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(asymm)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(left)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(right)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(unilat)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(hearing loss on r side)",hloss$MHTERM,ignore.case=T),]
hloss <- hloss[!grepl("(sudden hearing loss l ear)",hloss$MHTERM,ignore.case=T),]

hloss = hloss[!duplicated(hloss$PATNO),]

#Merge the two together
Test <- merge(MedConditions1,hloss,by="PATNO",all.x=T)
Test <- select(Test, c(PATNO, MHTERM.y, MHDIAGDT.y))

cohToTest <- merge(cohToTest,Test,by="PATNO",all.y=T)
cohToTest1 <- cohToTest[!is.na(cohToTest$MHTERM.y),]
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")

cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM.y),ifelse(cohToTest$hlDiff > (365 * 1) ,T,F),NA)

cohToTest <- merge(cohToTest,Demographics,by="PATNO",all.x=T)
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Tes1 <- select(cohToTest, c(PATNO, SEX, COHORT_DEFINITION, ENROLL_AGE, hlBefore))
Tes1 <- Tes1[!is.na(Tes1$COHORT_DEFINITION),]
Tes1[c("hlBefore")][is.na(Tes1[c("hlBefore")])] <- FALSE

Tes2 <- Tes1 %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ TRUE,
    COHORT_DEFINITION == "Prodromal" ~ TRUE,
    COHORT_DEFINITION == "Healthy Control" ~ FALSE,
    COHORT_DEFINITION == "SWEDD" ~ FALSE
  ))
Tes3 <- merge(Tes2,Demographics,by="PATNO",all.x=T)
Tes3 <- select(Tes3, c(PATNO, SEX.x, COHORT_DEFINITION, ENROLL_AGE, hlBefore, HISPLAT, RAASIAN, RABLACK, RAHAWOPI, RAINDALS, RAWHITE, RANOS, RAUNKNOWN, ID))
Tes3 <- Tes3 %>%
  mutate(Race=case_when(
    HISPLAT == 1 ~ "Hispanic/Latino",
    RAASIAN == 1 ~ "Asian",
    RABLACK == 1 ~ "Black/African American",
    RAHAWOPI == 1 ~ "Hawaiian/Other Pacific Islander",
    RAINDALS == 1 ~ "American Indian/Alaska Native",
    RAWHITE == 1 ~ "White",
    RANOS == 1 ~ "Unknown",
    RAUNKNOWN == 1 ~ "Unknown"
  ))
Tes3 <- Tes3 %>%
  mutate(Sex=case_when(
    SEX.x == 1 ~ "Male",
    SEX.x == 0 ~ "Female",
  ))
Tes3 <- Tes3 %>%
  mutate(Age=case_when(
    ENROLL_AGE <18 ~ "<18",
    ENROLL_AGE >=18 & ENROLL_AGE <24 ~ "18-24",
    ENROLL_AGE >=24 & ENROLL_AGE <36 ~ "24-36",
    ENROLL_AGE >=36 & ENROLL_AGE <42 ~ "36-42",
    ENROLL_AGE >=42 & ENROLL_AGE <56 ~ "42-56",
    ENROLL_AGE >=56 & ENROLL_AGE <66 ~ "56-66",
    ENROLL_AGE >=66 & ENROLL_AGE <76 ~ "66-76",
    ENROLL_AGE >=76 & ENROLL_AGE <86 ~ "76-86",
    ENROLL_AGE >=86 ~ ">86"
  ))
Tes3 <- Tes3 %>%
  mutate(HearingLoss=case_when(
    hlBefore == FALSE ~ "No Hearing Loss",
    hlBefore == TRUE ~ "Hearing Loss",
  ))
Tes3 <- Tes3 %>%
  mutate(ID=case_when(
    ID == TRUE ~ "Parkinson's or Prodromal",
    ID == FALSE ~ "Healthy Control or SWEDD",
  ))
Tes3 <- select(Tes3, c(PATNO, Sex, COHORT_DEFINITION, ENROLL_AGE, Age, HearingLoss, Race, ID))
colnames(Tes3)[which(names(Tes3) == "COHORT_DEFINITION")] <- "Parkinsons"
label(Tes3$ID) <- "Parkinson's or Healthy?"

Tes3 <- Tes3 %>%
  mutate(Race = factor(Race, levels=c("White", "Black/African American", "Hispanic/Latino", "Asian", "American Indian/Alaska Native", "Hawaiian/Other Pacific Islander", "Unknown")))
Tes3 <- Tes3 %>%
  mutate(Sex = factor(Sex, levels=c("Male", "Female")))
Tes3 <- Tes3 %>%
  mutate(Age = factor(Age, levels=c("24-36", "36-42", "42-56", "56-66", "66-76", "76-86", ">86")))
Tes3 <- Tes3 %>%
  mutate(Parkinsons = factor(Parkinsons, levels=c("Parkinson's Disease", "Prodromal", "Healthy Control", "SWEDD")))

table1(~ Race + Sex + Age + Parkinsons | HearingLoss, data=Tes3,
       title = "Demographics",
)

mod <- glm(ID ~ as.numeric(hlBefore) + ENROLL_AGE + SEX,
           data = Tes2,
           family = "binomial")
tab <- summary(mod)$coef

data1 <- data.frame(variable = rownames(tab),
                    oddsratio = round(exp(tab[,1]), 3),
                    ci_low = round(exp(tab[,1] - 1.96 * tab[,2]), 3),
                    ci_high = round(exp(tab[,1] + 1.96 * tab[,2]), 3),
                    pval = scales::pvalue(tab[,4]),
                    row.names = NULL)[-1,]
data1 <- data1[-c(2,3), ]
data1 <- data1 %>%
  mutate(variable = recode(variable,
                           'as.numeric(hlBefore)' = 'Hearing Loss'))

#### Hypertension ####
#Make the hypertension data
#Table of hypertension against race, sex, age, and Parkinson's status

cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Hypertension <- read_csv("data/MedConditions.csv")
Hypertension <- Hypertension[grepl("(Hypertension|hypertension|High Blood Pressure|High blood pressure|high blood pressure|Elevated blood pressure)",Hypertension$MHTERM,ignore.case = F),]
tmp <- Hypertension
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Test <- merge(MedConditions1,tmp,by="PATNO",all.x=T)
Test <- select(Test, c(PATNO, MHTERM.y, MHDIAGDT.y))

cohToTest <- merge(cohToTest,Test,by="PATNO",all.y=T)
cohToTest1 <- cohToTest[!is.na(cohToTest$MHTERM.y),]
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")

cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM.y),ifelse(cohToTest$hlDiff > (365 * 1) ,T,F),NA)

cohToTest <- merge(cohToTest,Demographics,by="PATNO",all.x=T)
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Tes1 <- cohToTest[!is.na(cohToTest$COHORT_DEFINITION),]
Tes1[c("hlBefore")][is.na(Tes1[c("hlBefore")])] <- FALSE

Tes2 <- Tes1 %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ TRUE,
    COHORT_DEFINITION == "Prodromal" ~ TRUE,
    COHORT_DEFINITION == "Healthy Control" ~ FALSE,
    COHORT_DEFINITION == "SWEDD" ~ FALSE
  ))
Tes3 <- select(Tes2, c(PATNO, SEX, COHORT_DEFINITION, ENROLL_AGE, hlBefore, HISPLAT, RAASIAN, RABLACK, RAHAWOPI, RAINDALS, RAWHITE, RANOS, RAUNKNOWN, ID))
Tes3 <- Tes3 %>%
  mutate(Race=case_when(
    HISPLAT == 1 ~ "Hispanic/Latino",
    RAASIAN == 1 ~ "Asian",
    RABLACK == 1 ~ "Black/African American",
    RAHAWOPI == 1 ~ "Hawaiian/Other Pacific Islander",
    RAINDALS == 1 ~ "American Indian/Alaska Native",
    RAWHITE == 1 ~ "White",
    RANOS == 1 ~ "Unknown",
    RAUNKNOWN == 1 ~ "Unknown"
  ))
Tes3 <- Tes3 %>%
  mutate(Sex=case_when(
    SEX == 1 ~ "Male",
    SEX == 0 ~ "Female",
  ))
Tes3 <- Tes3 %>%
  mutate(Age=case_when(
    ENROLL_AGE <18 ~ "<18",
    ENROLL_AGE >=18 & ENROLL_AGE <24 ~ "18-24",
    ENROLL_AGE >=24 & ENROLL_AGE <36 ~ "24-36",
    ENROLL_AGE >=36 & ENROLL_AGE <42 ~ "36-42",
    ENROLL_AGE >=42 & ENROLL_AGE <56 ~ "42-56",
    ENROLL_AGE >=56 & ENROLL_AGE <66 ~ "56-66",
    ENROLL_AGE >=66 & ENROLL_AGE <76 ~ "66-76",
    ENROLL_AGE >=76 & ENROLL_AGE <86 ~ "76-86",
    ENROLL_AGE >=86 ~ ">86"
  ))
Tes3 <- Tes3 %>%
  mutate(Hypertension=case_when(
    hlBefore == FALSE ~ "No Hypertension",
    hlBefore == TRUE ~ "Hypertension",
  ))
Tes3 <- Tes3 %>%
  mutate(ID=case_when(
    ID == TRUE ~ "Parkinson's or Prodromal",
    ID == FALSE ~ "Healthy Control or SWEDD",
  ))
Tes3 <- select(Tes3, c(PATNO, Sex, COHORT_DEFINITION, ENROLL_AGE, Age, Hypertension, Race, ID, hlBefore))
colnames(Tes3)[which(names(Tes3) == "COHORT_DEFINITION")] <- "Parkinsons"
label(Tes3$ID) <- "Parkinson's or Healthy?"

Tes3 <- Tes3 %>%
  mutate(Race = factor(Race, levels=c("White", "Black/African American", "Hispanic/Latino", "Asian", "American Indian/Alaska Native", "Hawaiian/Other Pacific Islander", "Unknown")))
Tes3 <- Tes3 %>%
  mutate(Sex = factor(Sex, levels=c("Male", "Female")))
Tes3 <- Tes3 %>%
  mutate(Age = factor(Age, levels=c("24-36", "36-42", "42-56", "56-66", "66-76", "76-86", ">86")))
Tes3 <- Tes3 %>%
  mutate(Parkinsons = factor(Parkinsons, levels=c("Parkinson's Disease", "Prodromal", "Healthy Control", "SWEDD")))

table1(~ Race + Sex + Age + Parkinsons | Hypertension, data=Tes3,
       title = "Demographics",
)

mod <- glm(ID ~ as.numeric(hlBefore) + ENROLL_AGE + SEX,
           data = Tes2,
           family = "binomial")
tab <- summary(mod)$coef

data2 <- data.frame(variable = rownames(tab),
                    oddsratio = round(exp(tab[,1]), 3),
                    ci_low = round(exp(tab[,1] - 1.96 * tab[,2]), 3),
                    ci_high = round(exp(tab[,1] + 1.96 * tab[,2]), 3),
                    pval = scales::pvalue(tab[,4]),
                    row.names = NULL)[-1,]
data2 <- data2[-c(2,3), ]
data2 <- data2 %>%
  mutate(variable = recode(variable,
                           'as.numeric(hlBefore)' = 'Hypertension'))


#### Hypotension ####
#Make the hypotension data
#Table of hypotension against race, sex, age, and Parkinson's status

cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Hypotension <- read_csv("data/MedConditions.csv")
Hypotension <- Hypotension[grepl("(Orthostatis Hypotension|ORTHOSTATIC HYPOTENSION|orthostatic hypotension|Neurogenic Orthostatic Hypotension|Neurogenic orthostatic hypotension|Low Blood Pressure\\b|Syncope\\b)",Hypotension$MHTERM,ignore.case = F),]
tmp <- Hypotension
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Test <- merge(MedConditions1,tmp,by="PATNO",all.x=T)
Test <- select(Test, c(PATNO, MHTERM.y, MHDIAGDT.y))

cohToTest <- merge(cohToTest,Test,by="PATNO",all.y=T)
cohToTest1 <- cohToTest[!is.na(cohToTest$MHTERM.y),]
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")

cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM.y),ifelse(cohToTest$hlDiff > (365 * 1) ,T,F),NA)

cohToTest <- merge(cohToTest,Demographics,by="PATNO",all.x=T)
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Tes1 <- cohToTest[!is.na(cohToTest$COHORT_DEFINITION),]
Tes1[c("hlBefore")][is.na(Tes1[c("hlBefore")])] <- FALSE

Tes2 <- Tes1 %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ TRUE,
    COHORT_DEFINITION == "Prodromal" ~ TRUE,
    COHORT_DEFINITION == "Healthy Control" ~ FALSE,
    COHORT_DEFINITION == "SWEDD" ~ FALSE
  ))
Tes3 <- select(Tes2, c(PATNO, SEX, COHORT_DEFINITION, ENROLL_AGE, hlBefore, HISPLAT, RAASIAN, RABLACK, RAHAWOPI, RAINDALS, RAWHITE, RANOS, RAUNKNOWN, ID))
Tes3 <- Tes3 %>%
  mutate(Race=case_when(
    HISPLAT == 1 ~ "Hispanic/Latino",
    RAASIAN == 1 ~ "Asian",
    RABLACK == 1 ~ "Black/African American",
    RAHAWOPI == 1 ~ "Hawaiian/Other Pacific Islander",
    RAINDALS == 1 ~ "American Indian/Alaska Native",
    RAWHITE == 1 ~ "White",
    RANOS == 1 ~ "Unknown",
    RAUNKNOWN == 1 ~ "Unknown"
  ))
Tes3 <- Tes3 %>%
  mutate(Sex=case_when(
    SEX == 1 ~ "Male",
    SEX == 0 ~ "Female",
  ))
Tes3 <- Tes3 %>%
  mutate(Age=case_when(
    ENROLL_AGE <18 ~ "<18",
    ENROLL_AGE >=18 & ENROLL_AGE <24 ~ "18-24",
    ENROLL_AGE >=24 & ENROLL_AGE <36 ~ "24-36",
    ENROLL_AGE >=36 & ENROLL_AGE <42 ~ "36-42",
    ENROLL_AGE >=42 & ENROLL_AGE <56 ~ "42-56",
    ENROLL_AGE >=56 & ENROLL_AGE <66 ~ "56-66",
    ENROLL_AGE >=66 & ENROLL_AGE <76 ~ "66-76",
    ENROLL_AGE >=76 & ENROLL_AGE <86 ~ "76-86",
    ENROLL_AGE >=86 ~ ">86"
  ))
Tes3 <- Tes3 %>%
  mutate(Hypertension=case_when(
    hlBefore == FALSE ~ "No Hypotension",
    hlBefore == TRUE ~ "Hypotension",
  ))
Tes3 <- Tes3 %>%
  mutate(ID=case_when(
    ID == TRUE ~ "Parkinson's or Prodromal",
    ID == FALSE ~ "Healthy Control or SWEDD",
  ))
Tes3 <- select(Tes3, c(PATNO, Sex, COHORT_DEFINITION, ENROLL_AGE, Age, Hypertension, Race, ID, hlBefore))
colnames(Tes3)[which(names(Tes3) == "COHORT_DEFINITION")] <- "Parkinsons"
label(Tes3$ID) <- "Parkinson's or Healthy?"

Tes3 <- Tes3 %>%
  mutate(Race = factor(Race, levels=c("White", "Black/African American", "Hispanic/Latino", "Asian", "American Indian/Alaska Native", "Hawaiian/Other Pacific Islander", "Unknown")))
Tes3 <- Tes3 %>%
  mutate(Sex = factor(Sex, levels=c("Male", "Female")))
Tes3 <- Tes3 %>%
  mutate(Age = factor(Age, levels=c("24-36", "36-42", "42-56", "56-66", "66-76", "76-86", ">86")))
Tes3 <- Tes3 %>%
  mutate(Parkinsons = factor(Parkinsons, levels=c("Parkinson's Disease", "Prodromal", "Healthy Control", "SWEDD")))

table1(~ Race + Sex + Age + Parkinsons | Hypertension, data=Tes3,
       title = "Demographics",
)

mod <- glm(ID ~ as.numeric(hlBefore) + ENROLL_AGE + SEX,
           data = Tes2,
           family = "binomial")
tab <- summary(mod)$coef

data3 <- data.frame(variable = rownames(tab),
                    oddsratio = round(exp(tab[,1]), 3),
                    ci_low = round(exp(tab[,1] - 1.96 * tab[,2]), 3),
                    ci_high = round(exp(tab[,1] + 1.96 * tab[,2]), 3),
                    pval = scales::pvalue(tab[,4]),
                    row.names = NULL)[-1,]
data3 <- data3[-c(2,3), ]
data3 <- data3 %>%
  mutate(variable = recode(variable,
                           'as.numeric(hlBefore)' = 'Hypotension'))


#### Constipation ####
#Make the constipation data
#Table of constipation against race, sex, age, and Parkinson's status

cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Constipation <- read_csv("data/MedConditions.csv")
Constipation <- Constipation[grepl("(Constipation|Chronic Constipation|Occasional constipation|chronic constipation|constipation|CONSTIPATION|Intermittent Constipation|intermittent constipation|Chronic Constipation|Chronic Idiopathic Constipation|Mild Constipation|Chronic constipation|Intermittent constipation|severe constipation|Stomach cramps and dysmotility\\b)",Constipation$MHTERM,ignore.case = F),]
tmp <- Constipation
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Test <- merge(MedConditions1,tmp,by="PATNO",all.x=T)
Test <- select(Test, c(PATNO, MHTERM.y, MHDIAGDT.y))

cohToTest <- merge(cohToTest,Test,by="PATNO",all.y=T)
cohToTest1 <- cohToTest[!is.na(cohToTest$MHTERM.y),]
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")

cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM.y),ifelse(cohToTest$hlDiff > (365 * 1) ,T,F),NA)

cohToTest <- merge(cohToTest,Demographics,by="PATNO",all.x=T)
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Tes1 <- cohToTest[!is.na(cohToTest$COHORT_DEFINITION),]
Tes1[c("hlBefore")][is.na(Tes1[c("hlBefore")])] <- FALSE

Tes2 <- Tes1 %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ TRUE,
    COHORT_DEFINITION == "Prodromal" ~ TRUE,
    COHORT_DEFINITION == "Healthy Control" ~ FALSE,
    COHORT_DEFINITION == "SWEDD" ~ FALSE
  ))
Tes3 <- select(Tes2, c(PATNO, SEX, COHORT_DEFINITION, ENROLL_AGE, hlBefore, HISPLAT, RAASIAN, RABLACK, RAHAWOPI, RAINDALS, RAWHITE, RANOS, RAUNKNOWN, ID))
Tes3 <- Tes3 %>%
  mutate(Race=case_when(
    HISPLAT == 1 ~ "Hispanic/Latino",
    RAASIAN == 1 ~ "Asian",
    RABLACK == 1 ~ "Black/African American",
    RAHAWOPI == 1 ~ "Hawaiian/Other Pacific Islander",
    RAINDALS == 1 ~ "American Indian/Alaska Native",
    RAWHITE == 1 ~ "White",
    RANOS == 1 ~ "Unknown",
    RAUNKNOWN == 1 ~ "Unknown"
  ))
Tes3 <- Tes3 %>%
  mutate(Sex=case_when(
    SEX == 1 ~ "Male",
    SEX == 0 ~ "Female",
  ))
Tes3 <- Tes3 %>%
  mutate(Age=case_when(
    ENROLL_AGE <18 ~ "<18",
    ENROLL_AGE >=18 & ENROLL_AGE <24 ~ "18-24",
    ENROLL_AGE >=24 & ENROLL_AGE <36 ~ "24-36",
    ENROLL_AGE >=36 & ENROLL_AGE <42 ~ "36-42",
    ENROLL_AGE >=42 & ENROLL_AGE <56 ~ "42-56",
    ENROLL_AGE >=56 & ENROLL_AGE <66 ~ "56-66",
    ENROLL_AGE >=66 & ENROLL_AGE <76 ~ "66-76",
    ENROLL_AGE >=76 & ENROLL_AGE <86 ~ "76-86",
    ENROLL_AGE >=86 ~ ">86"
  ))
Tes3 <- Tes3 %>%
  mutate(Hypertension=case_when(
    hlBefore == FALSE ~ "No Constipation",
    hlBefore == TRUE ~ "Constipation",
  ))
Tes3 <- Tes3 %>%
  mutate(ID=case_when(
    ID == TRUE ~ "Parkinson's or Prodromal",
    ID == FALSE ~ "Healthy Control or SWEDD",
  ))
Tes3 <- select(Tes3, c(PATNO, Sex, COHORT_DEFINITION, ENROLL_AGE, Age, Hypertension, Race, ID, hlBefore))
colnames(Tes3)[which(names(Tes3) == "COHORT_DEFINITION")] <- "Parkinsons"
label(Tes3$ID) <- "Parkinson's or Healthy?"

Tes3 <- Tes3 %>%
  mutate(Race = factor(Race, levels=c("White", "Black/African American", "Hispanic/Latino", "Asian", "American Indian/Alaska Native", "Hawaiian/Other Pacific Islander", "Unknown")))
Tes3 <- Tes3 %>%
  mutate(Sex = factor(Sex, levels=c("Male", "Female")))
Tes3 <- Tes3 %>%
  mutate(Age = factor(Age, levels=c("24-36", "36-42", "42-56", "56-66", "66-76", "76-86", ">86")))
Tes3 <- Tes3 %>%
  mutate(Parkinsons = factor(Parkinsons, levels=c("Parkinson's Disease", "Prodromal", "Healthy Control", "SWEDD")))

table1(~ Race + Sex + Age + Parkinsons | Hypertension, data=Tes3,
       title = "Demographics",
)

mod <- glm(ID ~ as.numeric(hlBefore) + ENROLL_AGE + SEX,
           data = Tes2,
           family = "binomial")
tab <- summary(mod)$coef

data4 <- data.frame(variable = rownames(tab),
                    oddsratio = round(exp(tab[,1]), 3),
                    ci_low = round(exp(tab[,1] - 1.96 * tab[,2]), 3),
                    ci_high = round(exp(tab[,1] + 1.96 * tab[,2]), 3),
                    pval = scales::pvalue(tab[,4]),
                    row.names = NULL)[-1,]
data4 <- data4[-c(2,3), ]
data4 <- data4 %>%
  mutate(variable = recode(variable,
                           'as.numeric(hlBefore)' = 'Constipation'))


#### Urinary Problems ####
#Make the urinary data
#Table of urinary against race, sex, age, and Parkinson's status

cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Urinary <- read_csv("data/MedConditions.csv")
Urinary <- Urinary[grepl("(Urinary\\b|uria\\b|urination\\b|prostatic hypertrophy\\b|hyperplasia\\b)",Urinary$MHTERM,ignore.case = F),]
tmp <- Urinary
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Test <- merge(MedConditions1,tmp,by="PATNO",all.x=T)
Test <- select(Test, c(PATNO, MHTERM.y, MHDIAGDT.y))

cohToTest <- merge(cohToTest,Test,by="PATNO",all.y=T)
cohToTest1 <- cohToTest[!is.na(cohToTest$MHTERM.y),]
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")

cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM.y),ifelse(cohToTest$hlDiff > (365 * 1) ,T,F),NA)

cohToTest <- merge(cohToTest,Demographics,by="PATNO",all.x=T)
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Tes1 <- cohToTest[!is.na(cohToTest$COHORT_DEFINITION),]
Tes1[c("hlBefore")][is.na(Tes1[c("hlBefore")])] <- FALSE

Tes2 <- Tes1 %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ TRUE,
    COHORT_DEFINITION == "Prodromal" ~ TRUE,
    COHORT_DEFINITION == "Healthy Control" ~ FALSE,
    COHORT_DEFINITION == "SWEDD" ~ FALSE
  ))
Tes3 <- select(Tes2, c(PATNO, SEX, COHORT_DEFINITION, ENROLL_AGE, hlBefore, HISPLAT, RAASIAN, RABLACK, RAHAWOPI, RAINDALS, RAWHITE, RANOS, RAUNKNOWN, ID))
Tes3 <- Tes3 %>%
  mutate(Race=case_when(
    HISPLAT == 1 ~ "Hispanic/Latino",
    RAASIAN == 1 ~ "Asian",
    RABLACK == 1 ~ "Black/African American",
    RAHAWOPI == 1 ~ "Hawaiian/Other Pacific Islander",
    RAINDALS == 1 ~ "American Indian/Alaska Native",
    RAWHITE == 1 ~ "White",
    RANOS == 1 ~ "Unknown",
    RAUNKNOWN == 1 ~ "Unknown"
  ))
Tes3 <- Tes3 %>%
  mutate(Sex=case_when(
    SEX == 1 ~ "Male",
    SEX == 0 ~ "Female",
  ))
Tes3 <- Tes3 %>%
  mutate(Age=case_when(
    ENROLL_AGE <18 ~ "<18",
    ENROLL_AGE >=18 & ENROLL_AGE <24 ~ "18-24",
    ENROLL_AGE >=24 & ENROLL_AGE <36 ~ "24-36",
    ENROLL_AGE >=36 & ENROLL_AGE <42 ~ "36-42",
    ENROLL_AGE >=42 & ENROLL_AGE <56 ~ "42-56",
    ENROLL_AGE >=56 & ENROLL_AGE <66 ~ "56-66",
    ENROLL_AGE >=66 & ENROLL_AGE <76 ~ "66-76",
    ENROLL_AGE >=76 & ENROLL_AGE <86 ~ "76-86",
    ENROLL_AGE >=86 ~ ">86"
  ))
Tes3 <- Tes3 %>%
  mutate(Hypertension=case_when(
    hlBefore == FALSE ~ "No Urinary Problems",
    hlBefore == TRUE ~ "Urinary Problems",
  ))
Tes3 <- Tes3 %>%
  mutate(ID=case_when(
    ID == TRUE ~ "Parkinson's or Prodromal",
    ID == FALSE ~ "Healthy Control or SWEDD",
  ))
Tes3 <- select(Tes3, c(PATNO, Sex, COHORT_DEFINITION, ENROLL_AGE, Age, Hypertension, Race, ID, hlBefore))
colnames(Tes3)[which(names(Tes3) == "COHORT_DEFINITION")] <- "Parkinsons"
label(Tes3$ID) <- "Parkinson's or Healthy?"

Tes3 <- Tes3 %>%
  mutate(Race = factor(Race, levels=c("White", "Black/African American", "Hispanic/Latino", "Asian", "American Indian/Alaska Native", "Hawaiian/Other Pacific Islander", "Unknown")))
Tes3 <- Tes3 %>%
  mutate(Sex = factor(Sex, levels=c("Male", "Female")))
Tes3 <- Tes3 %>%
  mutate(Age = factor(Age, levels=c("24-36", "36-42", "42-56", "56-66", "66-76", "76-86", ">86")))
Tes3 <- Tes3 %>%
  mutate(Parkinsons = factor(Parkinsons, levels=c("Parkinson's Disease", "Prodromal", "Healthy Control", "SWEDD")))

table1(~ Race + Sex + Age + Parkinsons | Hypertension, data=Tes3,
       title = "Demographics",
)

mod <- glm(ID ~ as.numeric(hlBefore) + ENROLL_AGE + SEX,
           data = Tes2,
           family = "binomial")
tab <- summary(mod)$coef

data5 <- data.frame(variable = rownames(tab),
                    oddsratio = round(exp(tab[,1]), 3),
                    ci_low = round(exp(tab[,1] - 1.96 * tab[,2]), 3),
                    ci_high = round(exp(tab[,1] + 1.96 * tab[,2]), 3),
                    pval = scales::pvalue(tab[,4]),
                    row.names = NULL)[-1,]
data5 <- data5[-c(2,3), ]
data5 <- data5 %>%
  mutate(variable = recode(variable,
                           'as.numeric(hlBefore)' = 'Urinary problems'))



#### Sleep Problems ####
#Make the sleep problems data
#Table of sleep problems against race, sex, age, and Parkinson's status

cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Sleep <- read_csv("data/MedConditions.csv")
Sleep <- Sleep[grepl("(sleep\\b|sleeping|somnia\\b|restless leg\\b|dream\\b|nightmare\\b)",Sleep$MHTERM,ignore.case = T),]
tmp <- Sleep
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Test <- merge(MedConditions1,tmp,by="PATNO",all.x=T)
Test <- select(Test, c(PATNO, MHTERM.y, MHDIAGDT.y))

cohToTest <- merge(cohToTest,Test,by="PATNO",all.y=T)
cohToTest1 <- cohToTest[!is.na(cohToTest$MHTERM.y),]
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")

cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM.y),ifelse(cohToTest$hlDiff > (365 * 1) ,T,F),NA)

cohToTest <- merge(cohToTest,Demographics,by="PATNO",all.x=T)
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Tes1 <- cohToTest[!is.na(cohToTest$COHORT_DEFINITION),]
Tes1[c("hlBefore")][is.na(Tes1[c("hlBefore")])] <- FALSE

Tes2 <- Tes1 %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ TRUE,
    COHORT_DEFINITION == "Prodromal" ~ TRUE,
    COHORT_DEFINITION == "Healthy Control" ~ FALSE,
    COHORT_DEFINITION == "SWEDD" ~ FALSE
  ))
Tes3 <- select(Tes2, c(PATNO, SEX, COHORT_DEFINITION, ENROLL_AGE, hlBefore, HISPLAT, RAASIAN, RABLACK, RAHAWOPI, RAINDALS, RAWHITE, RANOS, RAUNKNOWN, ID))
Tes3 <- Tes3 %>%
  mutate(Race=case_when(
    HISPLAT == 1 ~ "Hispanic/Latino",
    RAASIAN == 1 ~ "Asian",
    RABLACK == 1 ~ "Black/African American",
    RAHAWOPI == 1 ~ "Hawaiian/Other Pacific Islander",
    RAINDALS == 1 ~ "American Indian/Alaska Native",
    RAWHITE == 1 ~ "White",
    RANOS == 1 ~ "Unknown",
    RAUNKNOWN == 1 ~ "Unknown"
  ))
Tes3 <- Tes3 %>%
  mutate(Sex=case_when(
    SEX == 1 ~ "Male",
    SEX == 0 ~ "Female",
  ))
Tes3 <- Tes3 %>%
  mutate(Age=case_when(
    ENROLL_AGE <18 ~ "<18",
    ENROLL_AGE >=18 & ENROLL_AGE <24 ~ "18-24",
    ENROLL_AGE >=24 & ENROLL_AGE <36 ~ "24-36",
    ENROLL_AGE >=36 & ENROLL_AGE <42 ~ "36-42",
    ENROLL_AGE >=42 & ENROLL_AGE <56 ~ "42-56",
    ENROLL_AGE >=56 & ENROLL_AGE <66 ~ "56-66",
    ENROLL_AGE >=66 & ENROLL_AGE <76 ~ "66-76",
    ENROLL_AGE >=76 & ENROLL_AGE <86 ~ "76-86",
    ENROLL_AGE >=86 ~ ">86"
  ))
Tes3 <- Tes3 %>%
  mutate(Hypertension=case_when(
    hlBefore == FALSE ~ "No Sleep Problems",
    hlBefore == TRUE ~ "Sleep Problems",
  ))
Tes3 <- Tes3 %>%
  mutate(ID=case_when(
    ID == TRUE ~ "Parkinson's or Prodromal",
    ID == FALSE ~ "Healthy Control or SWEDD",
  ))
Tes3 <- select(Tes3, c(PATNO, Sex, COHORT_DEFINITION, ENROLL_AGE, Age, Hypertension, Race, ID, hlBefore))
colnames(Tes3)[which(names(Tes3) == "COHORT_DEFINITION")] <- "Parkinsons"
label(Tes3$ID) <- "Parkinson's or Healthy?"

Tes3 <- Tes3 %>%
  mutate(Race = factor(Race, levels=c("White", "Black/African American", "Hispanic/Latino", "Asian", "American Indian/Alaska Native", "Hawaiian/Other Pacific Islander", "Unknown")))
Tes3 <- Tes3 %>%
  mutate(Sex = factor(Sex, levels=c("Male", "Female")))
Tes3 <- Tes3 %>%
  mutate(Age = factor(Age, levels=c("24-36", "36-42", "42-56", "56-66", "66-76", "76-86", ">86")))
Tes3 <- Tes3 %>%
  mutate(Parkinsons = factor(Parkinsons, levels=c("Parkinson's Disease", "Prodromal", "Healthy Control", "SWEDD")))

table1(~ Race + Sex + Age + Parkinsons | Hypertension, data=Tes3,
       title = "Demographics",
)

mod <- glm(ID ~ as.numeric(hlBefore) + ENROLL_AGE + SEX,
           data = Tes2,
           family = "binomial")
tab <- summary(mod)$coef

data6 <- data.frame(variable = rownames(tab),
                    oddsratio = round(exp(tab[,1]), 3),
                    ci_low = round(exp(tab[,1] - 1.96 * tab[,2]), 3),
                    ci_high = round(exp(tab[,1] + 1.96 * tab[,2]), 3),
                    pval = scales::pvalue(tab[,4]),
                    row.names = NULL)[-1,]
data6 <- data6[-c(2,3), ]
data6 <- data6 %>%
  mutate(variable = recode(variable,
                           'as.numeric(hlBefore)' = 'Sleep problems'))



#### Loss of Smell ####
#Make the smell problems data
#Table of smell problems against race, sex, age, and Parkinson's status

cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Smell <- read_csv("data/MedConditions.csv")
Smell <- Smell[grepl("(smell\\b|osmia\\b)",Smell$MHTERM,ignore.case = T),]
tmp <- Smell
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Test <- merge(MedConditions1,tmp,by="PATNO",all.x=T)
Test <- select(Test, c(PATNO, MHTERM.y, MHDIAGDT.y))

cohToTest <- merge(cohToTest,Test,by="PATNO",all.y=T)
cohToTest1 <- cohToTest[!is.na(cohToTest$MHTERM.y),]
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")

cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM.y),ifelse(cohToTest$hlDiff > (365 * 1) ,T,F),NA)

cohToTest <- merge(cohToTest,Demographics,by="PATNO",all.x=T)
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Tes1 <- cohToTest[!is.na(cohToTest$COHORT_DEFINITION),]
Tes1[c("hlBefore")][is.na(Tes1[c("hlBefore")])] <- FALSE

Tes2 <- Tes1 %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ TRUE,
    COHORT_DEFINITION == "Prodromal" ~ TRUE,
    COHORT_DEFINITION == "Healthy Control" ~ FALSE,
    COHORT_DEFINITION == "SWEDD" ~ FALSE
  ))
Tes3 <- select(Tes2, c(PATNO, SEX, COHORT_DEFINITION, ENROLL_AGE, hlBefore, HISPLAT, RAASIAN, RABLACK, RAHAWOPI, RAINDALS, RAWHITE, RANOS, RAUNKNOWN, ID))
Tes3 <- Tes3 %>%
  mutate(Race=case_when(
    HISPLAT == 1 ~ "Hispanic/Latino",
    RAASIAN == 1 ~ "Asian",
    RABLACK == 1 ~ "Black/African American",
    RAHAWOPI == 1 ~ "Hawaiian/Other Pacific Islander",
    RAINDALS == 1 ~ "American Indian/Alaska Native",
    RAWHITE == 1 ~ "White",
    RANOS == 1 ~ "Unknown",
    RAUNKNOWN == 1 ~ "Unknown"
  ))
Tes3 <- Tes3 %>%
  mutate(Sex=case_when(
    SEX == 1 ~ "Male",
    SEX == 0 ~ "Female",
  ))
Tes3 <- Tes3 %>%
  mutate(Age=case_when(
    ENROLL_AGE <18 ~ "<18",
    ENROLL_AGE >=18 & ENROLL_AGE <24 ~ "18-24",
    ENROLL_AGE >=24 & ENROLL_AGE <36 ~ "24-36",
    ENROLL_AGE >=36 & ENROLL_AGE <42 ~ "36-42",
    ENROLL_AGE >=42 & ENROLL_AGE <56 ~ "42-56",
    ENROLL_AGE >=56 & ENROLL_AGE <66 ~ "56-66",
    ENROLL_AGE >=66 & ENROLL_AGE <76 ~ "66-76",
    ENROLL_AGE >=76 & ENROLL_AGE <86 ~ "76-86",
    ENROLL_AGE >=86 ~ ">86"
  ))
Tes3 <- Tes3 %>%
  mutate(Hypertension=case_when(
    hlBefore == FALSE ~ "No Smell Problems",
    hlBefore == TRUE ~ "Smell Problems",
  ))
Tes3 <- Tes3 %>%
  mutate(ID=case_when(
    ID == TRUE ~ "Parkinson's or Prodromal",
    ID == FALSE ~ "Healthy Control or SWEDD",
  ))
Tes3 <- select(Tes3, c(PATNO, Sex, COHORT_DEFINITION, ENROLL_AGE, Age, Hypertension, Race, ID, hlBefore))
colnames(Tes3)[which(names(Tes3) == "COHORT_DEFINITION")] <- "Parkinsons"
label(Tes3$ID) <- "Parkinson's or Healthy?"

Tes3 <- Tes3 %>%
  mutate(Race = factor(Race, levels=c("White", "Black/African American", "Hispanic/Latino", "Asian", "American Indian/Alaska Native", "Hawaiian/Other Pacific Islander", "Unknown")))
Tes3 <- Tes3 %>%
  mutate(Sex = factor(Sex, levels=c("Male", "Female")))
Tes3 <- Tes3 %>%
  mutate(Age = factor(Age, levels=c("24-36", "36-42", "42-56", "56-66", "66-76", "76-86", ">86")))
Tes3 <- Tes3 %>%
  mutate(Parkinsons = factor(Parkinsons, levels=c("Parkinson's Disease", "Prodromal", "Healthy Control", "SWEDD")))

table1(~ Race + Sex + Age + Parkinsons | Hypertension, data=Tes3,
       title = "Demographics",
)

mod <- glm(ID ~ as.numeric(hlBefore) + ENROLL_AGE + SEX,
           data = Tes2,
           family = "binomial")
tab <- summary(mod)$coef

data7 <- data.frame(variable = rownames(tab),
                    oddsratio = round(exp(tab[,1]), 3),
                    ci_low = round(exp(tab[,1] - 1.96 * tab[,2]), 3),
                    ci_high = round(exp(tab[,1] + 1.96 * tab[,2]), 3),
                    pval = scales::pvalue(tab[,4]),
                    row.names = NULL)[-1,]
data7 <- data7[-c(2,3), ]
data7 <- data7 %>%
  mutate(variable = recode(variable,
                           'as.numeric(hlBefore)' = 'Smell problems'))




#### Tremors ####
#Make the tremors data
#Table of tremors against race, sex, age, and Parkinson's status

cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Tremor <- read_csv("data/MedConditions.csv")
Tremor <- Tremor[grepl("(Tremor\\b|shaking\\b|twitch\\b|jerk\\b)",Tremor$MHTERM,ignore.case = T),]
tmp <- Tremor
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Test <- merge(MedConditions1,tmp,by="PATNO",all.x=T)
Test <- select(Test, c(PATNO, MHTERM.y, MHDIAGDT.y))

cohToTest <- merge(cohToTest,Test,by="PATNO",all.y=T)
cohToTest1 <- cohToTest[!is.na(cohToTest$MHTERM.y),]
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")

cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM.y),ifelse(cohToTest$hlDiff > (365 * 1) ,T,F),NA)

cohToTest <- merge(cohToTest,Demographics,by="PATNO",all.x=T)
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Tes1 <- cohToTest[!is.na(cohToTest$COHORT_DEFINITION),]
Tes1[c("hlBefore")][is.na(Tes1[c("hlBefore")])] <- FALSE

Tes2 <- Tes1 %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ TRUE,
    COHORT_DEFINITION == "Prodromal" ~ TRUE,
    COHORT_DEFINITION == "Healthy Control" ~ FALSE,
    COHORT_DEFINITION == "SWEDD" ~ FALSE
  ))
Tes3 <- select(Tes2, c(PATNO, SEX, COHORT_DEFINITION, ENROLL_AGE, hlBefore, HISPLAT, RAASIAN, RABLACK, RAHAWOPI, RAINDALS, RAWHITE, RANOS, RAUNKNOWN, ID))
Tes3 <- Tes3 %>%
  mutate(Race=case_when(
    HISPLAT == 1 ~ "Hispanic/Latino",
    RAASIAN == 1 ~ "Asian",
    RABLACK == 1 ~ "Black/African American",
    RAHAWOPI == 1 ~ "Hawaiian/Other Pacific Islander",
    RAINDALS == 1 ~ "American Indian/Alaska Native",
    RAWHITE == 1 ~ "White",
    RANOS == 1 ~ "Unknown",
    RAUNKNOWN == 1 ~ "Unknown"
  ))
Tes3 <- Tes3 %>%
  mutate(Sex=case_when(
    SEX == 1 ~ "Male",
    SEX == 0 ~ "Female",
  ))
Tes3 <- Tes3 %>%
  mutate(Age=case_when(
    ENROLL_AGE <18 ~ "<18",
    ENROLL_AGE >=18 & ENROLL_AGE <24 ~ "18-24",
    ENROLL_AGE >=24 & ENROLL_AGE <36 ~ "24-36",
    ENROLL_AGE >=36 & ENROLL_AGE <42 ~ "36-42",
    ENROLL_AGE >=42 & ENROLL_AGE <56 ~ "42-56",
    ENROLL_AGE >=56 & ENROLL_AGE <66 ~ "56-66",
    ENROLL_AGE >=66 & ENROLL_AGE <76 ~ "66-76",
    ENROLL_AGE >=76 & ENROLL_AGE <86 ~ "76-86",
    ENROLL_AGE >=86 ~ ">86"
  ))
Tes3 <- Tes3 %>%
  mutate(Hypertension=case_when(
    hlBefore == FALSE ~ "No Tremors",
    hlBefore == TRUE ~ "Tremors",
  ))
Tes3 <- Tes3 %>%
  mutate(ID=case_when(
    ID == TRUE ~ "Parkinson's or Prodromal",
    ID == FALSE ~ "Healthy Control or SWEDD",
  ))
Tes3 <- select(Tes3, c(PATNO, Sex, COHORT_DEFINITION, ENROLL_AGE, Age, Hypertension, Race, ID, hlBefore))
colnames(Tes3)[which(names(Tes3) == "COHORT_DEFINITION")] <- "Parkinsons"
label(Tes3$ID) <- "Parkinson's or Healthy?"

Tes3 <- Tes3 %>%
  mutate(Race = factor(Race, levels=c("White", "Black/African American", "Hispanic/Latino", "Asian", "American Indian/Alaska Native", "Hawaiian/Other Pacific Islander", "Unknown")))
Tes3 <- Tes3 %>%
  mutate(Sex = factor(Sex, levels=c("Male", "Female")))
Tes3 <- Tes3 %>%
  mutate(Age = factor(Age, levels=c("24-36", "36-42", "42-56", "56-66", "66-76", "76-86", ">86")))
Tes3 <- Tes3 %>%
  mutate(Parkinsons = factor(Parkinsons, levels=c("Parkinson's Disease", "Prodromal", "Healthy Control", "SWEDD")))

table1(~ Race + Sex + Age + Parkinsons | Hypertension, data=Tes3,
       title = "Demographics",
)

mod <- glm(ID ~ as.numeric(hlBefore) + ENROLL_AGE + SEX,
           data = Tes2,
           family = "binomial")
tab <- summary(mod)$coef

data8 <- data.frame(variable = rownames(tab),
                    oddsratio = round(exp(tab[,1]), 3),
                    ci_low = round(exp(tab[,1] - 1.96 * tab[,2]), 3),
                    ci_high = round(exp(tab[,1] + 1.96 * tab[,2]), 3),
                    pval = scales::pvalue(tab[,4]),
                    row.names = NULL)[-1,]
data8 <- data8[-c(2,3), ]
data8 <- data8 %>%
  mutate(variable = recode(variable,
                           'as.numeric(hlBefore)' = 'Tremors'))




#### Balance ####
#Make the balance data
#Table of balance against race, sex, age, and Parkinson's status

cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Balance <- read_csv("data/MedConditions.csv")
Balance <- Balance[grepl("(dizziness\\b|balance\\b|instability\\b|Unstable|Posture)",Balance$MHTERM,ignore.case = T),]
tmp <- Balance
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Test <- merge(MedConditions1,tmp,by="PATNO",all.x=T)
Test <- select(Test, c(PATNO, MHTERM.y, MHDIAGDT.y))

cohToTest <- merge(cohToTest,Test,by="PATNO",all.y=T)
cohToTest1 <- cohToTest[!is.na(cohToTest$MHTERM.y),]
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")

cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM.y),ifelse(cohToTest$hlDiff > (365 * 1) ,T,F),NA)

cohToTest <- merge(cohToTest,Demographics,by="PATNO",all.x=T)
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Tes1 <- cohToTest[!is.na(cohToTest$COHORT_DEFINITION),]
Tes1[c("hlBefore")][is.na(Tes1[c("hlBefore")])] <- FALSE

Tes2 <- Tes1 %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ TRUE,
    COHORT_DEFINITION == "Prodromal" ~ TRUE,
    COHORT_DEFINITION == "Healthy Control" ~ FALSE,
    COHORT_DEFINITION == "SWEDD" ~ FALSE
  ))
Tes3 <- select(Tes2, c(PATNO, SEX, COHORT_DEFINITION, ENROLL_AGE, hlBefore, HISPLAT, RAASIAN, RABLACK, RAHAWOPI, RAINDALS, RAWHITE, RANOS, RAUNKNOWN, ID))
Tes3 <- Tes3 %>%
  mutate(Race=case_when(
    HISPLAT == 1 ~ "Hispanic/Latino",
    RAASIAN == 1 ~ "Asian",
    RABLACK == 1 ~ "Black/African American",
    RAHAWOPI == 1 ~ "Hawaiian/Other Pacific Islander",
    RAINDALS == 1 ~ "American Indian/Alaska Native",
    RAWHITE == 1 ~ "White",
    RANOS == 1 ~ "Unknown",
    RAUNKNOWN == 1 ~ "Unknown"
  ))
Tes3 <- Tes3 %>%
  mutate(Sex=case_when(
    SEX == 1 ~ "Male",
    SEX == 0 ~ "Female",
  ))
Tes3 <- Tes3 %>%
  mutate(Age=case_when(
    ENROLL_AGE <18 ~ "<18",
    ENROLL_AGE >=18 & ENROLL_AGE <24 ~ "18-24",
    ENROLL_AGE >=24 & ENROLL_AGE <36 ~ "24-36",
    ENROLL_AGE >=36 & ENROLL_AGE <42 ~ "36-42",
    ENROLL_AGE >=42 & ENROLL_AGE <56 ~ "42-56",
    ENROLL_AGE >=56 & ENROLL_AGE <66 ~ "56-66",
    ENROLL_AGE >=66 & ENROLL_AGE <76 ~ "66-76",
    ENROLL_AGE >=76 & ENROLL_AGE <86 ~ "76-86",
    ENROLL_AGE >=86 ~ ">86"
  ))
Tes3 <- Tes3 %>%
  mutate(Hypertension=case_when(
    hlBefore == FALSE ~ "No Balance Problems",
    hlBefore == TRUE ~ "Balance Problems",
  ))
Tes3 <- Tes3 %>%
  mutate(ID=case_when(
    ID == TRUE ~ "Parkinson's or Prodromal",
    ID == FALSE ~ "Healthy Control or SWEDD",
  ))
Tes3 <- select(Tes3, c(PATNO, Sex, COHORT_DEFINITION, ENROLL_AGE, Age, Hypertension, Race, ID, hlBefore))
colnames(Tes3)[which(names(Tes3) == "COHORT_DEFINITION")] <- "Parkinsons"
label(Tes3$ID) <- "Parkinson's or Healthy?"

Tes3 <- Tes3 %>%
  mutate(Race = factor(Race, levels=c("White", "Black/African American", "Hispanic/Latino", "Asian", "American Indian/Alaska Native", "Hawaiian/Other Pacific Islander", "Unknown")))
Tes3 <- Tes3 %>%
  mutate(Sex = factor(Sex, levels=c("Male", "Female")))
Tes3 <- Tes3 %>%
  mutate(Age = factor(Age, levels=c("24-36", "36-42", "42-56", "56-66", "66-76", "76-86", ">86")))
Tes3 <- Tes3 %>%
  mutate(Parkinsons = factor(Parkinsons, levels=c("Parkinson's Disease", "Prodromal", "Healthy Control", "SWEDD")))

table1(~ Race + Sex + Age + Parkinsons | Hypertension, data=Tes3,
       title = "Demographics",
)

mod <- glm(ID ~ as.numeric(hlBefore) + ENROLL_AGE + SEX,
           data = Tes2,
           family = "binomial")
tab <- summary(mod)$coef

data9 <- data.frame(variable = rownames(tab),
                    oddsratio = round(exp(tab[,1]), 3),
                    ci_low = round(exp(tab[,1] - 1.96 * tab[,2]), 3),
                    ci_high = round(exp(tab[,1] + 1.96 * tab[,2]), 3),
                    pval = scales::pvalue(tab[,4]),
                    row.names = NULL)[-1,]
data9 <- data9[-c(2,3), ]
data9 <- data9 %>%
  mutate(variable = recode(variable,
                           'as.numeric(hlBefore)' = 'Balance Problems'))



#### PTSD ####
#Make the PTSD data
#Table of PTSD against race, sex, age, and Parkinson's status

cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

PTSD <- read_csv("data/MedConditions.csv")
PTSD <- PTSD[grepl("(Post traumatic stress disorder\\b|PTSD\\b|TBI\\b|traumatic brain injury\\b)",PTSD$MHTERM,ignore.case = T),]
tmp <- PTSD
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Test <- merge(MedConditions1,tmp,by="PATNO",all.x=T)
Test <- select(Test, c(PATNO, MHTERM.y, MHDIAGDT.y))

cohToTest <- merge(cohToTest,Test,by="PATNO",all.y=T)
cohToTest1 <- cohToTest[!is.na(cohToTest$MHTERM.y),]
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")

cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM.y),ifelse(cohToTest$hlDiff > (365 * 1) ,T,F),NA)

cohToTest <- merge(cohToTest,Demographics,by="PATNO",all.x=T)
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Tes1 <- cohToTest[!is.na(cohToTest$COHORT_DEFINITION),]
Tes1[c("hlBefore")][is.na(Tes1[c("hlBefore")])] <- FALSE

Tes2 <- Tes1 %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ TRUE,
    COHORT_DEFINITION == "Prodromal" ~ TRUE,
    COHORT_DEFINITION == "Healthy Control" ~ FALSE,
    COHORT_DEFINITION == "SWEDD" ~ FALSE
  ))
Tes3 <- select(Tes2, c(PATNO, SEX, COHORT_DEFINITION, ENROLL_AGE, hlBefore, HISPLAT, RAASIAN, RABLACK, RAHAWOPI, RAINDALS, RAWHITE, RANOS, RAUNKNOWN, ID))
Tes3 <- Tes3 %>%
  mutate(Race=case_when(
    HISPLAT == 1 ~ "Hispanic/Latino",
    RAASIAN == 1 ~ "Asian",
    RABLACK == 1 ~ "Black/African American",
    RAHAWOPI == 1 ~ "Hawaiian/Other Pacific Islander",
    RAINDALS == 1 ~ "American Indian/Alaska Native",
    RAWHITE == 1 ~ "White",
    RANOS == 1 ~ "Unknown",
    RAUNKNOWN == 1 ~ "Unknown"
  ))
Tes3 <- Tes3 %>%
  mutate(Sex=case_when(
    SEX == 1 ~ "Male",
    SEX == 0 ~ "Female",
  ))
Tes3 <- Tes3 %>%
  mutate(Age=case_when(
    ENROLL_AGE <18 ~ "<18",
    ENROLL_AGE >=18 & ENROLL_AGE <24 ~ "18-24",
    ENROLL_AGE >=24 & ENROLL_AGE <36 ~ "24-36",
    ENROLL_AGE >=36 & ENROLL_AGE <42 ~ "36-42",
    ENROLL_AGE >=42 & ENROLL_AGE <56 ~ "42-56",
    ENROLL_AGE >=56 & ENROLL_AGE <66 ~ "56-66",
    ENROLL_AGE >=66 & ENROLL_AGE <76 ~ "66-76",
    ENROLL_AGE >=76 & ENROLL_AGE <86 ~ "76-86",
    ENROLL_AGE >=86 ~ ">86"
  ))
Tes3 <- Tes3 %>%
  mutate(Hypertension=case_when(
    hlBefore == FALSE ~ "No PTSD",
    hlBefore == TRUE ~ "PTSD",
  ))
Tes3 <- Tes3 %>%
  mutate(ID=case_when(
    ID == TRUE ~ "Parkinson's or Prodromal",
    ID == FALSE ~ "Healthy Control or SWEDD",
  ))
Tes3 <- select(Tes3, c(PATNO, Sex, COHORT_DEFINITION, ENROLL_AGE, Age, Hypertension, Race, ID, hlBefore))
colnames(Tes3)[which(names(Tes3) == "COHORT_DEFINITION")] <- "Parkinsons"
label(Tes3$ID) <- "Parkinson's or Healthy?"

Tes3 <- Tes3 %>%
  mutate(Race = factor(Race, levels=c("White", "Black/African American", "Hispanic/Latino", "Asian", "American Indian/Alaska Native", "Hawaiian/Other Pacific Islander", "Unknown")))
Tes3 <- Tes3 %>%
  mutate(Sex = factor(Sex, levels=c("Male", "Female")))
Tes3 <- Tes3 %>%
  mutate(Age = factor(Age, levels=c("24-36", "36-42", "42-56", "56-66", "66-76", "76-86", ">86")))
Tes3 <- Tes3 %>%
  mutate(Parkinsons = factor(Parkinsons, levels=c("Parkinson's Disease", "Prodromal", "Healthy Control", "SWEDD")))

table1(~ Race + Sex + Age + Parkinsons | Hypertension, data=Tes3,
       title = "Demographics",
)

mod <- glm(ID ~ as.numeric(hlBefore) + ENROLL_AGE + SEX,
           data = Tes2,
           family = "binomial")
tab <- summary(mod)$coef

data10 <- data.frame(variable = rownames(tab),
                     oddsratio = round(exp(tab[,1]), 3),
                     ci_low = round(exp(tab[,1] - 1.96 * tab[,2]), 3),
                     ci_high = round(exp(tab[,1] + 1.96 * tab[,2]), 3),
                     pval = scales::pvalue(tab[,4]),
                     row.names = NULL)[-1,]
data10 <- data10[-c(2,3), ]
data10 <- data10 %>%
  mutate(variable = recode(variable,
                           'as.numeric(hlBefore)' = 'PTSD'))



#### Diabetes ####
#Make the diabetes data
#Table of diabetes against race, sex, age, and Parkinson's status

cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

Diabetes <- read_csv("data/MedConditions.csv")
Diabetes <- Diabetes[grepl("(Diabetes\\b|Diabetic\\b)",Diabetes$MHTERM,ignore.case = T),]
tmp <- Diabetes
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Test <- merge(MedConditions1,tmp,by="PATNO",all.x=T)
Test <- select(Test, c(PATNO, MHTERM.y, MHDIAGDT.y))

cohToTest <- merge(cohToTest,Test,by="PATNO",all.y=T)
cohToTest1 <- cohToTest[!is.na(cohToTest$MHTERM.y),]
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")

cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM.y),ifelse(cohToTest$hlDiff > (365 * 1) ,T,F),NA)

cohToTest <- merge(cohToTest,Demographics,by="PATNO",all.x=T)
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Tes1 <- cohToTest[!is.na(cohToTest$COHORT_DEFINITION),]
Tes1[c("hlBefore")][is.na(Tes1[c("hlBefore")])] <- FALSE

Tes2 <- Tes1 %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ TRUE,
    COHORT_DEFINITION == "Prodromal" ~ TRUE,
    COHORT_DEFINITION == "Healthy Control" ~ FALSE,
    COHORT_DEFINITION == "SWEDD" ~ FALSE
  ))
Tes3 <- select(Tes2, c(PATNO, SEX, COHORT_DEFINITION, ENROLL_AGE, hlBefore, HISPLAT, RAASIAN, RABLACK, RAHAWOPI, RAINDALS, RAWHITE, RANOS, RAUNKNOWN, ID))
Tes3 <- Tes3 %>%
  mutate(Race=case_when(
    HISPLAT == 1 ~ "Hispanic/Latino",
    RAASIAN == 1 ~ "Asian",
    RABLACK == 1 ~ "Black/African American",
    RAHAWOPI == 1 ~ "Hawaiian/Other Pacific Islander",
    RAINDALS == 1 ~ "American Indian/Alaska Native",
    RAWHITE == 1 ~ "White",
    RANOS == 1 ~ "Unknown",
    RAUNKNOWN == 1 ~ "Unknown"
  ))
Tes3 <- Tes3 %>%
  mutate(Sex=case_when(
    SEX == 1 ~ "Male",
    SEX == 0 ~ "Female",
  ))
Tes3 <- Tes3 %>%
  mutate(Age=case_when(
    ENROLL_AGE <18 ~ "<18",
    ENROLL_AGE >=18 & ENROLL_AGE <24 ~ "18-24",
    ENROLL_AGE >=24 & ENROLL_AGE <36 ~ "24-36",
    ENROLL_AGE >=36 & ENROLL_AGE <42 ~ "36-42",
    ENROLL_AGE >=42 & ENROLL_AGE <56 ~ "42-56",
    ENROLL_AGE >=56 & ENROLL_AGE <66 ~ "56-66",
    ENROLL_AGE >=66 & ENROLL_AGE <76 ~ "66-76",
    ENROLL_AGE >=76 & ENROLL_AGE <86 ~ "76-86",
    ENROLL_AGE >=86 ~ ">86"
  ))
Tes3 <- Tes3 %>%
  mutate(Hypertension=case_when(
    hlBefore == FALSE ~ "No Diabetes",
    hlBefore == TRUE ~ "Diabetes",
  ))
Tes3 <- Tes3 %>%
  mutate(ID=case_when(
    ID == TRUE ~ "Parkinson's or Prodromal",
    ID == FALSE ~ "Healthy Control or SWEDD",
  ))
Tes3 <- select(Tes3, c(PATNO, Sex, COHORT_DEFINITION, ENROLL_AGE, Age, Hypertension, Race, ID, hlBefore))
colnames(Tes3)[which(names(Tes3) == "COHORT_DEFINITION")] <- "Parkinsons"
label(Tes3$ID) <- "Parkinson's or Healthy?"

Tes3 <- Tes3 %>%
  mutate(Race = factor(Race, levels=c("White", "Black/African American", "Hispanic/Latino", "Asian", "American Indian/Alaska Native", "Hawaiian/Other Pacific Islander", "Unknown")))
Tes3 <- Tes3 %>%
  mutate(Sex = factor(Sex, levels=c("Male", "Female")))
Tes3 <- Tes3 %>%
  mutate(Age = factor(Age, levels=c("24-36", "36-42", "42-56", "56-66", "66-76", "76-86", ">86")))
Tes3 <- Tes3 %>%
  mutate(Parkinsons = factor(Parkinsons, levels=c("Parkinson's Disease", "Prodromal", "Healthy Control", "SWEDD")))

table1(~ Race + Sex + Age + Parkinsons | Hypertension, data=Tes3,
       title = "Demographics",
)

mod <- glm(ID ~ as.numeric(hlBefore) + ENROLL_AGE + SEX,
           data = Tes2,
           family = "binomial")
tab <- summary(mod)$coef

data11 <- data.frame(variable = rownames(tab),
                     oddsratio = round(exp(tab[,1]), 3),
                     ci_low = round(exp(tab[,1] - 1.96 * tab[,2]), 3),
                     ci_high = round(exp(tab[,1] + 1.96 * tab[,2]), 3),
                     pval = scales::pvalue(tab[,4]),
                     row.names = NULL)[-1,]
data11 <- data11[-c(2,3), ]
data11 <- data11 %>%
  mutate(variable = recode(variable,
                           'as.numeric(hlBefore)' = 'Diabetes'))



#### Merge all the data ####
total <- merge(data1,data2,by=c("variable", "oddsratio", "ci_low", "ci_high", "pval"),all=T)
total <- merge(data3,total,by=c("variable", "oddsratio", "ci_low", "ci_high", "pval"),all=T)
total <- merge(data4,total,by=c("variable", "oddsratio", "ci_low", "ci_high", "pval"),all=T)
total <- merge(data5,total,by=c("variable", "oddsratio", "ci_low", "ci_high", "pval"),all=T)
total <- merge(data6,total,by=c("variable", "oddsratio", "ci_low", "ci_high", "pval"),all=T)
total <- merge(data7,total,by=c("variable", "oddsratio", "ci_low", "ci_high", "pval"),all=T)
total <- merge(data8,total,by=c("variable", "oddsratio", "ci_low", "ci_high", "pval"),all=T)
total <- merge(data9,total,by=c("variable", "oddsratio", "ci_low", "ci_high", "pval"),all=T)
total <- merge(data10,total,by=c("variable", "oddsratio", "ci_low", "ci_high", "pval"),all=T)
total <- merge(data11,total,by=c("variable", "oddsratio", "ci_low", "ci_high", "pval"),all=T)

#Change column names
colnames(total)[which(names(total) == "variable")] <- "Variable"
colnames(total)[which(names(total) == "oddsratio")] <- "Odds Ratio"
colnames(total)[which(names(total) == "ci_low")] <- "Confidence Interval (Low)"
colnames(total)[which(names(total) == "ci_high")] <- "Confidence Interval (High)"
colnames(total)[which(names(total) == "pval")] <- "P-value"

#Now further analysis can be done with this combined dataset