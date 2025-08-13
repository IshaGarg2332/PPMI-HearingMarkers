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


#The logistic regression model examines whether participants are in the Parkinson’s/Prodromal group (vs. Healthy Control/SWEDD), using hearing loss, age, and sex as predictors.
#Individuals without clear cohort definitions or with missing hearing loss data were excluded
#The table shows that hearing loss prior to PD diagnosis or enrollment is associated with increased odds of being in the Parkinson’s/Prodromal group


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

#Keep only the hearing loss that was diagnosed at least a year prior to Parkinson's disease diagnosis
cohToTest <- merge(cohToTest,Test,by="PATNO",all.y=T)
cohToTest1 <- cohToTest[!is.na(cohToTest$MHTERM.y),]
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")

cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM.y),ifelse(cohToTest$hlDiff > (365 * 1) ,T,F),NA)

#Merge the data and remove duplicated
cohToTest <- merge(cohToTest,Demographics,by="PATNO",all.x=T)
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

Tes1 <- select(cohToTest, c(PATNO, SEX, COHORT_DEFINITION, ENROLL_AGE, hlBefore))
Tes1 <- Tes1[!is.na(Tes1$COHORT_DEFINITION),]
Tes1[c("hlBefore")][is.na(Tes1[c("hlBefore")])] <- FALSE

#Prodromal and Parkinson's is true while healthy controls and SWEDD are false
Tes2 <- Tes1 %>%
  mutate(ID=case_when(
    COHORT_DEFINITION == "Parkinson's Disease" ~ TRUE,
    COHORT_DEFINITION == "Prodromal" ~ TRUE,
    COHORT_DEFINITION == "Healthy Control" ~ FALSE,
    COHORT_DEFINITION == "SWEDD" ~ FALSE
  ))
Tes3 <- merge(Tes2,Demographics,by="PATNO",all.x=T)
Tes3 <- select(Tes3, c(PATNO, SEX.x, COHORT_DEFINITION, ENROLL_AGE, hlBefore, HISPLAT, RAASIAN, RABLACK, RAHAWOPI, RAINDALS, RAWHITE, RANOS, RAUNKNOWN, ID))

#Mutate the rest of the columns
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

#Select certain columns
Tes3 <- select(Tes3, c(PATNO, Sex, COHORT_DEFINITION, ENROLL_AGE, Age, HearingLoss, Race, ID))
colnames(Tes3)[which(names(Tes3) == "COHORT_DEFINITION")] <- "Parkinsons"
label(Tes3$ID) <- "Parkinson's or Healthy?"

#Mutate the order of the columns in the table
Tes3 <- Tes3 %>%
  mutate(Sex = factor(Sex, levels=c("Male", "Female")))
Tes3 <- Tes3 %>%
  mutate(Age = factor(Age, levels=c("24-36", "36-42", "42-56", "56-66", "66-76", "76-86", ">86")))
Tes3 <- Tes3 %>%
  mutate(Parkinsons = factor(Parkinsons, levels=c("Parkinson's Disease", "Prodromal", "Healthy Control", "SWEDD")))

#Make the table
table1(~ Sex + Age + Parkinsons | HearingLoss, data=Tes3,
       title = "Demographics",
)

#Make the proportion table
mod <- glm(ID ~ as.numeric(hlBefore) + ENROLL_AGE + SEX,
           data = Tes2,
           family = "binomial")
tab <- summary(mod)$coef

data <- data.frame(variable = rownames(tab),
                   oddsratio = round(exp(tab[,1]), 3),
                   ci_low = round(exp(tab[,1] - 1.96 * tab[,2]), 3),
                   ci_high = round(exp(tab[,1] + 1.96 * tab[,2]), 3),
                   pval = scales::pvalue(tab[,4]),
                   row.names = NULL)[-1,]
data1 <- data[-c(2,3), ]
data1 <- data1 %>%
  mutate(variable = recode(variable,
                           'as.numeric(hlBefore)' = 'Hearing Loss'))

label(data$ci_low) <- "Low Confidence Interval"
label(data$ci_high) <- "High Confidence Interval"
label(data$pval) <- "P-value"
label(data$oddsratio) <- "Odds Ratio"
label(data$variable) <- "Variable"
data <- data %>%
  mutate(variable=case_when(
    variable == "as.numeric(hlBefore)" ~ "Hearing Loss",
    variable == "ENROLL_AGE" ~ "Age",
    variable == "SEX" ~ "Sex",
  ))
label(data$variable) <- "Variables"

gt_tbl <- gt(data)
gt_tbl <-
  gt_tbl |>
  tab_header(
    title = "Summary Table",
  )
gt_tbl