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

#The table shows people’s sex, age, race, hearing loss, and BMI based on their Parkinson’s status
#You can see how many people in each group have hearing loss in Parkinson's patients, Prodromal patients, healthy controls, and SWEDD patients


#Import data
Participant <- read_csv("data/ParticipantStatus")
HeadInjury <- read_csv("data/HeadInjury.csv")
PDDiagHistory <- read.csv("data/PDDiagHistory.csv")
MedConditions <- read_csv("data/MedConditions.csv")
Demographics <- read_csv("data/Demographics.csv")


# Import Head Injury data
HeadInjury <- HeadInjury
tmp <- HeadInjury
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"
tmp2 <- merge(Participant,tmp,by="PATNO",all.x=T)

#Import data for hearing loss
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
cohToTest <- merge(tmp2,hloss,by="PATNO",all.x=T)
cohToTest = cohToTest[!duplicated(cohToTest$PATNO),]

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

cohToTest$hl <- ifelse(is.na(cohToTest$hlBefore),F,cohToTest$hlBefore)
cohToTest$hiq1 <- ifelse(is.na(cohToTest$hiq1),NA,ifelse(cohToTest$hiq1 == 9999,NA,cohToTest$hiq1))
cohToTest$hiqYes <- ifelse(cohToTest$hiq1 == 1,T,F)

cohToTest$ID <- ifelse(grepl("Park",cohToTest$COHORT_DEFINITION,ignore.case=T),T,F)

cohToTest <- merge(cohToTest,Demographics,by="PATNO",all.x=T)

cohToTest$testHiqAge <- ifelse(cohToTest$hiqa1_age <= 13,T,F)

cohToTest <- select(cohToTest, c(PATNO, hlBefore, COHORT_DEFINITION))

cohToTest <- cohToTest[!is.na(cohToTest$COHORT_DEFINITION),]
cohToTest[is.na(cohToTest)] <- FALSE



### Sex, Age, Race ###
#Merge the datasets and rename columns
total <- merge(Demographics,Participant_Status,by="PATNO")
total <- total[c("PATNO", "SEX", "RAASIAN", "RABLACK", "RAHAWOPI", "RAINDALS", "RANOS", "RAWHITE", "RAUNKNOWN", "COHORT_DEFINITION", "ENROLL_AGE")]
colnames(total)[which(names(total) == "RAHAWOPI")] <- "Hawaiian_Other_Pacific_Islander"
colnames(total)[which(names(total) == "RAINDALS")] <- "American_Indian_Alaska_Native"
colnames(total)[which(names(total) == "RANOS")] <- "Race_not_specified"

#Rename column values
total["RAASIAN"][total["RAASIAN"] == 0] <- NA
total["RAASIAN"][total["RAASIAN"] == 1] <- "Asian"

total["RABLACK"][total["RABLACK"] == 0] <- NA
total["RABLACK"][total["RABLACK"] == 1] <- "Black"

total["Hawaiian_Other_Pacific_Islander"][total["Hawaiian_Other_Pacific_Islander"] == 0] <- NA
total["Hawaiian_Other_Pacific_Islander"][total["Hawaiian_Other_Pacific_Islander"] == 1] <- "Hawaiian/Other Pacific Islander"

total["American_Indian_Alaska_Native"][total["American_Indian_Alaska_Native"] == 0] <- NA
total["American_Indian_Alaska_Native"][total["American_Indian_Alaska_Native"] == 1] <- "American Indian/Alaska Native"

total["Race_not_specified"][total["Race_not_specified"] == 0] <- NA
total["Race_not_specified"][total["Race_not_specified"] == 1] <- "Race not specified"

total["RAWHITE"][total["RAWHITE"] == 0] <- NA
total["RAWHITE"][total["RAWHITE"] == 1] <- "White"

total["RAUNKNOWN"][total["RAUNKNOWN"] == 0] <- NA
total["RAUNKNOWN"][total["RAUNKNOWN"] == 1] <- "Unknown"

#Keep certain columns and remove NA values
totalwhite <- total
totalwhite <- totalwhite[c("PATNO","RAWHITE")]
colnames(totalwhite)[which(names(totalwhite) == "RAWHITE")] <- "Race"
totalwhite <- na.omit(totalwhite)

totalblack <- total
totalblack <- totalblack[c("PATNO","RABLACK")]
colnames(totalblack)[which(names(totalblack) == "RABLACK")] <- "Race"
totalblack <- na.omit(totalblack)

totalasian <- total
totalasian <- totalasian[c("PATNO","RAASIAN")]
colnames(totalasian)[which(names(totalasian) == "RAASIAN")] <- "Race"
totalasian <- na.omit(totalasian)

totalhawaiian <- total
totalhawaiian <- totalhawaiian[c("PATNO","Hawaiian_Other_Pacific_Islander")]
colnames(totalhawaiian)[which(names(totalhawaiian) == "Hawaiian_Other_Pacific_Islander")] <- "Race"
totalhawaiian <- na.omit(totalhawaiian)

totalindian <- total
totalindian <- totalindian[c("PATNO","American_Indian_Alaska_Native")]
colnames(totalindian)[which(names(totalindian) == "American_Indian_Alaska_Native")] <- "Race"
totalindian <- na.omit(totalindian)

totalnotspecified <- total
totalnotspecified <- totalnotspecified[c("PATNO","Race_not_specified")]
colnames(totalnotspecified)[which(names(totalnotspecified) == "Race_not_specified")] <- "Race"
totalnotspecified <- na.omit(totalnotspecified)

totalunknown <- total
totalunknown <- totalunknown[c("PATNO", "RAUNKNOWN")]
colnames(totalunknown)[which(names(totalunknown) == "RAUNKNOWN")] <- "Race"
totalunknown <- na.omit(totalunknown)

total1 <- total
total1 <- total1[c("PATNO", "SEX", "COHORT_DEFINITION", "ENROLL_AGE")]

#Combine all datasets
combine <- merge(totalwhite, totalblack, by=c("PATNO", "Race"), all=T)
combine1 <- merge(totalasian, combine, by=c("PATNO", "Race"), all=T)
combine2 <- merge(totalhawaiian, combine1, by=c("PATNO", "Race"), all=T)
combine3 <- merge(totalindian, combine2, by=c("PATNO", "Race"), all=T)
combine4 <- merge(totalnotspecified, combine3, by=c("PATNO", "Race"), all=T)
combine5 <- merge(totalunknown, combine4, by=c("PATNO", "Race"), all=T)
combine6 <- merge(total1, combine4, by=c("PATNO"), all=T)

cohToTest <- cohToTest[c("PATNO", "hlBefore")]
combine6 <- merge(cohToTest, combine6, by=c("PATNO"), all=T)

#Remove unnecessary datasets
rm("combine", "combine1", "combine2", "combine3", "combine4", "combine5",
   "total1",
   "totalasian", "totalblack", "totalhawaiian", "totalindian", "totalnotspecified", "totalunknown", "totalwhite",
   "Demographics", "HeadInjury", "ParticipantStatus", "MedConditions", "PDDiagHistory",
   "HeadInjury", "tmp", "tmp2")

#Rename column values
combine6["SEX"][combine6["SEX"] == 0] <- "Female"
combine6["SEX"][combine6["SEX"] == 1] <- "Male"

#Create 2 categories for age
combine7 <- combine6 %>%
  mutate(ENROLLAGE=case_when(
    ENROLL_AGE<55 ~ "<55",
    ENROLL_AGE>=55 ~ ">55"))

#Sort the order of the rows for the table
combine7$ENROLLAGE = factor(combine7$ENROLLAGE, levels = c("<55",
                                                           ">55"), ordered = TRUE)

combine7$Race = factor(combine7$Race, levels = c("American Indian/Alaska Native", 
                                                 "Asian", 
                                                 "Black",
                                                 "Hawaiian/Other Pacific Islander",
                                                 "White",
                                                 "Race not specified"), ordered = TRUE)

colnames(combine7)[which(names(combine7) == "SEX")] <- "Sex"
colnames(combine7)[which(names(combine7) == "ENROLLAGE")] <- "Age"
label(combine7$hlBefore) <- "Hearing Loss before Parkinson's Disease"
label(combine7$ENROLL_AGE) <- "Age"


### BMI ###
#Load Vitals data
VitalSigns <- read_csv("data/VitalSigns.csv")

#Sort by date and keep the most recent date for each patient number
VitalSigns$ORIG_ENTRY <- as.Date(VitalSigns$ORIG_ENTRY, format = "%m/%Y/%d")

VitalSigns <- VitalSigns %>%
  arrange(PATNO, desc(ORIG_ENTRY)) %>%  
  group_by(PATNO) %>%
  slice(1) %>%
  ungroup()

#Sort vital signs and merge
VitalSigns1 <- VitalSigns[c("PATNO", "WGTKG", "HTCM")]
VitalSigns1 <- transform( 
  VitalSigns1, Heightm = (HTCM/100)) 
VitalSigns1 <- transform( 
  VitalSigns1, Height2 = (Heightm * Heightm)) 
VitalSigns1 <- transform( 
  VitalSigns1, BMI = (WGTKG/Height2)) 
VitalSigns2 <- VitalSigns1[c("PATNO", "BMI")]
combine8 <- merge(combine7, VitalSigns2, by=c("PATNO"), all.y=T)

#Remove NA rows
combine8 <- combine8 %>% drop_na(COHORT_DEFINITION)

#Create a table
table1(~ Sex +
         Age +
         ENROLL_AGE +
         Race +
         hlBefore +
         BMI| COHORT_DEFINITION, combine8,
       topclass="Rtable1-zebra",
       render.missing = NULL)