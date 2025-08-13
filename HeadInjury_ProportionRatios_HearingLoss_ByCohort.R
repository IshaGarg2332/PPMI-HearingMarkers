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


#The table displays the proportion ratios of head injury occurrence ("Yes" or "Unsure") among ParticipantStatuss with hearing loss compared to those without
#Cohorts included are Healthy Controls, Prodromal, and Parkinson’s Disease ParticipantStatuss
#A ratio above 1 indicates a higher proportion of head injury among ParticipantStatuss with hearing loss compared to those without hearing loss in that cohort


#Import data
ParticipantStatus <- read_csv("data/ParticipantStatus.csv")
PDDiagHistory <- read.csv("data/PDDiagHistory.csv")

#Combine ParticipantStatusStatus and DiagHistory
cohToTest <- ParticipantStatus
cohToTest <- cohToTest[!is.na(cohToTest$ENROLL_AGE),]
cohToTest$EnrollDate <- as.Date(paste0("01/",cohToTest$ENROLL_DATE),format="%d/%m/%Y")
cohToTest <- merge(cohToTest,PDDiagHistory[,c("PATNO","PDDXDT")],by="PATNO",all.x=T)

#Mutate concussion (head injury) data
FOUND_RFQ_Head_Injury_13Jun2024 <- read_csv("data/HeadInjury.csv")
HeadInjury <- read_csv("data/MedConditions.csv")
HeadInjury <- HeadInjury[grepl("(Concussion|concussion|concussions|skull fracture|Skull fracture|Head Injury|head injury|Head injury)",HeadInjury$MHTERM,ignore.case = F),]
tmp <- HeadInjury
names(tmp)[names(tmp) == "MHTERM"] <- "hiq1"
colnames(tmp)[colnames(tmp) == "patno"] <- "PATNO"

#Merge data
tmp2 <- merge(cohToTest,tmp,by="PATNO",all.x=T)

#Form hearing loss data
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

#Keep only the ones where they were diagnosed with hearing loss at least a year prior to diagnosis of Parkinson's disease
cohToTest$HLDate <- as.Date(paste0("01/",cohToTest$MHDIAGDT.y),format="%d/%m/%Y")
cohToTest$PDdate <- as.Date(paste0("01/",cohToTest$PDDXDT),format="%d/%m/%Y")

cohToTest$hlDiff <- ifelse(is.na(cohToTest$PDdate),cohToTest$EnrollDate - cohToTest$HLDate,cohToTest$PDdate - cohToTest$HLDate)
cohToTest$hlBefore <- ifelse(!is.na(cohToTest$MHTERM),ifelse(cohToTest$hlDiff > (-365 * 1) ,T,F),F)

cohToTest$COHORT_DEFINITION <- as.factor(cohToTest$COHORT_DEFINITION)

#Select certain columns
cohToTest <- select(cohToTest, c(PATNO, COHORT_DEFINITION, EnrollDate, HLDate, PDdate, hlDiff, hlBefore, hiq1, MHTERM))

#Make four copies of the dataframe and each are for one type of Parkinson's status
Par <- cohToTest
Par1 <- cohToTest
Par2 <- cohToTest
Par3 <- cohToTest

Par <- cohToTest[cohToTest$COHORT_DEFINITION %in% c("Parkinson's Disease"),]
Par1 <- cohToTest[cohToTest$COHORT_DEFINITION %in% c("Healthy Control"),]
Par2 <- cohToTest[cohToTest$COHORT_DEFINITION %in% c("Prodromal"),]
Par3 <- cohToTest[cohToTest$COHORT_DEFINITION %in% c("SWEDD"),]

names(Par)[names(Par) == "COHORT_DEFINITION"] <- "definition1"
names(Par1)[names(Par1) == "COHORT_DEFINITION"] <- "definition2"
names(Par2)[names(Par2) == "COHORT_DEFINITION"] <- "definition3"
names(Par3)[names(Par3) == "COHORT_DEFINITION"] <- "definition4"

rm("ParticipantStatus", "FOUND_RFQ_Head_Injury_13Jun2024", "MedConditions")

Parkinson <- Par
Healthy <- Par1
SWEDD <- Par3
Prodromal <- Par2

#Add a column of consecutive numbers
Healthy$consecutive<-1:nrow(Healthy)
Parkinson$consecutive<-1:nrow(Parkinson)
Prodromal$consecutive<-1:nrow(Prodromal)
SWEDD$consecutive<-1:nrow(SWEDD)

#Rename columns
names(Healthy)[names(Healthy) == "PATNO"] <- "patient1"
names(Parkinson)[names(Parkinson) == "PATNO"] <- "patient2"
names(Prodromal)[names(Prodromal) == "PATNO"] <- "patient3"
names(SWEDD)[names(SWEDD) == "PATNO"] <- "patient4"

names(Healthy)[names(Healthy) == "hiq1"] <- "Head1"
names(Parkinson)[names(Parkinson) == "hiq1"] <- "Head2"
names(Prodromal)[names(Prodromal) == "hiq1"] <- "Head3"
names(SWEDD)[names(SWEDD) == "hiq1"] <- "Head4"

names(Healthy)[names(Healthy) == "MHTERM"] <- "term1"
names(Parkinson)[names(Parkinson) == "MHTERM"] <- "term2"
names(Prodromal)[names(Prodromal) == "MHTERM"] <- "term3"
names(SWEDD)[names(SWEDD) == "MHTERM"] <- "term4"
rm("HeadInjury", "hloss", "Par", "Par1", "Par2", "Par3", "PDDiagHistory", "tmp", "tmp2")

#Mutate column values for healthy controls
Healthy <- Healthy %>%
  mutate(hlBefore=case_when(
    hlBefore == FALSE ~ "False",
    hlBefore == TRUE ~ "True",
  ))
Healthy <- Healthy %>%
  mutate(Head1=case_when(
    is.na(Head1) ~ "Unsure",
    !is.na(Head1) ~ "Yes"
  ))
Healthy <- Healthy[!is.na(Healthy$hlBefore),]

#Create a table for healthy controls
table1(~ Head1
       | hlBefore, data=Healthy, render.missing = NULL, 
       caption = "Healthy Control")

#Mutate column values for Parkinson's
Parkinson <- Parkinson %>%
  mutate(hlBefore=case_when(
    hlBefore == FALSE ~ "False",
    hlBefore == TRUE ~ "True",
  ))
Parkinson <- Parkinson %>%
  mutate(Head2=case_when(
    is.na(Head2) ~ "Unsure",
    !is.na(Head2) ~ "Yes"
  ))
Parkinson <- Parkinson[!is.na(Parkinson$hlBefore),]

#Create a table for Parkinson's
table2 <- table1(~ Head2
                 | hlBefore, data=Parkinson, render.missing = NULL, 
                 caption = "Parkinson's Disease")

#Mutate column values for Prodromal
Prodromal <- Prodromal %>%
  mutate(hlBefore=case_when(
    hlBefore == FALSE ~ "False",
    hlBefore == TRUE ~ "True",
  ))
Prodromal <- Prodromal %>%
  mutate(Head3=case_when(
    is.na(Head3) ~ "Unsure",
    !is.na(Head3) ~ "Yes"
  ))
Prodromal <- Prodromal[!is.na(Prodromal$hlBefore),]

#Create a table for Prodromal
table1(~ Head3
       | hlBefore, data=Prodromal, render.missing = NULL, 
       caption = "Prodromal")

#Mutate column values for SWEDD
SWEDD <- SWEDD %>%
  mutate(hlBefore=case_when(
    hlBefore == FALSE ~ "False",
    hlBefore == TRUE ~ "True",
  ))
SWEDD <- SWEDD %>%
  mutate(Head4=case_when(
    is.na(Head4) ~ "Unsure",
    !is.na(Head4) ~ "Yes"
  ))
SWEDD <- SWEDD[!is.na(SWEDD$hlBefore),]

#Create a table for SWEDD
table1(~ Head4
       | hlBefore, data=SWEDD, render.missing = NULL, 
       caption = "SWEDD")

#Make a dataframe with the values of the proportions
df <- data.frame(matrix(ncol = 4, nrow = 2))

healthyRatio <- prop.table(table(Healthy$Head1,Healthy$hlBefore),2)["Yes","True"] / prop.table(table(Healthy$Head1,Healthy$hlBefore),2)["Yes","False"]
ParkinsonRatio <- prop.table(table(Parkinson$Head2,Parkinson$hlBefore),2)["Yes","True"] / prop.table(table(Parkinson$Head2,Parkinson$hlBefore),2)["Yes","False"]
ProdromalRatio <- prop.table(table(Prodromal$Head3,Prodromal$hlBefore),2)["Yes","True"] / prop.table(table(Prodromal$Head3,Prodromal$hlBefore),2)["Yes","False"]

df[1,1] = "Yes"
df[1, 2] = healthyRatio
df[1, 3] = ProdromalRatio
df[1, 4] = ParkinsonRatio

healthyRatio1 <- prop.table(table(Healthy$Head1,Healthy$hlBefore),2)["Unsure","True"] / prop.table(table(Healthy$Head1,Healthy$hlBefore),2)["Unsure","False"]
ParkinsonRatio1 <- prop.table(table(Parkinson$Head2,Parkinson$hlBefore),2)["Unsure","True"] / prop.table(table(Parkinson$Head2,Parkinson$hlBefore),2)["Unsure","False"]
ProdromalRatio1 <- prop.table(table(Prodromal$Head3,Prodromal$hlBefore),2)["Unsure","True"] / prop.table(table(Prodromal$Head3,Prodromal$hlBefore),2)["Unsure","False"]

df[2,1] = "Unsure"
df[2, 2] = healthyRatio1
df[2, 3] = ProdromalRatio1
df[2, 4] = ParkinsonRatio1

rm("healthyRatio", "ParkinsonRatio", "ProdromalRatio", "healthyRatio1", "ParkinsonRatio1", "ProdromalRatio1", "table2")
rm("Healthy", "Parkinson", "Prodromal", "SWEDD")

names(df)[names(df) == "X1"] <- "Head Injury"
names(df)[names(df) == "X2"] <- "Healthy Control"
names(df)[names(df) == "X3"] <- "Prodromal"
names(df)[names(df) == "X4"] <- "Parkinson"

library(gt)

#Create a table
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
      columns = c("Head Injury")
    ))

