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
library("lubridate")


#Creates a summary table of all the different types of hearing loss and if they are healthy controls, Parkinson's patients, or Prodromal
#Displays the distribution of hearing loss types across different participant cohorts


#Import Data
HeadInjury <- read_csv("data/HeadInjury.csv")
HeadInjury[HeadInjury == '[not completed]'] <- NA
HeadInjury <- HeadInjury[!is.na(HeadInjury$head_injury_timestamp),]
Participant <- read_csv("data/ParticipantStatus.csv")
hloss <- read_csv("data/MedicalConditions.csv")

#Modify Hearing Loss
hloss <- hloss[grepl("(hear\\b|hearing)",hloss$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]
total <- merge(hloss,Participant,by="PATNO")
rm("hloss", "Participant")

#Add consecutive numbers
naframe <- as.data.frame(matrix(NA, nrow = 537, ncol = 54))
names(naframe) <- names(total)
total <- rbind(total, naframe)
HeadInjury$consecutive<-1:nrow(HeadInjury)
total$consecutive<-1:nrow(total)

#Select what we need
total <- select(total, c(PATNO, MHDIAGYR, MHTERM, COHORT_DEFINITION, ENROLL_AGE))
HeadInjury6 <- select(HeadInjury, c(patno, hiq1))
colnames(HeadInjury6)[which(names(HeadInjury6) == "patno")] <- "PATNO"

#Add necessary columns
place1 <- c("MHDIAGYR", "MHTERM", "COHORT_DEFINITION", "ENROLL_AGE")
place2 <- c("hiq1")
HeadInjury6[ , place1] <- NA
total[ , place2] <- NA

#Merge them
total1 <- rbind(HeadInjury6, total)

#Fix hearing loss
total1 <- total1 %>%
  mutate(MHTERM = recode(MHTERM, "bilateral hearing loss" = 'Bilateral hearing loss',
                         "hearing impaired" = 'Impaired hearing',
                         "hearing loss" = 'Hearing loss',
                         "hearing loss- subject wears a hearing aid" = 'Hearing loss with hearing aid',
                         "neurosensory hearing loss" = 'Neurosensory hearing loss',
                         "hearing loss - hearing aid use" = 'Hearing loss with hearing aid',
                         "hearing loss of both ears" = 'Hearing loss',
                         "acute hear loss with tinnitus" = 'Hearing loss with tinnitus',
                         "acute hearing loss" = 'Acute hearing loss	',
                         "Bilateral hearing aids" = 'Bilateral hearing loss',
                         "bilateral hearing deficit" = 'Bilateral hearing loss',
                         "decreased hearing bilaterally" = 'Bilateral hearing loss',
                         "hearing impairment" = 'Hearing loss',
                         "hearing loss and tinnitus both sides" = 'Hearing loss with tinnitus',
                         "high end loss of hearing" = 'High end loss of hearing',
                         "mild bilateral hearing loss" = 'Mild hearing loss',
                         "sensorineural hearing loss" = 'Sensorineural hearing loss',
                         "bilateral hardness of hearing" = 'Bilateral hearing loss',
                         "bilateral sensorineural hearing loss" = 'Bilateral sensorineural hearing loss',
                         "d/x: hearing impairment - hearing aids given about 3 years ago; occasionally wears it." = 'Hearing loss with hearing aid',
                         "hearing aids	" = 'Hearing loss with hearing aid',
                         "hearing loss bilateral" = 'Bilateral hearing loss',
                         "hearing loss, bilateral" = 'Bilateral hearing loss',
                         "hearing loss, mild" = 'Mild hearing loss',
                         "hearing loss, progressive" = 'Hearing loss',
                         "loss of hearing" = 'Hearing loss',
                         "mild hearing loss" = 'Mild hearing loss',
                         "mild loss of hearing (high frequencies)" = 'Mild hearing loss',
                         "partial hearing loss" = 'Mild hearing loss',
                         "sensorineural hearing loss, bilateral" = 'Bilateral sensorineural hearing loss',
                         "tinitis and hearing loss" = 'Hearing loss with tinnitus',
                         "bilateral hearing loss - hearing aids	" = 'Hearing loss with hearing aid',
                         "deterioration in hearing requiring hearing aids." = 'Hearing loss with hearing aid',
                         "hearing loss in both ears" = 'Bilateral hearing loss',
                         "hearing loss, b/l" = 'Bilateral hearing loss',
                         "hearing loss, normal, bilateral" = 'Bilateral hearing loss',
                         "sensorineural hearing loss + vertigo" = 'Sensorineural hearing loss',
                         "slight hearing loss" = 'Hearing loss',
                         "bilateral hearing loss - hearing aids" = 'Bilateral hearing loss',
                         "bilateral hearing aids" = 'Hearing loss with hearing aid',
                         "hearing aids" = 'Hearing loss with hearing aid',
                         "hearing loss with calcification of the stapedius, s/p bilateral stapedectomy in 1979	" = 'Otosclerosis',
                         "Hearing loss- subject wears a hearing aid	" = 'Hearing loss with hearing aid',
                         "Hearing Loss - Hearing Aid Use" = 'Hearing loss with hearing aid',
                         "hearing loss right ear" = 'Hearing loss in right ear',
                         "Bilateral Hearing Deficit" = 'Bilateral hearing loss',
                         "Decreased Hearing Bilaterally" = 'Bilateral hearing loss',
                         "Hearing Loss" = 'Hearing loss',
                         "HEARING LOSS" = 'Hearing loss',
                         "Hearing loss and tinnitus both sides" = 'Hearing loss with tinnitus',
                         "Hearing Loss left ear" = 'Hearing loss in left ear',
                         "Hearing loss on right side" = 'Hearing loss in right ear',
                         "left ear hearing loss" = 'Hearing loss in left ear',
                         "Left Ear- decreased hearing" = 'Hearing loss in left ear',
                         "MILD BILATERAL HEARING LOSS" = 'Mild hearing loss',
                         "Mild hearing loss in left ear" = 'Hearing loss in left ear',
                         "reduced hearing left ear" = 'Hearing loss in left ear',
                         "Right side hearing loss" = 'Hearing loss in right ear',
                         "Acoustic neuroma resulting in R hearing loss." = 'Acoustic neuroma',
                         "Bilateral hardness of hearing" = 'Bilateral hearing loss',
                         "BILATERAL HEARING LOSS" = 'Bilateral hearing loss',
                         "D/x: Hearing impairment - Hearing aids given about 3 years ago; occasionally wears it.	" = 'Hearing loss with hearing aid',
                         "decreased hearing Right ear" = 'Hearing loss in right ear',
                         "Hearing loss bilateral" = 'Bilateral hearing loss',
                         "Hearing loss Bilateral" = 'Bilateral hearing loss',
                         "Hearing loss on R side" = 'Hearing loss in right ear',
                         "Left hear mild hearing loss" = 'Hearing loss in left ear',
                         "LOSS OF HEARING" = 'Hearing loss',
                         "Mild Bilateral Hearing Loss" = 'Mild hearing loss',
                         "Mild hearing loss" = 'Mild hearing loss',
                         "neurosensory hearing loss - left ear" = 'Hearing loss in left ear',
                         "Partial hearing loss" = 'Partial hearing loss',
                         "Asymmetric hearing loss" = 'Partial hearing loss',
                         "right ear hearing loss" = 'Hearing loss in right ear',
                         "Right Ear Hearing Loss	" = 'Hearing loss in right ear',
                         "Right sided sensorineural hearing loss (due to neuroma)" = 'Hearing loss in right ear',
                         "Sensorineural hearing loss, bilateral" = 'Bilateral sensorineural hearing loss',
                         "Tinitis and hearing loss" = 'Hearing loss with tinnitus',
                         "Asymmetrical hearing loss" = 'Partial hearing loss',
                         "Bilateral hearing loss - Hearing aids" = 'Hearing loss with hearing aid',
                         "Deterioration in hearing requiring hearing aids." = 'Hearing loss with hearing aid',
                         "Hearing Loss Bilateral" = 'Bilateral hearing loss',
                         "Hearing loss in the right ear" = 'Hearing loss in right ear',
                         "Hearing Loss Right Ear" = 'Hearing loss in right ear',
                         "Implant bone-conducting hearing device" = 'Hearing loss with implant',
                         "Left-sided Hearing Loss" = 'Hearing loss in left ear',
                         "Loss of hearing in left ear" = 'Hearing loss in left ear',
                         "Mixed hearing loss with Eustachian tube dysfunction- right ear" = 'Hearing loss in left ear',
                         "Pt had a routine hearing test on October 20, 2021, results found that the pt's left ear has slightly reduced ability at higher frequencies; pt's doctor asked pt to get an MRI to evaluate why the left ear is has reduced functioning compared to the right ear; MRI scheduled for November 10, 2021" = 'Hearing loss in left ear',
                         "Sensorineural hearing loss + vertigo" = 'Sensorineural hearing loss',
                         "Slight hearing loss" = 'Mild hearing loss',
                         "hearing loss with calcification of the stapedius, s/p bilateral stapedectomy in 1979" = 'Otosclerosis',
                         "D/x: Hearing impairment  - Hearing aids given about 3 years ago; occasionally wears it." = 'Hearing loss with hearing aid',
                         "Bilateral Sensorineural Hearing Loss" = 'Bilateral sensorineural hearing loss',
                         "Hearing loss- subject wears a hearing aid" = 'Hearing loss in right ear',
                         "Right Ear Hearing Loss" = 'Hearing loss in right ear',
                         "Hearing impaired" = 'Impaired hearing',
                         "Hearing loss left ear" = 'Hearing loss in left ear',
                         "Sudden hearing loss L ear" = 'Hearing loss in left ear',
                         
  ))

#Change column names and remove NA values
label(total1$MHTERM) <- 'Hearing Loss'
total1 <- total1[!is.na(total1$COHORT_DEFINITION),]

#Create table
table1(~ MHTERM 
       | COHORT_DEFINITION, data=total1, render.missing = NULL)