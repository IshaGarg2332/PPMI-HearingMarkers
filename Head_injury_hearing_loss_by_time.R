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
library("table1")


#The table displays the counts and proportions of participants by their head injury status ("Yes," "No," "Maybe?," "Unsure") and hearing loss diagnoses
#The data is broken down across multiple time periods (1960–1980, 1980–1990, 1990–2000, 2000–2010, 2010–2020, and 2020–2024)
#It allows comparison of head injury prevalence and hearing loss categories over these time intervals.


#Import Data
HeadInjury <- read_csv("data/HeadInjury.csv")
HeadInjury <- HeadInjury
HeadInjury[HeadInjury == '[not completed]'] <- NA
HeadInjury <- HeadInjury[!is.na(HeadInjury$head_injury_timestamp),]
MedConditions <- read_csv("data/MedConditions.csv")
hloss <- MedConditions
rm("HeadInjury", "MedConditions")

#Fix hearing loss
hloss <- hloss[grepl("(hear\\b|hearing)",hloss$MHTERM,ignore.case = T),]
hloss <- hloss[!is.na(hloss$MHDIAGDT),]

#Combine them
naframe <- as.data.frame(matrix(NA, nrow = 537, ncol = 15))
names(naframe) <- names(hloss)
hearing <- rbind(hloss, naframe)
HeadInjury$consecutive<-1:nrow(HeadInjury)
hloss$consecutive<-1:nrow(hloss)

#Select what we need
hearing1 <- select(hloss, c(PATNO, MHDIAGYR, MHTERM))
HeadInjury$data <- format(as.Date(HeadInjury$head_injury_timestamp, format="%m/%d/%Y %H:%M"),"%Y")
HeadInjury1 <- select(HeadInjury, c(patno, data, hiq1))
rm("HeadInjury", "hloss", "naframe")

#Add necessary columns
place1 <- c("PATNO", "MHDIAGYR", "MHTERM")
place2 <- c("patno", "data", "hiq1")
HeadInjury1[ , place1] <- NA
hearing1[ , place2] <- NA

#Merge them
total <- rbind(HeadInjury1, hearing1)
total$dates <- paste(total$MHDIAGYR,total$data)
total1 <- total %>%
  mutate_at("dates", str_replace, "NA ", "") %>%
  mutate_at("dates", str_replace, " NA", "")

#Clean up and graph
rm("place1", "place2", "HeadInjury1", "hearing1", "total")
total1[total1 == 'NA'] <- NA
total1 <- total1[!is.na(total1$dates),]
total1$dates <- (as.numeric(total1$dates))
total2 <- total1 %>%
  mutate(dates1=case_when(
    dates<1980 & dates>=1960 ~ "1960-1980",
    dates<1990 & dates>=1980 ~ "1980-1990",
    dates<2000 & dates>=1990 ~ "1990-2000",
    dates<2010 & dates>=2000 ~ "2000-2010",
    dates<2020 & dates>=2010 ~ "2010-2020",
    dates<2024 & dates>=2020 ~ "2020-2024"))

total3 <- total2 %>%
  mutate(hiq1=case_when(
    hiq1 == 1 ~ "Yes",
    hiq1 == 0 ~ "No",
    hiq1 == 0.5 ~ "Maybe?",
    hiq1 == 9999 ~ "Unsure",
  ))

#Fix hearing loss
total4 <- total3 %>%
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
                         "D/x: Hearing impairment  - Hearing aids given about 3 years ago; occasionally wears it." = 'Hearing loss with hearing aid',
                         "Bilateral Sensorineural Hearing Loss" = 'Bilateral sensorineural hearing loss',
                         "Hearing loss- subject wears a hearing aid" = 'Hearing loss in right ear',
                         "Right Ear Hearing Loss" = 'Hearing loss in right ear',
                         "Hearing impaired" = 'Impaired hearing',
                         "Hearing loss left ear" = 'Hearing loss in left ear',
                         "Bilateral Hearing Loss" = 'Bilateral hearing loss',
                         "Hearing impairment" = "Impaired hearing"
  ))

#Mutate order of columns for table
total4 <- total4 %>% 
  mutate(hiq1 = factor(hiq1, labels=c("Yes", "No", "Maybe?", "Unsure")))

#Label columns
label(total4$hiq1) <- 'Have you ever had a head injury?'
label(total4$MHTERM) <- 'Hearing Loss'

#Make the table
total4 <- total4[!is.na(total4$dates1),]
table1(~ hiq1 + MHTERM 
       | dates1, data=total4, render.missing = NULL)
