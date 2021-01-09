rm(list=ls())
library(ndl)
library(plotfunctions)
library(data.table)
library(edl)
library(stringr)
library(rlist)
library(dplyr)
library(roperators)
source("C:/Users/Sanne/Dropbox/MasterProject/Modelling/VanHammeWassermanFunctions.R")

#Let's double the trials at first

#I'll do this by copy pasting the cues and then copy pasting each of the outcomes as well, such that we now have 32 and it's practically just a repeat of twice the same thing
Cue <- c("A_X", "B_X", "B_X", "A_X", "A_X", "B_X", "A_X", "B_X", "B_X", "A_X", "B_X", "A_X", "A_X", "B_X", "A_X", "B_X", "A_X", "B_X", "B_X", "A_X", "A_X", "B_X", "A_X", "B_X", "B_X", "A_X", "B_X", "A_X", "A_X", "B_X", "A_X", "B_X")
Cue <- paste("env", Cue, sep = "_")
Cues1 <- FillInCues("strawberries", "peanuts", "shrimp", Cue)
Cues2 <- FillInCues("bran", "cabbage", "yoghurt", Cue)
Cues3 <- FillInCues("chicken", "mustard", "bananas", Cue)
Cues4 <- FillInCues("walnuts", "peaches", "wheat", Cue)
Cues5 <- FillInCues("horseradish", "lobster", "corn", Cue)
Cues6 <- FillInCues("cheese", "pork", "blueberries", Cue)

###
Outcome000 <- c("Allergic", "Not", "Allergic", "Not", "Not", "Allergic", "Allergic", "Not", "Allergic", "Not", "Not", "Allergic", "Not", "Allergic", "Allergic", "Not", "Allergic", "Not", "Allergic", "Not", "Not", "Allergic", "Allergic", "Not", "Allergic", "Not", "Not", "Allergic", "Not", "Allergic", "Allergic", "Not")
Outcome050 <- c("Allergic", "Not", "Allergic", "Allergic", "Not", "Not", "Allergic", "Not", "Allergic", "Allergic", "Not", "Allergic", "Not", "Not", "Allergic", "Not", "Allergic", "Not", "Allergic", "Allergic", "Not", "Not", "Allergic", "Not", "Allergic", "Allergic", "Not", "Allergic", "Not", "Not", "Allergic", "Not")
Outcome100 <- c("Allergic", "Not", "Not", "Allergic", "Allergic", "Not", "Allergic", "Not", "Not", "Allergic", "Not", "Allergic", "Allergic", "Not", "Allergic", "Not", "Allergic", "Not", "Not", "Allergic", "Allergic", "Not", "Allergic", "Not", "Not", "Allergic", "Not", "Allergic", "Allergic", "Not", "Allergic", "Not")

#Group 1
#split in 3,4
Group1_1 <- bind_rows(ParticipantRun(Cues2, Cues1, Cues3, Outcome050, Outcome000, Outcome100, 3, NrOfTrials = 32))
Group1_2 <- bind_rows(ParticipantRun(Cues5, Cues6, Cues4, Outcome050, Outcome100, Outcome000, 4, NrOfTrials = 32))
#now I want to make one big 
Group1 <- rbind(Group1_1, Group1_2)

#Group2, split 4,4
Group2_1 <- bind_rows(ParticipantRun(Cues1, Cues3, Cues2, Outcome000, Outcome100, Outcome050, 4, NrOfTrials = 32))
Group2_2 <- bind_rows(ParticipantRun(Cues4, Cues5, Cues6, Outcome000, Outcome050, Outcome100, 4, NrOfTrials = 32))
Group2 <- rbind(Group2_1, Group2_2)

#Group3, split 3,3
Group3_1 <- bind_rows(ParticipantRun(Cues3, Cues2, Cues1, Outcome100, Outcome050, Outcome000, 3, NrOfTrials = 32))
Group3_2 <- bind_rows(ParticipantRun(Cues6, Cues4, Cues5, Outcome100, Outcome000, Outcome050, 3, NrOfTrials = 32))
Group3 <- rbind(Group3_1, Group3_2)

#Group4, split 4,4
Group4_1 <- bind_rows(ParticipantRun(Cues5, Cues6, Cues4, Outcome050, Outcome100, Outcome000, 4, NrOfTrials = 32))
Group4_2 <- bind_rows(ParticipantRun(Cues2, Cues1, Cues3, Outcome050, Outcome000, Outcome100, 4, NrOfTrials = 32))
Group4 <- rbind(Group4_1, Group4_2)

#Group5, split 5,5
Group5_1 <- bind_rows(ParticipantRun(Cues4, Cues5, Cues6, Outcome000, Outcome050, Outcome100, 5, NrOfTrials = 32))
Group5_2 <- bind_rows(ParticipantRun(Cues1, Cues3, Cues2, Outcome000, Outcome100, Outcome050, 5, NrOfTrials = 32))
Group5 <- rbind(Group5_1, Group5_2)

#Group6, split 5,4
Group6_1 <- bind_rows(ParticipantRun(Cues6, Cues4, Cues5, Outcome100, Outcome000, Outcome050, 4, NrOfTrials = 32))
Group6_2 <- bind_rows(ParticipantRun(Cues3, Cues2, Cues1, Outcome100, Outcome050, Outcome000, 4, NrOfTrials = 32))
Group6 <- rbind(Group6_1, Group6_2)

All <- rbind(Group1, Group2, Group3, Group4, Group5, Group6)
All$wordType <- NA
for(i in 1:nrow(All)){
  All[i,]$wordType <- getwords[gsub("...[0-9]+", "",row.names(All[i,]))]
}
aggAll <- aggregate(All$Allergic, list(All$Cond, All$wordType), mean)
aggAll
aggAllNot <- aggregate(All$Not, list(All$Cond, All$wordType), mean)
aggAllNot

Group1_1 <- bind_rows(ParticipantRun(Cues2, Cues1, Cues3, Outcome050, Outcome000, Outcome100, 3, etaNeg = TRUE, NrOfTrials = 32))
Group1_2 <- bind_rows(ParticipantRun(Cues5, Cues6, Cues4, Outcome050, Outcome100, Outcome000, 4, etaNeg = TRUE, NrOfTrials = 32))
Group1 <- rbind(Group1_1, Group1_2)

Group2_1 <- bind_rows(ParticipantRun(Cues1, Cues3, Cues2, Outcome000, Outcome100, Outcome050, 4, etaNeg = TRUE, NrOfTrials = 32))
Group2_2 <- bind_rows(ParticipantRun(Cues4, Cues5, Cues6, Outcome000, Outcome050, Outcome100, 4, etaNeg = TRUE, NrOfTrials = 32))
Group2 <- rbind(Group2_1, Group2_2)

Group3_1 <- bind_rows(ParticipantRun(Cues3, Cues2, Cues1, Outcome100, Outcome050, Outcome000, 3, etaNeg = TRUE, NrOfTrials = 32))
Group3_2 <- bind_rows(ParticipantRun(Cues6, Cues4, Cues5, Outcome100, Outcome000, Outcome050, 3, etaNeg = TRUE, NrOfTrials = 32))
Group3 <- rbind(Group3_1, Group3_2)

Group4_1 <- bind_rows(ParticipantRun(Cues5, Cues6, Cues4, Outcome050, Outcome100, Outcome000, 4, etaNeg = TRUE, NrOfTrials = 32))
Group4_2 <- bind_rows(ParticipantRun(Cues2, Cues1, Cues3, Outcome050, Outcome000, Outcome100, 4, etaNeg = TRUE, NrOfTrials = 32))
Group4 <- rbind(Group4_1, Group4_2)

Group5_1 <- bind_rows(ParticipantRun(Cues4, Cues5, Cues6, Outcome000, Outcome050, Outcome100, 5, etaNeg = TRUE, NrOfTrials = 32))
Group5_2 <- bind_rows(ParticipantRun(Cues1, Cues3, Cues2, Outcome000, Outcome100, Outcome050, 5, etaNeg = TRUE, NrOfTrials = 32))
Group5 <- rbind(Group5_1, Group5_2)

Group6_1 <- bind_rows(ParticipantRun(Cues6, Cues4, Cues5, Outcome100, Outcome000, Outcome050, 4, etaNeg = TRUE, NrOfTrials = 32))
Group6_2 <- bind_rows(ParticipantRun(Cues3, Cues2, Cues1, Outcome100, Outcome050, Outcome000, 4, etaNeg = TRUE, NrOfTrials = 32))
Group6 <- rbind(Group6_1, Group6_2)

All2 <- rbind(Group1, Group2, Group3, Group4, Group5, Group6)
All2$wordType <- NA
for(i in 1:nrow(All2)){
  All2[i,]$wordType <- getwords[gsub("...[0-9]+", "",row.names(All[i,]))]
  
}

#So now that we have all, we wanna aggregate over Cond and wordType, to see if we can get the same averages
aggAll2 <- aggregate(All2$Allergic, list(All2$Cond, All2$wordType), mean)
aggAll2
aggAllNot2 <- aggregate(All2$Not, list(All2$Cond, All2$wordType), mean)
aggAllNot2

ByTrial_RW <- aggregate(list(All$Allergic, All$Not), list(All$Trial, All$Cond, All$wordType), mean) 
colnames(ByTrial_RW) <- c("TrialNr", "Cond", "FoodGroup", "Allergic", "Not")
#head(ByTrial_RW)
Cond000 <- filter(ByTrial_RW, Cond == "Outcome000")
Cond050 <- filter(ByTrial_RW, Cond == "Outcome050")
Cond100 <- filter(ByTrial_RW, Cond == "Outcome100")


ByTrial_VHW <- aggregate(list(All2$Allergic, All2$Not), list(All2$Trial, All2$Cond, All2$wordType), mean) 
colnames(ByTrial_VHW) <- c("TrialNr", "Cond", "FoodGroup", "Allergic", "Not")
Cond000_2 <- filter(ByTrial_VHW, Cond == "Outcome000")
Cond050_2 <- filter(ByTrial_VHW, Cond == "Outcome050")
Cond100_2 <- filter(ByTrial_VHW, Cond == "Outcome100")
