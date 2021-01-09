rm(list=ls())
library(ndl)
library(plotfunctions)
library(data.table)
library(edl)
library(stringr)
library(rlist)
library(dplyr)
library(roperators)
source("C:/Users/Sanne/Dropbox/MasterProject/Modelling/VanHammeWassermanFunctions.R") #loading in the file for the functions created

#Categories. So each category (e.g. Apple) has specifics. However since here we don't really work with apple and golden delishious for example, we will work with A1 and A1Spec1

#i have to adjust the FillInCues function for this, so I'll put it in here instead of the one in VanHammeWassermanFunctions
FillInCues2 <- function(A, B, X, Cue){
  #reset the values when going in again
  i = 1
  j = 1
  k = 1
  for(a in 1:length(Cue)){
    #for every item in the cue, is it A_X or B_X?
    if(Cue[a] == "env_A_X"){
      #Replace A with An_AnSpeci, where n is the Cue nr (which is already given to the function) and i increases each time a specific item is seen. Same for X 
      Cue[a] <- str_replace(Cue[a], "A", paste(A, "_", A, "Spec", i, sep = "", collapse = NULL))
      Cue[a] <- str_replace(Cue[a], "X", paste(X, "_", X, "Spec", k, sep = "", collapse = NULL))
      i = i+1
      k = k+1
    }
    if(Cue[a] == "env_B_X"){
      Cue[a] <- str_replace(Cue[a], "B", paste(B, "_", B, "Spec", j, sep = "", collapse = NULL))
      Cue[a] <- str_replace(Cue[a], "X", paste(X, "_", X, "Spec", k, sep = "", collapse = NULL))
      j = j+1
      k = k+1
    }
  }
  return(Cue)
}

Cue <- c("A_X", "B_X", "B_X", "A_X", "A_X", "B_X", "A_X", "B_X", "B_X", "A_X", "B_X", "A_X", "A_X", "B_X", "A_X", "B_X")
Cue <- paste("env", Cue, sep = "_")
Cues1 <- FillInCues2("A1", "B1", "X1", Cue)
Cues2 <- FillInCues2("A2", "B2", "X2", Cue)
Cues3 <- FillInCues2("A3", "B3", "X3", Cue)
Cues4 <- FillInCues2("A4", "B4", "X4", Cue)
Cues5 <- FillInCues2("A5", "B5", "X5", Cue)
Cues6 <- FillInCues2("A6", "B6", "X6", Cue)


###
Outcome000 <- c("Allergic", "Not", "Allergic", "Not", "Not", "Allergic", "Allergic", "Not", "Allergic", "Not", "Not", "Allergic", "Not", "Allergic", "Allergic", "Not")
Outcome050 <- c("Allergic", "Not", "Allergic", "Allergic", "Not", "Not", "Allergic", "Not", "Allergic", "Allergic", "Not", "Allergic", "Not", "Not", "Allergic", "Not")
Outcome100 <- c("Allergic", "Not", "Not", "Allergic", "Allergic", "Not", "Allergic", "Not", "Not", "Allergic", "Not", "Allergic", "Allergic", "Not", "Allergic", "Not")


#Group 1
#split in 3,4
Group1_1 <- bind_rows(ParticipantRun(Cues2, Cues1, Cues3, Outcome050, Outcome000, Outcome100, 3))
Group1_2 <- bind_rows(ParticipantRun(Cues5, Cues6, Cues4, Outcome050, Outcome100, Outcome000, 4))
#now I want to make one big 
Group1 <- rbind(Group1_1, Group1_2)

#Group2, split 4,4
Group2_1 <- bind_rows(ParticipantRun(Cues1, Cues3, Cues2, Outcome000, Outcome100, Outcome050, 4))
Group2_2 <- bind_rows(ParticipantRun(Cues4, Cues5, Cues6, Outcome000, Outcome050, Outcome100, 4))
Group2 <- rbind(Group2_1, Group2_2)

#Group3, split 3,3
Group3_1 <- bind_rows(ParticipantRun(Cues3, Cues2, Cues1, Outcome100, Outcome050, Outcome000, 3))
Group3_2 <- bind_rows(ParticipantRun(Cues6, Cues4, Cues5, Outcome100, Outcome000, Outcome050, 3))
Group3 <- rbind(Group3_1, Group3_2)

#Group4, split 4,4
Group4_1 <- bind_rows(ParticipantRun(Cues5, Cues6, Cues4, Outcome050, Outcome100, Outcome000, 4))
Group4_2 <- bind_rows(ParticipantRun(Cues2, Cues1, Cues3, Outcome050, Outcome000, Outcome100, 4))
Group4 <- rbind(Group4_1, Group4_2)

#Group5, split 5,5
Group5_1 <- bind_rows(ParticipantRun(Cues4, Cues5, Cues6, Outcome000, Outcome050, Outcome100, 5))
Group5_2 <- bind_rows(ParticipantRun(Cues1, Cues3, Cues2, Outcome000, Outcome100, Outcome050, 5))
Group5 <- rbind(Group5_1, Group5_2)

#Group6, split 5,4
Group6_1 <- bind_rows(ParticipantRun(Cues6, Cues4, Cues5, Outcome100, Outcome000, Outcome050, 4))
Group6_2 <- bind_rows(ParticipantRun(Cues3, Cues2, Cues1, Outcome100, Outcome050, Outcome000, 4))
Group6 <- rbind(Group6_1, Group6_2)

All <- rbind(Group1, Group2, Group3, Group4, Group5, Group6)


word <- c("X1", "X2", "X3", "X4", "X5", "X6", "A1", "A2", "A3", "A4", "A5", "A6", "B1", "B2", "B3", "B4", "B5", "B6")
wordType <- c("X","X","X","X","X","X","A","A","A","A","A","A","B","B","B","B","B","B")
words <- data.frame(Word = word, wordType = wordType, stringsAsFactors = FALSE)
getwords <- words$wordType
names(getwords) <- words$Word


All$wordType <- NA
for(i in 1:nrow(All)){
  All[i,]$wordType <- getwords[gsub("...[0-9]+", "",row.names(All[i,]))]
}

aggAll <- aggregate(All$Allergic, list(All$Cond, All$wordType), mean)
aggAll
aggAllNot <- aggregate(All$Not, list(All$Cond, All$wordType), mean)
aggAllNot

Group1_1 <- bind_rows(ParticipantRun(Cues2, Cues1, Cues3, Outcome050, Outcome000, Outcome100, 3, etaNeg = TRUE))
Group1_2 <- bind_rows(ParticipantRun(Cues5, Cues6, Cues4, Outcome050, Outcome100, Outcome000, 4, etaNeg = TRUE))
Group1 <- rbind(Group1_1, Group1_2)

Group2_1 <- bind_rows(ParticipantRun(Cues1, Cues3, Cues2, Outcome000, Outcome100, Outcome050, 4, etaNeg = TRUE))
Group2_2 <- bind_rows(ParticipantRun(Cues4, Cues5, Cues6, Outcome000, Outcome050, Outcome100, 4, etaNeg = TRUE))
Group2 <- rbind(Group2_1, Group2_2)

Group3_1 <- bind_rows(ParticipantRun(Cues3, Cues2, Cues1, Outcome100, Outcome050, Outcome000, 3, etaNeg = TRUE))
Group3_2 <- bind_rows(ParticipantRun(Cues6, Cues4, Cues5, Outcome100, Outcome000, Outcome050, 3, etaNeg = TRUE))
Group3 <- rbind(Group3_1, Group3_2)

Group4_1 <- bind_rows(ParticipantRun(Cues5, Cues6, Cues4, Outcome050, Outcome100, Outcome000, 4, etaNeg = TRUE))
Group4_2 <- bind_rows(ParticipantRun(Cues2, Cues1, Cues3, Outcome050, Outcome000, Outcome100, 4, etaNeg = TRUE))
Group4 <- rbind(Group4_1, Group4_2)

Group5_1 <- bind_rows(ParticipantRun(Cues4, Cues5, Cues6, Outcome000, Outcome050, Outcome100, 5, etaNeg = TRUE))
Group5_2 <- bind_rows(ParticipantRun(Cues1, Cues3, Cues2, Outcome000, Outcome100, Outcome050, 5, etaNeg = TRUE))
Group5 <- rbind(Group5_1, Group5_2)

Group6_1 <- bind_rows(ParticipantRun(Cues6, Cues4, Cues5, Outcome100, Outcome000, Outcome050, 4, etaNeg = TRUE))
Group6_2 <- bind_rows(ParticipantRun(Cues3, Cues2, Cues1, Outcome100, Outcome050, Outcome000, 4, etaNeg = TRUE))
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
