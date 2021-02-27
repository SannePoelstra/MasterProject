rm(list=ls())
library(ndl)
library(plotfunctions)
library(data.table)
library(edl)
library(stringr)
library(rlist)
library(dplyr)
library(roperators)
source("C:/Users/Sanne/Dropbox/MasterProject/Modelling/VanHammeWassermanFunctionsOurExp.R") #loading in the file for the functions created

Cue <- c("A_X", "B_X", "B_X", "A_X", "A_X", "B_X", "A_X", "B_X", "B_X", "A_X", "B_X", "A_X", "A_X", "B_X", "A_X", "B_X")
Cue <- paste("env", Cue, sep = "_")
#As in the experiment, shuffle the array each time, such that participants get different cues for different conditions.
#LOWER CASE. this is because otherwise the replacement function in FillInCues will also replace the B in beetle etc
Cues <- sample(c("acorn", "beetle", "blackberries", "feather", "flower", "footprints", "log", "mouse", "puddle"))
Cues1 <- FillInCues(Cues[1], Cues[2], Cues[3], Cue)
Cues2 <- FillInCues(Cues[4], Cues[5], Cues[6], Cue)
Cues3 <- FillInCues(Cues[7], Cues[8], Cues[9], Cue)

###
Outcome000 <- c("Diamond", "NoDiamond", "Diamond", "NoDiamond", "NoDiamond", "Diamond", "Diamond", "NoDiamond", "Diamond", "NoDiamond", "NoDiamond", "Diamond", "NoDiamond", "Diamond", "Diamond", "NoDiamond")
Outcome050 <- c("Diamond", "NoDiamond", "Diamond", "Diamond", "NoDiamond", "NoDiamond", "Diamond", "NoDiamond", "Diamond", "Diamond", "NoDiamond", "Diamond", "NoDiamond", "NoDiamond", "Diamond", "NoDiamond")
Outcome100 <- c("Diamond", "NoDiamond", "NoDiamond", "Diamond", "Diamond", "NoDiamond", "Diamond", "NoDiamond", "NoDiamond", "Diamond", "NoDiamond", "Diamond", "Diamond", "NoDiamond", "Diamond", "NoDiamond")


#I've taken the same division of participants, as the 6 conditions are randomized with our experiment, each condition should approx get the same amount of testers anyway
#I've tested it with one participant more in every condition (which is 12 people more) and the results are the same
#Group 1
#split in 3,4
Group1_1 <- bind_rows(ParticipantRun(Cues2, Cues1, Cues3, Outcome050, Outcome000, Outcome100, 3))
Group1_2 <- bind_rows(ParticipantRun(Cues2, Cues3, Cues1, Outcome050, Outcome100, Outcome000, 4))
#now I want to make one big 
Group1 <- rbind(Group1_1, Group1_2)

#Group2, split 4,4
Group2_1 <- bind_rows(ParticipantRun(Cues1, Cues3, Cues2, Outcome000, Outcome100, Outcome050, 4))
Group2_2 <- bind_rows(ParticipantRun(Cues1, Cues2, Cues3, Outcome000, Outcome050, Outcome100, 4))
Group2 <- rbind(Group2_1, Group2_2)

#Group3, split 3,3
Group3_1 <- bind_rows(ParticipantRun(Cues3, Cues2, Cues1, Outcome100, Outcome050, Outcome000, 3))
Group3_2 <- bind_rows(ParticipantRun(Cues3, Cues1, Cues2, Outcome100, Outcome000, Outcome050, 3))
Group3 <- rbind(Group3_1, Group3_2)

#Group4, split 4,4
Group4_1 <- bind_rows(ParticipantRun(Cues2, Cues3, Cues1, Outcome050, Outcome100, Outcome000, 4))
Group4_2 <- bind_rows(ParticipantRun(Cues2, Cues1, Cues3, Outcome050, Outcome000, Outcome100, 4))
Group4 <- rbind(Group4_1, Group4_2)

#Group5, split 5,5
Group5_1 <- bind_rows(ParticipantRun(Cues1, Cues2, Cues3, Outcome000, Outcome050, Outcome100, 5))
Group5_2 <- bind_rows(ParticipantRun(Cues1, Cues3, Cues2, Outcome000, Outcome100, Outcome050, 5))
Group5 <- rbind(Group5_1, Group5_2)

#Group6, split 5,4
Group6_1 <- bind_rows(ParticipantRun(Cues3, Cues1, Cues2, Outcome100, Outcome000, Outcome050, 4))
Group6_2 <- bind_rows(ParticipantRun(Cues3, Cues2, Cues1, Outcome100, Outcome050, Outcome000, 4))
Group6 <- rbind(Group6_1, Group6_2)

Group1Test <- subset(Group1, Group1$Stage == "Test")
Group1Train <- subset(Group1, Group1$Stage == "Train")
Group1Train$Fossil <- NULL
Group2Test <- subset(Group2, Group2$Stage == "Test")
Group2Train <- subset(Group2, Group2$Stage == "Train")
Group2Train$Fossil <- NULL
Group3Test <- subset(Group3, Group3$Stage == "Test")
Group3Train <- subset(Group3, Group3$Stage == "Train")
Group3Train$Fossil <- NULL
Group4Test <- subset(Group4, Group4$Stage == "Test")
Group4Train <- subset(Group4, Group4$Stage == "Train")
Group4Train$Fossil <- NULL
Group5Test <- subset(Group5, Group5$Stage == "Test")
Group5Train <- subset(Group5, Group5$Stage == "Train")
Group5Train$Fossil <- NULL
Group6Test <- subset(Group6, Group6$Stage == "Test")
Group6Train <- subset(Group6, Group6$Stage == "Train")
Group6Train$Fossil <- NULL


All <- rbind(Group1, Group2, Group3, Group4, Group5, Group6)
AllTrain <- rbind(Group1Train, Group2Train, Group3Train, Group4Train, Group5Train, Group6Train)
AllTest <- rbind(Group1Test, Group2Test, Group3Test, Group4Test, Group5Test, Group6Test)

All$wordType <- NA
AllTrain$wordType <- NA
AllTest$wordType <- NA

#Created a new specific function for ours here, since our cues aren't always the same for each particpant!
word <- c(Cues[1], Cues[2], Cues[3], Cues[4], Cues[5], Cues[6], Cues[7], Cues[8], Cues[9])
wordType <- c("A", "B", "X", "A", "B", "X", "A", "B", "X")
words <- data.frame(Word = word, wordType = wordType, stringsAsFactors = FALSE)
getwords <- words$wordType
names(getwords) <- words$Word

for(i in 1:nrow(All)){
  All[i,]$wordType <- getwords[gsub("...[0-9]+", "",row.names(All[i,]))]
}
for(i in 1:nrow(AllTrain)){
  AllTrain[i,]$wordType <- getwords[gsub("...[0-9]+", "",row.names(AllTrain[i,]))]
}
for(i in 1:nrow(AllTest)){
  AllTest[i,]$wordType <- getwords[gsub("...[0-9]+", "",row.names(AllTest[i,]))]
}

aggAll <- aggregate(All$Diamond, list(All$Cond, All$wordType), mean)
aggAll

aggAllTrain <- aggregate(AllTrain$Diamond, list(AllTrain$Cond, AllTrain$wordType), mean)
aggAllTest <- aggregate(AllTest$Diamond, list(AllTest$Cond, AllTest$wordType), mean)

aggAllNot <- aggregate(All$NoDiamond, list(All$Cond, All$wordType), mean)
aggAllNot

aggAllNotTrain <- aggregate(AllTrain$NoDiamond, list(AllTrain$Cond, AllTrain$wordType),mean)
aggAllNotTest <- aggregate(AllTest$NoDiamond, list(AllTest$Cond, AllTest$wordType), mean)

Group1_1 <- bind_rows(ParticipantRun(Cues2, Cues1, Cues3, Outcome050, Outcome000, Outcome100, 3, etaNeg = TRUE))
Group1_2 <- bind_rows(ParticipantRun(Cues2, Cues3, Cues1, Outcome050, Outcome100, Outcome000, 4, etaNeg = TRUE))
Group1 <- rbind(Group1_1, Group1_2)

Group2_1 <- bind_rows(ParticipantRun(Cues1, Cues3, Cues2, Outcome000, Outcome100, Outcome050, 4, etaNeg = TRUE))
Group2_2 <- bind_rows(ParticipantRun(Cues1, Cues2, Cues3, Outcome000, Outcome050, Outcome100, 4, etaNeg = TRUE))
Group2 <- rbind(Group2_1, Group2_2)

Group3_1 <- bind_rows(ParticipantRun(Cues3, Cues2, Cues1, Outcome100, Outcome050, Outcome000, 3, etaNeg = TRUE))
Group3_2 <- bind_rows(ParticipantRun(Cues3, Cues1, Cues2, Outcome100, Outcome000, Outcome050, 3, etaNeg = TRUE))
Group3 <- rbind(Group3_1, Group3_2)

Group4_1 <- bind_rows(ParticipantRun(Cues2, Cues3, Cues1, Outcome050, Outcome100, Outcome000, 4, etaNeg = TRUE))
Group4_2 <- bind_rows(ParticipantRun(Cues2, Cues1, Cues3, Outcome050, Outcome000, Outcome100, 4, etaNeg = TRUE))
Group4 <- rbind(Group4_1, Group4_2)

Group5_1 <- bind_rows(ParticipantRun(Cues1, Cues2, Cues3, Outcome000, Outcome050, Outcome100, 5, etaNeg = TRUE))
Group5_2 <- bind_rows(ParticipantRun(Cues1, Cues3, Cues2, Outcome000, Outcome100, Outcome050, 5, etaNeg = TRUE))
Group5 <- rbind(Group5_1, Group5_2)

Group6_1 <- bind_rows(ParticipantRun(Cues3, Cues1, Cues2, Outcome100, Outcome000, Outcome050, 4, etaNeg = TRUE))
Group6_2 <- bind_rows(ParticipantRun(Cues3, Cues2, Cues1, Outcome100, Outcome050, Outcome000, 4, etaNeg = TRUE))
Group6 <- rbind(Group6_1, Group6_2)

Group1Test <- subset(Group1, Group1$Stage == "Test")
Group1Train <- subset(Group1, Group1$Stage == "Train")
Group1Train$Fossil <- NULL
Group2Test <- subset(Group2, Group2$Stage == "Test")
Group2Train <- subset(Group2, Group2$Stage == "Train")
Group2Train$Fossil <- NULL
Group3Test <- subset(Group3, Group3$Stage == "Test")
Group3Train <- subset(Group3, Group3$Stage == "Train")
Group3Train$Fossil <- NULL
Group4Test <- subset(Group4, Group4$Stage == "Test")
Group4Train <- subset(Group4, Group4$Stage == "Train")
Group4Train$Fossil <- NULL
Group5Test <- subset(Group5, Group5$Stage == "Test")
Group5Train <- subset(Group5, Group5$Stage == "Train")
Group5Train$Fossil <- NULL
Group6Test <- subset(Group6, Group6$Stage == "Test")
Group6Train <- subset(Group6, Group6$Stage == "Train")
Group6Train$Fossil <- NULL


All2 <- rbind(Group1, Group2, Group3, Group4, Group5, Group6)
AllTrain2 <- rbind(Group1Train, Group2Train, Group3Train, Group4Train, Group5Train, Group6Train)
AllTest2 <- rbind(Group1Test, Group2Test, Group3Test, Group4Test, Group5Test, Group6Test)

All2$wordType <- NA
AllTrain2$wordType <- NA
AllTest2$wordType <- NA

#Created a new specific function for ours here, since our cues aren't always the same for each particpant!
word <- c(Cues[1], Cues[2], Cues[3], Cues[4], Cues[5], Cues[6], Cues[7], Cues[8], Cues[9])
wordType <- c("A", "B", "X", "A", "B", "X", "A", "B", "X")
words <- data.frame(Word = word, wordType = wordType, stringsAsFactors = FALSE)
getwords <- words$wordType
names(getwords) <- words$Word

for(i in 1:nrow(All2)){
  All2[i,]$wordType <- getwords[gsub("...[0-9]+", "",row.names(All2[i,]))]
}
for(i in 1:nrow(AllTrain2)){
  AllTrain2[i,]$wordType <- getwords[gsub("...[0-9]+", "",row.names(AllTrain2[i,]))]
}
for(i in 1:nrow(AllTest2)){
  AllTest2[i,]$wordType <- getwords[gsub("...[0-9]+", "",row.names(AllTest2[i,]))]
}

aggAll2 <- aggregate(All$Diamond, list(All$Cond, All$wordType), mean)
aggAll2

aggAllTrain2 <- aggregate(AllTrain2$Diamond, list(AllTrain2$Cond, AllTrain2$wordType), mean)
aggAllTest2 <- aggregate(AllTest2$Diamond, list(AllTest2$Cond, AllTest2$wordType), mean)

aggAllNot2 <- aggregate(All2$NoDiamond, list(All2$Cond, All2$wordType), mean)
aggAllNot2

aggAllNotTrain2 <- aggregate(AllTrain2$NoDiamond, list(AllTrain2$Cond, AllTrain2$wordType),mean)
aggAllNotTest2 <- aggregate(AllTest2$NoDiamond, list(AllTest2$Cond, AllTest2$wordType), mean)


ByTrialTrain_RW <- aggregate(list(AllTrain$Diamond, AllTrain$NoDiamond), list(AllTrain$Trial, AllTrain$Cond, AllTrain$wordType), mean) 
colnames(ByTrialTrain_RW) <- c("TrialNr", "Cond", "FoodGroup", "Diamond", "NoDiamond")
#head(ByTrial_RW)
Cond000 <- filter(ByTrialTrain_RW, Cond == "Outcome000")
Cond050 <- filter(ByTrialTrain_RW, Cond == "Outcome050")
Cond100 <- filter(ByTrialTrain_RW, Cond == "Outcome100")


ByTrialTrain_VHW <- aggregate(list(AllTrain2$Diamond, AllTrain2$NoDiamond), list(AllTrain2$Trial, AllTrain2$Cond, AllTrain2$wordType), mean) 
colnames(ByTrialTrain_VHW) <- c("TrialNr", "Cond", "FoodGroup", "Diamond", "NoDiamond")
Cond000_2 <- filter(ByTrialTrain_VHW, Cond == "Outcome000")
Cond050_2 <- filter(ByTrialTrain_VHW, Cond == "Outcome050")
Cond100_2 <- filter(ByTrialTrain_VHW, Cond == "Outcome100")


