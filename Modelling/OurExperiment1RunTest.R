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



#food conditions and their outcomes can be given
wm_total <- list()
wm_totalTest <- list()
ParticipantRun <- function(FC1, FC2, FC3, O1, O2, O3, NrOfPart, etaNeg = FALSE, NrOfTrials = 16){
  #for each participant
  for(i in 1:NrOfPart){
    wm_list <- list()
    wm_list2 <- list()
    wm_list3 <- list()
    
    FirstRun <- data.frame(Cues = FC1, Outcomes = O1, stringsAsFactors = FALSE)
    train.FR <- createTrainingData(FirstRun, random = F)
    wm.FR <- EDLearning(train.FR, progress = F, etaNeg = etaNeg)
    for (i in 1:NrOfTrials){
      wm <- as.data.frame(getWM(wm.FR,i))
      wm$Trial <- i
      wm_list <- list.append(wm_list, wm)
    }
    Run1 <- bind_rows(wm_list)
    if(identical(O1, Outcome000)) Run1$Cond <- "Outcome000"
    if(identical(O1, Outcome050)) Run1$Cond <- "Outcome050"
    if(identical(O1, Outcome100)) Run1$Cond <- "Outcome100"
    Run1$Stage <- "Train"
    ###
    
    SecondRun <- data.frame(Cues = FC2, Outcomes = O2, stringsAsFactors = FALSE)
    train.SR <- createTrainingData(SecondRun, random = F)
    wm.SR <- EDLearning(train.SR, progress = F, etaNeg = etaNeg, wm = wm.FR[[NrOfTrials]]) #we take the working memory of the last run as our starting point, as we have the same outcomes
    
    #the for loop needs to be handled a bit differently, as we don't want to get the weights of foods from the previous trial, as participants don't score those anymore.
    for (i in 1:NrOfTrials){ 
      wm2<-as.data.frame(getWM(wm.SR,i)) #we want getWM and not just wm.SS[[i]], since for averaging and plotting it's important to note that the value of the not seen cue was 0 in the first trial.
      wm2$Trial <- i
      for(j in 1:nrow(wm2)){ #loop over the rows of the weight matrix
        cue = rownames(wm2) #get the names of those rows
        if(!(cue[j] %in% rownames(wm.FR[[NrOfTrials]])) | cue[j] == "env"){ #the cue has not been asked before and therefore can be taken into account in the average for the graph
          wm_list2 <- list.append(wm_list2, wm2[cue[j],]) #append the row to the list of working memorys for run 2
        }
      }
    }
    Run2 <- bind_rows(wm_list2)
    if(identical(O2, Outcome000)) Run2$Cond <- "Outcome000"
    if(identical(O2, Outcome050)) Run2$Cond <- "Outcome050"
    if(identical(O2, Outcome100)) Run2$Cond <- "Outcome100"
    Run2$Stage <- "Train"
    
    ###
    
    ThirdRun <- data.frame(Cues = FC3, Outcomes = O3, stringsAsFactors = FALSE)
    train.TR <- createTrainingData(ThirdRun, random = F)
    wm.TR <- EDLearning(train.TR, progress = F, etaNeg = etaNeg, wm = wm.SR[[NrOfTrials]])
    
    for (i in 1:NrOfTrials){ 
      wm3<-as.data.frame(getWM(wm.TR,i)) 
      wm3$Trial <- i
      for(j in 1:nrow(wm3)){ 
        cue = rownames(wm3) 
        if(!(cue[j] %in% rownames(wm.SR[[NrOfTrials]]))| cue[j] == "env"){
          wm_list3 <- list.append(wm_list3, wm3[cue[j],]) 
        }
      }
    }
    Run3 <- bind_rows(wm_list3)
    if(identical(O3, Outcome000)) Run3$Cond <- "Outcome000"
    if(identical(O3, Outcome050)) Run3$Cond <- "Outcome050"
    if(identical(O3, Outcome100)) Run3$Cond <- "Outcome100"
    Run3$Stage <- "Train"
    wm_total <- list.append(wm_total, Run1, Run2, Run3)
    #We now have wm_total which is a list
    
    ################################################################
    #That was train now test
    wm_list4 <- list()
    wm_list5 <- list()
    wm_list6 <- list()
    
    
    XBlock1 <- str_split(FC1[1], pattern = "_", simplify = TRUE)[1,3] #this will give the 3rd item (so the X), such that we can paste it onto the X from the new one
    NewCue <- str_replace("env_toadstool_X", "X", XBlock1)
    #These are the cues for test phase 1 AX, AX, BX, BX, New one, new one, AX, AX, BX, BX
    CuesTP1 <- c(FC1[1], FC1[1], FC1[2], FC1[2], NewCue, NewCue, FC3[1], FC3[1], FC3[2], FC3[2])
    CuesTP2 <- c(FC1[1], FC1[1], FC1[2], FC1[2], NewCue, NewCue, FC3[1], FC3[1], FC3[2], FC3[2]) #THIS ONE TOO
    CuesTP3 <- c("env_straw_shrimp", "env_straw_shrimp", "env_straw_shrimp", "env_peanuts_shrimp", "env_peanuts_shrimp", "env_peanuts_shrimp", 
                 "env_yoghurt_shrimp", "env_yoghurt_shrimp", "env_yoghurt_shrimp", "env_bran_pork", "env_bran_pork", "env_bran_pork",
                 "env_chicken_pork", "env_chicken_pork", "env_chicken_pork") #THIS ONE SHOULD BE RANDOM
    
    Outcomes1 <- c("Diamond", "Fossil", "Diamond", "Fossil","Diamond", "Fossil", "Diamond", "Fossil", "Diamond", "Fossil")
    Outcomes2 <- c("Diamond", "Fossil", "Diamond", "Fossil","Diamond", "Fossil", "Diamond", "Fossil", "Diamond", "Fossil")
    Outcomes3 <- c("Diamond", "Fossil", "NoDiamond", "Diamond", "Fossil", "NoDiamond", "Diamond", "Fossil", "NoDiamond", "Diamond", "Fossil", "NoDiamond", "Diamond", "Fossil", "NoDiamond")
    
    
    FirstTestRun <- data.frame(Cues = CuesTP1, Outcomes = Outcomes1, stringsAsFactors = FALSE)
    train.FTR <- createTrainingData(FirstTestRun, random = F)
    wm.FTR <- EDLearning(train.FTR, progress = F, etaNeg = etaNeg, wm = wm.TR[[16]])
    for (i in 1:10){ 
      wm4<-as.data.frame(getWM(wm.FTR,i)) 
      wm4$Trial <- i
      for(j in 1:nrow(wm4)){ 
        cue = rownames(wm4) 
        if(!(cue[j] %in% rownames(wm.FTR[[10]]))| cue[j] == "env"){
          wm_list4 <- list.append(wm_list4, wm4[cue[j],]) 
        }
      }
    }
    Run4 <- bind_rows(wm_list4)
    Run4$Cond <- "Phase1"
    Run4$Stage <- "Test"
  
    ###
    
    SecondTestRun <- data.frame(Cues = CuesTP2, Outcomes = Outcomes2, stringsAsFactors = FALSE)
    train.STR <- createTrainingData(SecondTestRun, random = T)
    wm.STR <- EDLearning(train.STR, progress = F, etaNeg = etaNeg, wm = wm.FTR[[10]])
    for (i in 1:10){
      wm5<-as.data.frame(getWM(wm.STR, i))
      wm5$Trial <- i
      for (j in 1:nrow(wm5)){
        cue = rownames(wm5)
        if(!(cue[j] %in% rownames(wm.STR[[10]]))|cue[j] == "env") {
          wm_list5 <- list.append(wm_list5, wm5[cue[j],])
        }
      }
    }
    Run5 <- bind_rows(wm_list5)
    Run5$Cond <- "Phase2"
    Run5$Stage <- "Test"
    
    ###
    
    ThirdTestRun <- data.frame(Cues = CuesTP3, Outcomes = Outcomes3, StringsAsFactors = FALSE)
    train.TTR <- createTrainingData(ThirdTestRun, random = T)
    wm.TTR <- EDLearning(train.TTR, progress = F, etaNeg = etaNeg, wm = wm.STR[[10]])
    for (i in 1:15){
      wm6 <- as.data.frame(getWM(wm.TTR, i))
      wm6$Trial <- i
      for(j in 1:nrow(wm6)){
        cue = rownames(wm6)
        if(!(cue[j] %in% rownames(wm.TTR[[15]]))|cue[j] == "env"){
          wm_list6 <- list.append(wm_list6, wm6[cue[j],])
        }
      }
    }
    Run6 <- bind_rows(wm_list6)
    Run6$Cond <- "Phase3"
    Run6$Stage <- "Test"
    wm_totalTest <- list.append(wm_totalTest, Run4, Run5, Run6)
    
  }
  wm_total <- list.append(wm_total, wm_totalTest)
  wm_total <- bind_rows(wm_total)
  return(wm_total)
}

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


All <- rbind(Group1Train, Group2Train, Group3Train, Group4Train, Group5Train, Group6Train)
All$wordType <- NA

#Created a new specific function for ours here, since our cues aren't always the same for each particpant!
word <- c(Cues[1], Cues[2], Cues[3], Cues[4], Cues[5], Cues[6], Cues[7], Cues[8], Cues[9])
wordType <- c("A", "B", "X", "A", "B", "X", "A", "B", "X")
words <- data.frame(Word = word, wordType = wordType, stringsAsFactors = FALSE)
getwords <- words$wordType
names(getwords) <- words$Word

for(i in 1:nrow(All)){
  All[i,]$wordType <- getwords[gsub("...[0-9]+", "",row.names(All[i,]))]
}

aggAll <- aggregate(All$Diamond, list(All$Cond, All$wordType), mean)
aggAll
aggAllNot <- aggregate(All$NoDiamond, list(All$Cond, All$wordType), mean)
aggAllNot

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


All2 <- rbind(Group1Train, Group2Train, Group3Train, Group4Train, Group5Train, Group6Train)
All2$wordType <- NA
for(i in 1:nrow(All2)){
  All2[i,]$wordType <- getwords[gsub("...[0-9]+", "",row.names(All[i,]))]
  
}

#So now that we have all, we wanna aggregate over Cond and wordType, to see if we can get the same averages
aggAll2 <- aggregate(All2$Diamond, list(All2$Cond, All2$wordType), mean)
aggAll2
aggAllNot2 <- aggregate(All2$NoDiamond, list(All2$Cond, All2$wordType), mean)
aggAllNot2

ByTrial_RW <- aggregate(list(All$Diamond, All$NoDiamond), list(All$Trial, All$Cond, All$wordType), mean) 
colnames(ByTrial_RW) <- c("TrialNr", "Cond", "FoodGroup", "Diamond", "NoDiamond")
#head(ByTrial_RW)
Cond000 <- filter(ByTrial_RW, Cond == "Outcome000")
Cond050 <- filter(ByTrial_RW, Cond == "Outcome050")
Cond100 <- filter(ByTrial_RW, Cond == "Outcome100")


ByTrial_VHW <- aggregate(list(All2$Diamond, All2$NoDiamond), list(All2$Trial, All2$Cond, All2$wordType), mean) 
colnames(ByTrial_VHW) <- c("TrialNr", "Cond", "FoodGroup", "Diamond", "NoDiamond")
Cond000_2 <- filter(ByTrial_VHW, Cond == "Outcome000")
Cond050_2 <- filter(ByTrial_VHW, Cond == "Outcome050")
Cond100_2 <- filter(ByTrial_VHW, Cond == "Outcome100")
