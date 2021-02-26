rm(list=ls())
library(ndl)
library(plotfunctions)
library(data.table)
library(edl)
library(stringr)
library(rlist)
library(dplyr)
library(roperators)
source("C:/Users/Sanne/Dropbox/MasterProject/Modelling/OurExperiment1Run.R")



#food conditions and their outcomes can be given
wm_total <- list()
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
    ###
    
    SecondRun <- data.frame(Cues = FC2, Outcomes = O2, stringsAsFactors = FALSE)
    train.SR <- createTrainingData(SecondRun, random = F)
    wm.SR <- EDLearning(train.SR, progress = F, etaNeg = etaNeg, wm = wm.FR[[16]]) #we take the working memory of the last run as our starting point, as we have the same outcomes
    
    #the for loop needs to be handled a bit differently, as we don't want to get the weights of foods from the previous trial, as participants don't score those anymore.
    for (i in 1:16){ 
      wm2<-as.data.frame(getWM(wm.SR,i)) #we want getWM and not just wm.SS[[i]], since for averaging and plotting it's important to note that the value of the not seen cue was 0 in the first trial.
      wm2$Trial <- i
      for(j in 1:nrow(wm2)){ #loop over the rows of the weight matrix
        cue = rownames(wm2) #get the names of those rows
        if(!(cue[j] %in% rownames(wm.FR[[16]])) | cue[j] == "env"){ #the cue has not been asked before and therefore can be taken into account in the average for the graph
          wm_list2 <- list.append(wm_list2, wm2[cue[j],]) #append the row to the list of working memorys for run 2
        }
      }
    }
    Run2 <- bind_rows(wm_list2)
    if(identical(O2, Outcome000)) Run2$Cond <- "Outcome000"
    if(identical(O2, Outcome050)) Run2$Cond <- "Outcome050"
    if(identical(O2, Outcome100)) Run2$Cond <- "Outcome100"
    
    ###
    
    ThirdRun <- data.frame(Cues = FC3, Outcomes = O3, stringsAsFactors = FALSE)
    train.TR <- createTrainingData(ThirdRun, random = F)
    wm.TR <- EDLearning(train.TR, progress = F, etaNeg = etaNeg, wm = wm.SR[[16]])
    
    for (i in 1:16){ 
      wm3<-as.data.frame(getWM(wm.TR,i)) 
      wm3$Trial <- i
      for(j in 1:nrow(wm3)){ 
        cue = rownames(wm3) 
        if(!(cue[j] %in% rownames(wm.SR[[16]]))| cue[j] == "env"){
          wm_list3 <- list.append(wm_list3, wm3[cue[j],]) 
        }
      }
    }
    Run3 <- bind_rows(wm_list3)
    if(identical(O3, Outcome000)) Run3$Cond <- "Outcome000"
    if(identical(O3, Outcome050)) Run3$Cond <- "Outcome050"
    if(identical(O3, Outcome100)) Run3$Cond <- "Outcome100"
    wm_total <- list.append(wm_total, Run1, Run2, Run3)
    
  }
  wm_total <- bind_rows(wm_total)
  return(wm_total)
}
























#check how to find what the cue from block 1,2 and 3 were




Cue <- c("A_X", "B_X", "B_X", "A_X", "A_X", "B_X", "A_X", "B_X", "B_X", "A_X", "B_X", "A_X", "A_X", "B_X", "A_X", "B_X")
Cue <- paste("env", Cue, sep = "_")







Cues1 <- FillInCues(Cues[1], Cues[2], Cues[3], Cue)
Cues2 <- FillInCues(Cues[4], Cues[5], Cues[6], Cue)
Cues3 <- FillInCues(Cues[7], Cues[8], Cues[9], Cue)

###
Outcome000 <- c("Allergic", "Not", "Allergic", "Not", "Not", "Allergic", "Allergic", "Not", "Allergic", "Not", "Not", "Allergic", "Not", "Allergic", "Allergic", "Not")