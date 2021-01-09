#The update to both the UpdateWeights function and the RWlearning function to also include VHW way of learning


#Set etaNeg to true if you want the VHW way of learning, leave it false if you want the RWlearning way
updateWeights2 <- function (cur.cues, cur.outcomes, wm = NULL, eta = 0.01, lambda = 1, 
                            alpha = 0.1, beta1 = 0.1, beta2 = 0.1, etaNeg = FALSE)
{
  bg <- getOption("background")
  cur.cues <- c(bg, cur.cues)
  if (is.null(wm)) {
    wm <- createWM(cues = cur.cues, outcomes = cur.outcomes)
  }
  else {
    wm <- checkWM(cues = cur.cues, outcomes = cur.outcomes, 
                  wm = wm)
  }
  Vtotal = 0
  if (length(cur.cues) <= 1) {
    Vtotal = wm[cur.cues, ]
  }
  else {
    if (ncol(wm) > 1) {
      Vtotal = colSums(wm[cur.cues, ], na.rm = TRUE)
    }
    else {
      Vtotal = sum(wm[cur.cues, ], na.rm = TRUE)
    }
  }
  Lambda = rep(0, ncol(wm))
  Lambda[which(colnames(wm) %in% cur.outcomes)] <- lambda
  lr = rep(eta, length(Lambda)) 
  ###################################################
  #New code
  
  m <- matrix() #create an empty matrix (matrix(rep) does not work in the VHW case)
  flag <- FALSE #flag since a completely empty matrix is not possible, so we remove the first row (however we don't want to keep doing that)
  if(etaNeg == TRUE){
    for (i in 1:nrow(wm)){
      cues = rownames(wm) #for each row in wm, get the rowname and that is the cue
      if(cues[i] %in% cur.cues){ #if that cue is one of the current cues we can treat it like RW and let the lr be positive
        if(all(dim(m) == c(1,1)) && flag == FALSE){ #is the first row?
          m <- matrix(lr * (Lambda - Vtotal), nrow = 1, ncol = length(Lambda), byrow = TRUE) #if so don't rowbind but just overwrite it
          flag <- TRUE
        }
        else{ #else rowbind it
          m <- rbind(m,matrix(lr * (Lambda - Vtotal), nrow = 1, ncol = length(Lambda), byrow = TRUE))
        }
      }
      else if(!(cues[i] %in% cur.cues)){ #if that cue is NOT one of the current cues we set lr to negative, since the cue is absent now
        if(all(dim(m) == c(1,1)) && flag == FALSE){
          m <- matrix(lr * (-1) * (Lambda - Vtotal), nrow = 1, ncol = length(Lambda), byrow = TRUE)
          flag <- TRUE
        }
        else{
          m <- rbind(m,matrix(lr * (-1) * (Lambda - Vtotal), nrow = 1, ncol = length(Lambda), byrow = TRUE))
        }
      }
    }
    matrix <- m
  }
  
  ##################################################################
  if (is.null(eta)) {
    lr = alpha * (beta1 * Lambda + beta2 * (lambda - Lambda))
  }
  
  ################################################################
  if(etaNeg == TRUE){ #if you have VHW learning, we don't have to update only the current cues, we wanna update them all
    wm = wm + matrix
  }
  if(etaNeg == FALSE){ #but if we have RW learning we want to only update the current ones and we can just keep the lr the same everywhere
    wm[cur.cues, ] = wm[cur.cues, ] + matrix(rep(lr * (Lambda - Vtotal), length(cur.cues)), nrow = length(cur.cues), byrow = TRUE)
  }
  return(wm)
}
#that was updateWeights!

##################################################################################################################################################################

##The only thing to change here is making sure that etaNeg is given and that updateWeights2 is called instead of the normal one
EDLearning <- function (data, wm = NULL, eta = 0.01, etaNeg = FALSE, lambda = 1, alpha = 0.1, 
                        beta1 = 0.1, beta2 = 0.1, progress = TRUE, ...) 
{
  if (!all(c("Cues", "Outcomes") %in% names(data))) {
    stop("Specify a column Cues and a column Outcomes in data.")
  }
  out <- list()
  lout <- 0
  if (!is.null(wm)) {
    if (is.list(wm)) {
      out <- wm
      lout <- length(wm)
      wm <- wm[[length(wm)]]
    }
    if (!is.matrix(wm)) {
      stop(sprintf("Argument wm cannot be class %s: wm should specify weight matrix or list of weight matrices.", 
                   class(wm)[1]))
    }
  }
  if (progress & (nrow(data) > 2)) {
    pb <- txtProgressBar(style = 3, min = 1, max = nrow(data))
    step <- min(c(max(c(1, round(0.01 * nrow(data)))), nrow(data)))
    for (i in 1:nrow(data)) {
      if ((i%%step == 0) | i == 1 | i == nrow(data)) {
        setTxtProgressBar(pb, i)
      }
      wm <- updateWeights2(cur.cues = getValues(data[i, 
                                                     ]$Cues, ...), cur.outcomes = getValues(data[i, 
                                                                                                 ]$Outcomes, ...), wm = wm, eta = eta, lambda = lambda, 
                           alpha = alpha, beta1 = beta1, beta2 = beta2, etaNeg = etaNeg)
      out[[length(out) + 1]] = wm
    }
    close(pb)
  }
  else {
    for (i in 1:nrow(data)) {
      wm <- updateWeights2(cur.cues = getValues(data[i, 
                                                     ]$Cues, ...), cur.outcomes = getValues(data[i, 
                                                                                                 ]$Outcomes, ...), wm = wm, eta = eta, lambda = lambda, 
                           alpha = alpha, beta1 = beta1, beta2 = beta2, etaNeg = etaNeg)
      out[[length(out) + 1]] = wm
    }
  }
  return(out)
}
#################################################################################################################################################################

#If you put in a string of cues in the form of A_X, B_X etc, then it'll transform it to whatever you tell it that A, B and X are
FillInCues <- function(A, B, X, Cue){
  Cue <- str_replace(Cue, "A", A)
  Cue <- str_replace(Cue, "B", B)
  Cue <- str_replace(Cue, "X", X)
  return(Cue)
}

##################################################################################################################################################################

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


#########################################################################
word <- c("shrimp", "yoghurt", "bananas", "wheat", "corn", "blueberries", "strawberries", "bran", "chicken", "walnuts", "horseradish", "cheese", "peanuts", "cabbage", "mustard", "peaches", "lobster", "pork")
wordType <- c("X","X","X","X","X","X","A","A","A","A","A","A","B","B","B","B","B","B")
words <- data.frame(Word = word, wordType = wordType, stringsAsFactors = FALSE)
getwords <- words$wordType
names(getwords) <- words$Word

############################
