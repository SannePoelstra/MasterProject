#install.packages("ndl")
library(ndl)
#install.packages("http://www.jacolienvanrij.com/NDL/NDLvisualization_0.4.tar.gz", repos=NULL, type="source", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
library(NDLvisualization)
library(plotfunctions)
library(data.table)

cues <- c("Light", "Light_Tone") #_ is two cues, so not one cue!!  
outcomes <- c("Food", "Food")
frequency <- c(300,300)

#define unique learning events and corresponding frequency
dat.block <- data.frame(Cues = cues, Outcomes = outcomes, Frequency = frequency, stringsAsFactors = FALSE)

#Then create the training dataframe with non-randomized events
train.block <- createTrainingData(dat.block, random = F)


#now w train the learning network
wm.block <- RWlearning(train.block, progress = F, alpha = 1, beta1 = 0.01, beta2 = 0.01, lambda =1)
#this gives a list of weight matrices for each training event
#if we wanna get a certain weight matrix of a certain learning event (e.g the first one) we can do this:
getWM(wm.block, 1)


#plot weights from all (two) cues to the specified outcome
plot <- plotOutcomeWeights(wm.block, outcome = "Food", main = "Weights to outcome \"Food\"", xlab="Trial", add.labels = F)
text(x = plot$x - 100, y = plot$y + c(-0.05, 0.05), labels = plot$labels, col=plot$col)

################################################################################################
#3.2
dat0 <- data.frame(Cues = c("Light_loudTone", "Light_softTone"), Outcomes = c("Food", "Food"), Frequency = c(600,300), stringsAsFactors = FALSE)
train0 <- createTrainingData(dat0, random = T)
wm0 <- RWlearning(train0, progress = F, alpha = 1, beta1 = 0.01, beta2 = 0.01, lambda = 1)

plot <- plotOutcomeWeights(wm0, outcome = "Food", main = "Weights to outcome \"Food\"", xlab = "Trial", ylim = c(0,1), add.labels = F)
text(x= plot$x - 100, y = plot$y + c(0.05, 0.05, -0.05), labels = plot$labels, col = plot$col)

##########################################
#Outcome competition
cues <- c("Food", "Food")
outcomes <- c("Light", "Light_Tone")
frequency <- c(300,300)

dat.block.rev <- data.frame(Cues = cues, Outcomes = outcomes, Frequency = frequency, stringsAsFactors = FALSE)
train.block.rev <- createTrainingData(dat.block.rev, random = F)
wm.block.rev <- RWlearning(train.block.rev, progress = F, alpha = 1, beta1 = 0.01, beta2 = 0.01, lambda = 1)

plotCueWeights(wm.block.rev, cue = "Food",  main = "Weights from cue \"Food\"", xlab = "Trial", ylim = c(0,1))

#############################################
#5: interaction of cue and outcome competition
animals1 <- data.frame(Cues = c("tail-wagging_big", "barking_small", "tail-wagging_small", "hopping_small"), Outcomes = c("DOG", "DOG", "DOG", "RABBIT"), Frequency = c(50,50,50,50), stringsAsFactors = FALSE)
train1 <- createTrainingData(animals1, random = T) #ranodm here because we see different things through each other, not first all dogs and then all rabbits (not like first the light then the ligth and noise combo)
head(train1)
#first normal EDL
wm1 <- RWlearning(train1, progress = F)
#then cue comp turned off
#wm1.noCueComp <- RW_noCueComp(train1)
#and outcome comp turned off
#wm1.noOutcomeComp <- RW_noOutcomeComp(train1)

################################################
#6: Network asymmetry
animals2 <- data.frame(Features = c("tail-wagging_big", "barking_small", "long-ears_big", "hopping_small"), Labels = c("DOG", "DOG", "RABBIT", "RABBIT"), Frequency = c(90,10,10,90),stringsAsFactors = F)
#object first training, so this creature is tail wagging and big, what is it?
objectFirst2 <- data.frame(Cues = animals2$Features, Outcomes = animals2$Labels, Frequency = animals2$Frequency, stringsAsFactors = F)
train2.objectFirst <- createTrainingData(objectFirst2, nruns = 60, random = T)
wm2.objectFirst <- RWlearning(train2.objectFirst, progress = FALSE)

#label first training, so hey this is a dog, what features is it likely to have
labelFirst2 <- data.frame(Cues = animals2$Labels, Outcomes = animals2$Features, Frequency = animals2$Frequency, stringsAsFactors = F)
train2.labelFirst <- createTrainingData(labelFirst2, nruns = 60, random = T)
wm2.labelFirst <- RWlearning(train2.labelFirst, progress = FALSE)


#################################################
#7: Model response

####Object-first
trial <- 10000
options.activations <- c(getActivations(wm2.objectFirst, cues = "small_barking", select.outcomes = "DOG")[trial, "DOG"],
                                        getActivations(wm2.objectFirst, cues = "small_barking", select.outcomes = "RABBIT")[trial, "RABBIT"])
#this was to give the activations of both outcomes after completed training

#to display this info nicely, setup a data frame summarizing all of it
options.choiceAlternatives <- c("DOG", "RABBIT")
options <- data.frame(activations = options.activations, choiceAlternatives = options.choiceAlternatives, cues = "small_barking", choiceProbabilty = 0, stringsAsFactors = F)
#note that choiceProb will be filled below, so is now just 0's
options

#choiceProb will be done with Luce Choice function. This needs positive values (ReLu is suggested), and the choice baseline is there (0.5 here 0.25 in label-first)
for(r in 1:nrow(options)){
  options$choiceProbabilty[r] <- luce(relu(options$activations[r]),
                                      relu(options$activations))
}

###Label-first
trial <- 10000 # last training trial
options.activations <-
  c(getOutcomesetActivations(wm2.labelFirst, c("small", "barking"), "DOG")[trial],
    getOutcomesetActivations(wm2.labelFirst, c("big", "tail-wagging"), "DOG")[trial],
    getOutcomesetActivations(wm2.labelFirst, c("big", "long-ears"), "DOG")[trial],
    getOutcomesetActivations(wm2.labelFirst, c("small", "hopping"), "DOG")[trial])
#use getOutcomesetActivations instead of get Activations since we wanna look at the set as a whole (probs and such)
options.choiceAlternatives <-
  c("small_barking", "big_tail-wagging", "big_long-ears", "small_hopping")
# all outcome sets that are choice alternatives

options <- data.frame(activations = options.activations,
                      cue = "DOG",
                      choiceAlternatives = options.choiceAlternatives,
                      choiceProbability = 0, # dummy column to fill below
                      stringsAsFactors = F)
for (r in 1:nrow(options)){
  options$choiceProbability[r] <- luce(relu(options$activations[r]),
                                       relu(options$activations))
}


############################################
#8: Cue and outcome representations