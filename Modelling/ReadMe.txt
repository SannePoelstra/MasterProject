VanHammeWassermanFunctions.R : a function file to call in other files, such that I don't have to copy paste updateWeights2 etc
OriginalExperimentRun.R: a function file that runs the original experiment with both VHW and RW learning, 3 sheets for each participant (used for plots)
FirstSetupVHWExp.Rmd/html: First try at simulating VHW experiment, outdated for now, as I've made some mistakes. Will delete after next meeting
Plots.Rmd/html: Compares plots of orginal paper, to plots of the models
FixingVHW.Rmd/html: file to replace FirstSetupVHWExp, mistakes have been ironed out, more structured, makes use of the function file.
AdjustedRuns.Rmd/html: file to plot the models response to different input (more, randomized, etc) and compare them.
MoreTrials.R: A run of the experiment with more trials, to be used in the plotting file