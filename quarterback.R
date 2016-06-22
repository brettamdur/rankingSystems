
# File:			quarterback.R
#	Author:		Brett Amdur
#	Project:	Measuring Ranking System Performance
# Notes:		This file is where user sets the argument values for parameters used in functions. 
#						Also includes wrapper functions to run to process match data.


library(ggplot2)
library(dplyr)
library(reshape2)

source('./rankings.R')
source('./processingFunctions.R')


rankdf = ranksCombinedClean

# set these variables to user preferred values.  They are, in turn, used as arguments to
# many functions called subsequently:
limitno = 20 # analysis only includes matches where both players were ranked higher than limitno
minfreq = 5  # only include points where two ranks opposed each other at least minfreq times
plotStartRank = 1 # start grids at this rank number 
plotStopRank = 20 # end grids at this rank number
# Need to add error checking to make sure startRank and stopRank are both less than limitno

# This is the order in which to run the functions.  See README file, and the functions
# themselves, for descriptions of the functions' input/output and what they do.
actuals <- limitRanks(rankdf = rankdf,
		      limitno = limitno, 
		      minfreq = minfreq, 
		      plotStartRank = plotStartRank, 
		      plotStopRank = plotStopRank)
hold <- perfectValues(limitno)
hold2 <- perfectReg(hold)
hold3 <- makePredictions(hold2, actuals, plotStartRank = plotStartRank, plotStopRank = plotStopRank)
# makePredictions return five things: 4 graphs (see the comments in the function) and the
# score (the MSE) for the collection of win/loss data passed to the function.

