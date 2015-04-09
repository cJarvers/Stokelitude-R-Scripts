################## Some functions for the analysis of strokelitude data

######### Functions for getting paramters of the data (borders, the end, etc.)############

##### A function to determine borders of measurement periods, based on gaps in the data
getBorders <- function(flyTraces, threshold){
	
	# find the borders by finding timegaps in the data that are bigger than threshold (sec)
	jump <- c(0,(diff(flyTraces$Time) > threshold))
	borders <- unique(jump * flyTraces$Time)
}

##### A function to return the last second of the data
getLastSecond <- function(flyData){
	return(flyData$Time[length(flyData$Time)])	# return the last timestamp in the dataframe
}

##### A function to convert borders (e.g. from getBorders) to starting and ending timepoints of measurement periods
bordersToStarts <- function(borders, condOrder, lastSecond){ # given the borders and the order of conditions, get the starting and ending times of each measurement period
# condOrder should be a vector, the length of which is the number of measurement periods in the experiment.
# The number of value i in condOrder should indicate, which condition that measurement belonged to.
# For example, if there were three measurements, the first being baseline, the second condition A and the third again baseline, condOrder should be c(0,1,0) or c(1,2,1)
	
	if(length(borders) != length(condOrder)){                                                     # if the numbers of measurements do not match up
		stop("Number of measurements determined from borders does not match specified number.") # throw an error
	}

	conditions <- sort(unique(condOrder)) # determine the number of conditions and sort them in ascending order
	
	# initialize some housekeeping variables
	measurement <- c() # a vector to save the measurements of each condition
	startTimes <- vector("list", length = length(conditions))	# a list to save the vector of starting times for each condition
	endTimes <- vector("list", length = length(conditions))	# a list to save the vector of ending times for each condition

	# go through all conditions and determine the starting times
	for(ind in 1:length(conditions)){   # iterate the conditions (the 1:length(conditions) should protect against stupid numbering, e.g. orderCond <- c(0,4,5)
		measurement <- (condOrder == conditions[ind])		# get a logical vector, which is 1 for every measurement of the current condition
		startTimes[[ind]] <- unique(measurement * borders)	# save the timepoints at which the measurement periods start in startTimes
		if(measurement[length(measurement)] != 0){		# check if the last measurement belongs to this condition to avoid index-out-of-bound errors 
			measurement <- subset(c(0, measurement),as.logical(c(rep(1, times = length(measurement)-1),0))) # shift measurement one to the right, in order to get the end values of each measurement
			endTimes[[ind]] <- unique(measurement*borders)
			endTimes[[ind]] <- append(endTimes[[ind]], lastSecond)							# append the time of the end of the experiment in order to have the time the last measurement ended
		} else { # if the last measurement is not part of this condition, shift measurement one to the right
			measurement <- subset(c(0, measurement),as.logical(c(rep(1, times = length(measurement)-1),0))) # shift measurement one to the right, in order to get the end values of each measurement
			endTimes[[ind]] <- unique(measurement*borders)
		}
	}

	# return a list named output, which contains the list of starting vectors and the list of ending vectors
	output <- vector("list", length = 2)
	output[[1]] <- startTimes
	output[[2]] <- endTimes

	return(output)
}

##### a function to get the starting and ending times (same output as bordersToStarts) for data without gaps, either querying the user or using a hardcoded protocol
parameterQuery <- function(){

	# use I/O query to determine the paramters

	print("Please enter the number of conditions:")
	nCond <- scan(n=1)
	
	condLengths <- vector("numeric", length = nCond)
	# read the number of measurements for each condition
	for(ind in 1:nCond){
		print(paste("Please enter the number of measurement periods for condition", ind))
		condLengths[ind] <- scan(n=1)
	}
	# initialize the vectors in which the starting end ending times will be saved
	startTimes <- vector("list", length = nCond)
	endTimes <- vector("list", length = nCond)
	for(ind in 1:nCond){
		startTimes[[ind]] <- vector("numeric", length = condLengths[ind])
		endTimes[[ind]] <- vector("numeric", length = condLengths[ind])
	}
	# read the starting and ending times for each condition
	for(ind in 1:nCond){
		for(index in 1:condLengths[ind]){
			print(paste("Please enter the starting time (sec) for measurement", index, "of condition", ind))
			startTimes[[ind]][index] <- scan(n=1)
			print(paste("Please enter the end time (sec) for measurement", index, "of condition", ind))
			endTimes[[ind]][index] <- scan(n=1)
		}
	}

	##### return a list containing startTimes and endTimes
	output <- vector("list", length = 2)
	output[[1]] <- startTimes
	output[[2]] <- endTimes
	return(output)
} 

##### a function to return hardcoded parameters of an optomotor task with order brbl brbl brbl brbl b and 30 seconds recording time per condition
parametersHardcoded <-  function(){
	startTimes  <- vector("list", length = 3)
	endTimes <- vector("list", length = 3)

	# the times for the baseline measurements
	startTimes[[1]] <- c(0,60,120,180,240,300,360,420,480)
	endTimes[[1]] <- c(30,90,150,210,270,330,390,450,510)

	# the times for the measurements to the right
	startTimes[[2]] <- c(30,150,270,390)
	endTimes[[2]] <- c(60,180,300,420)

	# the times for the measurements to the left
	startTimes[[3]] <- c(90,210,330,450)
	endTimes[[3]] <- c(120,240,360,480)

	output <- vector("list", length = 2)
	output[[1]] <- startTimes
	output[[2]] <- endTimes
	return(output)	
}

##### a function that calculates starting and ending times from the length of the experiment and the number of measurements per condition
parameterCalc <- function(nCond, measLength, nMeas){
	# nCond is the number of conditions, measLength is the length of each measurement and nMeas is the number of measurements

	# Assumes that conditions are iterated through and then repeated 
	startTimes <- vector("list", length = nCond)
	endTimes <- vector("list", length = nCond)
	condCount <- 1		# an integer to keep track of which condition a measurement belongs to
	
	# use a for-loop to iterate through measurements
	for(ind in 1:nMeas){
		startTimes[[condCount]] <- append(startTimes[[condCount]],(ind-1)*measLength)	# the interval starts after ind-1 measurement periods (the first at 0, the second at measLength, etc.)
		endTimes[[condCount]] <- append(endTimes[[condCount]],ind*measLength)		# the interval ends a measurement interval later
		
		condCount <- condCount+1	# keep track of which condition you are in
		if(nCond < condCount){		# once you went through all conditions, start with the first again
			condCount <- 1
		}
	}

	##### return a list containing startTimes and endTimes
	output <- vector("list", length = 2)
	output[[1]] <- startTimes
	output[[2]] <- endTimes
	return(output)
}

##### a function that queries the labels for conditions
conditionQuery <- function(nCond){
	
	# make a vector in which to save the condition labels
	conditionLabels <- vector("character", length = nCond)

	for(ind in 1:nCond){	# go through all conditions
		print(paste("Please enter the label for condition", ind))
		conditionLabels[ind] <- scan(n=1, what = "character")
	}

	return(conditionLabels)
}





############# Functions for formatting the data according to paramters ##########

##### a function to sort the data into different conditions
flyDataSections <- function(flyTraces, sectionStarts, sectionEnds){
	# flyTraces should be a dataframe containing preprocessed strokelitude data
	# sectionStarts should be a list of vectors, where each vector contains the starting timepoints of the measurements of one condition
	# sectionEnds should be a list of vectors, where each vector contains the ending timepoints of the measurements of one condition
	# The order of conditions should be the same in sectionStarts and sectionEnds
	
	#create a list in which to save the sorted data
	sorted <- vector("list", length = length(sectionStarts))

	# a for-loop to sort the data
	for(index in 1:length(sectionStarts)){ # for every contition
		for(counter in 1:length(sectionStarts[[index]])){ # for every measurement
			# append the difference between left and right wing angle of the measurement period to the list of values for that condition
			sorted[[index]] <- append(sorted[[index]], flyTraces$Right[(sectionStarts[[index]][counter] < flyTraces$Time) & (flyTraces$Time < sectionEnds[[index]][counter])] - flyTraces$Left[(sectionStarts[[index]][counter] < flyTraces$Time) & (flyTraces$Time < sectionEnds[[index]][counter])])
		}
	}

	return(sorted)	
}

##### a function that adds a factor containing the conditions to the dataframe
labelConditions <- function(flyData, startTimes, endTimes, conditionLabels){
	# conditionLabels should have the same order of conditions as startTimes and endTimes

	# make sure the input makes sense
	if((length(startTimes) != length(endTimes)) | (length(startTimes) != length(conditionLabels))){
		print(paste("Length of startTimes:", startTimes))
		print(paste("Length of endTimes:", endTimes))
		print(paste("Length of conditionLabels:", conditionLabels))
		stop("Error in labelConditons: Lengths of condition vectors to not match.")
	}
	
	# check whether there are double values in startTimes and endTimes (e.g. when first period ends at 100 and second starts at 100)
	overlap <- FALSE
	allTimes <- c()
	for(ind in 1:length(startTimes)){						# use a for-loop to write all times into one vector
		allTimes <- append(allTimes, startTimes[[ind]])
		allTimes <- append(allTimes, endTimes[[ind]])
	}
	overlap <- (length(allTimes) != length(unique(allTimes)))		# if there are overlapping times, the unique vector will be shorter

	## make a factor that contains the condition label for each datapoint
	dataLabels <- vector("character", length = length(flyData$Time))
	# the for-loop that writes the values into the vector
	for(ind in 1:length(conditionLabels)){		# iterate through all the conditions
		for(index in 1:length(startTimes[[ind]])){	# iterate through all the measurement periods of that condition
			if(overlap){	# if the interval borders overlap, leave the upper end open (<, not <=)
				# write the condition label of that condition into all datapoints of the current measurement period
				dataLabels[(flyData$Time >= startTimes[[ind]][index]) & (flyData$Time < endTimes[[ind]][index])] <- rep(conditionLabels[ind], times = sum((flyData$Time >= startTimes[[ind]][index]) & (flyData$Time < endTimes[[ind]][index])))
			} else {		# if the interval borders do not overlap, write into the whole interval
				# do the same as above, just with a <=
				dataLabels[(flyData$Time >= startTimes[[ind]][index]) & (flyData$Time <= endTimes[[ind]][index])] <- rep(conditionLabels[ind], times = sum((flyData$Time >= startTimes[[ind]][index]) & (flyData$Time <= endTimes[[ind]][index])))
			}
		}
	}
	
	# check whether there are still empty values in dataLabels (which indicates that some datapoints are not included in the measurement periods)
	if(sum(is.na(dataLabels)) != 0){
		stop("Error: Not all measurement points could be assigned to a condition.")
	}

	## put dataLabels as a factor into the dataframe flyData
	flyData$Condition <- as.factor(dataLabels)

	return(flyData)
}


