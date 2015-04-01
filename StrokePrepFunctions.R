################## Some functions to import and preprocess strokelitude data

####################### Functions for importing data ##########################

##### Importing strokelitude data from a .txt file
flyDataImport <- function() {

	## Import the data from the right .txt file. This line has to be adjusted depending on which data is to be processed
	flyData <- read.table(file.choose())

	## Get the right data
	  # the following line would create a dataframe containing timestamp, right angle and left angle of all timepoints where one angle was measured
	  # flyTraces <- data.frame("Time" = flyData[[1]][(is.na(flyData[[4]]) & is.na(flyData[[5]])) == FALSE], "Right" = flyData[[4]][(is.na(flyData[[4]]) & is.na(flyData[[5]])) == FALSE], "Left" = flyData[[5]][(is.na(flyData[[4]]) & is.na(flyData[[5]])) == FALSE])
	# create a dataframe with timestamp, right angle and left angle for all datapoints at which both wing angles were measured
	flyTracesFiltered <- data.frame("Time" = flyData[[1]][(is.na(flyData[[4]]) | is.na(flyData[[5]])) == FALSE], "Right" = flyData[[4]][(is.na(flyData[[4]]) | is.na(flyData[[5]])) == FALSE], "Left" = flyData[[5]][(is.na(flyData[[4]]) | is.na(flyData[[5]])) == FALSE])
	
	# reset the "Time" variable to make everything more readable
	flyTracesFiltered$Time <- (flyTracesFiltered$Time - flyTracesFiltered$Time[1])

	return(flyTracesFiltered)
}

##### Importing strokelitude data from a .h5 file (hdf5 format)
#flyDataImportH5 <- function() {
#	library(rhdf5)
#
#	# read the data
#	flyData <- h5read(file.choose(), "/stroke_data")
#	flyTraces <- data.frame("Time" = flyData$timestamp[(is.na(flyData$right) & is.na(flyData$left)) == FALSE],"Right" = flyData$right[(is.na(flyData$right) & is.na(flyData$left)) == FALSE], "Left" = flyData$left[(is.na(flyData$right) & is.na(flyData$left)) == FALSE])
#	flyTracesFiltered <- data.frame("Time" = flyData$timestamp[(is.na(flyData$right) | is.na(flyData$left)) == FALSE], "Right" = flyData$right[(is.na(flyData$right) | is.na(flyData$left)) == FALSE], "Left" = flyData$left[(is.na(flyData$right) | is.na(flyData$left)) == FALSE])
#
#	return(flyTracesFiltered)
#}

##### Importing data from the torque-meter
flyTorqueImport <- function(){
	
	torqueData <- read.table(file.choose(), header = TRUE)		# read the file

	torqueTraces <- vector("list", length = length(torqueData))	# make a list in which one dataframe per fly will be saved
	
	# a for-loop assigning the traces
	for(ind in 1:length(torqueTraces)){
		torqueTraces[[ind]] <- data.frame("Trace" = torqueData[,ind], "Time" = seq(0,(length(torqueData[,ind])-1)/20,0.05))
	}
	
	return(torqueTraces)
}






########################## Functions for Filtering Data #######################

##### using a low-pass butterworth filter on all elements of the data
flyDataFilter <- function(flyTrace, frequency = 0.12, order = 3){
	## define the filter
	library(signal)
	flyFilter <- butter(order, frequency, type = "low")


	## filter the data
	fRight <- filtfilt(flyFilter, flyTrace$Right)
	fLeft <- filtfilt(flyFilter, flyTrace$Left)
	if(is.null(flyTrace$Trace)){
		return(data.frame("Time" = flyTrace$Time, "Right" = fRight, "Left" = fLeft))
	}else{
		fTrace <- filtfilt(flyFilter, flyTrace$Trace)
		return(data.frame("Time" = flyTrace$Time, "Right" = fRight, "Left" = fLeft, "Trace"= fTrace))
	}
}




############### Functions for downsampling (with bins or a sliding average)############

##### downsampling the data by binning
flyDataDownsample <- function(flyTracesFiltered, binsize) {

	# create the vectors in which to save the downsampled data
	timeDownsampled <- vector(mode = "numeric", length = ceiling(length(flyTracesFiltered$Time)/binsize))
	rightDownsampled <- vector(mode = "numeric", length = ceiling(length(flyTracesFiltered$Right)/binsize))
	leftDownsampled <- vector(mode = "numeric", length = ceiling(length(flyTracesFiltered$Left)/binsize))

	# for loop that does the downsampling for the time
	for (index in seq(1,length(flyTracesFiltered$Time),binsize)) {
		if(index < (length(flyTracesFiltered$Time)-binsize)) { # check whether we reached the end of the data; if not:
			timeDownsampled[((index-1)/binsize)+1] <- sum(flyTracesFiltered$Time[index:(index+binsize-1)])/binsize  # average all data in the bin and save it in the right slot of the downsampled vector
		} else {  # in case we reached the end
			timeDownsampled[((index-1)/binsize)+1] <- sum(flyTracesFiltered$Time[index:length(flyTracesFiltered$Time)])/length(flyTracesFiltered$Time[index:length(flyTracesFiltered$Time)]) # average over the remaining values and save the result
		}
	}
	# for loop that does the downsampling for the right trace
	for (index in seq(1,length(flyTracesFiltered$Right),binsize)) {
		if(index < (length(flyTracesFiltered$Right)-binsize)) { # check whether we reached the end of the data; if not:
			rightDownsampled[((index-1)/binsize)+1] <- sum(flyTracesFiltered$Right[index:(index+binsize-1)])/binsize  # average all data in the bin and save it in the right slot of the downsampled vector
		} else {  # in case we reached the end
			rightDownsampled[((index-1)/binsize)+1] <- sum(flyTracesFiltered$Right[index:length(flyTracesFiltered$Right)])/length(flyTracesFiltered$Right[index:length(flyTracesFiltered$Right)]) # average over the remaining values and save the result
		}
	}
	# for loop that does the downsampling for the left trace
	for (index in seq(1,length(flyTracesFiltered$Left),binsize)) {
		if(index < (length(flyTracesFiltered$Left)-binsize)) { # check whether we reached the end of the data; if not:
			leftDownsampled[((index-1)/binsize)+1] <- sum(flyTracesFiltered$Left[index:(index+binsize-1)])/binsize  # average all data in the bin and save it in the right slot of the downsampled vector
		} else {  # in case we reached the end
			leftDownsampled[((index-1)/binsize)+1] <- sum(flyTracesFiltered$Left[index:length(flyTracesFiltered$Left)])/length(flyTracesFiltered$Left[index:length(flyTracesFiltered$Left)]) # average over the remaining values and save the result
		}
	}

	# bind the downsampled vectors into one dataframe
	flyTracesDown <- data.frame("Time" = timeDownsampled, "Right" = rightDownsampled, "Left" = leftDownsampled)

	# return the downsampled vector
	return(flyTracesDown)
}

##### downsampling the data with a sliding average
flyDataSlidingAverage <- function(flyTracesFiltered, binsize) {
	
	# create the vectors in which to save the downsampled data
	timeDownsampled <- vector(mode = "numeric", length = length(flyTracesFiltered$Time))
	rightDownsampled <- vector(mode = "numeric", length = length(flyTracesFiltered$Time))
	leftDownsampled <- vector(mode = "numeric", length = length(flyTracesFiltered$Time))

	# for loop that does the downsampling for the time
	for (index in (1:length(flyTracesFiltered$Time))) {
		if(index < (length(flyTracesFiltered$Time)-binsize)) { # check whether we reached the end of the data; if not:
			timeDownsampled[index] <- sum(flyTracesFiltered$Time[index:(index+binsize-1)])/binsize  # average all data in the bin and save it in the right slot of the downsampled vector
		} else {  # in case we reached the end
			timeDownsampled[index] <- sum(flyTracesFiltered$Time[index:length(flyTracesFiltered$Time)])/length(index:length(flyTracesFiltered$Time)) # average over the remaining values and save the result
		}
	}
	# for loop that does the downsampling for the right trace
	for (index in (1:length(flyTracesFiltered$Right))) {
		if(index < (length(flyTracesFiltered$Right)-binsize)) { # check whether we reached the end of the data; if not:
			rightDownsampled[index] <- sum(flyTracesFiltered$Right[index:(index+binsize-1)])/binsize  # average all data in the bin and save it in the right slot of the downsampled vector
		} else {  # in case we reached the end
			rightDownsampled[index] <- sum(flyTracesFiltered$Right[index:length(flyTracesFiltered$Right)])/length(index:length(flyTracesFiltered$Right)) # average over the remaining values and save the result
		}
	}
	# for loop that does the downsampling for the left trace
	for (index in (1:length(flyTracesFiltered$Left))) {
		if(index < (length(flyTracesFiltered$Left)-binsize)) { # check whether we reached the end of the data; if not:
			leftDownsampled[index] <- sum(flyTracesFiltered$Left[index:(index+binsize-1)])/binsize  # average all data in the bin and save it in the right slot of the downsampled vector
		} else {  # in case we reached the end
			leftDownsampled[index] <- sum(flyTracesFiltered$Left[index:length(flyTracesFiltered$Left)])/length(index:length(flyTracesFiltered$Left)) # average over the remaining values and save the result
		}
	}

	# bind the downsampled vectors into one dataframe
	flyTracesSlidingAverage <- data.frame("Time" = timeDownsampled, "Right" = rightDownsampled, "Left" = leftDownsampled)

	# return the downsampled vector
	return(flyTracesSlidingAverage)
}






################## Functions for making excerpts from the data ################

##### making an excerpt from the data and plotting it
flyDataExcerptPlot <- function(flyTracesFiltered, startSecond, endSecond, binsize){

	#### single out a specific time window from the data
	flyTracesWindow <- subset(flyTracesFiltered, subset = (startSecond <= flyTracesFiltered$Time) & (flyTracesFiltered$Time <= endSecond))

	# downsample the data in that timewindow
	flyTracesDown <- flyDataDownsample(flyTracesWindow, binsize)

	
	## plot the traces and save them as .png files
	png(file = "RightTraceExcerpt.png", width = 1920) # direct the following output to the image RightTrace.png
	plot(x = flyTracesWindow$Time, y = flyTracesWindow$Right, type = "l", main = "Trace of the Right Wing", xlab = "Time (sec)", ylab = "Right Anterior Wingstroke Angle")
	png(file = "LeftTraceExcerpt.png", width = 1920)
	plot(x = flyTracesWindow$Time, y = flyTracesWindow$Left, type = "l", main = "Trace of the Left Wing", xlab = "Time (sec)", ylab = "Left Anterior Wingstroke Angle")
	png(file = "TraceExcerpt.png", width = 1920)
	plot(x = flyTracesWindow$Time, y = flyTracesWindow$Right-flyTracesWindow$Left, type = "l", main = "Difference in Wingstroke Amplitude", xlab = "Time (sec)", ylab = "Wingstroke Angle Difference")
	
	## plotting downsampled data
	png(file = "TraceDownsampledExcerpt.png", width = 1920)
	plot(x = flyTracesDown$Time, y = (flyTracesDown$Right-flyTracesDown$Left), type = "l", main = "Difference in Wingstroke Amplitude, Downsampled", xlab = "Time (sec)", ylab = "Wingstroke Angle Difference")
	graphics.off()
}

##### plot an excerpt of a fly trace (not requiring strokelitude data, can also plot traces from the torque meter)
flyTraceExcerptPlot <- function(Trace, Time, startSecond, endSecond, filename){
	
	# select the relevant subsets from the vectors Trace and Time
	TraceWindow <- Trace[(startSecond <= Time) & (Time <= endSecond)]
	TimeWindow <- Time[(startSecond <= Time) & (Time <= endSecond)]

	# plot and save in the file filename
	png(file = filename, width = 1920)
	plot(x = TimeWindow, y = TraceWindow, type = "l", main = "Torque Trace", xlab = "Time", ylab = "Yaw Torque")
	graphics.off()
}


##### just making the excerpt and returning the data, no plotting
flyDataExcerpt <- function(flyTracesFiltered, startSecond, endSecond){

	#### single out a specific time window from the data
	flyTracesWindow <- subset(flyTracesFiltered, subset = (startSecond <= flyTracesFiltered$Time) & (flyTracesFiltered$Time <= endSecond))	

	return(flyTracesWindow)
}







############################### Other Functions ###############################

######### Plotting transformations of the data (does not really help)
flyDataTransformations <- function(flyTracesFiltered) {

	## plot the traces and save them as .png files
	png(file = "TraceLogIn.png", width = 1920)
	plot(x = flyTracesFiltered$Time, y = log(flyTracesFiltered$Right)-log(flyTracesFiltered)$Left, type = "l", main = "Difference in Wingstroke Amplitude", xlab = "Time (sec)", ylab = "log(Wingstroke Angle) Difference")

	png(file = "TraceLogOut.png", width = 1920)
	plot(x = flyTracesFiltered$Time, y = (sign(flyTracesFiltered$Right-flyTracesFiltered$Left) * log(abs(flyTracesFiltered$Right-flyTracesFiltered$Left))), type = "l", main = "Difference in Wingstroke Amplitude", xlab = "Time (sec)", ylab = "log(Wingstroke Angle Difference)")

	png(file = "TraceRootIn.png", width = 1920)
	plot(x = flyTracesFiltered$Time, y = sqrt(flyTracesFiltered$Right)-sqrt(flyTracesFiltered$Left), type = "l", main = "Difference in Wingstroke Amplitude", xlab = "Time (sec)", ylab = "sqrt(Wingstroke Angle) Difference")

	png(file = "TraceRootOut.png", width = 1920)
	plot(x = flyTracesFiltered$Time, y = (sign(flyTracesFiltered$Right-flyTracesFiltered$Left) * sqrt(abs(flyTracesFiltered$Right-flyTracesFiltered$Left))), type = "l", main = "Difference in Wingstroke Amplitude", xlab = "Time (sec)", ylab = "sqrt(Wingstroke Angle Difference)")

	png(file = "TraceExp.png", width = 1920)
	plot(x = flyTracesFiltered$Time, y = sign(flyTracesFiltered$Right - flyTracesFiltered$Left) * (flyTracesFiltered$Right - flyTracesFiltered$Left)^2, type = "l", main = "Difference in Wingstroke Amplitude", xlab = "Time (sec)", ylab = "(Wingstroke Angle)^2 Difference")

	png(file = "TraceSquare.png", width = 1920)
	plot(x = flyTracesFiltered$Time, y = exp(flyTracesFiltered$Right-flyTracesFiltered$Left), type = "l", main = "Difference in Wingstroke Amplitude", xlab = "Time (sec)", ylab = "exp(Wingstroke Angle Difference)")
	graphics.off()
}
