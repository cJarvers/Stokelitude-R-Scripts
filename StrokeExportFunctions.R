################### A set of funtions to export processed strokelitude data

##### export the whole dataframe containing strokelitude data
strokeToTXT <- function(strokeData, filename){
	write.table(strokeData, file = filename, row.names=FALSE)
}

##### export only the trace and timestamps
traceToTXT <- function(strokeData, filename){

	if(is.null(strokeData$Trace)){	# check whether the data already contain a trace
		# if not, compute the trace and save it in a dataframe with timestamps to export
		exportFrame <- data.frame("Time" = strokeData$Time, "Trace" = strokeData$Right-strokeData$Left)
		write.table(exportFrame, file = filename, row.names = FALSE)
	}else{
		# if there is a trace already, just export the relevant subset of the dataframe
		write.table(subset(strokeData, select = c("Time", "Trace")), file = filename, row.names = FALSE)
	}
}

##### export only the trace, without timestamps
# the problem here is that the timestamps would usually tell you where data is missing
# the argument interpol (default is TRUE) estimates the number of missing datapoints from the interval between timestamps and the average timestamp distance and inserts NAs
traceToCol <- function(strokeData, filename, interpol = TRUE){
	if(interpol){
		timeDiffs <- diff(strokeData$Time)		# get a vector containing all the time differences in the data
		gaps <- median(timeDiffs)*2 < timeDiffs	# estimate where there are gaps in the data (anything bigger than 2* the median of timestamp differences is considered a gap)
		gapInds <- which(gaps)				# get an array which contains all the indices of gaps in the data
		trace <- strokeData$Right-strokeData$Left	# compute the trace so that the NAs can be added in below

		for(ind in gapInds){	# iterate through all gaps
			gaplength  <- round(timeDiffs[ind]/median(timeDiffs))				# determine how long the gap is approximately (how many values are missing)
			trace <- append(trace, values = rep(NA, times = gaplength), after = ind)	# append the right number of NAs at the index (which iterates through gap indices, so its always in a gap)
		}

		#write the trace into a file
		write.table(trace, file = filename, row.names = FALSE, col.names = FALSE)
	} else { # if interpolation is not required
		if(is.null(strokeData$Trace)){	# check whether the data already contain a trace
			# if not, compute the trace and save it in a dataframe with timestamps to export
			write.table(strokeData$Right-strokeData$Left, file = filename, row.names = FALSE, col.names = FALSE)
		}else{
			# if there is a trace already, just export the relevant subset of the dataframe
			write.table(strokeData$Trace, file = filename, row.names = FALSE, col.names = FALSE)
		}
	}
}

