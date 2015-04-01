####################### A script to plot a whole strokelitude trace in variable windows

## if you did not do so yet, set the right working directory
## source the script with the functions needed for analysis
source("StrokePrepFunctions.R")

## ask the user how big the windows should be
print("Please enter the required window durations in seconds.")
winLength <- scan(n=1)

##### read the data with the corresponding function
flyTraces <- flyDataImport()
flyTracesFiltered <- flyDataFilter(flyTraces, frequency = .12, order = 3)	# frequency can be set from 0 to 1, where 1 is the nyquist frequency
bufferInd <- 0

for(ind in seq(winLength,max(flyTracesFiltered$Time),winLength)){ 	# iterate all timestamps in windows of winLength, starting at the end of the first window
	png(file = paste("Traces",bufferInd,"to",ind,".png", sep = ""), width = 1920)
	plot(x = flyTracesFiltered$Time[(bufferInd < flyTracesFiltered$Time) & (flyTracesFiltered$Time < ind)], y = flyTracesFiltered$Right[(bufferInd < flyTracesFiltered$Time) & (flyTracesFiltered$Time < ind)]-flyTracesFiltered$Left[(bufferInd < flyTracesFiltered$Time) & (flyTracesFiltered$Time < ind)], type = "l", main = paste("Difference in Wingstroke Amplitude ",bufferInd,"s to ",ind,"s", sep = ""), xlab = "Time (sec)", ylab = "Wingstroke Angle Difference", ylim = c(-30,30))
	dev.off()

	bufferInd <- ind		# save the current index in bufferInd (basically make the end of the current window the start of the next))
}

graphics.off()
