################ An R-script to analyse strokelitude data from a set of flies
#### up till now it only plots the data

## source the scripts with functions needed for analysis
source("StrokePrepFunctions.R")
source("StrokeFunctions.R")

## ask the user for the number of flies to be analysed and for the binsize for averaging
print("Please enter the number of flies to be included in the analysis:")
nFlies <- scan(n=1)
print("Please enter the binsize for averaging:")
binsize <- scan(n=1)   # set the size of bins to be averaged

## create the list in which the data will be administrated
flyData <- vector("list", length = nFlies)
flyDataDown <- vector("list", length = nFlies)	# another list, for the downsampled data

## read the data
for(ind in 1:nFlies){				# iterate over flies
	flyTraces  <- flyDataImport()								# import the data
	flyData[[ind]] <- flyDataFilter(flyTraces, frequency = .12, order = 3)	# filter the data
	flyDataDown[[ind]] <- flyDataDownsample(flyData[[ind]],binsize)		# downsampling for all the data
}

# determine an absolute minimum, needed for the plot
absmin <- vector("numeric", length = nFlies)
for(ind in 1:nFlies){
	absmin[ind] <- min(flyData[[ind]]$Right - flyData[[ind]]$Left)
}
downmin <- vector("numeric", length = nFlies)
for(ind in 1:nFlies){
	downmin[ind] <- min(flyDataDown[[ind]]$Right - flyDataDown[[ind]]$Left)
}

## plot the traces
for(ind in 1:nFlies){

	# plot the actual trace	
	png(file = paste("Trace",ind,".png",sep=""), width = 1920)
	plot(x = flyData[[ind]]$Time, y = flyData[[ind]]$Right-flyData[[ind]]$Left, type = "l", main = "Difference in Wingstroke Amplitude", xlab = "Time (sec)", ylab = "Wingstroke Angle Difference") + segments(x0 = 0, y0 = absmin[ind], x1 = 30, y1 = absmin[ind], col = "blue", lwd = 5) + segments(x0 = 30, y0 = absmin[ind], x1 = 60, y1 = absmin[ind], col = "red", lwd = 5) + segments(x0 = 60, y0 = absmin[ind], x1 = 90, y1 = absmin[ind], col = "blue", lwd = 5) + segments(x0 = 90, y0 = absmin[ind], x1 = 120, y1 = absmin[ind], col = "green", lwd = 5) + segments(x0 = 120, y0 = absmin[ind], x1 = 150, y1 = absmin[ind], col = "blue", lwd = 5) + segments(x0 = 150, y0 = absmin[ind], x1 = 180, y1 = absmin[ind], col = "red", lwd = 5) + segments(x0 = 180, y0 = absmin[ind], x1 = 210, y1 = absmin[ind], col = "blue", lwd = 5) + segments(x0 = 210, y0 = absmin[ind], x1 = 240, y1 = absmin[ind], col = "green", lwd = 5) + segments(x0 = 240, y0 = absmin[ind], x1 = 270, y1 = absmin[ind], col = "blue", lwd = 5) + segments(x0 = 270, y0 = absmin[ind], x1 = 300, y1 = absmin[ind], col = "red", lwd = 5) + segments(x0 = 300, y0 = absmin[ind], x1 = 330, y1 = absmin[ind], col = "blue", lwd = 5) + segments(x0 = 330, y0 = absmin[ind], x1 = 360, y1 = absmin[ind], col = "green", lwd = 5) + segments(x0 = 360, y0 = absmin[ind], x1 = 390, y1 = absmin[ind], col = "blue", lwd = 5)+ segments(x0 = 390, y0 = absmin[ind], x1 = 420, y1 = absmin[ind], col = "red", lwd = 5) + segments(x0 = 420, y0 = absmin[ind], x1 = 450, y1 = absmin[ind], col = "blue", lwd = 5) + segments(x0 = 450, y0 = absmin[ind], x1 = 480, y1 = absmin[ind], col = "green", lwd = 5) + segments(x0 = 480, y0 = absmin[ind], x1 = 510, y1 = absmin[ind], col = "blue", lwd = 5)
  # The segments here illustrate the conditions for an optomotor task of the order brbl brbl brbl brbl b (b = baseline, r = right, l = left) and 30s per condition. For now, a variable extension to other protocols will not be implemented.

	# plot the downsampled trace
	png(file = paste("TraceDownsampled",ind,".png", sep=""), width = 1920)
	plot(x = flyDataDown[[ind]]$Time, y = (flyDataDown[[ind]]$Right-flyDataDown[[ind]]$Left), type = "l", main = "Difference in Wingstroke Amplitude, Downsampled", xlab = "Time (sec)", ylab = "Wingstroke Angle Difference") + segments(x0 = 0, y0 = downmin[ind], x1 = 30, y1 = downmin[ind], col = "blue", lwd = 5) + segments(x0 = 30, y0 = downmin[ind], x1 = 60, y1 = downmin[ind], col = "red", lwd = 5) + segments(x0 = 60, y0 = downmin[ind], x1 = 90, y1 = downmin[ind], col = "blue", lwd = 5) + segments(x0 = 90, y0 = downmin[ind], x1 = 120, y1 = downmin[ind], col = "green", lwd = 5) + segments(x0 = 120, y0 = downmin[ind], x1 = 150, y1 = downmin[ind], col = "blue", lwd = 5) + segments(x0 = 150, y0 = downmin[ind], x1 = 180, y1 = downmin[ind], col = "red", lwd = 5) + segments(x0 = 180, y0 = downmin[ind], x1 = 210, y1 = downmin[ind], col = "blue", lwd = 5) + segments(x0 = 210, y0 = downmin[ind], x1 = 240, y1 = downmin[ind], col = "green", lwd = 5) + segments(x0 = 240, y0 = downmin[ind], x1 = 270, y1 = downmin[ind], col = "blue", lwd = 5) + segments(x0 = 270, y0 = downmin[ind], x1 = 300, y1 = downmin[ind], col = "red", lwd = 5) + segments(x0 = 300, y0 = downmin[ind], x1 = 330, y1 = downmin[ind], col = "blue", lwd = 5) + segments(x0 = 330, y0 = downmin[ind], x1 = 360, y1 = downmin[ind], col = "green", lwd = 5) + segments(x0 = 360, y0 = downmin[ind], x1 = 390, y1 = downmin[ind], col = "blue", lwd = 5)+ segments(x0 = 390, y0 = downmin[ind], x1 = 420, y1 = downmin[ind], col = "red", lwd = 5) + segments(x0 = 420, y0 = downmin[ind], x1 = 450, y1 = downmin[ind], col = "blue", lwd = 5) + segments(x0 = 450, y0 = downmin[ind], x1 = 480, y1 = downmin[ind], col = "green", lwd = 5) + segments(x0 = 480, y0 = downmin[ind], x1 = 510, y1 = downmin[ind], col = "blue", lwd = 5)
	dev.off()
}

#### compute an averaged trace (downsampled)
# find the shortest trace
shortest <- 0
for(ind in 1:nFlies){
	if(ind == 1){
		shortest <- length(flyDataDown[[ind]]$Time)
	} else {
		if(length(flyDataDown[[ind]]$Time) < shortest){
			shortest <- length(flyDataDown[[ind]]$Time)
		}
	}
}
averageTrace <- vector("numeric", length = shortest)
# compute the average
for(ind in 1:nFlies){ # compute the sum over all flies for each timepoint
	averageTrace  <- averageTrace + (flyDataDown[[ind]]$Right[1:shortest] - flyDataDown[[ind]]$Left[1:shortest])
}
averageTrace <- averageTrace/nFlies	# divide through number of flies to get the average

avgmin  <- 0
for(ind in 1:nFlies){
	if(min(averageTrace) < avgmin){
		avgmin <- min(averageTrace)
	}
}

# plot the average Trace (the segments part adds a colored bar showing the conditions - HARDCODED)
png("AverageTrace.png", width = 1920)
plot(y = averageTrace, x = flyDataDown[[1]]$Time[1:shortest], type = "l", xlab = "Average Wingstroke Amplitude Difference", ylab = "Time", main = paste("Average Trace over", nFlies, "Flies")) + segments(x0 = 0, y0 = avgmin, x1 = 30, y1 = avgmin, col = "blue", lwd = 5) + segments(x0 = 30, y0 = avgmin, x1 = 60, y1 = avgmin, col = "red", lwd = 5) + segments(x0 = 60, y0 = avgmin, x1 = 90, y1 = avgmin, col = "blue", lwd = 5) + segments(x0 = 90, y0 = avgmin, x1 = 120, y1 = avgmin, col = "green", lwd = 5) + segments(x0 = 120, y0 = avgmin, x1 = 150, y1 = avgmin, col = "blue", lwd = 5) + segments(x0 = 150, y0 = avgmin, x1 = 180, y1 = avgmin, col = "red", lwd = 5) + segments(x0 = 180, y0 = avgmin, x1 = 210, y1 = avgmin, col = "blue", lwd = 5) + segments(x0 = 210, y0 = avgmin, x1 = 240, y1 = avgmin, col = "green", lwd = 5) + segments(x0 = 240, y0 = avgmin, x1 = 270, y1 = avgmin, col = "blue", lwd = 5) + segments(x0 = 270, y0 = avgmin, x1 = 300, y1 = avgmin, col = "red", lwd = 5) + segments(x0 = 300, y0 = avgmin, x1 = 330, y1 = avgmin, col = "blue", lwd = 5) + segments(x0 = 330, y0 = avgmin, x1 = 360, y1 = avgmin, col = "green", lwd = 5) + segments(x0 = 360, y0 = avgmin, x1 = 390, y1 = avgmin, col = "blue", lwd = 5)+ segments(x0 = 390, y0 = avgmin, x1 = 420, y1 = avgmin, col = "red", lwd = 5) + segments(x0 = 420, y0 = avgmin, x1 = 450, y1 = avgmin, col = "blue", lwd = 5) + segments(x0 = 450, y0 = avgmin, x1 = 480, y1 = avgmin, col = "green", lwd = 5) + segments(x0 = 480, y0 = avgmin, x1 = 510, y1 = avgmin, col = "blue", lwd = 5)

#### make HARDCODED vectors of condition labels for boxplots
condLabels1 <- as.factor(c(rep(c("baseline", "right", "baseline", "left", "baseline", "right", "baseline", "left", "baseline", "right", "baseline", "left", "baseline", "right", "baseline", "left", "baseline"), each = round(length(averageTrace)/17)), rep("baseline", times = abs(length(averageTrace)-round(length(averageTrace)/17)*17))))
condLabels2 <- as.factor(c(rep(c("1 baseline", "1 right", "2 baseline", "2 left", "3 baseline", "3 right", "4 baseline", "4 left", "5 baseline", "5 right", "6 baseline", "6 left", "7 baseline", "7 right", "8 baseline", "8 left", "9 baseline"), each = round(length(averageTrace)/17)), rep("9 baseline", times = abs(length(averageTrace)-round(length(averageTrace)/17)*17))))

## plot boxplots
png("Boxplot.png", width = 480)
boxplot(averageTrace ~ condLabels1)
png("BoxplotDetail.png", width = 1440)
boxplot(averageTrace ~ condLabels2)
dev.off()

########### TODO: add the actual analysis


