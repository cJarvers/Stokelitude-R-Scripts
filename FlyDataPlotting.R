################## An R-script to import strokelitude data and to plot it in several ways

## source the script with the functions needed for analysis
source("StrokePrepFunctions.R")

##### read the data with the corresponding function
flyTraces <- flyDataImport()
flyTracesFiltered <- flyDataFilter(flyTraces, frequency = .12, order = 3) # these filtering parameters were chosen by trying out - no deep theoretical reasons behind them

## plot the traces and save them as .png files
png(file = "RightTrace.png", width = 1920) # direct the following output to the image RightTrace.png
plot(x = flyTracesFiltered$Time, y = flyTracesFiltered$Right, type = "l", main = "Trace of the Right Wing", xlab = "Time (sec)", ylab = "Right Anterior Wingstroke Angle")
png(file = "LeftTrace.png", width = 1920)
plot(x = flyTracesFiltered$Time, y = flyTracesFiltered$Left, type = "l", main = "Trace of the Left Wing", xlab = "Time (sec)", ylab = "Left Anterior Wingstroke Angle")
png(file = "Trace.png", width = 1920)
plot(x = flyTracesFiltered$Time, y = flyTracesFiltered$Right-flyTracesFiltered$Left, type = "l", main = "Difference in Wingstroke Amplitude", xlab = "Time (sec)", ylab = "Wingstroke Angle Difference")
graphics.off()



##### do the downsampling with the corresponding function
print("Please enter the binsize for averaging:")
binsize <- scan(n=1)   # set the size of bins to be averaged

flyTracesDown <- flyDataDownsample(flyTracesFiltered, binsize)

## do the sliding average with the corresponding function
flyTracesSlide <- flyDataSlidingAverage(flyTracesFiltered, binsize)


## plotting downsampled data
png(file = "TraceDownsampled.png", width = 1920)
plot(x = flyTracesDown$Time, y = (flyTracesDown$Right-flyTracesDown$Left), type = "l", main = "Difference in Wingstroke Amplitude, Downsampled", xlab = "Time (sec)", ylab = "Wingstroke Angle Difference")
png(file = "TraceSlidingAverage.png", width = 1920)
plot(x = flyTracesSlide$Time, y = (flyTracesSlide$Right - flyTracesSlide$Left), type = "l", main = "Difference in Wingstroke Amplitude, Sliding Average", xlab = "Time (sec)", ylab = "Wingstrole Angle Difference")



##### plot an excerpt of the data
print("Please enter starting second of the interval to be plotted:")
startSecond <- scan(n=1)
print("Please enter final second of the interval to be plotted:")
endSecond <- scan(n=1)
flyDataExcerptPlot(flyTracesFiltered, startSecond, endSecond, binsize)



graphics.off()

