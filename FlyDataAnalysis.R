################## An R-script to import, analyse and export strokelitude data

## source the script with the functions needed for analysis
source("StrokePrepFunctions.R")
source("StrokeExportFunctions.R")

############################ Import the Data #################################
##### read the data with the corresponding function
flyTraces <- flyDataImport()




############################ Preprocessing ###################################
##### Filter the Data
flyTracesFiltered <- flyDataFilter(flyTraces, frequency = .12, order = 3)

strokeToTXT(flyTraces, filename = "Unfiltered.txt")
strokeToTXT(flyTracesFiltered, filename = "Filtered.txt")

##### do the downsampling with the corresponding function
print("Please enter the binsize for averaging:")
binsize <- scan(n=1)   # set the size of bins to be averaged

flyTracesDown <- flyDataDownsample(flyTracesFiltered, binsize)

## do the sliding average with the corresponding function
flyTracesSlide <- flyDataSlidingAverage(flyTracesFiltered, binsize)




############################ Analyse the Data ################################






############################ Export the Data
# query the user how to export
print("Please enter the filename under which the data should be saved.")
fileName <- scan(n=1, what = "character")
print("How would you like to export the data? All data (1), Trace with Timestamps (2), Only Trace (3), Only Trace with Gaps (4)")
expPref <- scan(n=1)

# do the export
if(expPref == 2){
	traceToTXT(flyTracesFiltered, filename = fileName)
} else if(expPref == 3){
	traceToCol(flyTracesFiltered, filename = fileName, interpol = FALSE)
} else if(expPref == 4){
	traceToCol(flyTracesFiltered, filename = fileName)
} else {
	strokeToTXT(flyTracesFiltered, filename = fileName)
}
