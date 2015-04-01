# Stokelitude-R-Scripts
A set of R scripts for the analysis of fly wingstroke data acquired with fview strokelitude.

Generally, scripts called "Stroke..." contain functions used by other scripts. Scripts called "Fly..." do the actual analysis.

StrokePrepFunctions.R contains functions for import and preprocessing of strokelitude data.
StrokeFunctions.R contains functions to sort data into conditions for the analysis of experimental paradigms.
StrokeExportFunctions.R (surprisingly) contains functions to export processed strokelitude data.

FlyDataPlotting.R reads data, preprocesses it and plots several graphics.
FlyData30sPlots.R reads data and plots the complete trace (right wingstroke amplitude minus left wingstroke amplitude) in windows of a specified size, e.g. 30 seconds.
FlyDataAnalysis.R reads, preprocesses and exports data. At this point it does not do any actual analysis, but that can be added.
FlyDataAnalysisSeveral.R reads, preprocesses and plots data from several flies. It also plots an average trace over all the flies. This can be useful if several flies were recorded using the same protocol, e.g. to measure optomotor responses.
