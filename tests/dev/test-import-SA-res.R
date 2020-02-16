rm(list=ls())
library(ospsuite)
library(ospsuite.reportingengine)
#setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow/Workflow-20200215-235159/Results/individualPksimSim-popData/SensitivityAnalysisResults")

#sf<-"C:/Users/ahamadeh/Dropbox/rproject/workflow/Workflow-20200215-235159/Results/individualPksimSim-popData/Inputs/individualPksimSim.pkml"
#sim <- loadSimulation(sf)
rfs<-c("C:/Users/ahamadeh/Dropbox/rproject/workflow/Workflow_20200215_235159/Results/individualPksimSim-popData/SensitivityAnalysisResults/individualPksimSim-popData-SensitivityAnalysisResults-IndividualId-27-1.csv",
       "C:/Users/ahamadeh/Dropbox/rproject/workflow/Workflow_20200215_235159/Results/individualPksimSim-popData/SensitivityAnalysisResults/individualPksimSim-popData-SensitivityAnalysisResults-IndividualId-27-2.csv",
       "C:/Users/ahamadeh/Dropbox/rproject/workflow/Workflow_20200215_235159/Results/individualPksimSim-popData/SensitivityAnalysisResults/individualPksimSim-popData-SensitivityAnalysisResults-IndividualId-27-3.csv",
       "C:/Users/ahamadeh/Dropbox/rproject/workflow/Workflow_20200215_235159/Results/individualPksimSim-popData/SensitivityAnalysisResults/individualPksimSim-popData-SensitivityAnalysisResults-IndividualId-27-4.csv",
       "C:/Users/ahamadeh/Dropbox/rproject/workflow/Workflow_20200215_235159/Results/individualPksimSim-popData/SensitivityAnalysisResults/individualPksimSim-popData-SensitivityAnalysisResults-IndividualId-27-5.csv",
       "C:/Users/ahamadeh/Dropbox/rproject/workflow/Workflow_20200215_235159/Results/individualPksimSim-popData/SensitivityAnalysisResults/individualPksimSim-popData-SensitivityAnalysisResults-IndividualId-27-6.csv")

file.remove(rfs)

#res <- importSensitivityAnalysisResultsFromCSV(simulation = sim,filePaths = rfs)
