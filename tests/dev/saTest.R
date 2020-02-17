rm(list = ls())

library(ospsuite)

simFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/simpleMobiEventSim_nonzeroinitial.pkml"
# simFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/simpleMobiEventSim.pkml"
# simFilePath <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"

sim <- ospsuite::loadSimulation(simFilePath)

# allParams <- ospsuite::getAllParametersForSensitivityAnalysisMatching(paths = "**",simulation = sim)

allParams <- ospsuite::potentialVariableParameterPathsFor(sim)

# sa <- SensitivityAnalysis$new(simulation = sim)

sa <- SensitivityAnalysis$new(simulation = sim)
sa$addParameterPaths(allParams)

# sa <- SensitivityAnalysis$new(simulation = sim,
#                                parameters = allParams[[1]]$fullPath)

# sa <- SensitivityAnalysis$new(simulation = sim,
#                                parameters = allParams)

saro <- SensitivityAnalysisRunOptions$new(
  numberOfCoresToUse = 1,
  showProgress = TRUE
)

resa <- runSensitivityAnalysis(
  sensitivityAnalysis = sa,
  sensitivityAnalysisRunOptions = saro
)


exportSensitivityAnalysisResultsToCSV(results = resa, "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/resa.csv")

outputSelections <- sim$outputSelections

print(sim$outputSelections$allOutputs[[1]]$path)

print(resa$allPKParameterSensitivitiesFor(pkParameterName = "C_max", outputPath = sim$outputSelections$allOutputs[[1]]$path, totalSensitivityThreshold = 1))
print(resa$allPKParameterSensitivitiesFor(pkParameterName = "C_max", outputPath = sim$outputSelections$allOutputs[[2]]$path, totalSensitivityThreshold = 1))
print(resa$allPKParameterSensitivitiesFor(pkParameterName = "C_max", outputPath = sim$outputSelections$allOutputs[[3]]$path, totalSensitivityThreshold = 1))
