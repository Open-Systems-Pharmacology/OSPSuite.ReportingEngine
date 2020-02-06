# Example:


# #SINGLE CORE
# setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow/")
# devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")
# simfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"
# popfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/popData.csv"
# pwf <- PopulationWorkflow$new(simulationFile = simfile,
#                               populationFile = popfile)
# pwf$setPopulationSimulationSettings()
# pwf$setPopulationPKParameterSettings()
# res<-pwf$runWorkflow()
# print(pwf$populationSimulation$generatedResultFileNames)
# print(pwf$populationPKParameters$generatedResultFileNames)


# MULTIPLE CORE
setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow/")
devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")
simfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"
popfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/popData.csv"
pwf <- PopulationWorkflow$new(
  simulationFile = simfile,
  populationFile = popfile
)
pwf$setPopulationSimulationSettings(numberOfCores = 4)
pwf$setPopulationPKParameterSettings()
res <- pwf$runWorkflow()
print(pwf$populationSimulation$generatedResultFileNames)
print(pwf$populationPKParameters$generatedResultFileNames)
