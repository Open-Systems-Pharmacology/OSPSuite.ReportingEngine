# Example:


#SINGLE CORE
setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow/")
devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")
simfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"
popfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/popData.csv"
pwf <- PopulationWorkflow$new(simulationFile = simfile,
                              populationFile = popfile)
pwf$setPopulationSimulationSettings()
res<-pwf$runWorkflow()
print(pwf$populationSimulation$generatedResultFileNames)


# # MULTIPLE CORE
# setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow/")
# devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")
# simfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"
# popfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/popData.csv"
# pwf <- PopulationWorkflow$new(
#   simulationFile = simfile,
#   populationFile = popfile
# )
# pwf$setPopulationSimulationSettings(numberOfCores = 2)
# res <- pwf$runWorkflow()
# print(pwf$populationSimulation$generatedResultFileNames)
