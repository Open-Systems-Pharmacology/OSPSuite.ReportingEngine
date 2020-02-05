#Example:
devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")
simfile <- "./data/simpleMobiEventSim.pkml"
popfile <- "./data/popData.csv"
pwf <- PopulationWorkflow$new(simulationFile = simfile,
                              populationFile = popfile,
                              numberOfCores = 3)
res<-pwf$runWorkflow()
