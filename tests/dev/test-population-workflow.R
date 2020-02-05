#Example:


#SINGLE CORE
setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow/")
devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")
simfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"
popfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/popData.csv"
pwf <- PopulationWorkflow$new(simulationFile = simfile,
                              populationFile = popfile,
                              numberOfCores = 1)

res<-pwf$runWorkflow()



#MULTIPLE CORE
setwd("C:/Users/ahamadeh/Dropbox/rproject/workflow/")
devtools::load_all("C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine")
simfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"
popfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/popData.csv"
pwf <- PopulationWorkflow$new(simulationFile = simfile,
                              populationFile = popfile,
                              numberOfCores = 3)
res<-pwf$runWorkflow()
