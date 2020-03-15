# rm(list=ls())
library(ospsuite)
library(ospsuite.reportingengine)

simfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/individualPksimSim.pkml"
# simfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/simpleMobiEventSim.pkml"
popfile <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/popData.csv"


sim1 <- loadSimulation(simfile, loadFromCache = FALSE)
sim2 <- loadSimulation(simfile, loadFromCache = FALSE)
pop <- loadPopulation(popfile)

parlist <- pop$getParameterValuesForIndividual(individualId = 4)
sapply(
  1:length(parlist$paths),
  function(n, sim, par) {
    setSimulationParameterValues(
      parameterPaths = par$paths[n],
      values = par$values[n],
      simulation = sim
    )
  },
  sim1,
  parlist
)

print(getAllParametersMatching(paths = parlist$paths[6], container = sim1))
print(getAllParametersMatching(paths = parlist$paths[6], container = sim2))

# LL<-getEnum(simfile)

# ospsuite::setSimulationParameterValues(LL$Neighborhoods$nh_blockA_blockB$mol1$mainTransport$k)

# pars <- ospsuite::getAllParametersForSensitivityAnalysisMatching("**",sim)

# ospsuite::setSimulationParameterValues(LL$smarties$`Solubility gain per charge`$path,values = 5000,simulation = sim)


# ospsuite::exportIndividualSimulations(population = loadPopulation(popfile),
#                                       simulation = sim,
#                                       individualIds = 4,outputFolder =  )
