rm(list = ls())
library(ospsuite)
sfp <- "C:/Users/ahamadeh/Dropbox/GitHub/OSP/OSPSuite.ReportingEngine/data/simpleMobiEventSim.pkml"
sim <- loadSimulation(sfp)

parObjects1 <- getAllParametersMatching(paths = "**", container = sim)
pth1 <- parObjects1[[6]]$path
print(pth1)
par1 <- getParameter(path = pth1, container = sim)

setParameterValues(parameters = par1, values = 2)


parObjects2 <- getAllParametersMatching(paths = "**", container = sim)
pth2 <- parObjects1[[6]]$path
print(pth1)
par2 <- getParameter(path = pth1, container = sim)



print(par2)
