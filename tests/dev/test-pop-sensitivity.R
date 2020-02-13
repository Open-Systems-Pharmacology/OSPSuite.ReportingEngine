inputFolderName = "C:/Users/ahamadeh/Dropbox/rproject/workflow/"
simulationFileName = "C:/Users/ahamadeh/Dropbox/rproject/workflow/individualPksimSim.pkml"
populationFileName = "C:/Users/ahamadeh/Dropbox/rproject/workflow/popData.csv"
resultsFolderName = "C:/Users/ahamadeh/Dropbox/rproject/workflow/res20200212/"
resultsFileName = "popSensSim"
numberOfCores = 1

popSim <- ospsuite.reportingengine::SimulationTask$new(
  inputFolderName,
  simulationFileName,
  populationFileName,
  resultsFolderName,
  resultsFileName,
  numberOfCores)



popPK <- ospsuite.reportingengine::CalculatePKParametersTask$new(
  simulationFilePath = paste0(simulationFileName,simulationFileName,".pkml"),
  simulationResultFilePaths = paste0(resultsFolderName,resultsFileName,".csv"),
  NULL,
  NULL,
  "C:/Users/ahamadeh/Dropbox/rproject/workflow/res20200212/pkParamResults.csv")



ospsuite.reportingengine::simulateModel(
  simFilePath = file.path(popSim$inputFolderName, paste0(popSim$simulationFileName, ".pkml")),
  popDataFilePath = file.path(popSim$inputFolderName, paste0(popSim$populationFileName, ".csv")),
  resultsFilePath = file.path(popSim$resultsFolderName,popSim$resultsFileName,".csv"))


if (self$populationPKParameters$active) {
  if (self$populationPKParameters$validateInput()) {
    print("Starting PK parameter calculation")
    self$populationPKParameters$generatedResultFileNames <- calculatePKParameters(
      simulationFilePath = self$populationPKParameters$simulationFilePath,
      simulationResultFilePaths = self$populationSimulation$generatedResultFileNames,
      pkParameterResultsFilePath = self$populationPKParameters$pkParameterResultsFilePath
    )
  }
}

# TO DO: Abdullah, I don't know if this chunk have to be included somewhere so I left it as is
# self$numberOfCores,
# self$populationSimulation$input$population,
# self$inputFolder,
# self$population,
# simFileName,
# popFileName,
# wdir,
# inputFolder,
# pkParametersFolder,
# resultsFileName,

#
#             library("Rmpi")
#             mpi.spawn.Rslaves(nslaves = self$numberOfCores)
#
#             # Check that the correct number of slaves has been spawned.
#             if (!(mpi.comm.size() - 1 == self$numberOfCores)) { #-1 since mpi.comm.size() counts master
#               mpi.close.Rslaves()
#               stop(paste0(self$numberOfCores, " cores were not successfully spawned."))
#             }
#             mpi.bcast.cmd(library("ospsuite"))
#             mpi.bcast.cmd(library("ospsuite.reportingengine"))
#             tempPopDataFiles <- ospsuite::splitPopulationFile(
#               csvPopulationFile = self$populationSimulation$input$population,
#               numberOfCores = self$numberOfCores,
#               pkParametersFolder = paste0(inputFolder, "/"),
#               outputFileName = popFileName
#             )
#             mpi.bcast.Robj2slave(obj = simFileName)
#             mpi.bcast.Robj2slave(obj = popFileName)
#             mpi.bcast.Robj2slave(obj = tempPopDataFiles)
#             mpi.bcast.Robj2slave(obj = wdir)
#             mpi.bcast.Robj2slave(obj = inputFolder)
#             mpi.bcast.Robj2slave(obj = pkParametersFolder)
#
#             mpi.remote.exec(ospsuite.reportingengine::simulatePopulation(
#               simFileName = paste0(simFileName, ".pkml"),
#               simFileFolder = paste0(wdir, "/", inputFolder, "/"),
#               popDataFileName = paste0(popFileName, "_", mpi.comm.rank(), ".csv"),
#               popDataFileFolder = paste0(wdir, "/", inputFolder, "/"),
#               resultFileName = paste0(resultsFileName, "_", mpi.comm.rank(), ".csv"),
#               resultFileFolder = paste0(wdir, "/", pkParametersFolder, "/")
#             ))
#             mpi.close.Rslaves() # Move to end of workflow

# TO DO: plug plot tasks to actual results
if (self$plotDemography$active) {
  logInfo(message = self$plotDemography$message)
}
if (self$plotGoF$active) {
  logInfo(message = self$plotGoF$message)
}
if (self$plotPKParameters$active) {
  logInfo(message = self$plotPKParameters$message)
}
if (self$plotSensitivity$active) {
  logInfo(message = self$plotSensitivity$message)
}
},


# TO DO: include these chunk into the previous task
# if (self$pkParametersCalculation$active) {
#   # if (self$pkParametersCalculation$validateInput()){
#   # calculatePKParameters()
#   # }
# }
# if (self$sensitivityAnalysis$active) {
#   if (self$sensitivityAnalysis$validateInput()) {
#     simulation <- loadSimulation(self$demographyPlot$input$simulation)
#
#     pkSensitivities <- analyzeSensitivity(simulation = simulation)
#     save(pkSensitivities, file = self$sensitivityAnalysis$output$sensitivityAnalysis)
#   }
# }
# if (self$demographyPlot$active) {
#   if (self$demographyPlot$validateInput()) {
#     population <- loadPopulation(self$demographyPlot$input$population)
#     simulation <- loadSimulation(self$demographyPlot$input$simulation)
#
#     # The last properties of plotDemograpy will be set within task settings
#     demographyPlot <- plotDemography(
#       simulation = simulation,
#       population = population,
#       parameterNames = c(StandardPath$Age, StandardPath$Weight, StandardPath$Height),
#       plotConfiguration = NULL
#     )
#
#     save(demographyPlot, file = self$demographyPlot$output$demographyResults)
#     dir.create(self$demographyPlot$output$demographyPlot)
#     for (plotName in names(demographyPlot)) {
#       ggplot2::ggsave(
#         filename = file.path(self$demographyPlot$output$demographyPlot, paste0(removeForbiddenLetters(plotName), ".png")),
#         plot = demographyPlot[[plotName]]
#       )
#     }
#   }
# }
# if (self$gofPlot$active) {
#   if (self$gofPlot$validateInput()) {
#     load(file = self$gofPlot$input$populationSimulation)
#     population <- loadPopulation(self$gofPlot$input$population)
#     observedData <- self$gofPlot$input$observedData
#
#     gofPlot <- plotGoodnessOfFit(
#       populationSimulation = populationSimulation,
#       population = population,
#       observedData = observedData,
#       quantity = NULL,
#       plotConfiguration = NULL
#     )
#
#     save(gofPlot, file = self$gofPlot$output$gofResults)
#     dir.create(self$gofPlot$output$gofPlot)
#     for (plotName in names(gofPlot)) {
#       ggplot2::ggsave(
#         filename = file.path(self$gofPlot$output$gofPlot, paste0(removeForbiddenLetters(plotName), ".png")),
#         plot = gofPlot[[plotName]]
#       )
#     }
#   }
# }
# if (self$pkParametersPlot$active) {
#   self$pkParametersPlot$output <- plotPKParameters()
# }
# if (self$sensitivityPlot$active) {
#   if (self$sensitivityPlot$validateInput()) {
#     load(file = self$sensitivityPlot$input$sensitivityAnalysis)
#
#     sensitivityPlot <- plotSensitivity(sensitivityAnalysis)
#     save(sensitivityPlot, file = file.path(self$sensitivityPlot$output$sensitivityPlot))
#   }
# }

#' @description
#' Print workflow list of tasks
#' TO DO: add simulationSets to print() method
#' @return Task list information
print = function() {
  taskOrder <- list(
    "Task 1" = self$populationSimulation$print(),
    "Task 2" = self$pkParametersCalculation$print(),
    "Task 3" = self$sensitivityAnalysis$print(),
    "Task 4" = self$plotDemography$print(),
    "Task 5" = self$plotGoF$print(),
    "Task 6" = self$plotPKParameters$print(),
    "Task 7" = self$plotSensitivity$print()
  )
  invisible(self)

  return(taskOrder)
}
)
)
