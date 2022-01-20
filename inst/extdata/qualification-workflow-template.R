#' @title createQualificationReport
#' @description Run a qualification workflow to create a qualification report.
#' @param qualificationRunnerFolder Folder where QualificationRunner.exe is located
#' @param pkSimPortableFolder Folder where PK-Sim is located.
#' If not specified, installation path will be read from the registry (available only in case of full **non-portable** installation).
#' This option is **MANDATORY** for the portable version of PK-Sim.
#' @param createWordReport Logical defining if a `docx` version of the report should also be created.
#' Note that `pandoc` installation is required for this feature
#' [https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingEngine/wiki/Installing-pandoc]
#' @example
#' createQualificationReport("C:/Software/QualificationRunner9.1.1")
#'
createQualificationReport <- function(qualificationRunnerFolder,
                                      pkSimPortableFolder = NULL,
                                      createWordReport = TRUE) {

  #-------- STEP 1: Define workflow settings --------#
  #' replace `workingDirectory` and `qualificationPlanName` with your paths
  #'
  #' The directories are assumed the following structure
  #' `workingDirectory`
  #'   - `input`
  #'     - `qualificationPlanName`
  #'   - `re_input`
  #'     - `configurationPlanFile`
  #'   - `re_output`
  #'     - `report`
  #'
  #' In case your folder structure is different from assumed above,
  #' the paths below must be adjusted as well:
  #' - `qualificationPlanFile`: path of qualification plan file run by Qualification Runner
  #' - `reInputFolder`: input path for the Reporting engine
  #' - `reOutputFolder`: outputs of the Reporting Engine will be created here
  #' - `reportName`:  path of final report
  #'
  #' **Template parameters to be replaced below**

  #' `workingDirectory`: current directory is used as default working directory
  workingDirectory <- getwd()

  qualificationPlanName <- "qualification_plan.json"
  qualificationPlanFile <- file.path(workingDirectory, "input", qualificationPlanName)

  #' The default outputs of qualification runner should be generated under `<workingDirectory>/re_input`
  reInputFolder <- file.path(workingDirectory, "re_input")
  #' The default outputs or RE should be generated under `<workingDirectory>/re_output`
  reOutputFolder <- file.path(workingDirectory, "re_output")

  #' Configuration Plan created from the Qualification Plan by the Qualification Runner
  configurationPlanName <- "report-configuration-plan"
  configurationPlanFile <- file.path(reInputFolder, paste0(configurationPlanName, ".json"))

  #' Option to record the time require to run the workflow.
  #' The timer will calculate calculation time form internal `Sys.time` function
  recordWorkflowTime <- TRUE

  #' Set watermark that will appear in all generated plots
  #' Default is no watermark. `Label` objects from `tlf` package can be used to specifiy watermark font.
  watermark <- ""

  #' If not set, report created will be named `report.md` and located in  `reOutputFolder`
  reportName <- file.path(reOutputFolder, "report.md")
  
  #----- Optional parameters for the Qualification Runner -----#
  #' If not null, `logFile` is passed internally via the `-l` option
  logFile <- NULL
  #' If not null, `logLevel` is passed internally via the `--logLevel` option
  logLevel <- NULL
  #' If `overwrite` is set to true, eventual results from the previous run of the QualiRunner/RE will be removed first
  overwrite <- TRUE

  #-------- STEP 2: Qualification Runner  --------#
  #' Start timer to track time if option `recordWorkflowTime` is set to TRUE
  if (recordWorkflowTime) {
    tic <- as.numeric(Sys.time())
  }

  #' Start Qualification Runner to generate inputs for the reporting engine
  startQualificationRunner(
    qualificationRunnerFolder = qualificationRunnerFolder,
    qualificationPlanFile = qualificationPlanFile,
    outputFolder = reInputFolder,
    pkSimPortableFolder = pkSimPortableFolder,
    configurationPlanName = configurationPlanName,
    overwrite = overwrite,
    logFile = logFile,
    logLevel = logLevel
  )

  #' Print timer tracked time if option `recordWorkflowTime` is set to TRUE
  if (recordWorkflowTime) {
    toc <- as.numeric(Sys.time())
    print(paste0("Qualification Runner Duration: ", round((toc - tic) / 60, 1), " minutes"))
  }

  #-------- STEP 3: Run Qualification Workflow  --------#
  #' Load `QualificationWorkflow` object from configuration plan
  workflow <- loadQualificationWorkflow(
    workflowFolder = reOutputFolder,
    configurationPlanFile = configurationPlanFile
  )

  #' Set the name of the final report
  workflow$reportFileName <- reportName
  workflow$createWordReport <- createWordReport

  #' Set watermark. If set, it will appear in all generated plots
  workflow$setWatermark(watermark)

  #' Run the `QualificatitonWorklfow` tasklist of ConfigurationPlan
  workflow$runWorkflow()

  #' Print timer tracked time if option `recordWorkflowTime` is set to TRUE
  if (recordWorkflowTime) {
    toc <- as.numeric(Sys.time())
    print(paste0("Qualification Workflow Total Duration: ", round((toc - tic) / 60, 1), " minutes"))
  }
  return(invisible())
}
