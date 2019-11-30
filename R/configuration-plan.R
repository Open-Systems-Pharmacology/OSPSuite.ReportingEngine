#' @title ConfigurationPlan
#' @docType class
#' @description  R6 class for ConfigurationPlan
#' @field reInputPath Input path to run the Configuration Plan
#' @field reOutputPath Output path where Configuration Plan results will be stored
#' @field simulationMapping R6 class mapping simulations to needed for plots
#' @field observedDataSets R6 class storing the observed data
#' @field plots R6 class defining the plots to be performed in Configuration Plan
#' @field inputs Inputs to be moved as is in Configuration Plan output
#' @field sections Description of output structure tree as a data.frame
#' @field intro Introduction to be moved as is in Configuration Plan output
#' @export
ConfigurationPlan <- R6::R6Class(
  "ConfigurationPlan",
  public = list(
    reInputPath = NULL,
    reOutputPath = NULL,
    simulationMapping = NULL,
    observedDataSets = NULL,
    plots = NULL,
    inputs = NULL,
    sections = NULL,
    intro = NULL,

    initialize = function(jsonFile,
                              reInputPath,
                              reOutputPath,
                              overwriteREOutputPath = TRUE) {
      print(paste(
        "Reporting Engine: Initilization of Configuration Plan from",
        jsonFile
      ))
      configurationPlan <- jsonlite::fromJSON(jsonFile)
      self$reInputPath <- reInputPath
      self$reOutputPath <- reOutputPath

      checkOverwriteExisitingPath(reOutputPath)

      # Warning already accounted for by previous check
      # No need to show warnings twice
      dir.create(reOutputPath, showWarnings = FALSE)

      self$simulationMapping <- configurationPlan$SimulationMapping
      self$observedDataSets <- configurationPlan$ObservedDataSets
      self$plots <- configurationPlan$Plots
      self$inputs <- configurationPlan$Inputs
      self$sections <- configurationPlan$Sections
      self$intro <- configurationPlan$Intro

      # Create Section Tree and linearize section variable
      self$sections <- createRESections(
        self$sections,
        self$reInputPath,
        self$reOutputPath
      )
    }
  )
)

# Function taken from Matlab RE
# Take Sections Tree input from .json, create folders
# linearize Sections as a data.frame
createRESections <- function(sectionsInput,
                             reInputPath,
                             reOutputPath) {
  # Initialize Section Output
  sectionsOutput <- data.frame(
    id = NULL,
    title = NULL,
    content = NULL,
    path = NULL
  )

  for (sectionIndex in seq(1, length(sectionsInput$Id))) {
    # Create folder with numeric and name to ensure correct order
    # TO DO: remove forbidden letters for folders
    path <- file.path(
      reOutputPath,
      sprintf(
        "%0.3d_%s",
        sectionsInput$Id[sectionIndex],
        sectionsInput$Title[sectionIndex]
      )
    )

    dir.create(path)

    # Create title markdown "_title.md" within the folder
    # This file has the actual section title
    knitr::knit(
      text = paste(sectionsInput$Title[sectionIndex], "\n"),
      output = file.path(path, "_title.md")
    )

    # Copy Content file indicated in sections with a consistent name "_content.md"
    # for markdown joiner to read
    file.copy(
      file.path(reInputPath, sectionsInput$Content[sectionIndex]),
      file.path(path, "_content.md")
    )

    # Fill the linearized Sections to be output
    sectionsOutput[
      length(sectionsOutput$id) + 1,
      c("id", "title", "content", "path")
    ] <- c(
      sectionsInput[
        sectionIndex,
        c("Id", "Title", "Content")
      ],
      path
    )

    # Check for sub-Sections
    if (!is.null(sectionsInput$Sections[[sectionIndex]])) {
      subSectionsOutput <- createRESections(sectionsInput$Sections[[sectionIndex]],
        reInputPath,
        reOutputPath = path
      )
      sectionsOutput <- rbind.data.frame(sectionsOutput, subSectionsOutput)
    }
  }
  return(sectionsOutput)
}
