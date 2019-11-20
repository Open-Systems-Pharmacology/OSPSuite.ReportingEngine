#' @title ConfigurationPlan
#' @docType class
#' @description  Abstract class for ConfigurationPlan
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

      if (dir.exists(reOutputPath)) {
        warning(paste("Reporting Engine output path:", reOutputPath, "already exists"),
          call. = FALSE
        )
        if (overwriteREOutputPath) {
          warning("Overwrite option is TRUE, files within path were deleted",
            call. = FALSE
          )
          unlink(reOutputPath, recursive = TRUE)
        }
      }
      # Warning already accounted in previous loop
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


createRESections <- function(sectionsInput,
                             reInputPath,
                             reOutputPath) {
  sectionsOutput <- data.frame(
    id = NULL,
    title = NULL,
    content = NULL,
    path = NULL
  )

  for (sectionIndex in seq(1, length(sectionsInput$Id))) {
    # Create folder and add its path in the structure
    path <- file.path(
      reOutputPath,
      sprintf(
        "%0.3d_%s",
        sectionsInput$Id[sectionIndex],
        sectionsInput$Title[sectionIndex]
      )
    )

    dir.create(path)

    # Create title markdown within the folder
    knitr::knit(
      text = paste(sectionsInput$Title[sectionIndex], "\n"),
      output = file.path(path, "_title.md")
    )

    # Copy Content
    file.copy(
      file.path(reInputPath, sectionsInput$Content[sectionIndex]),
      file.path(path, "_content.md")
    )

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
