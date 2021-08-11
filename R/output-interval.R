#' @title OutputInterval
#' @description R6 class for specifying output intervals in a simulation
#' @field startTime is the starting time of the output interval
#' @field endTime is the end time of the output interval
#' @field resolution is the resolution of the output interval
#' @export
OutputInterval <- R6::R6Class(
  "OutputInterval",
  public = list(
    #' @description
    #' Create a new `OutputInterval` object.
    #' @param startTime is the starting time of the output interval
    #' @param endTime is the end time of the output interval
    #' @param resolution is the resolution of the output interval
    #' @return A new `OutputInterval` object
    initialize = function(startTime,
                          endTime,
                          resolution) {
      validateIsNumeric(object = startTime)
      validateIsNumeric(object = endTime)
      validateIsNumeric(object = resolution)
      validateIsNonNegative(object = startTime)

      validateIsPositive(object = endTime - startTime)
      validateIsPositive(object = resolution)

      private$.startTime <- startTime
      private$.endTime <- endTime
      private$.resolution <- resolution
    },

    addOutputIntervalToSimulation = function(simulation, intervalName) {
      ospsuite::addOutputInterval(
        simulation = simulation,
        startTime = private$.startTime,
        endTime = private$.endTime,
        resolution = private$.resolution,
        intervalName = intervalName
      )
    }
  ),
  active = list(
    startTime = function(value) {
      if (missing(value)) {
        return(private$.startTime)
      } else {
        validateIsNumeric(object = value)
        validateIsNonNegative(object = value)
        validateIsPositive(object = private$.endTime - value)
        private$.startTime <- value
      }
    },

    endTime = function(value) {
      if (missing(value)) {
        return(private$.endTime)
      } else {
        validateIsNumeric(object = value)
        validateIsPositive(object = value - private$.startTime)
        private$.endTime <- value
      }
    },

    resolution = function(value) {
      if (missing(value)) {
        return(private$.resolution)
      } else {
        validateIsNumeric(object = value)
        validateIsPositive(object = value)
        private$.resolution <- value
      }
    }
  ),
  private = list(
    .startTime = NULL,
    .endTime = NULL,
    .resolution = NULL
  )
)
