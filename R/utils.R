#' @title geomean
#' @description
#' Calculate the geometric mean
#' @param x values
#' @param na.rm logical defining removal of `NA` values
#' @return Geometric mean
#' @export
geomean <- function(x, na.rm = TRUE) {
  logX <- log(x[x > 0])
  exp(mean(logX, na.rm = na.rm))
}

#' @title geomeanMultipliedBySD
#' @description
#' Calculate the geometric mean * geometric SD
#' @param x values
#' @param na.rm logical defining removal of `NA` values
#' @return Geometric mean * geometric SD
#' @export
geomeanMultipliedBySD <- function(x, na.rm = TRUE) {
  logX <- log(x[x > 0])
  exp(mean(logX, na.rm = na.rm) + stats::sd(logX, na.rm = na.rm))
}

#' @title geomeanDividedBySD
#' @description
#' Calculate the geometric mean / geometric SD
#' @param x values
#' @param na.rm logical defining removal of `NA` values
#' @return Geometric mean / geometric SD
#' @export
geomeanDividedBySD <- function(x, na.rm = TRUE) {
  logX <- log(x[x > 0])
  exp(mean(logX, na.rm = na.rm) - stats::sd(logX, na.rm = na.rm))
}

#' @title calculateGeometricErrorRange
#' @param values Numeric values of the geometric mean
#' @param errorValues Numeric values of the geometric error
#' @return A named list, with `ymin` and `ymax`, of the range calculated from the geometric mean and errors.
#' @description
#' Calculate the range from the geometric mean and error.
#' @export
calculateGeometricErrorRange <- function(values, errorValues) {
  return(list(
    ymin = values / errorValues,
    ymax = values * errorValues
  ))
}

#' @title calculateArithmeticErrorRange
#' @param values Numeric values of the arithmetic mean
#' @param errorValues Numeric values of the arithmetic error
#' @return A named list, with `ymin` and `ymax`, of the range calculated from the arithmetic mean and errors.
#' @description
#' Calculate the range from the arithmetic mean and error.
#' @export
calculateArithmeticErrorRange <- function(values, errorValues) {
  return(list(
    ymin = values - errorValues,
    ymax = values + errorValues
  ))
}

#' @title trimFileName
#' @param path character string containing the name of the path or file to trim
#' @param extension character string containing the extension file
#' @param sep character string separating path elements. "/" is default value.
#' @return fileName character string of the trimmed filed name
#' @description
#' Trim path and extension of a file
#' @examples
#' \dontrun{
#' pathName <- "folder/subfolder/testFile.txt"
#' trimFileName(pathName, extension = "txt")
#' }
#' @export
trimFileName <- function(path, extension = NULL, sep = "/") {
  fileName <- sub(
    pattern = paste0("^.*[", sep, "]"),
    replacement = "",
    x = path
  )
  if (!is.null(extension)) {
    fileName <- sub(
      pattern = paste0("[.].*", extension),
      replacement = "",
      x = fileName
    )
  }
  return(fileName)
}

#' @title removeForbiddenLetters
#' @param text character string to be evaluated
#' @param forbiddenLetters characters to be removed if in the `text`.
#' Default value of `forbiddenLetters` is `"[[:punct:]]"`
#' meaning that all punctuation characters are forbidden.
#' @param replacement character replacing the `forbiddenLetters`.
#' Default value of `forbiddenLetters` is "_".
#' @return `text` character string with forbidden letters replaced
#' @description
#' Trim path and extension of a file
#' @examples
#' \dontrun{
#' removeForbiddenLetters(text)
#' }
#' @export
removeForbiddenLetters <- function(text, forbiddenLetters = "[[:punct:][:blank:]]", replacement = "_") {
  # Remove accents from characters
  text <- iconv(x = text, to = "ASCII//TRANSLIT")
  gsub(
    pattern = forbiddenLetters,
    replacement = replacement,
    x = text
  )
}

#' @title generateResultFileNames
#' @return A list of filenames to be output by each core
#' @param numberOfCores to be used in parallel computation
#' @param folderName where result files will be saved
#' @param fileName prefix of result file names
#' @param separator used between file name prefix and index
#' @param extension for result file type.  default is CSV
#' @description
#' #Generate a list containing names of CSV result files that will be output by each core in parallel computation
#' @export
generateResultFileNames <- function(numberOfCores, folderName, fileName, separator = "-", extension = ".csv") {
  allResultsFileNames <- sapply(
    X = 1:numberOfCores, function(x, folderName, fileName) {
      return(file.path(folderName, paste0(fileName, separator, x, extension)))
    },
    folderName = folderName,
    fileName = fileName,
    USE.NAMES = FALSE
  )
  return(allResultsFileNames)
}

#' @title lastPathElement
#' @param path simulation path
#' @return last path element as character string
#' @export
#' @import ospsuite
#' @import utils
lastPathElement <- function(path) {
  pathArray <- ospsuite::toPathArray(path)
  lastElement <- utils::tail(pathArray, 1)

  return(lastElement)
}

#' @title replaceInfWithNA
#' @param data numeric vector
#' @return numeric vector
#' @keywords internal
replaceInfWithNA <- function(data) {
  infData <- is.infinite(data)
  Ninf <- sum(infData)
  if (Ninf > 0) {
    logDebug(paste0(Ninf, " values were infinite and transformed into missing values (NA)"))
  }
  data[infData] <- NA
  return(data)
}

#' @title removeMissingValues
#' @param data data.frame
#' @param dataMapping name of variable on which the missing values are checked
#' @return filtered data.frame
#' @keywords internal
removeMissingValues <- function(data, dataMapping = NULL) {
  if (isEmpty(data)) {
    return(data)
  }
  data[, dataMapping] <- replaceInfWithNA(data[, dataMapping])
  naData <- is.na(data[, dataMapping])
  Nna <- sum(naData)
  data <- data[!naData, ]

  if (Nna > 0) {
    logDebug(paste0(Nna, " values were missing (NA) from variable '", dataMapping, "' and removed from the analysis"))
  }
  return(data)
}

#' @title removeNegativeValues
#' @param data data.frame
#' @param dataMapping name of variable on which the missing values are checked
#' @return filtered data.frame
#' @keywords internal
removeNegativeValues <- function(data, dataMapping = NULL) {
  if (isEmpty(data)) {
    return(data)
  }
  negativeData <- data[, dataMapping] <= 0
  Nnegative <- sum(negativeData, na.rm = TRUE)
  data <- data[!negativeData, ]

  if (Nnegative > 0) {
    logDebug(paste0(Nnegative, " values from variable '", dataMapping, "' were negative and removed from the analysis"))
  }
  return(data)
}

#' @title newOutputColor
#' @description
#' Find a color for new `Output` objects
#' @return A color from OSP Suite Color Map
#' @keywords internal
newOutputColor <- function() {
  outputNames <- getObjectNamesInGlobalEnv("Output")
  usedColors <- sapply(
    outputNames,
    function(outputName) {
      output <- get(outputName)
      return(output$color)
    }
  )
  # OSP Suite color map includes 50 unique colors used here
  remainingColors <- setdiff(tlf::ColorMaps$ospDefault, usedColors)
  if (!isEmpty(remainingColors)) {
    return(head(remainingColors, 1))
  }
  # If the colors were already used, in previous Outputs,
  # Use new round of osp suite colors
  colorIndex <- 1 + (length(outputNames) %% length(tlf::ColorMaps$ospDefault))
  return(tlf::ColorMaps$ospDefault[colorIndex])
}



#' @title getAllowedCores
#'
#' @description Get allowed number of CPU cores for computation
#'
#' @return Allowed number of CPU cores for computation
#' @keywords internal
getAllowedCores <- function() {
  return(
    getAllowedCoresLinuxKubernetes() %||%
      getOSPSuiteSetting(settingName = "numberOfCores")
  )
}


#' @title getAllowedCoresLinuxKubernetes
#'
#' @description
#' Relevant only when reporting engine is executed on a Linux Kubernetes cluster.
#'
#' @return Allowed number of CPU cores for computation
#' @keywords internal
getAllowedCoresLinuxKubernetes <- function() {
  cores <- tryCatch(
    {
      # get cpu allowance from files
      cfs_quota_us <- as.numeric(system("cat /sys/fs/cgroup/cpu/cpu.cfs_quota_us", intern = TRUE))
      cfs_period_us <- as.numeric(system("cat /sys/fs/cgroup/cpu/cpu.cfs_period_us", intern = TRUE))
      cores <- floor(cfs_quota_us / cfs_period_us)
      if (cores < 1) {
        return(NULL)
      }
      return(cores)
    },
    error = function(cond) {
      return(NULL)
    },
    warning = function(cond) {
      return(NULL)
    }
  )
  return(cores)
}

#' @title parseVariableToObject
#' @description Create an expression of type `objectName$variableName <- variableName`
#' @param objectName Name of the object whose field is updated
#' @param variableName Name of the variable and field of `objectName`
#' @param keepIfNull logical `objectName$variableName <- variableName \%||\% objectName$variableName`
#' @return An expression to `eval()`
#' @importFrom ospsuite.utils %||%
#' @keywords internal
parseVariableToObject <- function(objectName, variableName, keepIfNull = FALSE) {
  if (keepIfNull) {
    return(parse(text = paste0(objectName, "$", variableName, " <- ", variableName, " %||% ", objectName, "$", variableName)))
  }
  return(parse(text = paste0(objectName, "$", variableName, " <- ", variableName)))
}

#' @title parseVariableFromObject
#' @description Create an expression of type `variableName <- objectName$variableName`
#' @param objectName Name of the object whose field is updated
#' @param variableName Name of the variable and field of `objectName`
#' @param keepIfNull logical `variableName <- objectName$variableName \%||\% variableName`
#' @return An expression to `eval()`
#' @keywords internal
parseVariableFromObject <- function(objectName, variableName, keepIfNull = FALSE) {
  if (keepIfNull) {
    return(parse(text = paste0(variableName, " <- ", objectName, "$", variableName)))
  }
  return(parse(text = paste0(variableName, " <- ", objectName, "$", variableName)))
}

#' @title calculateGMFE
#' @description Calculate Geometric Mean Fold Error between `x` and `y`.
#' Strictly positive pairs of values are kept in the calculation
#' @param x x values to compare
#' @param y y values to compare
#' @return GMFE
#' @export
calculateGMFE <- function(x, y) {
  positiveValues <- (y > 0 & x > 0)
  log10Error <- log10(y[positiveValues]) - log10(x[positiveValues])
  return(10^(sum(abs(log10Error)) / length(log10Error)))
}


#' @title getObjectNameAsString
#' @description Return the name of an object as a string
#' @param object the name of which is to be returned
#' @return the name of the `object` as a string
#' @keywords internal
getObjectNameAsString <- function(object) {
  return(deparse(substitute(object)))
}

#' @title saveFigure
#' @description Save figure and catches
#' @param plotObject A `ggplot` object
#' @param fileName Name of the file in which `plotObject` is saved
#' @param simulationSetName Name of the simulation set for `PlotTask` results
#' @keywords internal
saveFigure <- function(plotObject, fileName, simulationSetName = NULL) {
  tryCatch(
    {
      ggplot2::ggsave(
        filename = fileName,
        plot = plotObject,
        width = reEnv$defaultPlotFormat$width,
        height = reEnv$defaultPlotFormat$height,
        dpi = reEnv$defaultPlotFormat$dpi,
        units = reEnv$defaultPlotFormat$units
      )
    },
    error = function(e) {
      stop(messages$ggsaveError(fileName, simulationSetName, e))
    }
  )
  return(invisible())
}

#' @title getObjectNamesInGlobalEnv
#' @description Get object names of certain type/class in the Global Environment
#' @param objectType Object type or class
#' @return Array of Number of `Output` objects in the Global Environment
#' @keywords internal
getObjectNamesInGlobalEnv <- function(objectType) {
  objectNames <- ls(envir = .GlobalEnv)
  if (isEmpty(objectNames)) {
    return(NULL)
  }
  objectNames[sapply(
    objectNames,
    function(objectName) {
      isOfType(get(objectName, envir = .GlobalEnv), objectType)
    }
  )]
}
