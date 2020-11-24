myUserFunction <- function(workflow)
{
  resHistoConfiguration <- PlotConfiguration$new()
  
  resHistoConfiguration$background$outerBackground$fill <- "blue"
  resHistoConfiguration$background$innerBackground$fill <- "lightblue"

  plotConfigurations <- list(resHisto = resHistoConfiguration)
  mySettings <- list(plotConfigurations = plotConfigurations)
  
  workflow$plotTimeProfilesAndResiduals$settings <- mySettings
}