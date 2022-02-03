rm(list = ls())
library(ospsuite.reportingengine)

tlf::useTheme(tlf::loadThemeFromJson(system.file("extdata", "template-theme.json", package = "tlf")))

data <- readRDS(file = "tests/testthat/test-images/ddi-data.rds")

ddiData <- na.omit(data$ddiPlotDataframe)

ddiDataMapping <- tlf::DDIRatioDataMapping$new(
  x = data$axesSettings$X$label,
  y = data$axesSettings$Y$label,
  shape = "Caption",
  color = "Caption",
  minRange = c(0.1, 10),
  residualsVsObserved = FALSE
)

ddiPlotConfiguration <- tlf::DDIRatioPlotConfiguration$new(
  data = ddiData,
  dataMapping = ddiDataMapping
)

ddiPlotConfiguration$export$width <- 2 * (1.6 / 1.2) * (data$plotSettings$width / 96)
ddiPlotConfiguration$export$height <- 2 * 1.0 * (data$plotSettings$height / 96)
ddiPlotConfiguration$export$units <- "in"

# Set axis label font size
ddiPlotConfiguration$labels$xlabel$font$size <- 2 * data$plotSettings$axisFontSize
ddiPlotConfiguration$labels$ylabel$font$size <- 2 * data$plotSettings$axisFontSize

# Set axis tick font size
ddiPlotConfiguration$xAxis$font$size <- 2 * data$plotSettings$axisFontSize
ddiPlotConfiguration$yAxis$font$size <- 2 * data$plotSettings$axisFontSize

# Set watermark font size
ddiPlotConfiguration$background$watermark$font$size <- 2 * data$plotSettings$watermarkFontSize

# Set legend font size
ddiPlotConfiguration$legend$font$size <- 2 * data$plotSettings$legendFontSize

# Set line color and type
ddiPlotConfiguration$lines$color <- "black"
ddiPlotConfiguration$lines$linetype <- c("solid", "dotted", "solid")

# Set axes scaling
ddiPlotConfiguration$xAxis$scale <- tlf::Scaling$log
ddiPlotConfiguration$yAxis$scale <- tlf::Scaling$log

qualificationDDIPlot <- tlf::plotDDIRatio(
  data = ddiData,
  plotConfiguration = ddiPlotConfiguration,
  dataMapping = ddiDataMapping
)

qualificationDDIPlot <- qualificationDDIPlot + ggplot2::scale_color_manual(values = sapply(data$aestheticsList$color, function(x) {
  x
}))
qualificationDDIPlot <- qualificationDDIPlot + ggplot2::scale_shape_manual(values = sapply(data$aestheticsList$shape, function(x) {
  x
}))

# Force legend to be only one column to maintain plot panel width, and left-justify legend entries
qualificationDDIPlot <- qualificationDDIPlot + ggplot2::guides(col = ggplot2::guide_legend(ncol = 1, label.hjust = 0))

xlabel <- paste(data$axesSettings$X$label)
ylabel <- paste(data$axesSettings$Y$label)

qualificationDDIPlot <- qualificationDDIPlot + ggplot2::xlab(xlabel) + ggplot2::ylab(ylabel)

show(qualificationDDIPlot)
