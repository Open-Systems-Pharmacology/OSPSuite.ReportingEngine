context("Qualification Workflow")

reOutputFolder <- "report-qualification"
configurationPlanFile <- getTestDataFilePath("qualification/report-configuration-plan.json")

reOutputFolderReferenceFiles <- c(
  "images", "SimulationResults", "PKAnalysisResults",
  "log-debug.txt", "log-info.txt", "Report-word.md", "Report.docx", "Report.md", "testcontent.md"
  )

# Load and run test qualification workflow
workflow <- loadQualificationWorkflow(
  workflowFolder = reOutputFolder,
  configurationPlanFile = configurationPlanFile
)
workflow$runWorkflow()


test_that("Output folder has the appropriate structure", {
  expect_setequal(list.files(reOutputFolder), reOutputFolderReferenceFiles)
})

test_that("Content files are all appropriately copied", {
  for(fileName in list.files(getTestDataFilePath("qualification/Content"), recursive = TRUE)){
    expect_true(file.exists(file.path(workflow$workflowFolder, fileName)))
  }
})

test_that("Semicolon-separated data files with comma decimal points are read correctly", {
  dataFile <- getTestDataFilePath("qualification/ObservedData/test-data-semicolon-sep-dec-comma.csv")
  observedDataFrame <- readObservedDataFile(dataFile)
  expect_true(is.data.frame(observedDataFrame))
  expect_true("AUCR Avg" %in% names(observedDataFrame))
  expect_true(is.numeric(observedDataFrame$`AUCR Avg`))
})


test_that("Simulations and PK Analyses were saved", {
  expect_setequal(
    list.files(file.path(reOutputFolder, "SimulationResults")),
    c("Mini-MiniModel2-SimulationResults.csv", "PKRatio-Larson 2013 8-18y meal-SimulationResults.csv")
  )
  expect_setequal(
    list.files(file.path(reOutputFolder, "PKAnalysisResults")),
    c("Mini-MiniModel2-PKAnalysisResults.csv", "PKRatio-Larson 2013 8-18y meal-PKAnalysisResults.csv")
  )
})

reportContent <- readLines(workflow$reportFilePath)

test_that("Final report managed appropriately intro, TOC and content", {
  intro <- grep(pattern = "# Title Page", reportContent)
  toc <- grep(pattern = "# Table of Contents", reportContent)
  contentInput <- grep(pattern = "## 1.1 With Content", reportContent)
  noContentInput <- grep(pattern = "## 1.2 Without Content", reportContent)

  expect_length(intro, 1)
  expect_length(toc, 1)
  expect_length(contentInput, 1)
  expect_length(noContentInput, 1)
  expect_gt(toc, intro)
  expect_gt(contentInput, toc)
  expect_gt(noContentInput, contentInput)
})

test_that("Final report managed appropriately the order and numbering of sections", {
  section1 <- grep(pattern = "# 1 Test Content Input", reportContent)
  section11 <- grep(pattern = "## 1.1 With Content", reportContent)
  section12 <- grep(pattern = "## 1.2 Without Content", reportContent)
  section2 <- grep(pattern = "# 2 Time Profile Tests", reportContent)
  section21 <- grep(pattern = "## 2.1 Time Profile", reportContent)
  section22 <- grep(pattern = "## 2.2 Comparison Time Profile", reportContent)
  section23 <- grep(pattern = "## 2.3 Goodness of fit", reportContent)
  section3 <- grep(pattern = "# 3 PK Ratio Tests", reportContent)
  section4 <- grep(pattern = "# 4 DDI Ratio Tests", reportContent)
  
  expect_length(section1, 1)
  expect_length(section11, 1)
  expect_length(section12, 1)
  expect_length(section2, 1)
  expect_length(section21, 1)
  expect_length(section22, 1)
  expect_length(section23, 1)
  expect_length(section3, 1)
  expect_length(section4, 1)

  expect_gt(section11, section1)
  expect_gt(section12, section11)
  expect_gt(section2, section12)
  expect_gt(section21, section2)
  expect_gt(section22, section21)
  expect_gt(section23, section22)
  expect_gt(section3, section23)
  expect_gt(section4, section3)
  
  # Unreferenced section not in toc
  unreferencedSection <- grep(pattern = "## Unreferenced section", reportContent)
  expect_length(unreferencedSection, 1)
  tocContent <- reportContent[grep(pattern = "\\[", x = reportContent)]
  unreferencedSectionInTOC <- grep(pattern = "Unreferenced section", tocContent)
  expect_length(unreferencedSectionInTOC, 0)
  
})

test_that("Final report included the correct amount of figures and tables", {
  # Used digit to get only numered values (removing TOC from count)
  numberOfFigures <- grep(pattern = "Figure [[:digit:]]", reportContent)
  numberOfTables <- grep(pattern = "Table [[:digit:]]", reportContent)

  expect_length(numberOfFigures, 4)
  expect_length(numberOfTables, 4)
})

# Clear the output
unlink(reOutputFolder, recursive = TRUE)
