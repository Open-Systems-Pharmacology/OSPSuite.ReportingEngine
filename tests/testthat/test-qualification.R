context("Qualification Workflow")

reOutputFolder <- "report-qualification"
configurationPlanFile <- getTestDataFilePath("qualification/report-configuration-plan.json") 

reOutputFolderReferenceFiles <- c(
  "images", "SimulationResults", "PKAnalysisResults", 
  "log-debug.txt", "log-info.txt", "Report-word.md", "Report.docx", "Report.md"
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

reportContent <- readLines(workflow$reportFileName)

test_that("Final report managed appropriately intro, TOC and content", {
  intro <- grep(pattern = "# Title Page", reportContent)
  toc <- grep(pattern = "# Table of Contents", reportContent)
  contentInput <- grep(pattern = "# 1 Test Content Input", reportContent)
  
  expect_length(intro, 1)
  expect_length(toc, 1)
  expect_length(contentInput, 1)
  expect_gt(toc, intro)
  expect_gt(contentInput, toc)
})

test_that("Final report managed appropriately the order of sections", {
  section2 <- grep(pattern = "# 2 Time Profile Tests", reportContent)
  section21 <- grep(pattern = "## 2.1 Time Profile", reportContent)
  section22 <- grep(pattern = "## 2.2 Comparison Time Profile", reportContent)
  section23 <- grep(pattern = "## 2.3 Goodness of fit", reportContent)
  section3 <- grep(pattern = "# 3 PK Ratio Tests", reportContent)
  section4 <- grep(pattern = "# 4 DDI Ratio Tests", reportContent)
  
  expect_length(section2, 1)
  expect_length(section21, 1)
  expect_length(section22, 1)
  expect_length(section23, 1)
  expect_length(section3, 1)
  expect_length(section4, 1)
  
  expect_gt(section21, section2)
  expect_gt(section22, section21)
  expect_gt(section23, section22)
  expect_gt(section3, section23)
  expect_gt(section4, section3)
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
