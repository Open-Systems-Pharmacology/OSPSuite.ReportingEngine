test_that("Timer works as expected", {
  t0 <- ospsuite.reportingengine:::tic()
  Sys.sleep(4)
  t1 <- ospsuite.reportingengine:::getElapsedTime(t0)
  expect_equal(t1, "0.1 min")
})

unlink("test-logs", recursive = TRUE)
test_that("resetLogs reset log messages and set the log folder", {
  resetLogs("test-logs")
  expect_equal(
    ospsuite.reportingengine:::reEnv$log$folder, 
    "test-logs"
    )
  # Because messages is empty list, cannot use expect_null
  expect_length(
    ospsuite.reportingengine:::reEnv$log$messages, 
    0
  )
})

test_that("log functions record to the appropriate files", {
  logInfo("info message")
  expect_true(file.exists("test-logs/log-info.txt"))
  logError("error message")
  expect_true(file.exists("test-logs/log-error.txt"))
  logDebug("debug message")
  expect_true(file.exists("test-logs/log-debug.txt"))
})


test_that("Every warning and error message in logCatch is logged", {
  # Actually catch, display warning as message, keep recording in same logs
  unlink("test-logs/log-error.txt", recursive = TRUE)
  logCatch({warning("warning message")})
  expect_true(file.exists("test-logs/log-error.txt"))
  expect_equal(
    ospsuite.reportingengine:::reEnv$log$folder, 
    "test-logs"
  )
  
  # Actually catch, display error as error and reset to prevent new messages in old logs
  unlink("test-logs/log-error.txt", recursive = TRUE)
  expect_error(logCatch({stop("error message")})) 
  expect_true(file.exists("test-logs/log-error.txt"))
  expect_null(ospsuite.reportingengine:::reEnv$log$folder)
})

resetLogs()
unlink("test-logs", recursive = TRUE)

