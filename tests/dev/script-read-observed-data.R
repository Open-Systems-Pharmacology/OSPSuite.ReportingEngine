library(ospsuite.reportingengine)

# Read Observed Data from Example 01 - MeanModel Raltegravir
observedData <- readObservedDataFile("01_MeanModel_Raltegravir/Raltegravir_PK.csv")
head(observedData)

# Read Dictionary from Example 01 - MeanModel Raltegravir
dictionary <- readObservedDataFile("01_MeanModel_Raltegravir/tpDictionary.csv")
dictionary

# data filters
dataFilters <- c(
  'Grouping %in% "10mg_"',
  'Grouping %in% "50mg"'
)

# Function called internally by wokflow
rowFilter10mg <- evalDataFilter(observedData, parse(text = dataFilters[1]))
# rowFilter50mg <- evalDataFilter(observedData, parse(text = dataFilters[2]))

observedData[rowFilter10mg, ]
# observedData[rowFilter50mg,]
