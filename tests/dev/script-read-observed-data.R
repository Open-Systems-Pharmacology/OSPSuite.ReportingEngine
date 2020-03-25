library(ospsuite.reportingengine)


# Read Observed Data from Example 01 - MeanModel Raltegravir
observedData <- readObservedDataFile('01_MeanModel_Raltegravir/Raltegravir_PK.csv')
head(observedData)

# Read Dictionary from Example 01 - MeanModel Raltegravir
dictionary <- readObservedDataFile('01_MeanModel_Raltegravir/tpDictionary.csv')
dictionary