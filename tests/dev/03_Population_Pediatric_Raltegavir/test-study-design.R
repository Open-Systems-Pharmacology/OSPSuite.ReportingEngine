library(ospsuite.reportingengine)

myPopulation <- loadPopulation("Larson 2013 8-18y meal-Population.csv")

# Update Population object with addStudyParameters and the study design file
addStudyParameters(population = myPopulation, studyDesignFile = "StudyDesign.csv")

populationData <- populationAsDataFrame(myPopulation) 

# Print the resulting population table
print(populationData[1:30, c("Organism|Weight", "Gender", "App1|Drugmass", "App2|Drugmass")])
