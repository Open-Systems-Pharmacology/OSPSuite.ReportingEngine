library(ospsuite.reportingengine)

myPopulation <- loadPopulation("Larson 2013 8-18y meal-Population.csv")
mySimulation <- loadSimulation("PKML/Larson 2013 8-18y meal.pkml")

# Update Population object with addStudyParameters and the study design file
addStudyParameters(population = myPopulation, simulation = mySimulation, studyDesignFile = "StudyDesign.csv")

populationData <- populationAsDataFrame(myPopulation)
# Print the resulting population table
pathForDrugMass <- "Applications|Larson 400mg|filmcoated tablet (original Merck formulation)|Application_1|ProtocolSchemaItem|DrugMass"
print(populationData[1:30, c("Organism|Weight", "Gender", pathForDrugMass)])

# Study design objects can be loaded to check expressions and assigned values in their base units
myStudyDesign <- loadStudyDesign(studyDesignFile = "StudyDesign.csv", population = myPopulation, simulation = mySimulation)
myStudyDesign
