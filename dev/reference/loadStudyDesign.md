# loadStudyDesign

Load a StudyDesign object from a file containing a study design table In
this table, Line 1 is path, Line 2 is unit, Line 3 is type and
subsequent lines are values. The cells for type can include "SOURCE" OR
"TARGET". "SOURCE" type can also include subtype "MIN", "MAX" or
"EQUALS". In the current version, unit is expected to be intern units of
simulation.

## Usage

``` r
loadStudyDesign(studyDesignFile, population, simulation)
```

## Arguments

- studyDesignFile:

  file name of study design table

- population:

  `Population` object

- simulation:

  `Simulation` object

## Value

A `StudyDesign` object\`
