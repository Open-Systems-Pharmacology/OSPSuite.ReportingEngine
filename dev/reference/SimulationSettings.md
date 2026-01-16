# SimulationSettings

R6 class for Population Simulation Settings

## Active bindings

- `numberOfCores`:

  is the number of cores to use for simulation

- `showProgress`:

  is a logical field TRUE shows progress of simulation.

- `maxSimulationsPerCore`:

  Scale factor used in a parallel simulation. The product of this scale
  factor and the number of allowable cores (allowedCores) sets the
  maximum number of simulations that may be run on one core.

- `allowedCores`:

  is the number of cores assigned to the user session.

- `mcRepetitions`:

  is the number of repetitions when performing a Monte Carlo Simulation

- `mcRandomSeed`:

  is the Random Seed Number when performing a Monte Carlo Simulation
  which allows repeatability of the simulations

## Methods

### Public methods

- [`SimulationSettings$new()`](#method-SimulationSettings-new)

- [`SimulationSettings$clone()`](#method-SimulationSettings-clone)

------------------------------------------------------------------------

### Method `new()`

Create a `SimulationSettings` object

#### Usage

    SimulationSettings$new(
      numberOfCores = NULL,
      showProgress = TRUE,
      maxSimulationsPerCore = NULL
    )

#### Arguments

- `numberOfCores`:

  number of cores for parallel computation

- `showProgress`:

  logical indicating if simulation progress is printed on console

- `maxSimulationsPerCore`:

  Scale factor used in a parallel simulation. The product of this scale
  factor and the number of allowable cores (allowedCores) sets the
  maximum number of simulations that may be run on one core.

#### Returns

A new `SimulationSettings` object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SimulationSettings$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
