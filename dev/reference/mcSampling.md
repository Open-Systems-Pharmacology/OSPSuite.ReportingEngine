# mcSampling

Perform repeatable Monte Carlo random sampling for a data.frame obtained
from a `PKAnalyses` object

## Usage

``` r
mcSampling(
  dataSize,
  sampleSize,
  n = getDefaultMCRepetitions(),
  seed = getDefaultMCRandomSeed()
)
```

## Arguments

- dataSize:

  Number of rows/unique individuals in `data`

- sampleSize:

  Number of sampled individuals in each Monte Carlo repetition

- n:

  Number of repetitions

- seed:

  Random Seed Number in order to get repeatable results

## Value

A list of `n` elements that include `sampleSize` integers sampled from
`1:dataSize`
