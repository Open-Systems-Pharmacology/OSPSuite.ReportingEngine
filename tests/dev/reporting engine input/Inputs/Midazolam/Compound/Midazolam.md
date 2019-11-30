# Compound: Midazolam

## Parameters

Name                                             | Value          | Value Origin                                      | Alternative  | Default |
------------------------------------------------ | -------------- | ------------------------------------------------- | ------------ | ------- |
Solubility at reference pH                       | 0.049 mg/ml    | Publication-FaSSIF (Heikkinen 2012)               | Measurement  | True    |
Reference pH                                     | 6.5            | Publication-FaSSIF (Heikkinen 2012)               | Measurement  | True    |
Lipophilicity                                    | 3.13 Log Units | Database-Unknown-Drugbank                         | Measurement  | True    |
Fraction unbound (plasma, reference value)       | 0.2            | Parameter Identification-Parameter Identification | Measurement  | True    |
Specific intestinal permeability (transcellular) | 2E-06 dm/min   | Parameter Identification-Parameter Identification | Optimization | True    |
Cl                                               | 1              |                                                   |              |         |
F                                                | 1              |                                                   |              |         |
Is small molecule                                | Yes            |                                                   |              |         |
Molecular weight                                 | 325.77 g/mol   |                                                   |              |         |
Plasma protein binding partner                   | Albumin        |                                                   |              |         |
Enable supersaturation                           | No             |                                                   |              |         |
## Calculation methods

Name                    | Value               |
----------------------- | ------------------- |
Partition coefficients  | Rodgers and Rowland |
Cellular permeabilities | PK-Sim Standard     |
## Processes

### Metabolizing Enzyme: CYP3A4-Optimization

Molecule: CYP3A4
#### Parameters

Name                 | Value        | Value Origin                                      |
-------------------- | ------------ | ------------------------------------------------- |
Enzyme concentration | 1 µmol/l     |                                                   |
Vmax                 | 0 µmol/l/min |                                                   |
Km                   | 2.73 µmol/l  |                                                   |
kcat                 | 13 1/min     | Parameter Identification-Parameter Identification |
### Systemic Process: Glomerular Filtration-GFR

Species: Human
#### Parameters

Name         | Value | Value Origin |
------------ | -----:| ------------: |
GFR fraction |     1 |              |
