# Dipol2Red
## Dipol-2 methods for data reduction
### Description
Implements a legacy data processing tool. Consumes observations on form of `(date, obs_mag)` and transforms it into polarization measurements.
The tool supports polarimeters with 22.5 deg steps, converting every 4 magnitude measuremetns into one polarization measurement.

### Intallation
```r
  devtools::install_github("Ilia-Kosenkov/Dipol2Red")
```

### Usage
`Sigma_2` handles the *2-sgima* reduction procedure that filters out outliers. 
`Sigma_2` supports `tidy` evaluation of `date` and `obs` paramters and requries `bandInfo` `tibble` that contains the apropriate callibrations.
Default calibrations can be accessed through `BandInfo` data (`data(BandInfo)`) and should be updated if newer calibrations are available.
It is possible to obtain preliminary estimates without proper calibrations (as instrument's accuracy allows it), but for the proper final result calibrations are required.

For the sake of legacy compatibility, `ReadLegacyDescriptor` producer a decriptor of individual input files and `LoadFromLegacyDescriptor` uses the output of the previus function to colelct data from `*.csv` files.
Alternatively, data can be loaded into a table manually and date/measurement column names can be controlled through `date` and `obs_mag` parameters of `Sigma_2`.
