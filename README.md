
# Phase1.2 Quality Control R package

The goal of Phase1.2QCpackage is to perform Quality Control on data
generated for Phase 1.2.

## Installation

``` r
devtools::install_github("https://github.com/covidclinical/Phase1.2QualityControlRPackage", upgrade=FALSE)
```

## Running QC

``` r
### enter input path, output path and site name
dir.input = ""   # path to the 1.2 data
dir.output = ""  # path to save the 1.2 QC report
site.nm = ""     # 4CE site ID


### run QC
runQC_Phase1.2_report(dir.input, dir.output, site.nm)
```
