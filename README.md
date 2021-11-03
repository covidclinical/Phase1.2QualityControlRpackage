
# Phase1.2QualityControlRpackage.0

The goal of Phase1.2QCpackage is to perform Quality Control on data
generated for Phase
1.2.

## Installation

``` r
devtools::install_github("https://github.com/covidclinical/Phase1.2QualityControlRPackage", upgrade=FALSE)
## basic example code
```

## Running QC

``` r
### enter input path, output path and site name
dir.input = ""
dir.output = ""
site.nm=""

### read the data
dat.DailyCounts=read.csv(paste0(dir.input,"/DailyCounts-",site.nm,".csv"))
dat.ClinicalCourse=read.csv(paste0(dir.input,"/ClinicalCourse-",site.nm,".csv"))
dat.AgeSex=read.csv(paste0(dir.input,"/AgeSex-",site.nm,".csv"))
dat.DiagProcMed=read.csv(paste0(dir.input,"/DiagProcMed-",site.nm,".csv"))
dat.Labs=read.csv(paste0(dir.input,"/Labs-",site.nm,".csv"))
dat.LabCodes=read.csv(paste0(dir.input,"/LabCodes-",site.nm,".csv"))
dat.RaceByLocalCode=read.csv(paste0(dir.input,"/RaceByLocalCode-",site.nm,".csv"))
dat.RaceBy4CECode=read.csv(paste0(dir.input,"/RaceBy4CECode-",site.nm,".csv"))
dat.SiteRegister = read.csv(paste0(dir.input,"/site_register.csv"))

### run QC
file.nm1=file.path(dir.output, paste0("phase1.2.qc.report.", site.nm,".txt"))
runQC_Phase1.2_report(file.nm1, dat.DailyCounts, dat.ClinicalCourse, dat.AgeSex, dat.DiagProcMed,
                      dat.Labs, dat.RaceByLocalCode, dat.RaceBy4CECode, dat.LabCodes,
                      nm.report.file, icd.list, lab.range, site.nm)
```
