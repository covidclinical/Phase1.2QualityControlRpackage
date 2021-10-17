sink.txt=function(x, file, method=print, append){sink(file, append=append); method(x); sink()}

runQC_Phase1.2_report=function(file.nm1, dat.DailyCounts, dat.ClinicalCourse, dat.AgeSex, dat.DiagProcMed,
                               dat.Labs, dat.RaceByLocalCode, dat.RaceBy4CECode, dat.LabCodes,
                               nm.report.file, icd.list, lab.range, site.nm){

  print(site.nm)
  race.list.all = c('asian','black','no_information','other','white','american_indian','hawaiian_pacific_islander')
  cohort.cat = c('PosAdm','U071Adm','NegAdm','PosNotAdm','U071NotAdm','NegNotAdm')
  quarter.cat = c('2020Q1','2020Q2','2020Q3','2020Q4','2021Q1','2021Q2')
  cohort.list.all = NULL
  for (c.cat in cohort.cat){
    for(q.cat in quarter.cat){
      cohort.list.all = c(cohort.list.all, paste0(c.cat,q.cat))
    }
  }
  cohort.all = Reduce(intersect, list(unique(dat.AgeSex$cohort),
                                      unique(dat.DailyCounts$cohort),
                                      unique(dat.ClinicalCourse$cohort),
                                      unique(dat.DiagProcMed$cohort),
                                      unique(dat.Labs$cohort),
                                      unique(dat.RaceByLocalCode$cohort),
                                      unique(dat.RaceBy4CECode$cohort)))

  sink.txt("\n\n       \n",file=file.nm1, cat, append=T)
  tryCatch(sink.txt("Phase1.2 QC Report\n", file=file.nm1, cat, append=F), error=function(e) NA)
  tryCatch(sink.txt(paste0(Sys.Date(),"\n\n"), file=file.nm1, cat, append=T), error=function(e) NA)
  sink.txt("\n\n______________________\n",file=file.nm1, cat, append=T)

  sink.txt("\n\nMissing cohorts:\n",file=file.nm1, cat, append=T)
  cohort.miss = setdiff(cohort.list.all, cohort.all)
  if (is.null(cohort.miss)){sink.txt("no cohort missing\n",file=file.nm1, cat, append=T)}else{
    sink.txt(cohort.miss, file=file.nm1, cat, append=T)
  }
  sink.txt("\n\n______________________\n",file=file.nm1, cat, append=T)

  for (cohort.nm in cohort.all){
    dat.DailyCounts.c=dat.DailyCounts%>%filter(cohort==as.character(cohort.nm))
    dat.ClinicalCourse.c=dat.ClinicalCourse%>%filter(cohort==as.character(cohort.nm))
    dat.AgeSex.c=dat.AgeSex%>%filter(cohort==as.character(cohort.nm))
    dat.DiagProcMed.c=dat.DiagProcMed%>%filter(cohort==as.character(cohort.nm))
    dat.Labs.c=dat.Labs%>%filter(cohort==as.character(cohort.nm))
    dat.RaceByLocalCode.c=dat.RaceByLocalCode%>%filter(cohort==as.character(cohort.nm))
    dat.RaceBy4CECode.c=dat.RaceBy4CECode%>%filter(cohort==as.character(cohort.nm))

    print(as.character(cohort.nm))

    tryCatch(sink.txt(paste0('Cohort : ', cohort.nm), file=file.nm1, cat, append=T), error=function(e) NA)

    qc.res=qc_site(dat.DailyCounts.c, dat.ClinicalCourse.c, dat.AgeSex.c, dat.DiagProcMed.c,
                   dat.Labs.c, dat.RaceByLocalCode.c, dat.RaceBy4CECode.c, dat.LabCodes,
                   nm.report.file, icd.list, lab.range, site.nm)


    colnames(qc.res$qc.grp$err.report)=
      colnames(qc.res$qc.col$err.report)=
      colnames(qc.res$qc.cros$err.report)=
      colnames(qc.res$qc.as$err.report)=
      colnames(qc.res$qc.cc$err.report)=
      colnames(qc.res$qc.dc$err.report)=
      colnames(qc.res$qc.dt$err.report)=
      colnames(qc.res$qc.dpm$err.report)=
      colnames(qc.res$qc.lab$err.report)=
      colnames(qc.res$qc.lab.val$err.report)=
      colnames(qc.res$qc.rc$err.report)=
      colnames(qc.res$qc.rc.mis$err.report)=
      c("SiteID", "Possible Issues")


    tryCatch(sink.txt("\n\nColumn Names:\n", file=file.nm1, cat, append=T), error=function(e) NA)
    if(dim(qc.res$qc.col$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.col$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nCrossover:\n", file=file.nm1, cat, append=T), error=function(e) NA)
    if(dim(qc.res$qc.cros$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.cros$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nAgeSex:\n", file=file.nm1, cat, append=T), error=function(e) NA)
    if(dim(qc.res$qc.as$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.as$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nClinicalCourse:\n", file=file.nm1, cat, append=T), error=function(e) NA)
    if(dim(qc.res$qc.cc$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.cc$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nDailyCount:\n", file=file.nm1, cat, append=T), error=function(e) NA)
    if(dim(qc.res$qc.dc$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.dc$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nDailyCountDate:\n", file=file.nm1, cat, append=T), error=function(e) NA)
    if(dim(qc.res$qc.dt$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.dt$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nDiagProcMed:\n", file=file.nm1, cat, append=T), error=function(e) NA)
    if(dim(qc.res$qc.dpm$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.dpm$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nLabs:\n", file=file.nm1, cat, append=T))
    if(dim(qc.res$qc.lab$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.lab$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nLabs missing:\n", file=file.nm1, cat, append=T))
    tryCatch(sink.txt(as.data.frame(qc.res$qc.lab.mis$err.report), file=file.nm1, print, append=T), error=function(e) NA)
    tryCatch(sink.txt("\n\nLab units:\n",file=file.nm1, cat, append=T))
    if(dim(qc.res$qc.lab.val$err.report)[1]!=0 & is.na(qc.res$qc.lab.val$err.report[1])!=1){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.lab.val$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nRace:\n",file=file.nm1, cat, append=T))
    if(dim(qc.res$qc.rc$err.report)[1]!=0){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.rc$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    tryCatch(sink.txt("\n\nMissing Race Category:\n",file=file.nm1, cat, append=T))
    if(dim(qc.res$qc.rc.mis$err.report)[1]!=0 ){
      tryCatch(sink.txt(as.data.frame(qc.res$qc.rc.mis$err.report), file=file.nm1, print, append=T), error=function(e) NA)}else{
        sink.txt("no issue identified\n", file=file.nm1, cat, append=T)
      }
    sink.txt("\n\n______________________ \n",file=file.nm1, cat, append=T)
  }

  sink.txt("\n\n______________________ END\n",file=file.nm1, cat, append=T)
  qc.res
}
