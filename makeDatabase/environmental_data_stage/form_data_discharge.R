tribFile<-"~/process-data/data_store/original_data/tribDischarge.xlsx"
# wbFile<-"~/process-data/data_store/original_data/westbrookDischarge.xlsx"
wbFile<-"~/process-data/data_store/original_data/West Brook YSI Data with discharge calculated.xls"

tribs<-data.table(read_excel(tribFile))
tribs<-tribs[,list(river,date,discharge,stake)]

# wb<-data.table(read_excel(wbFile))
# setnames(wb,tolower(names(wb)))
# wb[,river:="wb"]
# wb<-wb[,list(river,date,discharge,stake)]

wb<-data.table(read_excel(wbFile,
                          col_types=c("numeric","numeric",
                                      "numeric","numeric","numeric",
                                      "numeric","numeric","numeric",
                                      "numeric","numeric","numeric",
                                      "numeric")))
setnames(wb,tolower(names(wb)))
wb<-wb[,list(date,time,discharge,depth)]
wb[,date:=as.POSIXct(date*60*60*24,origin="1899-12-30",tz="UTC")]
wb<-wb[,list(discharge=mean(discharge)),by=date]
wb[,river:="wb"]
wb[,stake:=NA]
wb<-wb[,list(river,date,discharge,stake)]

measuredDischarge<-rbind(tribs,wb)
measuredDischarge[river=='mitchell',discharge:=discharge+0.001]
measuredDischarge[,date:=as.Date(date)]



predictDischarge<-function(riverName){
  q<-measuredDischarge[river==riverName]
  setkey(q,date)
  
  depth<-dailyDischarge[river==riverName,list(date,logDepth)]
  
  data<-depth[q]
  
  if(riverName=='wb'){
    data[,logDepth:=log(exp(logDepth)+10)]
  }
  
  fit<-lm(log(discharge)~logDepth,data=data)
  plot(log(discharge)~logDepth,data=data)
  abline(fit)
  
  cat(riverName,"R^2 =",summary(fit)$r.squared,"\n")
  
  dailyDischarge[river==riverName,
                 discharge:=exp(predict(fit,data.frame(logDepth=logDepth)))]
}

for(r in unique(dailyDischarge$river)){
  predictDischarge(r)
}

dailyDischarge<-dailyDischarge[,list(date,river,discharge,q2)]

dbWriteTable(link$conn,"data_discharge",dailyDischarge,
             row.names=FALSE, overwrite=TRUE, append=FALSE)
assign("dailyDischarge",dailyDischarge,env=shared_data)