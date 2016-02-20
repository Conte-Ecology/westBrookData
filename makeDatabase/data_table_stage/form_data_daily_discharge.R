highResEnv<-tbl(conDplyr,"raw_depth") %>%
            collect() %>%
            data.table()

wb<-highResEnv[river=="west brook",.(depth=mean(depth)),by=as.Date(datetime)] %>%
  setnames("as.Date","date") %>%
  setkey(date)


cleanQImport<-function(riverCode){
  q<-data.table(importDVs(riverCode,code="00060",stat="00003",sdate="1997-01-01",
                          edate="2015-10-29"))
  setnames(q,c('val','dates'),c('discharge','date'))
  q[discharge<0,discharge:=NA]
  q<-q[,list(date,discharge)]
  
  setkey(q,date)
  
  return(q)
}

#run the imports and merge with YSI data
wb<-cleanQImport("01171500") %>%
  .[,list(date,discharge)] %>%
  setnames("discharge","q2") %>%
  setkey(date) %>%
  .[wb]

k1<- -0.04
k2<- 15
wb[,early:=as.numeric(depth<1.2)][
   ,logDepth:=log(depth+c(k2,k1)[early+1])]

fit1<-lm(log(q2)~logDepth,data=wb[depth<1.2])
fit2<-lm(log(q2)~logDepth,data=wb[depth>1.2])

rescaleWb<-function(depth,date){
  depth[date<as.Date("2007-01-01")]<-(suppressWarnings(log(depth[date<as.Date("2007-01-01")]+k1))*coef(fit1)[2]+
                                            coef(fit1)[1]-coef(fit2)[1])/
                                          coef(fit2)[2]
  depth[date>as.Date("2007-01-01")]<-log(depth[date>as.Date("2007-01-01")]+k2)
  depth<-exp(depth)
  return(depth)
}

allDates<-data.table(date=seq(as.Date("1997-05-27"),as.Date("2015-05-29"),"day")) %>%
          setkey(date)

daily<-tbl(conDplyr,"raw_depth") %>%
       collect() %>%
       data.table() %>%
       .[river=="west brook",depth:=rescaleWb(depth,as.Date(datetime))] %>%
       .[,.(depth=mean(depth,na.rm=T)),by=list(as.Date(datetime),river)] %>%
       setnames("as.Date","date") %>%
       setkey(date) %>%
       .[,.SD[allDates],by=river] %>%
       setkey(date)

#load YSI data for west brook discharge
wbFile<-"C:/Users/Evan/Desktop/Conte/process-data/data_store/original_data/West Brook YSI Data with discharge calculated.xls"
wb<-suppressWarnings((read_excel(wbFile,
                                 col_types=c("numeric","numeric",
                                             "numeric","numeric","numeric",
                                             "numeric","numeric","numeric",
                                             "numeric","numeric","numeric",
                                             "numeric")))) %>%
  data.table() %>%
  setnames(tolower(names(.))) %>%
  .[,list(date,discharge)] %>%
  .[,date:=as.Date(date,origin="1899-12-30")] %>%
  .[,.(discharge=mean(discharge)),by=date] %>%
  setkey(date)


# allDates<-allDates[river=='wb',list(date)]
# wb<-wb[allDates[river=="wb",]]

#Function imports discharge data from USGS database online
cleanQImport<-function(riverCode){
  q<-data.table(importDVs(riverCode,code="00060",stat="00003",sdate="1997-05-27",
                          edate="2015-05-29"))
  setnames(q,c('val','dates'),c('discharge','date'))
  q[discharge<0,discharge:=NA]
  q<-q[,list(date,discharge)]
  
  setkey(q,date)
  
  return(q)
}
#import these rivers
riverCodes<-c("01169900",
              "01171500",
              "01161000")
k<-c(10,5,0)
#run the imports and merge with YSI data
for(code in riverCodes){
  q<-cleanQImport(code)
  q<-q[,list(date,log(discharge+k[which(code==riverCodes)]))]
  setnames(q,c("date",paste0("q",which(code==riverCodes))))
  setkey(q,date)
  wb<-wb[q]
}
# wb<-wb[!is.na(q1)&!is.na(q2)&!is.na(q3)]

wbLm<-lm(log(discharge)~q1*q2*q3,data=wb)

# wb[is.na(discharge),
#    discharge:=exp(predict(wbLm,data.frame(q1=q1,q2=q2,q3=q3)))]




badDates<-list("wb jimmy"=
                 c(seq(as.Date("2009-07-30"),as.Date("2009-12-27"),"day"),
                   seq(as.Date("2009-01-15"),as.Date("2009-04-29"),"day")),
               "wb mitchell"=
                 c(seq(as.Date("2013-03-01"),as.Date("2015-05-31"),"day"),
                   seq(as.Date("2009-12-12"),as.Date("2010-01-12"),"day"),
                   seq(as.Date("2010-04-02"),as.Date("2010-07-19"),"day")),
               "wb obear"=
                 c(seq(as.Date("2009-07-30"),as.Date("2009-12-27"),"day")),
               "west brook"=
                 c(seq(as.Date("2008-02-26"),as.Date("2010-02-26"),"day"))
)
for(r in unique(daily$river)){
  daily[river==r & 
          date %in% badDates[[r]],depth:=NA]
}

wb<-daily[river=='west brook'][wb]
wb[depth==0,depth:=NA]
wb[,logDepth:=log(depth+200)]
depthLm<-lm(log(discharge)~logDepth,data=wb)

setkey(wb,river,date)
setkey(daily,river,date)

daily<-wb[,list(river,date,q1,q2,q3,discharge)][daily]
#first estimate discharge from measured depths
daily[river=="west brook"&is.na(discharge),discharge:=exp(predict(depthLm,data.frame(logDepth=log(depth+200))))]
#then fill with flow extension when depth was not measured
daily[river=="west brook"&is.na(discharge),discharge:=exp(predict(wbLm,data.frame(q1=q1,q2=q2,q3=q3)))]

dbWriteTable(con, 'data_daily_discharge', data.frame(daily), row.names=FALSE,
             overwrite=TRUE, append=FALSE)


