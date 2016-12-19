#this script corrects discrepancies among loggers within sites using relationships with the flow extension
#it also fills all of the mainstem daily values, prioritizing depth measurements, then filling with flow extension

highResEnv<-tbl(conDplyr,"raw_depth") %>%
            collect(n=Inf) %>%
            data.table()

wb<-highResEnv[river=="west brook",.(depth=mean(depth)),by=as.Date(datetime)] %>%
  setnames("as.Date","date") %>%
  setkey(date)

getUsgsData<-function (staid="01171500", code = "00060", stat = "00003", sdate = "1851-01-01", 
                       edate = as.Date(Sys.Date(), format = "%Y-%m-%d")) 
{
  if (is.character(staid) == FALSE) 
    stop("staid needs to have quotes around it")
  if (nchar(staid) < 8) 
    stop("staid must be at least 8 characters")
  base_url <- "http://waterservices.usgs.gov/nwis/dv?"
  url <- paste(base_url, "site=", staid, "&parameterCd=", code, 
               "&statCd=", stat, sep = "")
  url <- paste(url, "&startDt=", sdate, "&endDt=", edate, sep = "")
  doc <- read_xml(url)
  i <- 1
  val <- vector(mode = "numeric", length = 1)
  
  dates<-suppressWarnings(xml_attr(xml_children(xml_children(xml_children(doc)[2])[3]),"dateTime"))
  dates<-suppressWarnings(as.Date(substr(dates,1,10)))
  
  value<-suppressWarnings(xml_double(xml_children(xml_children(xml_children(doc)[2])[3])))
  
  dat<-data.frame(val=value,dates=dates)
  dat<-dat[!is.na(dates),]
  return(dat)
}

cleanQImport<-function(riverCode){
  q<-data.table(getUsgsData(riverCode,code="00060",stat="00003",sdate="1997-01-01",
                          edate=as.character(Sys.Date())))
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
   ,logDepth:=suppressWarnings(log(depth+c(k2,k1)[early+1]))]

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

allDates<-data.table(date=seq(as.Date("1997-05-27"),Sys.Date(),"day")) %>%
          setkey(date)

daily<-tbl(conDplyr,"raw_depth") %>%
       collect(n=Inf) %>%
       data.table() %>%
       .[river=="west brook",depth:=rescaleWb(depth,as.Date(datetime))] %>%
       .[,.(depth=mean(depth,na.rm=T)),by=list(as.Date(datetime),river)] %>%
       setnames("as.Date","date") %>%
       setkey(date) %>%
       .[,.SD[allDates],by=river] %>%
       setkey(date)

#load YSI data for west brook discharge
wbFile<-file.path(original_data_dir,"West Brook YSI Data with discharge calculated.xls")
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
  q<-data.table(getUsgsData(riverCode,code="00060",stat="00003",sdate="1997-05-27",
                          edate=Sys.Date()))
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
                 c(seq(as.Date("2003-01-08"),as.Date("2003-01-10"),"day"),
                   seq(as.Date("2008-02-26"),as.Date("2010-02-26"),"day"),
                   seq(as.Date("2004-06-15"),as.Date("2004-10-20"),"day"))
)

wb[date %in% badDates$`west brook`,discharge:=NA]
wbLm<-lm(log(discharge)~log(q2+200),data=wb[!date %in% badDates$`west brook`])

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

daily<-wb[,list(river,date,q2,discharge)][daily]
daily[!is.na(depth),source:="measuredDepth"]
#first estimate discharge from measured depths
daily[river=="west brook"&is.na(discharge),
      discharge:=exp(predict(depthLm,data.frame(logDepth=log(depth+200))))]
#then fill with flow extension when depth was not measured
daily[river=="west brook"&is.na(discharge),
      discharge:=exp(predict(wbLm,data.frame(q2=q2)))]


#####################################################################
#fill in missing depth data in tribs using regression with mainstem
setkey(daily,date)
wb<-daily %>%
  .[river=="west brook",list(date,discharge,q2)] %>%
  setnames("discharge","wbDischarge")

k<-data.table(river=c("west brook","wb jimmy","wb mitchell","wb obear"),
              k=c(0,10,0,100))
daily<-daily[,list(river,date,discharge,depth,source)][,.SD[wb],by=river] %>%
      .[,logDepth:=log(depth+k$k[match(river,k$river)])] %>%
      .[,logWbDischarge:=log(wbDischarge)]


fillDepth<-function(riverName){
  data<-daily[!is.na(logDepth)&river==riverName]
  
#   if(riverName=="mitchell"){
#     fit1<-lm(logDepth~logWbDischarge,data=data[logWbDischarge< -4.1])
#     fit2<-lm(logDepth~logWbDischarge,data=data[logWbDischarge> -4.1])
#     plot(logDepth~logWbDischarge,data=data)
#     abline(fit1)
#     abline(fit2)
#     par(new=T)
#     plot(NA,xlim=c(0,1),ylim=c(0,1),axes=F,xlab='',ylab='')
#     text(0.2,0.9,bquote(fit1~R^2~"="~.(round(summary(fit1)$r.squared,2))))
#     text(0.2,0.8,bquote(fit2~R^2~"="~.(round(summary(fit2)$r.squared,2))))
#     text(0.2,0.7,riverName)
#     
#     daily[river==riverName&logWbDischarge< -4.1,
#                    logDepth:=predict(fit1,
#                                      data.frame(logWbDischarge=logWbDischarge))] 
#     daily[river==riverName&logWbDischarge> -4.1,
#                    logDepth:=predict(fit2,
#                                      data.frame(logWbDischarge=logWbDischarge))]
#   } else {
    
    fit<-lm(logDepth~logWbDischarge,data=data)
    # plot(logDepth~logWbDischarge,data=data)
    # par(new=T)
    # plot(NA,xlim=c(0,1),ylim=c(0,1),axes=F,xlab='',ylab='')
    # text(0.2,0.8,bquote(R^2~"="~.(round(summary(fit)$r.squared,2))))
    # text(0.2,0.7,riverName)
    
    if(riverName %in% c('wb mitchell','wb obear')){
      fit<-lm(logDepth~q2,data=data)
      daily[river==riverName,
                     logDepth:=predict(fit,
                                       data.frame(q2=q2))]
    } else{
      daily[is.na(logDepth)&river==riverName,
                     logDepth:=predict(fit,
                                       data.frame(logWbDischarge=logWbDischarge))]
    }
  # }
}

for(r in c("wb jimmy","wb mitchell","wb obear")){
  fillDepth(r)
}

tribFile<-file.path(original_data_dir,"tribDischarge.xlsx")
riverNames<-data.table(new=c("west brook","wb jimmy","wb mitchell","wb obear"),
                       old=c("wb","jimmy","mitchell","obear"))
measuredDischarge<-read_excel(tribFile) %>%
                   data.table() %>%
                   .[,list(river,date,discharge,stake)] %>%
                   .[river=='mitchell',discharge:=discharge+0.001] %>%
                   .[,date:=as.Date(date)] %>%
                   .[,river:=riverNames[match(river,old),new]]



predictDischarge<-function(riverName){
  q<-measuredDischarge[river==riverName]
  setkey(q,date)
  
  depth<-daily[river==riverName,list(date,logDepth)]
  setkey(depth,date)
  
  data<-depth[q]
  
  fit<-lm(log(discharge)~logDepth,data=data)
  # plot(log(discharge)~logDepth,data=data)
  # abline(fit)
  # par(new=T)
  # plot(NA,xlim=c(0,1),ylim=c(0,1),axes=F,xlab='',ylab='')
  # text(0.2,0.8,bquote(R^2~"="~.(round(summary(fit)$r.squared,2))))
  # text(0.2,0.7,riverName)
  
  
  daily[river==riverName&!is.na(logDepth),
                 discharge:=exp(predict(fit,
                                        data.frame(logDepth=logDepth)))]
}

for(r in c("wb jimmy","wb mitchell","wb obear")){
  predictDischarge(r)
}

daily[is.na(source),source:="flowExt"]
daily[date %in% daily[river=="west brook"&source=="measuredDepth",date] &
        river != "west brook" & source=="flowExt",source:="predictFromWb"]

daily<-daily[,list(date,river,discharge,source)]

dbWriteTable(con, 'data_daily_discharge', data.frame(daily), row.names=FALSE,
             overwrite=TRUE, append=FALSE)


