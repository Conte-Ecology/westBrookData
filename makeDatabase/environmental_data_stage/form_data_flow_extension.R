q<-data.table(
        suppressWarnings(
          read_excel(file.path(original_data_dir,"West Brook Flow Extension.xls"))
        )
      )
setnames(q,c("date","doy","totalRain","avgDailyTemp","tempSource","discharge","discharge_source"))
q<-q[!is.na(discharge),list(date=as.Date(date,format="%m/%d/%Y"),
                                    q=as.numeric(discharge),
                                    discharge_source)]

getMillData<-function (staid="01171500", code = "00060", stat = "00003", sdate = "1851-01-01", 
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

# riverCodes<-c("01169900",
#               "01171500",
#               "01161000")

qGaged<-data.table(getMillData(sdate="1997-05-15",edate=as.Date(Sys.time())))
setnames(qGaged,c('val','dates'),c('qGaged','date'))
qGaged[qGaged<0,qGaged:=NA]
qGaged<-qGaged[,list(date,qGaged)]

#Could use a regression with 2 gaged rivers, but it doesn't improve the prediction much
# qGaged2<-data.table(importDVs(riverCodes[1],code="00060",stat="00003",
#                               sdate=min(q$date),
#                               edate=max(q$date)))
# setnames(qGaged2,c('val','dates'),c('qGaged2','date'))
# qGaged2[qGaged2<0,qGaged2:=NA]
# qGaged2<-qGaged2[,list(date,qGaged2)]
# setkey(qGaged2,date)
setkey(qGaged,date)

# qGaged<-qGaged[qGaged2]

setkey(q,date)
q<-qGaged[q]

flowExt<-lm(q~qGaged,data=q[discharge_source=='STAKE'])

qGaged[,qPredicted:=predict(flowExt,data.frame(qGaged=qGaged))]
qGaged[,":="(river="west brook",
             source="flowExtension")]
qGaged[,qGaged:=NULL]
qGaged[,date:=as.POSIXct(as.character(date),format="%F")]

dbDropTable("data_flow_extension")
dbWriteTable(con,"data_flow_extension",qGaged,row.names=F)



