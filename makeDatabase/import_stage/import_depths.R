solinstPath<-file.path(original_data_dir,"West Drainage Solinst logger data.xlsx")
earlyPath<-file.path(original_data_dir,"earlyWestBrookEnv.csv")

parseDateTime<-function(date,time){
  time<-as.POSIXlt(time)
  date<-as.POSIXlt(date)
  
  time$year<-date$year
  time$mon<-date$mon
  time$mday<-date$mday
  
  return(as.POSIXct(time))
}

sheetNamesSolinst<-excel_sheets(solinstPath)

nameMapSolinst<-list(river="River",
                     section="Section",
                     date="Date",
                     time="Time",
                     depth="Depth (cm)",
                     temp="Temperature(*C)",
                     comment="Comment",
                     source="Source"
)

keep<-c("river","section","dateTime","temp","depth","source")

objectNames<-NULL

for(n in sheetNamesSolinst){
  name<-paste0(gsub(" ","",n),"Solinst")
  assign(name,
         data.table(
           suppressWarnings(read_excel(solinstPath,n))
         )
  )
  setnames(get(name),map(colnames(get(name)),nameMapSolinst))
  get(name)[,dateTime:=parseDateTime(date,time)]
  assign(name,get(name)[,keep,with=F])
  
  objectNames<-c(objectNames,as.name(name))
  
}

highResEnv<-do.call(rbind,args=as.list(objectNames))
highResEnv[,river:=tolower(river)]

early<-suppressWarnings(fread(earlyPath))
setnames(early,c("date/time","final depth"),c("dateTime","finalDepth"))
early<-early[,list(river="west brook",
                   section=NA,
                   dateTime=as.POSIXct(dateTime*24*60*60,origin=as.POSIXct("1899-12-30 00:00:00",tz="UTC")),
                   temp=temp,
                   depth=finalDepth,
                   source="earlyDepthLogger")]

highResEnv<-rbind(highResEnv,early)
setkey(highResEnv,river,section,dateTime)
highResEnv[,dateTime:=force_tz(dateTime,"EST")]
highResEnv[,dateTime:=as.POSIXct(format(dateTime,tz="America/New_York",usetz=T))]
highResEnv[,section:=as.character(section)]
highResEnv[section=="100",section:="B100"]
setnames(highResEnv,"dateTime","datetime")
dbWriteTable(con, 'raw_depth', data.frame(highResEnv), row.names=FALSE,
             overwrite=TRUE, append=FALSE)





