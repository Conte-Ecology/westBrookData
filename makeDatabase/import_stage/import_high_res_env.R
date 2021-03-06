solinstPath<-file.path(original_data_dir,"West Drainage Solinst logger data.xlsx")
tempPath<-file.path(original_data_dir,"finished west brook temperature.xlsx")
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

sheetNamesTemp<-c("smolt trap",
                  "Section 6",
                  "section 30",
                  "section 45",
                  "WB Jimmy",
                  "WB Mitchell",
                  "WB Obear") #there are extra sheets that shouldn't be read

sheetNameMap<-list(smoltTrap="smolt trap",
                   section6="Section 6",
                   section30="section 30",
                   section45="section 45",
                   jimmy="WB Jimmy",
                   mitchell="WB Mitchell",
                   obear="WB Obear")
#specifying column types makes time parsing easier

nameMapTemp=list(river="River",
                 section="Section",
                 date="Date",
                 time="Time",
                 temp="Temperature(*C)",
                 source="Source",
                 notes="Notes")

for(n in names(sheetNameMap)){
  name<-paste0(n,"Temp")
  assign(name,
         data.table(
           suppressWarnings(read_excel(tempPath,sheetNameMap[[n]]))
           )
         )
  setnames(get(name),map(colnames(get(name)),nameMapTemp))
  
  get(name)[,depth:=NA]
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
highResEnv[river!="west brook",
           river:=unlist(strsplit(river,"wb "))[2],
           by=river]
setkey(highResEnv,river,section,dateTime)
highResEnv[,dateTime:=force_tz(dateTime,"EST")]
highResEnv[,dateTime:=as.POSIXct(format(dateTime,tz="America/New_York",usetz=T))]
highResEnv[section=="100",section:="B100"]
highResEnv[section=="SMOLT TRAP",section:="smoltTrap"]
highResEnv[section=="16",section:="15"]
dbWriteTable(con, 'raw_high_res_env', highResEnv, row.names=FALSE,
             overwrite=TRUE, append=FALSE)
