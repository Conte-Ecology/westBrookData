solinstPath<-file.path(original_data_dir,"West Drainage Solinst logger data.xlsx")
tempPath<-file.path(original_data_dir,"finished west brook temperature.xlsx")

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
           suppress.warnings(read_excel(solinstPath,n))
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
           read_excel(tempPath,sheetNameMap[[n]])
           )
         )
  setnames(get(name),map(colnames(get(name)),nameMapTemp))
  
  get(name)[,depth:=NA]
  get(name)[,dateTime:=parseDateTime(date,time)]
  assign(name,get(name)[,keep,with=F])

  
  objectNames<-c(objectNames,as.name(name))
}

highResEnv<-do.call(rbind,args=as.list(objectNames))
setkey(highResEnv,river,section,dateTime)

dbWriteTable(con, 'raw_high_res_env', highResEnv, row.names=FALSE,
             overwrite=TRUE, append=FALSE)