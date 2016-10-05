allflexRiverM<-tbl(conDplyr,"antenna_deployment") %>%
  collect() %>%
  filter(grepl("allflex",antenna_name)) %>%
  data.table() %>%
  .[,reader:=tstrsplit(antenna_name,"[(]")[2]] %>%
  .[,reader:=substr(reader,1,1)] %>%
  .[,.(river_meter,reader)] %>%
  unique()

antennaDir<-paste0(original_data_dir,"/antenna/oldAllflexToImport")
dateFormats<-c("dmY HMS","mdY HMS","mdy HMS")

deployed<-NULL
myWarnings<-NULL

getDateTime<-function(dateTime,formats=c("%m/%d/%y %H:%M:%S",
                                         "%m/%d/%Y %H:%M:%S",
                                         "%d-%m-%Y %H:%M:%S")){
  for(f in formats){
    suppressWarnings(datetime<-as.POSIXct(dateTime,format=f,tz="UTC"))
    if(!any(is.na(datetime)|datetime>Sys.time()|datetime<as.POSIXct("01-01-1950 12:00:00"))){
      break
    }
  }
  if(any(is.na(datetime)|datetime>Sys.time()|datetime<as.POSIXct("01-01-1950 12:00:00"))){
    stop(paste("weird dates produced from this format:",dateTime[1],"-->",datetime[1]))
  }
  return(datetime)
}

readAnt<-function(file){
  #get overall details
    river<-"wb mitchell"
  #read in the data but add warnings to a list rather than printing
  wHandler<-function(w){
    myWarnings<<-c(myWarnings,list(w))
    invokeRestart("muffleWarning")
  }
  
  #get the data into the right format  
    data<-withCallingHandlers(fread(paste0(antennaDir,"/",file)),
                              warning=wHandler)
    setnames(data,c("reader","tag","detection_date"))
    #data[,detection_date:=as.POSIXct(detection_date,format="%m/%d/%y %H:%M:%S")]
    data<-data[,":="(detection_date=getDateTime(detection_date),
                     tag=tolower(tag))]
    riverM<-allflexRiverM[match(data$reader,allflexRiverM$reader),river_meter]
    location<-"wb mitchellBelow"
    data<-data[,.(detection_date,tag,reader_type="stationary 2001-allflex")]

  splitTag<-function(x){
    tSplit<-strsplit(x,"[.]")[[1]]
    return(ifelse(length(tSplit)>1,tSplit[2],tSplit))
  }
  data<-data %>%
    .[,tag:=splitTag(tag),by=tag] %>%
    .[,":="(location=location,
            river=river,
            river_meter=as.numeric(riverM))]
  return(data)
}
badOnes<-NULL
filesToRead<-list.files(antennaDir)
for(file in filesToRead){
  assign(file,readAnt(file))
  if(is.na(get(file)$detection_date[1])){
    badOnes<-c(badOnes,file)
  }
}

dataList<-lapply(filesToRead,get)#,envir=temp)
# dataList<-lapply(filesToRead,get)

antennaData<-do.call(rbind,dataList)
antennaData<-antennaData[!duplicated(antennaData)]

antennaData[,":="(drainage="west")]

dbWriteTable(con, 'tags_allflex_to_2011', antennaData, row.names=FALSE,
             overwrite=TRUE, append=FALSE)
