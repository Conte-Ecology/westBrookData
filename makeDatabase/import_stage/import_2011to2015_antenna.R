allflexRiverM<-read_excel(paste0(original_data_dir,"/antenna/Table of Sites.xlsx")) %>%
  data.table() %>%
  setnames(c("index","section","location","riverM")) %>%
  .[grep("llflex",location)] %>%
  .[,reader:=strsplit(location,"-")[[1]][3],by=location]

antennaDir<-paste0(original_data_dir,"/antenna/compiledDataToImport")
dateFormats<-c("dmY HMS","mdY HMS","mdy HMS")

deployed<-NULL
myWarnings<-NULL
weirdDates<-NULL

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
  fileDetails<-strsplit(file,"_")[[1]]
  location<-paste(fileDetails[c(1,2)],collapse=" ")
  riverM<-fileDetails[3]
  startDate<-as.Date(substr(fileDetails[4],1,8),format="%m%d%Y")
  endDate<-as.Date(substr(fileDetails[4],10,17),format="%m%d%Y")
  
  
  river<-c("west brook","wb jimmy","wb mitchell","wb obear")[
            grepl(substr(location,1,8),c("wb above","wb jimmy","wb mitch","wb obear")) %>% which()
            ]

  #combine details into deployment info
  deployed<<-rbind(deployed,
                  data.table(river=river,
                             location=location,
                             river_meter=riverM,
                             start_date=startDate,
                             end_date=endDate))
  
  #read in the data but add warnings to a list rather than printing
  wHandler<-function(w){
    myWarnings<<-c(myWarnings,list(w))
    invokeRestart("muffleWarning")
  }


  #get the data into the right format  
  if(riverM=="allflex"){
    data<-withCallingHandlers(fread(paste0(antennaDir,"/",file)),
                              warning=wHandler)
    setnames(data,c("reader","tag","detection_date"))
    #data[,detection_date:=as.POSIXct(detection_date,format="%m/%d/%y %H:%M:%S")]
    data<-data[,":="(detection_date=getDateTime(detection_date),
                     tag=tolower(tag))]
    riverM<-allflexRiverM[match(data$reader,allflexRiverM$reader),riverM]
    data<-data[,.(detection_date,tag,reader_type="stationary 2001-allflex")]
  } else {
    data<-withCallingHandlers(fread(paste0(antennaDir,"/",file),header=F),
                              warning=wHandler)
  if(ncol(data)>=6){
  data<-data[,.(V3,V4,V5)]
  } 
  setnames(data,c("date","time","tag"))
  data<-data %>%
            #.[,detection_date:=as.POSIXct(paste(date,time),format="%m/%d/%y %H:%M:%S")] %>%
            .[,detection_date:=getDateTime(paste(date,time))] %>%
            .[,.(detection_date,tag=tolower(tag),reader_type="stationary 2001-iso")]
  }
  splitTag<-function(x){
    tSplit<-strsplit(x,"[.]")[[1]]
    return(ifelse(length(tSplit)>1,tSplit[2],tSplit))
  }
  data<-data %>%
            .[,tag:=splitTag(tag),by=tag] %>%
            .[,":="(location=location,
                    river=river,
                    river_meter=as.numeric(riverM))]
  if(any(!as.Date(data$detection_date) %in% seq.Date(startDate,endDate,1))){
    weirdDates<<-c(weirdDates,file)
  }
  
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
dataList<-lapply(filesToRead,get,envir=temp)
# dataList<-lapply(filesToRead,get)

antennaData<-do.call(rbind,dataList)
antennaData<-antennaData[!duplicated(antennaData)]

antennaData[,":="(drainage="west")]

dbWriteTable(con, 'tags_antenna_2011_2015', antennaData, row.names=FALSE,
             overwrite=TRUE, append=FALSE)
