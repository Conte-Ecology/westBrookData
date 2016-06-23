original_data_dir<-"C:/Users/Evan/Desktop/Conte/process-data/data_store/original_data"
library(dplyr)
library(data.table)
library(lubridate)
library(readxl)

allflexRiverM<-read_excel(paste0(original_data_dir,"/antenna/Table of Sites.xlsx")) %>%
  data.table() %>%
  setnames(c("index","section","location","riverM")) %>%
  .[grep("llflex",location)] %>%
  .[,reader:=strsplit(location,"-")[[1]][3],by=location]

antennaDir<-paste0(original_data_dir,"/antenna/compiledDataToImport")
dateFormats<-c("mdY HMS","dmY HMS","mdy HMS")

myWarnings<-NULL
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

  #read in the data but add warnings to a list rather than printing
  wHandler<-function(w){
    myWarnings<<-c(myWarnings,list(w))
    invokeRestart("muffleWarning")
  }


  #get the data into the right format  
  if(riverM=="allflex"){
    data<-withCallingHandlers(fread(paste0(antennaDir,"/",file)),
                              warning=wHandler)
    setnames(data,c("reader","tag","datetime"))
    #data[,datetime:=as.POSIXct(datetime,format="%m/%d/%y %H:%M:%S")]
    data<-data[,":="(datetime=parse_date_time(datetime,orders=dateFormats),
                     tag=tolower(tag))]
    riverM<-allflexRiverM[match(data$reader,allflexRiverM$reader),riverM]
    data<-data[,datetime,tag]
  } else {
    data<-withCallingHandlers(fread(paste0(antennaDir,"/",file),header=F),
                              warning=wHandler)
  if(ncol(data)>=6){
  data<-data[,.(V3,V4,V5)]
  } 
  setnames(data,c("date","time","tag"))
  data<-data %>%
            #.[,datetime:=as.POSIXct(paste(date,time),format="%m/%d/%y %H:%M:%S")] %>%
            .[,datetime:=parse_date_time(paste(date,time),orders=dateFormats)] %>%
            .[,.(datetime,tag=tolower(tag))]
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
  
  return(data)
}
badOnes<-NULL
filesToRead<-list.files(antennaDir)
for(file in filesToRead){
  assign(file,readAnt(file))
  if(is.na(get(file)$datetime[1])){
    badOnes<-c(badOnes,file)
  }
}
dataList<-lapply(filesToRead,get)
antennaData<-do.call(rbind,dataList)
antennaData<-antennaData[!duplicated(antennaData)]

antennaData[,":="(drainage="west",
                  area="antenna",
                  sample_type="stationary 1001m",
                  alive_or_dead="alive-stationary")]

