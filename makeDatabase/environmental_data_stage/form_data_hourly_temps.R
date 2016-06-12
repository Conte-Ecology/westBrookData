###could make this quicker by identifying gap size, then doing approx on entire vector, and returning big gaps to NA

temps<-data.table(dbGetQuery(con,"SELECT * FROM raw_temps"))
temps[,datetime:=as.POSIXct(round(datetime,"hours"))]


badDates<-list("West Brook 30"=
                 list(start=as.POSIXct(c("2001-10-17 12:00:00","2013-04-01 12:00:00")),
                      stop=as.POSIXct(c("2001-10-22 12:00:00","2013-11-01 12:00:00"))),
               "West Brook 45"=
                 list(start=as.POSIXct(c("2007-06-11 11:00:00",
                                         "2012-04-01 12:00:00",
                                         "2013-04-01 12:00:00")),
                     stop=as.POSIXct(c("2007-06-13 11:00:00",
                                     "2012-04-26 12:00:00",
                                     "2013-11-01 12:00:00"))))

               
for(l in names(badDates)){
  for(b in 1:length(badDates[[l]]$start)){
    temps<-temps[location!=l|datetime<=badDates[[l]]$start[b]|
            datetime>=badDates[[l]]$stop[b]]
  }
}

temps<-temps[,list(temperature=mean(temperature,na.rm=T)),
                       by=list(river,datetime)]

jimmy<-temps[river=="wb jimmy"]
mitchell<-temps[river=="wb mitchell"]
obear<-temps[river=='wb obear']
wb<-temps[river=="west brook"]

allDateTime<-data.table(datetime=seq(min(temps$datetime),
                                     max(temps$datetime),
                                     "hour"))

setkey(allDateTime,datetime)

riverObjects<-c("jimmy","mitchell","obear","wb")
for(r in riverObjects){
  assign(r,get(r)[,list(datetime,temperature,source="measured")])
  setkey(get(r),datetime)
  assign(r,get(r)[allDateTime])
  setnames(get(r),c("temperature","source"),c(paste0(r,"Temp"),paste0(r,"Source")))
}

data<-jimmy[mitchell]
data<-data[obear]
data<-data[wb]
setkey(data,datetime)


#first pass is interpolating gaps <=4hrs
  riverNames<-c("jimmy","mitchell","obear","wb")
  for(r in riverNames){
    
    tempName<-paste0(r,"Temp")
    sourceName<-paste0(r,"Source")
    #identify times at beginning of gaps in the record
    nas<-data[is.na(get(tempName)),which=T]
    start = c(1, which(diff(nas) != 1) + 1)
    end = c(start[-1]-1, length(nas))
    for(i in 1:length(start)){
      get(r)[nas[start[i]]:nas[end[i]],group:=i]
    }
    measureRange<-range(which(!is.na(get(r)[[tempName]])))
    get(r)[!is.na(group),groupSize:=nrow(.SD),by=group]
    get(r)[measureRange[1]:measureRange[2],
           c(tempName):=list(approx(get(tempName),n=nrow(.SD))$y)]
    get(r)[nas,c(sourceName):=list("interpolated")]
    get(r)[groupSize>4,c(tempName):=list(NA)]
    get(r)[is.na(get(tempName)),c(sourceName):=list(NA)]
    get(r)[,groupSize:=NULL]
    get(r)[,group:=NULL]
  }
  
# #that function only adjusts the river specific objects, so recollate to data
data<-jimmy[mitchell]
data<-data[obear]
data<-data[wb]
setkey(data,datetime)

#there are some big gaps in the temperature records
#this fills larger gaps using regressions with the other rivers' temps
#when they have data, prioritizing the strongest relationships when filling
fillTemp<-function(river){
  cat("\n","Filling",river,"by regression","\n","\n")
  otherRivers<-riverObjects[riverObjects!=river]
  
  #run regressions with each otherRiver
  for(r in otherRivers){
    assign(paste0(r,"Lm"),lm(get(paste0(river,"Temp"))~get(paste0(r,"Temp")),data=data))
  }
  
  #get the r-squared from those regressions
  rsq<-NULL
  for(r in 1:3){
    rsq[r]<-summary(get(paste0(otherRivers[r],"Lm")))$r.squared
  }
  
  #assign unknowns in the river specific object based on regressions
  #choosing strongest r2 first and going down the line for still unfilled values
  for(r in otherRivers[order(rsq,decreasing=T)]){
    #which are na in the river but not in the otherRiver (r)
    #uses data which is not modified during the process to
    #avoid prediction using predictions
    rowsToPredict<-which(is.na(get(river)[[paste0(river,"Temp")]]) &
                           !is.na(data[[paste0(r,"Temp")]]))
    if(length(rowsToPredict)==0){
      cat(r,"has no rows to predict\n")
      next}
    
    #fill the rowsToPredict with the predicted values from the regression
    set(get(river),rowsToPredict,
        which(names(get(river))==c(paste0(river,"Temp"))),
        predict(get(paste0(r,"Lm")),data[rowsToPredict,paste0(r,"Temp"),with=F]))
    set(get(river),rowsToPredict,which(names(get(river))==paste0(river,"Source")),
        paste0("predictedFrom",toupper(substr(r,1,1)),substr(r,2,nchar(r))))
    
    cat("filled",length(rowsToPredict),"rows from",r,"with r-squared = ",
        rsq[which(r==otherRivers)],"\n")
  }
  remainingNas<-nrow(get(river)[is.na(get(paste0(river,"Temp")))])
  cat("\n",remainingNas,"NA temps remain\n")
  biggestGap<-get(river)[!is.na(get(paste0(river,"Temp"))),max(diff(datetime))]
  cat("The largest remaining gap with no temps is ",biggestGap,units(biggestGap),"\n")
}

for(r in riverObjects){
  fillTemp(r)
  setnames(get(r),c("datetime","temperature","source"))
  get(r)[,river:=r]
}

#put back in the long format
data<-rbind(jimmy,mitchell,
            obear,wb)
nameMap<-data.table(oldName=c("jimmy","obear","mitchell","wb"),
                     newName=c("wb jimmy","wb obear","wb mitchell","west brook"))

data[,river:=nameMap$newName[match(river,nameMap$oldName)]]

dbWriteTable(con, 'data_hourly_temperature', data, row.names=FALSE,
             overwrite=TRUE, append=FALSE)

