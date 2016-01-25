###could make this quicker by identifying gap size, then doing approx on entire vector, and returning big gaps to NA

#temps<-data.table(dbGetQuery(con,"SELECT * FROM raw_temps"))
temps<-readRDS("C:/Users/Evan/Desktop/fromSheds.rds") %>% data.table()
temps[,datetime:=as.POSIXct(round(datetime,"hours"))]

temps<-temps[,list(temperature=mean(temperature,na.rm=T)),
                       by=list(river,datetime)]

jimmy<-temps[river=="jimmy"]
mitchell<-temps[river=="mitchell"]
obear<-temps[river=='obear']
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
  setnames(get(r),c("temperature"),c(paste0(r,"Temp")))
}

data<-jimmy[mitchell]
data<-data[obear]
data<-data[wb]
setkey(data,datetime)


#first pass is interpolating gaps <=4hrs
interpolateTemps<-function(data){
  
  #find the last temperature record before time in question
  earlierTime<-function(time,river){
    return(data[!is.na(get(tempName))
                &datetime<time,
                max(datetime)])
  }
  
  #find first temperature record after time in question
  laterTime<-function(time,river){
    return(data[!is.na(get(tempName))
                &datetime>time,
                min(datetime)])
  }
  
  riverNames<-c("jimmy","mitchell","obear","wb")
  for(r in riverNames){
    
    tempName<-paste0(r,"Temp")
    
    #identify times at beginning of gaps in the record
    nas<-data[is.na(get(tempName)),which=T]
    nonNas<-data[!is.na(get(tempName)),which=T]
    data[nonNas,source:="measured"]
    gapStarts<-nas[(nas-1) %in% nonNas]
    gapStarts<-data$datetime[gapStarts]
    
    #set up progress bar
    cat(r,"\n")
    pb<-txtProgressBar(min=0,
                       max=length(gapStarts),style=3)
    #decide whether to fill each gap and fill it if appropriate
    for(t in 1:length(gapStarts)){
      time<-gapStarts[t]
      
      earlier<-earlierTime(time,r)
      later<-laterTime(time,r) 
      
      #don't interpolate if there is no earlier or later temperature or the gap is >4hr
      if(any(is.na(earlier),is.na(later),
             difftime(later,earlier,units="hours")>4)){
        #update progress bar and move on
        setTxtProgressBar(pb,t)
        next
      } 
      
      #interpolate in the gap in the river specific object
      rows<-get(r)[datetime>=earlier&
               datetime<=later,,which=T]
      set(get(r),rows,which(names(get(r))==tempName),
          approx(get(r)[datetime>=earlier&datetime<=later,get(tempName)],n=length(rows))$y)
      set(get(r),rows[2:(length(rows)-1)],which(names(get(r))=="source"),"interpolated")
      
      #update progress bar
      setTxtProgressBar(pb,t)
    }
    #provide feedback on what was done
    cat("\n",nrow(data[is.na(get(tempName))])-
          nrow(get(r)[is.na(get(tempName))]),
        "NAs in",r,"were filled by interpolation\n")
  }
}

interpolateTemps(data)

#that function only adjusts the river specific objects, so recollate to data
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
    set(get(river),rowsToPredict,which(names(get(river))=="source"),
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
# nameMap<-data.table(oldName=c("jimmy","obear","mitchell","wb"),
#                     newName=c("wb jimmy","wb obear","wb mitchell","west brook"))

# data[,river:=nameMap$newName[match(river,nameMap$oldName)]]

dbWriteTable(con, 'data_hourly_temperature', data, row.names=FALSE,
             overwrite=TRUE, append=FALSE)
