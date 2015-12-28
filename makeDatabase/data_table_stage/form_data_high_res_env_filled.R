highResEnv<-data.table(dbGetQuery(con,"SELECT * FROM raw_high_res_env"))
highResEnv[,dateTime:=as.POSIXct(round(dateTime,"hours"))]

highResEnv<-highResEnv[,list(temp=mean(temp,na.rm=T),
                             depth=suppressWarnings(mean(depth,na.rm=T))),
                       by=list(river,dateTime)]

jimmy<-highResEnv[river=="wb jimmy"]
mitchell<-highResEnv[river=="wb mitchell"]
obear<-highResEnv[river=='wb obear']
wb<-highResEnv[river=="west brook"]

allDateTime<-data.table(dateTime=seq(as.POSIXct("1997-05-15 00:00:00","GMT"),
                                     max(highResEnv$dateTime),
                                     "hour"))
setkey(allDateTime,dateTime)

riverObjects<-c("jimmy","mitchell","obear","wb")
for(r in riverObjects){
  assign(r,get(r)[,list(dateTime,temp,depth)])
  setkey(get(r),dateTime)
  assign(r,get(r)[allDateTime])
  setnames(get(r),c("temp","depth"),c(paste0(r,"Temp"),paste0(r,"Depth")))
}

data<-jimmy[mitchell]
data<-data[obear]
data<-data[wb]
setkey(data,dateTime)

#first pass is interpolating gaps <=4hrs
interpolateTemps<-function(data){
  
  #find the last temp record before time in question
  earlierTime<-function(time,river){
    return(data[!is.na(get(tempName))
                &dateTime<time,
                max(dateTime)])
  }
  
  #find first temp record after time in question
  laterTime<-function(time,river){
    return(data[!is.na(get(tempName))
                &dateTime>time,
                min(dateTime)])
  }
  
  riverNames<-c("jimmy","mitchell","obear","wb")
  for(r in riverNames){
    
    tempName<-paste0(r,"Temp")
    
    #identify times at beginning of gaps in the record
    nas<-data[is.na(get(tempName)),which=T]
    nonNas<-data[!is.na(get(tempName)),which=T]
    data[nonNas,source:="measured"]
    gapStarts<-nas[(nas-1) %in% nonNas]
    gapStarts<-data$dateTime[gapStarts]
    
    #set up progress bar
    cat(r,"\n")
    pb<-txtProgressBar(min=0,
                       max=length(gapStarts),style=3)
    #decide whether to fill each gap and fill it if appropriate
    for(t in 1:length(gapStarts)){
      time<-gapStarts[t]
      
      earlier<-earlierTime(time,r)
      later<-laterTime(time,r) 
      
      #don't interpolate if there is no earlier or later temp or the gap is >4hr
      if(any(is.na(earlier),is.na(later),
             difftime(later,earlier,units="hours")>4)){
        #update progress bar and move on
        setTxtProgressBar(pb,t)
        next
      } 
      
      #interpolate in the gap in the river specific object
      get(r)[dateTime>=earlier&
               dateTime<=later,
             c(paste0(r,"Temp"),"source"):=
               list(approx(get(tempName),xout=1:length(get(tempName)))$y,
               "interpolated")]
      
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
setkey(data,dateTime)

#there are some big gaps in the temp records
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
    get(river)[rowsToPredict,c(paste0(river,"Temp"),"source"):list(
      predict(get(paste0(r,"Lm")),data[rowsToPredict,paste0(r,"Temp"),with=F]),
      paste0("predictedFrom",r))]
    cat("filled",length(rowsToPredict),"rows from",r,"with r-squared = ",
        rsq[which(r==otherRivers)],"\n")
  }
  remainingNas<-nrow(get(river)[is.na(get(paste0(river,"Temp")))])
  cat("\n",remainingNas,"NA temps remain\n")
  biggestGap<-get(river)[!is.na(get(paste0(river,"Temp"))),max(diff(dateTime))]
  cat("The largest remaining gap with no temps is ",biggestGap,units(biggestGap),"\n")
}

for(r in riverObjects){
  fillTemp(r)
  setnames(get(r),c("dateTime","temp","depth"))
  get(r)[,river:=r]
}

#put back in the long format
data<-rbind(jimmy,mitchell,
            obear,wb)

data<-data[!is.na(temp)]

dbWriteTable(con, 'data_high_res_env_filled', data, row.names=FALSE,
             overwrite=TRUE, append=FALSE)
