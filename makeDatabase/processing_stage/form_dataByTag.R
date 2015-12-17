#use captures as the base
captureQuery<-paste("SELECT tag,species,sample_name,cohort,observed_length",
                    "FROM data_tagged_captures")
captures<-data.table(dbGetQuery(con,captureQuery))
setkey(captures,tag)

#functions to get info by tag
getSpecies<-function(species,tag){
  species<-unique(na.omit(species))
  if(length(species==1)){return(species)}
  if(length(species==0)){return(as.character(NA))}
  
  stop("multiple species listed for tag",tag[1],
       " should have been caught in fix_tag_properties.R")
}

getMinLength<-function(x){
  m<-suppressWarnings(min(x,na.rm=T))
  if(m!=Inf) return(m) else return(as.numeric(NA))
}

#get data from from seasonal sampling by tag
dataByTag<-captures[,list(species=getSpecies(species),
                          minLength=getMinLength(observed_length),
                          minLengthDate=detection_date[which.min(observed_length)],
                          firstSampleObserved=min(sample_name),
                          lastSampleObserved=max(sample_name)
                          ),by=tag]

#add in lastAntennaDetection from antenna data
antenna<-NULL
antennaStub<-"SELECT tag,detection_date FROM"
for(nom in c("data_stationary_antenna","data_portable_antenna")){
  antenna<-rbind(antenna,
                 data.table(dbGetQuery(con,paste(antennaStub,nom)))
  )
}
setkey(antenna,tag)
antenna<-antenna[,list(lastAntennaDetection=max(detection_date)),by=tag]

dataByTag<-dataByTag[antenna]

#add dateKnownDead from tags_dead
dead<-data.table(dbGetQuery(con,"SELECT * FROM tags_dead"))
setkey(dead,tag)

dataByTag<-dataByTag[dead]

dbDropTable("dataByTag")
dbWriteTable(con, 'dataByTag', dataByTag, row.names=FALSE)

