#use captures as the base
captureQuery<-paste("SELECT tag,species,sample_name,cohort,observed_length",
                    "FROM data_tagged_captures")
captures<-data.table(dbGetQuery(con,captureQuery))
setkey(captures,tag)



#functions and reference data to get info by tag
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

cohortBins<-data.table(dbGetQuery(con,"SELECT * FROM data_yoy_bins"))

getCohort<-function(cohort,species,length,sample){
  execEnv<-env()
  species<-unique(na.omit(species))
  if(length(species)>1) stop("cannot assign cohort for multiple species tag")
  if(length(species==0)) return(NA)
  cohort<-unique(na.omit(cohort))
  if(length(cohort==1)) return(cohort)
  if(length(cohort==0)){
    minLength<-suppressWarnings(min(length,na.rm=T))
    if(minLength==Inf) return(NA)
    sample<-sample[which(length==minLength)]
    bins<-cohortBins[species==get('species',envir=execEnv)&
                          sample_name==get('sample',envir=execEnv),
                          list(cohort_min_length,
                               cohort_max_length,
                               cohort)]
    cohort<-bins$cohort[intersect(which(minLength>bins$cohort_min_length),
                                  which(minLength<bins$cohort_max_length))]
    return(cohort)
  }
  if(length(cohort>1)){
    whichHaveCohort<-which(!is.na(cohort))
    #return the cohort call made first if multiple were made
    return(cohort[which.min(sample[whichHaveCohort])])
  }
}

#get data from from seasonal sampling by tag
dataByTag<-captures[,list(species=getSpecies(species),
                          minLength=getMinLength(observed_length),
                          minLengthSample=detection_date[which.min(observed_length)],
                          firstCaptureSample=min(sample_name),
                          lastCaptureSample=max(sample_name)
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

