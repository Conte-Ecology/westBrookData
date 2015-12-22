#use captures as the base
captureQuery<-paste("SELECT tag,species,sample_name,cohort,observed_length,river",
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

seasonalSampling<-
  data.table(
   dbGetQuery(con,"SELECT sample_name,season,year FROM data_seasonal_sampling")
  )
 
setkey(cohortBins,sample_name)
setkey(seasonalSampling,sample_name)
cohortBins<-seasonalSampling[cohortBins]

getCohort<-function(cohort,species,length,sample,river){
  execEnv<-environment()
  species<-unique(na.omit(species))
  if(length(species)>1) stop("cannot assign cohort for multiple species tag")
  if(length(species)==0|!species %in% c("bkt","bnt","ats")) return(as.numeric(NA))
  cohort<-as.numeric(unique(na.omit(cohort)))
  if(length(cohort)==1) return(cohort)
  if(length(cohort)==0){
    minLength<-suppressWarnings(min(length,na.rm=T))
    if(minLength==Inf) return(as.numeric(NA))
    sample<-min(sample[which(length==minLength)])
    river<-river[which(length==minLength)]
    if(species=='ats'){river<-'west brook'}#bins only assigned in west brook for salmon
    bins<-cohortBins[species==get('species',envir=execEnv)&
                      sample_name==get('sample',envir=execEnv)&
                      river==get('river',envir=execEnv) ,
                          list(cohort_min_length,
                               cohort_max_length,
                               cohort)]
    if(nrow(bins)==0){
      thisSeason<-seasonalSampling[sample_name==sample,season]
      bins<-cohortBins[species==get('species',envir=execEnv)&
                         river==get('river',envir=execEnv)&
                         season==thisSeason,
                       list(meanMin=mean(cohort_min_length),
                            meanMax=mean(cohort_max_length)),
                       by=age]
      setkey(bins,age)
      bins[,cohort_max_length:=(meanMax+shift(meanMin,1,type='lead'))/2]
      bins[age==max(age),cohort_max_length:=meanMax]
      bins[,cohort_min_length:=c(meanMin[1],cohort_max_length[1:(nrow(bins)-1)])]
      bins[,cohort:=seasonalSampling[sample_name==sample,year]-age]
    }
    if(minLength>max(bins$cohort_max_length)){
      #if first length is bigger than the bins assigned for that stream, it probably came from west brook
      river<-"west brook"
      bins<-cohortBins[species==get('species',envir=execEnv)&
                         sample_name==get('sample',envir=execEnv)&
                         river==get('river',envir=execEnv) ,
                       list(cohort_min_length,
                            cohort_max_length,
                            cohort)]
    }
    cohort<-bins$cohort[intersect(which(minLength>=bins$cohort_min_length),
                                  which(minLength<=bins$cohort_max_length))]
    
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
                          first_capture_sample=min(sample_name),
                          last_capture_sample=max(sample_name),
                          cohort=getCohort(cohort,species,observed_length,sample_name,river)
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
antenna<-antenna[,list(last_antenna_detection=max(detection_date)),by=tag]

dataByTag<-antenna[dataByTag]

#add dateKnownDead from tags_dead
dead<-data.table(dbGetQuery(con,"SELECT * FROM tags_dead"))
setkey(dead,tag)

dataByTag<-dead[dataByTag]

dbDropTable("data_by_tag")
dbWriteTable(con, 'data_by_tag', dataByTag, row.names=FALSE)

