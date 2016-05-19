#use captures as the base
captureQuery<-paste("SELECT tag,species,sample_name,cohort,",
                      "observed_length,river,detection_date,sex,drainage",
                    "FROM data_tagged_captures")
captures<-data.table(dbGetQuery(con,captureQuery))
setkey(captures,tag)

#functions and reference data to get info by tag
getSpecies<-function(species,tag){
  species<-unique(na.omit(species))
  if(length(species)==1){return(species)}
  if(length(species)==0){return(as.character(NA))}
  
  stop("multiple species listed for tag ",tag[1],
       " should have been caught in fix_tag_properties.R")
}

getSex<-function(sex,tag){
  sex<-unique(na.omit(sex))
  if(length(sex)==1){return(sex)}
  if(length(sex)==0){return(as.character(NA))}
  
  stop("multiple sexes listed for tag ",tag[1],
       " should have been caught in fix_tag_properties.R")
}

getMinLength<-function(x){
  m<-suppressWarnings(min(x,na.rm=T))
  if(m!=Inf) return(m) else return(as.numeric(NA))
}

cohortBins<-data.table(dbGetQuery(con,"SELECT * FROM data_yoy_bins"))

seasonalSampling<-
  data.table(
    dbGetQuery(con,"SELECT distinct sample_name,season,year,drainage FROM data_seasonal_sampling")
  )

setkey(cohortBins,sample_name,drainage)
setkey(seasonalSampling,sample_name,drainage)
cohortBins<-seasonalSampling[cohortBins]

getCohort<-function(cohort,species,length,sample,river,drainage){

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
    if(sample==2.5&unique(na.omit(drainage))=="stanley"){sample<-3}#no bins assigned for fyke net sample
    river<-river[which(sample==get('sample',envir=execEnv))]
    drainage<-drainage[which(sample==get('sample',envir=execEnv))]
    if(species=='ats'){river<-'west brook'}#bins only assigned in west brook for salmon
    bins<-cohortBins[species==get('species',envir=execEnv)&
                      sample_name==get('sample',envir=execEnv)&
                      river==get('river',envir=execEnv)&
                      drainage==get('drainage',envir=execEnv),
                          list(cohort_min_length,
                               cohort_max_length,
                               cohort)]
    if(nrow(bins)==0){
      thisSeason<-seasonalSampling[sample_name==sample&
                                     drainage=="west",season]
      bins<-cohortBins[species==get('species',envir=execEnv)&
                         river==get('river',envir=execEnv)&
                         drainage==get('drainage',envir=execEnv)&
                         season==thisSeason,
                       list(meanMin=mean(cohort_min_length),
                            meanMax=mean(cohort_max_length)),
                       by=age]
      setkey(bins,age)
      bins[,cohort_max_length:=(meanMax+shift(meanMin,1,type='lead'))/2]
      bins[age==max(age),cohort_max_length:=meanMax]
      bins[,cohort_min_length:=c(meanMin[1],cohort_max_length[1:(nrow(bins)-1)])]
      bins[,cohort:=seasonalSampling[sample_name==sample&
                                       drainage==get('drainage',execEnv),year]-age]
      bins[,cohort_min_length:=cohort_min_length+0.01]
    }
    if(minLength>max(bins$cohort_max_length)&drainage=="west"){
      #if first length is bigger than the bins assigned for that stream, it probably came from west brook
      river<-"west brook"
      bins<-cohortBins[species==get('species',envir=execEnv)&
                         sample_name==get('sample',envir=execEnv)&
                         drainage==get('drainage',envir=execEnv)&
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

#started adding acoustic tag, but it shouldn't be necessary because pit is in acoustic data
# acousticTags<-tbl(conDplyr,"raw_stanley_acoustic_data") %>%
#               select(tag,acoustic_tag) %>%
#               distinct() %>%
#               collect() %>%
#               data.table()
# 
# getAcoustic<-function(tag,drainage){
#   if("west" %in% drainage){return(as.character(NA))}
#   
# }
getFirstLastSample<-function(sample,fun){
  return(as.character(fun(as.numeric(sample))))
}

#get data from from seasonal sampling by tag
dataByTag<-captures[,list(species=getSpecies(species,tag),
                          first_capture_sample=getFirstLastSample(sample_name,min),
                          last_capture_sample=getFirstLastSample(sample_name,max),
                          last_capture_date=max(detection_date),
                          cohort=getCohort(cohort,species,observed_length,sample_name,river,drainage),
                          sex=getSex(sex,tag)
                          ),by=tag]

#add in lastAntennaDetection from antenna data
antenna<-NULL
boundary_antennas <- c('a1','a2','03','04','05','06','wb above')
antennaStub<-"SELECT tag,detection_date,reader_id FROM"
for(nom in c("data_stationary_antenna","data_portable_antenna")){
  antenna<-rbind(antenna,
                 data.table(dbGetQuery(con,paste(antennaStub,nom)))
  )
}
setkey(antenna,tag)
antenna<-antenna[,list(last_antenna_detection=max(detection_date),
                       last_on_boundary=any(reader_id[which(detection_date==max(detection_date))]
                                               %in% boundary_antennas)
                      ),
                       by=tag]

dataByTag<-antenna[dataByTag]

emigrated<-function(lastAntenna,lastCapture,onBoundary){
  if(!onBoundary){return(as.Date(NA))}
  if(lastCapture>lastAntenna){return(as.Date(NA))
    } else {return(as.Date(lastAntenna))}
}

dataByTag[!is.na(last_on_boundary),
          date_emigrated:=emigrated(last_antenna_detection,
                                    last_capture_date,
                                    last_on_boundary),
          by=tag]
dataByTag[,':='(last_capture_date=NULL,
                last_on_boundary=NULL)]

#add dateKnownDead from tags_dead
dead<-data.table(dbGetQuery(con,"SELECT * FROM tags_dead"))
setkey(dead,tag)
dead[,date_known_dead:=as.Date(date_known_dead,format="%m/%d/%Y")]
dead<-dead[,list(date_known_dead=min(date_known_dead)),by=tag]

dataByTag<-dead[dataByTag]

family<-dbGetQuery(con,"SELECT tag, family_id FROM data_family") %>%
  data.table() %>%
  setkey(tag)

dataByTag<-family[dataByTag]


dbDropTable("data_by_tag")
dbWriteTable(con, 'data_by_tag', dataByTag, row.names=FALSE)

