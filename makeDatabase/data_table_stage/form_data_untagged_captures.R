column_code <- list(
	fish_number = function(fish_number) {
		return(as.character(as.numeric(fish_number)))
	},
	species = function(species) return(species),
	cohort = function(cohort,comments,date,measured_length) {
	  cohort<-round(as.numeric(cohort))
	  smallNoLength<-which(is.na(cohort)&grepl("too small",comments)&is.na(measured_length))
	  datesForTooSmall<-parse_date_time(x=date[smallNoLength], orders=date.format)
	  lastYear<-as.numeric(yday(datesForTooSmall)<130)
	  cohort[smallNoLength]<-year(datesForTooSmall)-lastYear
		return(cohort)
	},
	sample_number = function(sample_name,drainage) {
	  sample_number<-sample_name_to_sample_number(sample_name,drainage)
	  return(sample_number)
	},
	detection_date = function(date) {
		require(lubridate)
		detection_date <- parse_date_time(x=date, orders=date.format)
		detection_date[year(detection_date)<100]<-detection_date[year(detection_date)<100]+years(2000)
		detection_date[detection_date > now()] <- 
			detection_date[detection_date > now()] - years(100)
		return(detection_date)
	},
	season_number =  function(detection_date) {
		season <- day_of_year_to_season(yday(detection_date), output='season_number')
		return(season)
	},
	drainage = function(drainage,river){
	  drainage[river %in% c("west brook","wb jimmy","wb mitchell","wb obear")]<-
	    "west"
	  return(drainage)
	},
	river = function(river) return(river),
	area = function(area){
	          area[area=="aboveabove"]<-"above above"
	          return(area)},
	section = function(section) return(unlist(strsplit(section,"\\.00"))),
	observed_length = function(measured_length) {
	  observed_length<-as.numeric(measured_length)
		return(ifelse(observed_length==0,NA,observed_length))
	},
  observed_weight = function(measured_weight){
    observed_weight = as.numeric(measured_weight)
    return(ifelse(observed_weight==0,NA,observed_weight))},
  maturity=function(maturity){return(maturity)},
  sex=function(sex){return(sex)},
	survey = function(survey) return(survey),
	sample_name = function(sample_name) return(sample_name),
	comments = function(comments) return(comments)
)


source_data <- dbGetQuery(con, "SELECT * FROM raw_captures WHERE tag is NULL;")
untaggedCaptures <- pipeline_data_transformation(
	data=source_data, pipeline=column_code) %>% data.table()

cohortBins<-data.table(dbGetQuery(con,"SELECT * FROM data_yoy_bins")) %>%
  .[,drainage:="west"]

seasonalSampling<-
  data.table(
    dbGetQuery(con,"SELECT distinct sample_name,season,year,drainage FROM data_seasonal_sampling")
  )

setkey(cohortBins,sample_name,drainage)
setkey(seasonalSampling,sample_name,drainage)
cohortBins<-seasonalSampling[cohortBins]

getCohort<-function(species,length,sample,river,drainage){
  if(drainage=="stanley"){return(as.numeric(NA))}
  execEnv<-environment()
  if(length(species)==0|!species %in% c("bkt","bnt","ats")) return(as.numeric(NA))
    if(is.na(length)) return(as.numeric(NA))
    if(species=='ats'){river<-'west brook'}#bins only assigned in west brook for salmon
    bins<-cohortBins[species==get('species',envir=execEnv)&
                       sample_name==get('sample',envir=execEnv)&
                       river==get('river',envir=execEnv) ,
                     list(cohort_min_length,
                          cohort_max_length,
                          cohort)]
    if(nrow(bins)==0){
      thisSeason<-unique(seasonalSampling[sample_name==as.numeric(sample)&
                                            drainage=="west",season])
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
      bins[,cohort:=seasonalSampling[sample_name==as.numeric(sample)&
                                       drainage=="west",unique(year)]-age]
    }
    if(length>max(bins$cohort_max_length)){
      #if first length is bigger than the bins assigned for that stream, it probably came from west brook
      river<-"west brook"
      bins<-cohortBins[species==get('species',envir=execEnv)&
                         sample_name==as.numeric(get('sample',envir=execEnv))&
                         river==get('river',envir=execEnv) ,
                       list(cohort_min_length,
                            cohort_max_length,
                            cohort)]
    }
    cohort<-bins$cohort[intersect(which(length>=bins$cohort_min_length),
                                  which(length<bins$cohort_max_length))]
    
    return(cohort)
}

untaggedCaptures[,cohort:=as.numeric(cohort)]
for(i in which(is.na(untaggedCaptures$cohort)&!is.na(untaggedCaptures$observed_length))){
  set(untaggedCaptures,i,which(names(untaggedCaptures)=="cohort"),
      with(untaggedCaptures,
      getCohort(species[i],observed_length[i],sample_name[i],river[i],drainage[i])))
}

dbWriteTable(con, 'data_untagged_captures', data.frame(untaggedCaptures), row.names=FALSE,
						 overwrite=TRUE, append=FALSE)



