recaptures <- dbGetQuery(link$conn, 
	statement = "SELECT * FROM data_seasonal_captures;")

recaptures[['status']] <- 'recaptured'

tags <- unique(recaptures[['tag']])

trap_recaptures <- dbGetQuery(link$conn, "SELECT * FROM data_trap_captures;")
trap_recaptures <- trap_recaptures[ trap_recaptures[['tag']] %in% tags,]
boundary_detections <- dbGetQuery(link$conn, "SELECT * FROM data_boundary_detections;")
boundary_detections <- boundary_detections[ boundary_detections[['tag']] %in% tags,]

detections <- rbind(
	data.frame(
		trap_recaptures[,c('tag','detection_date','observed_length','survey')],
		sample_name='trap', status='trap_recapture', fish_number=as.numeric(NA)),
	data.frame(boundary_detections[,c('tag','detection_date','survey')], 
		observed_length=NA, sample_name='antenna_detection',
		status='boundary_detection', fish_number=as.numeric(NA))
)

extra_columns_for_detections <- names(recaptures)[!(names(recaptures) %in% names(detections))]
extra_columns_for_recaptures <- names(detections)[!(names(detections) %in% names(recaptures))]
for (col in extra_columns_for_detections) { detections[[col]] <- NA }
for (col in extra_columns_for_recaptures) { recaptures[[col]] <- NA }

detections <- detections[,names(recaptures)] ## Sets column order!
tag_history <- do.call(what=rbind, args=list(recaptures, detections))
tag_history <- tag_history[order(tag_history[['tag']], tag_history[['detection_date']]),]



#BUGGY AND NEED TO REVISIT, SO COMMENTING COHORT ASSIGNMENT FOR INDIVIDUALS WITHOUT FIELD CALLS FOR THE MOMENT 12/3/2015
#########################################################################
# Estimates cohortEstimated for those that don't have one assigned
# yoy_bins<-fread(file.path(processed_data_dir,"yoy_bins.csv"))
# tag_history<-data.table(tag_history)
# tag_history[,cohort := as.numeric(cohort)]
# tag_history[,cohort := ifelse(any(!is.na(cohort)),
#                         min(cohort[which(!is.na(cohort))]),
#                         as.numeric(NA)),
#       by=tag]
# tag_history[,cohortEstimated:=cohort]
# 
# hatch.year.no.tag<-function(Length,Sample,Species,River){
#   if(all(is.na(Length))){return(as.numeric(NA))} else{
#     
#     #A few samples/species/rivers don't have cohortEstimateds assigned, and these adjustments use the best substitute to fill the gaps
#     if(Sample == 41.8){Sample <- 41}
#     if(Species== 'bnt' & Sample %in% c(40:54,56:60) & River == 'wb jimmy'){River <- 'west brook'} #No assignments have been made for jimmy pre sample 60, but river specific breaks could be determined
#     if(Species== 'bnt' & Sample ==55 & River == 'wb jimmy'){River <- 'wb mitchell'} #No assignments have been made for jimmy pre sample 60, but river specific breaks could be determined
#     if(Species== 'bnt' & Sample == 40 & River == 'wb mitchell'){River <- 'west brook'} #no bins assinged for this sample in mitchell
#     if(Species== 'bnt' & Sample == 47 & River == 'wb mitchell' & Length>200){River <- 'west brook'} #these guys are bigger than the largest bin that was assigned
#     if(Species== 'bnt' & Sample == 82 & River == 'wb jimmy' & Length>210){River <- 'west brook'} #bigger than assigned bins
#     if(Species== 'bnt' & Sample == 91 & River == 'wb mitchell' & Length == 90){return(2014)}
#     if(Species== 'bkt' & Sample == 74 & River == 'wb jimmy' & Length>190){River <- 'west brook'}
#     if(Species== 'ats' & River != 'west brook'){River<-'west brook'}
#     
#     upper<-yoy_bins[sample==Sample&species==Species&river==River,cohort_max_length]
#     lower<-yoy_bins[sample==Sample&species==Species&river==River,cohort_min_length]
#     hatch_year<-yoy_bins[sample==Sample&species==Species&river==River,cohort][
#       intersect(which(Length<=upper),which(Length>=lower))]
#     if(length(hatch_year)>0) {return(as.numeric(hatch_year))} else{
#       return(as.numeric(NA))
#     }
#   }
# }
# 
# hatch.year.tag<-function(Length,Sample,Species,River) {
#   if(all(is.na(Length))){return(as.numeric(NA))} else{
#     firstObs<-which(Length==min(Length,na.rm=T))[1]
#     return(hatch.year.no.tag(Length[firstObs],Sample[firstObs],Species[firstObs],River[firstObs]))
#   }}
# 
# tag_history[is.na(cohortEstimated),cohortEstimated:=
#         hatch.year.tag(Length=observed_length,
#                        Sample=sample_name,Species=species,River=river),
#       by=tag]

dbWriteTable(link$conn, 'data_tag_history', tag_history, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)



