column_code <- list(
	tag = function(tag, species) {
#		return(paste(tag, species, sep='-'))
		return(tag)
	},
	fish_number = function(fish_number) {
		return(as.character(as.numeric(fish_number)))
	},
	species = function(species) return(species),
	cohort = function(cohort, tag) {  ## cohort is not well defined.
		return(cohort)
	},
	sample_number = function(sample_name,drainage) {
	  	sample_number<-sample_name_to_sample_number(sample_name,drainage)
	  	return(sample_number)
	},
	detection_date = function(date,time_of_capture) {
		require(lubridate)
	  date<-data.frame(strsplit(date," "))[1,]
	  time<-data.frame(strsplit(time_of_capture," "))[2,]
	  
	  datetime<-paste(date,time,sep=" ")
	  
		detection_date <- parse_date_time(x=datetime, orders=date.format)
		
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
  pass=function(pass) return(as.numeric(pass)),
	observed_length = function(measured_length) {
	  observed_length<-as.numeric(measured_length)
		return(ifelse(observed_length==0,NA,observed_length))
	},
  observed_weight = function(measured_weight){
    observed_weight = as.numeric(measured_weight)
    return(ifelse(observed_weight==0,NA,observed_weight))},
  maturity=function(maturity,sex){
    maturity[which(maturity %in% c("m","f"))]<-sex[which(maturity %in% c("m","f"))]
    return(maturity)},
  sex=function(sex,maturity){
    sex[which(sex=="p")]<-maturity[which(sex=="p")]
    return(sex)},
	survey = function(survey) return(survey),
	sample_name = function(sample_name) return(sample_name),
  comments = function(comments) return(comments)
)


source_data <- dbGetQuery(con, "SELECT * FROM tags_captures;")
source_data <- pipeline_data_transformation(
	data=source_data, pipeline=column_code)



dbWriteTable(con, 'data_tagged_captures', source_data, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)



