#'needs to include year,season,medianDate,proportionSampled
#'
#'medianDate should fill in observedDate, add option to addJulian version,
#'year should be minimum year for the sample

addSampleProperties<-function(data){
  if(defaultColumns==T){
    columns<-c("proportion_sampled")
  }
}