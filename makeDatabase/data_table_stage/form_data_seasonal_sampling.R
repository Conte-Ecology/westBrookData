seasonalSampling<-data.table(dbGetQuery(con,"SELECT * FROM data_seasonal_sampling"))
season_map<-data.table(dbGetQuery(con,"SELECT * FROM season_map"))
getSeason<-function(sample_name,startDay,endDay){
  if(startDay<=endDay){
    seasons<-season_map[day %in% startDay:endDay,season_number]
  } else {
    seasons<-season_map[day %in% c(startDay:365,1:endDay),season_number]
  }
  if(length(unique(seasons))>1){
    warning(paste0("\nsample ",sample_name," crosses season boundary: ",
                   round(sum(seasons==unique(seasons[1]))/length(seasons)*100),
                         "% of days in one season"))}
  season<-round(median(seasons))
  return(season)
}

seasonalSampling[,season:=as.numeric(NA)]
for(i in 1:nrow(seasonalSampling)){
  seasonalSampling[i,season:=getSeason(sample_name,start_julian_day,end_julian_day)]
}
dbDropTable("data_seasonal_sampling")
dbWriteTable(con,name="data_seasonal_sampling",value=seasonalSampling,row.names=FALSE)
