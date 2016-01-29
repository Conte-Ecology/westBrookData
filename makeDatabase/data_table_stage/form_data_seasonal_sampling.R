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

seasonalSampling[,proportion_sampled:=1]
seasonalSampling[year==2002&season==4&river=="west brook",proportion_sampled:=2/47]
seasonalSampling[river == 'west brook' & year == 2003 & season == 4, proportion_sampled:=30/47 ] 
seasonalSampling[ river == 'west brook' & year == 2004 & season == 4, proportion_sampled:=3/47 ]

fillProp0<-function(riv,y,seas){
  if(nrow(seasonalSampling[year==y&season==seas])>0&
     nrow(seasonalSampling[year==y&season==seas&river==riv])==0){
  newRow<-seasonalSampling[year==y&season==seas,
                            .(sample_name=unique(sample_name),
                              river=riv,
                              median_date=unique(median_date),
                              start_date=unique(median_date),
                              end_date=unique(median_date),
                              order=unique(order),
                              seasonal=unique(seasonal),
                              start_julian_day=as.numeric(format(unique(median_date),"%j")),
                              end_julian_day=as.numeric(format(unique(median_date),"%j")),
                              year=y,
                              sample_number=unique(sample_number),
                              season=seas,
                              proportion_sampled=0)]
  seasonalSampling<<-rbind(seasonalSampling,newRow)
  }
}
propsToFill<-data.table(river=c("west brook","west brook","wb jimmy","wb obear","wb mitchell"),
                        year= c( 2005,        2007,       2002,       2002,      2002),
                        season=c(4,           4,          4,          4,         4))
for(i in 1:nrow(propsToFill)){
  fillProp0(riv=propsToFill$river[i],
            y=propsToFill$year[i],
            seas=propsToFill$season[i])
}
# seasonalSampling[ river == 'west brook' & year == 2005 & season == 4, proportion_sampled:=0]
# seasonalSampling[ river == 'west brook' & year == 2007 & season == 4, proportion_sampled:=0 ]
# seasonalSampling[year==2002 & season==4 & river!="west brook",proportion_sampled:=0]

dbDropTable("data_seasonal_sampling")
dbWriteTable(con,name="data_seasonal_sampling",value=seasonalSampling,row.names=FALSE)
