sampling <- dbGetQuery(con, "SELECT * FROM data_seasonal_sampling WHERE seasonal IS TRUE;")
sampling <- sampling[order(sampling[['start_date']]),]

season_kmeans <- kmeans(x=sampling$start_julian_day, centers=c(90,15,280,350))
row_cluster <- season_kmeans[['cluster']]
cluster_centers <- season_kmeans[['centers']]
season_from_cluster <- order(cluster_centers)[row_cluster]

sampling[['season']] <- season_from_cluster

sample_melt <- melt(
  data=sampling[,c('sample_number','order','seasonal','start_julian_day','end_julian_day','year','season')],
  id.vars=c('sample_number','order','seasonal','year','season')
)
sample_melt[ sample_melt[['value']] < 20,'value'] <- 366
pl_samples_by_number <- ggplot(
  data=sample_melt[sample_melt[['seasonal']],],
  aes(x=value, y=year, colour=factor(sample_number), size=factor(season))
) + geom_line()


season_breaks <- aggregate(
	formula = start_julian_day ~ season,
	data = sampling,
	FUN=function(x) sort(x)[1]  #### K had the third earliest sampling date, but that made a bunch of samples overlap seasons
)

pl_sampling_points <- ggplot(
	data=sampling, 
	aes(x=start_julian_day, fill=factor(season))
) + geom_histogram() +
		geom_vline(xintercept=season_breaks[['start_julian_day']], colour='blue') +
		geom_vline(xintercept=sort(cluster_centers), colour='green')

## We used the start of sampling, but we're calculating the END of the
## season.
names(season_breaks)[names(season_breaks) == 'start_julian_day'] <- 'end_julian_day'

dbWriteTable(conn=con, name='data_season', value=season_breaks,
						 row.names=FALSE, overwrite=TRUE, append=FALSE)

