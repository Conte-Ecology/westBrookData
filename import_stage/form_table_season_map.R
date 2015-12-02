stmt <- paste0( "SELECT * FROM season_data;")
season_breaks <- dbGetQuery(link$conn, stmt)

days_of_year_set <- 1:366
season_end <- season_breaks[['end_julian_day']]

intervals <- list(
	spring = days_of_year_set[
		days_of_year_set > season_end[1] & days_of_year_set <= season_end[2]],
	summer = days_of_year_set[
		days_of_year_set > season_end[2] & days_of_year_set <= season_end[3]],
	autumn = days_of_year_set[
		days_of_year_set > season_end[3] & days_of_year_set <= season_end[4]],
	winter = days_of_year_set[
		days_of_year_set <= season_end[1] | days_of_year_set > season_end[4]]
)
for (i in names(intervals)) {
	intervals[[i]] <- data.frame(intervals[[i]],i)
	names(intervals[[i]]) <- c('day','season_name')
}
intervals <- do.call(what=rbind, args=intervals)
season_map <- intervals[order(intervals[['day']]),]
season_map[['season_number']] <- rep(NA,nrow(season_map))
season_map[season_map[['season_name']] == 'spring','season_number'] <-1
season_map[season_map[['season_name']] == 'summer','season_number'] <-2
season_map[season_map[['season_name']] == 'autumn','season_number'] <-3
season_map[season_map[['season_name']] == 'winter','season_number'] <-4

dbWriteTable(conn=link$conn, name='season_map', value=season_map,
						 row.names=FALSE, overwrite=TRUE, append=FALSE)

day_of_year_to_season <- function(day, output='season_name') {
	if (!(output %in% c('season_name','season_number'))) {
		stop("Output type must be 'season_name' or 'season_number'.")}
	if (all(day >= 1 & day <= 366)) {
		return(map[day,output])
	} else {
		stop("Argument 'day' must be (1,366).")
	}
}
assign(x='map', value=season_map)
assign('day_of_year_to_season',day_of_year_to_season,envir=shared_data)

day_count <- aggregate(
	formula = duration ~ season_name + season_number,
	data = data.frame(season_map,duration=1),
	FUN=length
)

season_breaks <- merge(
	x=season_breaks, by.x='season', y=day_count, by.y='season_number')


dbWriteTable(conn=link$conn, name='season_data', value=season_breaks,
						 row.names=FALSE, overwrite=TRUE, append=FALSE)


	


