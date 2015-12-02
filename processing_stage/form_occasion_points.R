id_table <- dbGetQuery(link$conn, "SELECT * FROM data_per_tag;")

sampling <- dbGetQuery(link$conn, "SELECT * FROM data_sampling;")
season_breaks <- dbGetQuery(link$conn, "SELECT * FROM season_data;")

start_year <- year(min(sampling[['start_date']])) - 4
end_year <- year(max(sampling[['start_date']])) + 4
number_of_years <- end_year - start_year + 1

occasions <- data.frame(
	season_number = rep(1:4, number_of_years),
	year = sort(rep(start_year:end_year, 4)),
	detection_date = as.POSIXct(NA)
)

for ( i in start_year:end_year ) {
	first_day <- ymd_hms(paste0(i,'-01-01 00:00:10')	)
	for ( j in 1:nrow(season_breaks)) {
		occasions[
			occasions[['year']] == i &
			occasions[['season_number']] == j,
			'detection_date'
		] <- first_day + days(season_breaks[j,'end_julian_day'] - 1)
	}
}
occasions[['occasion_number']] <- 1:nrow(occasions)


occasion_points <- mcmapply(
	FUN = function(tag, lb, ub, occasion) {
		relevant_rows <- 
			occasion[['detection_date']] > lb & occasion[['detection_date']] < ub
		return(occasion[relevant_rows,'occasion_number'])
	},
	tag = id_table[['tag']],
	lb = id_table[['possible_capture_lower_bound']],
	ub = id_table[['possible_capture_upper_bound']],
	MoreArgs = list(occasion=occasions)
)

fail <- sapply(occasion_points,function(x) class(x) == 'try-error')
if (any(fail)) {
	stop("Some occasion point calculations generated (silent) errors.")
}

count_unknown_sample_numbers <- sum(sapply(occasion_points, function(x) any(is.na(x))))
if (count_unknown_sample_numbers > 0)
	stop("All occasion points must be known.")

occasion_points <- lapply(occasion_points, sort)

saveRDS(object=occasion_points, file=file.path(processed_data_dir,'occasion_points.rds'))
dbWriteTable(conn=link$conn, name='data_occasions',value=occasions, overwrite=TRUE, row.names=FALSE)




