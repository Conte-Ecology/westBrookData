sampling <- dbGetQuery(con, "SELECT * FROM data_seasonal_sampling;")
id_table <- dbGetQuery(con, "SELECT * FROM data_per_tag;")

sample_points <- mcmapply(
	FUN = function(tag, lb, ub, sampling) {
		relevant_rows <- 
			sampling[['start_date']] > lb & sampling[['end_date']] < ub
		return(sampling[relevant_rows,'sample_number'])
	},
	tag = id_table[['tag']],
	lb = id_table[['possible_capture_lower_bound']],
	ub = id_table[['possible_capture_upper_bound']],
	MoreArgs = list(sampling=sampling[sampling[['seasonal']],])
)

fail <- sapply(sample_points,function(x) class(x) == 'try-error')
if (any(fail)) {
	stop("Some sample point calculations generated (silent) errors.")
}

count_unknown_sample_numbers <- sum(sapply(sample_points, function(x) any(is.na(x))))
if (count_unknown_sample_numbers > 0)
	stop("All sample points must be known.")

sample_points <- lapply(sample_points, sort)

saveRDS(object=sample_points, file=file.path(processed_data_dir,'sample_points.rds'))
