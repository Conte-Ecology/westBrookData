id_table <- dbGetQuery(link$conn, "SELECT * FROM data_per_tag;")
id_tables <- split(x=id_table, f=id_table[['tag']])
tags <- names(id_tables)

sample_points <- readRDS(file=file.path(processed_data_dir,'sample_points.rds'))
sampling <- dbGetQuery(link$conn, "SELECT * FROM data_sampling ORDER BY sample_number;")

tag_history <- dbGetQuery(link$conn, "SELECT * FROM data_corrected_tag_history;")

if (any(tags != names(id_tables))) stop("Sort id_tables before continuing.")
if (any(tags != names(sample_points))) stop("Sort sample_points before continuing.")


sampling_rows <- mcmapply(
	FUN=function(tag, sample_points, tag_table, sampling, recaptures) {
		sample_points <- sample_points[!(sample_points %in% recaptures[[tag]][['sample_number']])]
		rows <- which(
			(sampling[['sample_number']] %in% sample_points) 
		)
		nr <- length(rows)
		if (nr == 0) return(NULL)
		day_diff <- difftime(sampling[rows,'end_date'], sampling[rows,'start_date'],units = 'days')
		class(day_diff) <- "numeric"
		day_diff <- days(round(day_diff*runif(nr,0,1))) 
		imputed_date <- sampling[rows,'start_date'] + day_diff
		o <- data.frame(
			tag = tag,
			species = tag_table[['species']],
			cohort = tag_table[['cohort']],
			sample_number = sampling[rows,'sample_number'],
			detection_date = imputed_date,
			river = as.character(NA),
			area = as.character(NA),
			section = as.character(NA),
			observed_length = as.numeric(NA),
			survey = "shock",
			sample_name = sampling[rows,'sample_name'],
			status = "uncaptured"
		)
		return(o)
	},
	tag = tags,
	sample_points = sample_points,
	tag_table = id_tables,
	MoreArgs = list(
		sampling=sampling,
		recaptures=split(x=tag_history, f=tag_history[['tag']])
	),
	SIMPLIFY=FALSE
)

sampling_rows <- batch_rbind(sampling_rows)

dbWriteTable(conn=link$conn, name='state_sampling_rows', value=sampling_rows, overwrite=TRUE, row.names=FALSE)





