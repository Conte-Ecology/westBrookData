id_table <- dbGetQuery(con, "SELECT * FROM data_per_tag;")
id_tables <- split(x=id_table, f=id_table[['tag']])
tags <- names(id_tables)

occasion_points <- readRDS(file=file.path(processed_data_dir,'occasion_points.rds'))
occasions <- dbGetQuery(con, "SELECT * FROM data_occasions;")

if (any(tags != names(id_tables))) stop("Sort id_tables before continuing.")
if (any(tags != names(occasion_points))) stop("Sort occasion_points before continuing.")

occasion_rows <- mcmapply(
	FUN=function(tag, occasion_points, tag_table, occasion) {
		rows = occasion_points
		o <- data.frame(
			tag = tag,
			species = tag_table[['species']],
			cohort = tag_table[['cohort']],
			sample_number = as.numeric(NA),
			detection_date = occasion[rows,'detection_date'],
			river = as.character(NA),
			area = as.character(NA),
			section = as.character(NA),
			observed_length = as.numeric(NA),
			survey = "shock",
			sample_name = 'pretend',
			status = "season_break"
		)
		return(o)
	},
	tag = tags,
	occasion_points = occasion_points,
	tag_table = id_tables,
	MoreArgs = list(occasion=occasions),
	SIMPLIFY=FALSE
)

occasion_rows <- batchRbind(occasion_rows)

dbWriteTable(conn=con, name='state_occasion_rows', value=occasion_rows, overwrite=TRUE, row.names=FALSE)




