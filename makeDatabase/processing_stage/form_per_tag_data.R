per_id_function <- function(id_history, cohort_table) {
		id_attributes <- with(data=id_history, expr={
			tag <- unique(tag)
			if (any(!is.na(species))) {
				species <- unique(species)[!is.na(unique(species))]
				if (length(species) != 1) species <- species[1]
			} else { species <- NA }

			## Field-declared cohort
			if (any(!is.na(cohort))) {
				cohort <- unique(cohort[!is.na(cohort)])
				if (length(cohort) != 1) cohort <- min(cohort)
			} else { cohort <- NA }

			## Lab-declared cohort
			if (tag %in% cohort_table[['tag']]) {
				cohort <- cohort_table[['cohort']][tag == cohort_table[['tag']] ]
			} 
			
			
			number_of_detections <- length(detection_date)	
			number_of_recaptures <- sum(status=='recaptured')

			first_capture <- ymd_hms(min(detection_date, na.rm=TRUE), truncated=3)
			if (!is.finite(first_capture)) first_capture <- NA
			last_recapture <- ymd_hms(max(detection_date[status == 'recaptured'], na.rm=TRUE), truncated=3)
			if (!is.finite(last_recapture)) last_recapture <- NA
			last_detection <- ymd_hms(max(detection_date, na.rm=TRUE), truncated=3)
			if (!is.finite(last_detection)) last_detection <- NA
			if (!is.na(cohort)) {
				min_cohort <- as.numeric(cohort) - 1
				first_possible_capture <- ymd(paste0(min_cohort,'-05-01'))
			} else {
				if (min(observed_length, na.rm=TRUE) < 100) {
					first_possible_capture <- first_capture - days(365*2)
				} else {
					first_possible_capture <- first_capture - days(1300)
				}
			}
			if (last_recapture == last_detection) {
				last_possible_capture <- last_detection + days(round(365*1.5))
			} else {
				last_possible_capture <- ymd_hms(last_detection, truncated=3) + days(round(365*1.5))
			}

			cohort_min <- year(first_possible_capture)
			if (month(first_capture) > 5) {
				cohort_max <- year(first_capture)
			} else {
				cohort_max <- year(first_capture) - 1
			}
      

			return(data.frame(
				tag = tag,
				species = species,
				cohort = cohort,
				cohort_min = cohort_min,
				cohort_max = cohort_max,
				first_capture = first_capture,
				last_recapture = last_recapture,
				last_detection = last_detection,
				number_of_detections = number_of_detections,
				number_of_recaptures = number_of_recaptures,
				possible_capture_lower_bound = first_possible_capture,
				possible_capture_upper_bound = last_possible_capture
			))
		})
	return(id_attributes)
}


tag_history <- dbGetQuery(link$conn, "SELECT * FROM data_corrected_tag_history;")
per_tag_history <- split(x=tag_history, f=tag_history[['tag']], drop=FALSE)

assigned_cohorts <- readRDS(
	file=file.path(original_data_dir,'cohort_assignment.rds')
)
colnames(assigned_cohorts) <- c('tag','cohort')
assigned_cohorts[['tag']] <- tolower(assigned_cohorts[['tag']])

id_tables <- mclapply(
	X=per_tag_history,
	FUN=per_id_function,
	cohort_table = assigned_cohorts
)

fail <- sapply(id_tables,function(x) class(x) == 'try-error')
if (any(fail)) {
	stop("Some id histories generated (silent) errors.")
}

if (any(lapply(id_tables,nrow) > 1)) 
	stop("Each id must contribute only one row to 'id_table'.")

id_table <- do.call(what=rbind, args=id_tables)

dbWriteTable(link$conn, 'data_per_tag', id_table, row.names=FALSE, overwrite=TRUE, append=FALSE)




#
#tag_melt <- melt(id_table,
#								 id.vars=c('tag','species','cohort','number_of_recaptures','number_of_detections'))
#tag_plot <- ggplot(
#	data=tag_melt[tag_melt[['tag']] %in% sample(x=names(id_tables), size=100),], 
#	aes(x=value, y=tag, colour=variable)) + geom_point(size=2)
#




 
