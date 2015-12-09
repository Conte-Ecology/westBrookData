
covariate_pipeline <- list(
	cohort = function(cohort, verified_cohort) {
		cohort <- verified_cohort 
		return(cohort)
	},
	age_group = function(detection_date, verified_cohort, birthday, breaks) {
		if (is.na(verified_cohort)) {
			return(as.numeric(NA))
		} else {
			age_group <- sapply(
				X=detection_date, FUN=age, birthday=birthday, breaks=breaks)
			return(age_group)
		}
		stop("Something is rotten in Denmark.")
	},
	start_date = function(detection_date) {
		l <- length(detection_date)
		date <- c(ymd_hms(NA),detection_date[1:(l-1)])
		return(date)
	},
	stop_date = function(detection_date) {
		l <- length(detection_date)
		date <- detection_date[1:l]
		return(date)
	},
	zst = function(start_date, stop_date, covariate_table) {
		measurement_dates <- covariate_table[['date_ct']]
		measurements <- covariate_table[['zst']]

		## Apply to whole set of start_date/stop_date (s).
		summary <- mapply(
			FUN=function(start_date, stop_date, measurement_dates, measurements) {
				## NA's
				if (is.na(start_date) || is.na(stop_date)) return(as.numeric(NA))

				## Subset:
				included <- 	
					measurement_dates >  start_date & 
					measurement_dates <= stop_date &
					!is.na(measurements)
			
				## Quality check:
				##measurement_dates <- measurement_dates[included]\
				days_in_interval <- round(as.numeric(difftime(stop_date, start_date, units='days'))) + 1
				##days_in_interval <- suppressWarnings(expr={days(round(stop_date - start_date))})
				days_measured <- sum(included)
				if ( (days_measured / days_in_interval) < 0.8 ) return(as.numeric(NA))
			
				## Reduction:
				summary <- subset_reduction(included, measurements, mean)
		
			},
			start_date = start_date,
			stop_date = stop_date,
			MoreArgs=list(
				measurement_dates=measurement_dates,
				measurements=measurements
			), SIMPLIFY=TRUE
		)
		
		return(summary)	
	},
	zst_day = function(detection_date, covariate_table) {
		## Reduce granularity.
		measurement_days <- yday(covariate_table[['date_ct']])
		measurement_years <- year(covariate_table[['date_ct']])

		detection_day <- yday(detection_date)
		detection_year <- year(detection_date)

		## Find t's
		measurement <- mapply(
			FUN=function(day, year, m_days, m_years, m_zst) {
				measurement_idx <- which(m_days==day & m_years==year)
				if (length(measurement_idx) == 0)
					return(as.numeric(NA))
				else 
					return(m_zst[measurement_idx][1])
			}, 
			day=detection_day, year=detection_year, 
			MoreArgs=list(m_days=measurement_days, m_years=measurement_years, m_zst = covariate_table[['zst']]),
			SIMPLIFY=TRUE
		)
		return(measurement)	
	},
	zsd = function(start_date, stop_date, covariate_table) {
		measurement_dates <- covariate_table[['date_ct']]
		measurements <- covariate_table[['zsd']]

		## Apply to whole set of start_date/stop_date (s).
		summary <- mapply(
			FUN=function(start_date, stop_date, measurement_dates, measurements) {
				## NA's
				if (is.na(start_date) || is.na(stop_date)) return(as.numeric(NA))

				## Subset:
				included <- 	
					measurement_dates >  start_date & 
					measurement_dates <= stop_date &
					!is.na(measurements)
			
				## Quality check:
				##measurement_dates <- measurement_dates[included]\
				days_in_interval <- round(as.numeric(difftime(stop_date, start_date, units='days'))) + 1
				##days_in_interval <- suppressWarnings(expr={days(round(stop_date - start_date))})
				days_measured <- sum(included)
				if ( (days_measured / days_in_interval) < 0.8 ) return(as.numeric(NA))
			
				## Reduction:
				summary <- subset_reduction(included, measurements, mean)
		
			},
			start_date = start_date,
			stop_date = stop_date,
			MoreArgs=list(
				measurement_dates=measurement_dates,
				measurements=measurements
			), SIMPLIFY=TRUE
		)
		
		return(summary)	
	},
	zsd_day = function(detection_date, covariate_table) {
		## Reduce granularity.
		measurement_days <- yday(covariate_table[['date_ct']])
		measurement_years <- year(covariate_table[['date_ct']])

		detection_day <- yday(detection_date)
		detection_year <- year(detection_date)

		## Find t's
		measurement <- mapply(
			FUN=function(day, year, m_days, m_years, m_zsd) {
				measurement_idx <- which(m_days==day & m_years==year)
				if (length(measurement_idx) == 0)
					return(as.numeric(NA))
				else 
					return(m_zsd[measurement_idx][1])
			}, 
			day=detection_day, year=detection_year, 
			MoreArgs=list(m_days=measurement_days, m_years=measurement_years, m_zsd = covariate_table[['zsd']]),
			SIMPLIFY=TRUE
		)

		return(measurement)	
	},
	log10_discharge_day = function(detection_date, covariate_table) {
		## Reduce granularity.
		measurement_days <- yday(covariate_table[['date_ct']])
		measurement_years <- year(covariate_table[['date_ct']])

		detection_day <- yday(detection_date)
		detection_year <- year(detection_date)

		## Find t's
		measurement <- mapply(
			FUN=function(day, year, m_days, m_years, m_daily_log10_discharge) {
				measurement_idx <- which(m_days==day & m_years==year)
				if (length(measurement_idx) == 0)
					return(as.numeric(NA))
				else 
					return(m_daily_log10_discharge[measurement_idx][1])
			}, 
			day=detection_day, year=detection_year, 
			MoreArgs=list(
				m_days=measurement_days, 
				m_years=measurement_years, 
				m_daily_log10_discharge = covariate_table[['daily_log10_discharge']]),
			SIMPLIFY=TRUE
		)
		return(measurement)	
	},
	surviving = function(stop_date, recruit_date, last_detection) {
		surviving <- rep(0,length(stop_date))
		known_surviving <-
			recruit_date < stop_date &
			stop_date <= last_detection
		surviving[known_surviving] <- 1
		return(surviving)
	},
	tagged = function(status) {
		tagged <- vector(mode='numeric', length=length(status))
		first <- min(which(status == 'recaptured'))
		tagged[(first+1):length(tagged)] <- 1
		return(tagged)
	},
	censored = function(status) {
		censored <- vector(mode='numeric', length=length(status))
		if (!any(status == 'boundary_detection') &&
				!any(status == 'trap_recapture'    )
		) {
			return(censored)
		} else {
			last <- max(which(status %in% c('trap_recapture','boundary_detection')))
			censored[(last+1):length(censored)] <- 1
		}
		return(censored)
	},
	cjs_classification = function(status, surviving, censored, tagged) {
		cjs_classification <- vector(mode='numeric', length=length(status))
		cjs_classification[tagged == 1 & surviving == 1 & status == 'recaptured'] <- 1
		cjs_classification[tagged == 1 & surviving == 1 & status == 'uncaptured'] <- 2
		cjs_classification[tagged == 1 & surviving == 1 & status == 'boundary_detection'] <- 3
		cjs_classification[tagged == 1 & surviving == 1 & status == 'trap_recapture'] <- 4
		cjs_classification[tagged == 1 & surviving == 1 & status == 'season_break'] <- 5
		cjs_classification[tagged == 1 & surviving == 0 & censored == 0 & 
											 (status == 'season_break' | status == 'uncaptured')] <- 6
		cjs_classification[tagged == 1 & surviving == 0 & censored == 1] <- 7
		cjs_classification[tagged == 0 & status == 'recaptured'] <- 8
		return(cjs_classification)
	},
	cjs_row_type = function(cjs_classification) {
		cjs_row_type <- vector(mode='character', length=length(cjs_classification))
		cjs_row_type[ cjs_classification == 6 ] <- 'ambiguous'
		cjs_row_type[ cjs_classification != 6 ] <- 'unambiguous'
		cjs_row_type[ cjs_classification == 8 ] <- 'first-capture'
		cjs_row_type[ cjs_classification == 0 ] <- 'pre-capture'
		return(cjs_row_type)
	},
	antennas_functional = function(stop_date, antennas_start) {
		antennas_functional <- vector(mode='numeric', length=length(stop_date))
		antennas_functional[stop_date >= antennas_start] <- 1
		return(antennas_functional)
	},
	interval = function(start_date, stop_date) {
		interval <- difftime(stop_date, start_date, units='days')
		return(interval)
	},
	pivot_row = function(cohort, stop_date) {
		pivot_row <- vector(mode='numeric', length=length(stop_date))
		cohort <- unique(cohort)[!is.na(unique(cohort))]
		if (length(cohort)==0) {
			center <- stop_date[round(length(stop_date)/2)]
		} else {
			center <- ymd(paste0(as.numeric(cohort[1])+1,'-05-20'))
		}
		distances <- as.numeric(stop_date - center)
		pivot <- which(abs(distances) == min(abs(distances)))
		pivot_row[1:(pivot-1)] <- 0
		pivot_row[pivot] <- 1
		pivot_row[(pivot+1):length(stop_date)] <- 2
		return(pivot_row)
	},
	season_number = function(age_group) {
		season_number <- age_group %% 4 + 1
		return(season_number)
	},
	season = function(season_number) {
		season <- c('spring','summer','autumn','winter')[season_number]
		return(season)
	},
	smolt_season = function(season_number, cjs_classification) {
		smolt_season <- season_number
		smolt_season[cjs_classification == 4] <- 1
		return(smolt_season)
	},
	age_year = function(age_group) {
		age_year <- age_group %/% 4 + 1
		return(age_year)
	},
	observed_growth = function(observed_length, tag) {
		observed_growth <- c(NA,diff(observed_length))
		first_tag_row <- tag %>% factor %>% as.numeric %>% diff %>% c(1,.)
		observed_growth[first_tag_row] <- NA
		return(observed_growth)
	}
)



