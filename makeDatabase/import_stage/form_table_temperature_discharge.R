## Functions:
day_to_season <- function(day) {
  if (day > 66 & day <= 139)
    return('spring')
  if (day > 139 & day <= 246)
    return('summer')
  if (day > 246 & day <= 334)
    return('autumn')
  if (day > 334 | day <=  66)
    return('winter')
}


## Data cleaning:
ed <- read.csv(
  file = file.path(original_data_dir,'westbrook_core_environmental_data.csv'), 
  stringsAsFactors=FALSE, colClasses='character',check.names=FALSE) 
names(ed) <- tolower(names(ed))

name_map <- list(
  temperature = 'avg daily temp',  
  discharge = 'avg daily discharge',
  temperature_source = 'temp source',
  discharge_source = 'discharge source'
)

colnames(ed) <- map(x=colnames(ed), map=name_map)

keep <- c(
  'drainage', 'river', 
  'date',
  'temperature', 'temperature_source',
  'discharge', 'discharge_source'
)

ed <- ed[,keep]

ed[['date_ct']] <- parse_date_time(x=ed[['date']], orders=c('mdy')) 
bad <- ed[['date_ct']] > now()
ed[['date_ct']][bad] <- ed[['date_ct']][bad] - years(100)
ed[['temperature']] <- as.numeric(ed[['temperature']])
ed[['discharge']] <- as.numeric(ed[['discharge']])

ed <- ed[tolower(ed[['temperature_source']]) == 'sec 6 dl',]
ed <- ed[tolower(ed[['discharge_source']]) == 'flow extension',]
ed <- ed[order(ed[['date_ct']]),]
ed[['month']] <- month(ed[['date_ct']])
ed[['year']] <- year(ed[['date_ct']])
ed[['day_of_year']] <- yday(ed[['date_ct']])
ed[['season']] <- sapply(ed[['day_of_year']], day_to_season)

## Save cleaned daily data.
dbWriteTable(conn=con, name='data_environmental', value=ed,
             row.names=FALSE, overwrite=TRUE, append=FALSE)

## Construct smooth for temperature:
temperature_smooth <- loess(formula = temperature ~ day_of_year, data=ed, span =1/5)
temperature_average <- data.frame(
  day_of_year=1:366, 
  temperature=predict(object=temperature_smooth, newdata=data.frame(day_of_year=1:366))
)
temperature_average[['season']] <- sapply(temperature_average[['day_of_year']], day_to_season)


## Calculate seasonal mean for temperature and merge:
temperature_seasonal_mean <- aggregate(formula = temperature ~ season, data=ed, FUN=mean, na.rm=TRUE)
colnames(temperature_seasonal_mean)[colnames(temperature_seasonal_mean) == 'temperature'] <- 
  'seasonal_mean_temperature'
temperature_average <- merge(x=temperature_average, y=temperature_seasonal_mean, by='season')
temperature_average <- temperature_average[order(temperature_average[['day_of_year']]),]

## Construct smooth for discharge:
discharge_smooth <- loess(formula = log10(discharge) ~ day_of_year, data=ed, span =1/4)
discharge_average <- data.frame(
  day_of_year=1:366, 
  discharge=predict(object=discharge_smooth, newdata=data.frame(day_of_year=1:366))
)
discharge_average[['season']] <- sapply(discharge_average[['day_of_year']], day_to_season)

## Calculate seasonal mean for temperature:
discharge_seasonal_mean <- aggregate(formula = log10(discharge) ~ season, data=ed, FUN=mean, na.rm=TRUE)
colnames(discharge_seasonal_mean)[colnames(discharge_seasonal_mean) == 'log10(discharge)'] <- 
  'seasonal_mean_log10_discharge'
discharge_average <- merge(x=discharge_average, y=discharge_seasonal_mean, by='season')
discharge_average <- discharge_average[order(discharge_average[['day_of_year']]),]

## Smoothed environmental data:
average_ed <- data.frame(
  day_of_year = temperature_average[['day_of_year']], 
  typical_temperature = temperature_average[['temperature']],
  seasonal_mean_temperature = temperature_average[['seasonal_mean_temperature']],
  typical_log10_discharge=discharge_average[['discharge']],
  seasonal_mean_discharge = discharge_average[['seasonal_mean_log10_discharge']],
  season = temperature_average[['season']]
)
dbWriteTable(conn=con, name='data_environmental_average',
             value=average_ed, row.names=FALSE, overwrite=TRUE, append=FALSE)




# Merge:
edj <- merge(x=ed, y=average_ed, by='day_of_year')
edj[['daily_deviation_from_typical_temperature']] <-
  edj[['temperature']] - edj[['typical_temperature']]
edj[['zst']] <- scale(edj[['daily_deviation_from_typical_temperature']])[,1]
edj[['daily_deviation_from_seasonal_mean_temperature']] <- 
  edj[['temperature']] - edj[['seasonal_mean_temperature']]
edj[['zstm']] <- scale(edj[['daily_deviation_from_seasonal_mean_temperature']])[,1]
edj[['daily_log10_discharge']] <- log10(edj[['discharge']])
edj[['daily_deviation_from_typical_log10_discharge']] <-
  log10(edj[['discharge']]) - edj[['typical_log10_discharge']]
edj[['zsd']] <- scale(edj[['daily_deviation_from_typical_log10_discharge']])[,1]
edj[['daily_deviation_from_seasonal_mean_discharge']] <- 
  log10(edj[['discharge']]) - edj[['seasonal_mean_discharge']]
edj[['zsdm']] <- scale(edj[['daily_deviation_from_seasonal_mean_discharge']])[,1]
edj[['season']] <- sapply(edj[['day_of_year']], day_to_season)
dbWriteTable(conn=con, name='data_environmental_with_zst_zsd', value=edj, overwrite=TRUE, row.names=FALSE)





