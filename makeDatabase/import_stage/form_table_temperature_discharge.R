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




