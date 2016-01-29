# stmt <- paste0(
# 	"SELECT distinct(sample_name) FROM tags_captures;"
# )
sampling<-tbl(conDplyr,"tags_captures") %>%
            select(sample_name,river,date) %>%
            collect()

sampling<-sampling %>%
            mutate(date=parse_date_time(x=date, orders=date.format)) %>%
            group_by(sample_name) %>%
            mutate(median_date=median(date)) %>%
            ungroup() %>%
            group_by(sample_name,river) %>%
            summarize(median_date=median(median_date),
                      start_date=min(date),
                      end_date=max(date)) %>%
            ungroup() %>%
            mutate(order=as.numeric(sample_name))


seasonal_samples<-
    c("1",  "7",  "8",  "9", 
      "11", "15", "17", "18",
      "20", "23", "24", "25", 
      "27", "30", "31", "32", 
      "34", "36", "37", 
      "38", "40", "41", "41.8", 
      "43", "45", "46", "47", 
      "48", "49", "50", "51", 
      "52", "53", "54", "55", 
      "56", "57", "58", "59", 
      "60", "61", "62", "63", 
      "64", "65", "66", "67", 
      "68", "69", "70", "71", 
      "72", "73", "74", "75",
      "76", "77", "78", "79",
      "80", "81", "82", "83",
      "84", "85", "86", "87",
      "88", "89", "90", "91",
      "92", "93", "94")

sampling<- sampling %>%
            mutate(seasonal=sample_name %in% seasonal_samples) %>%
            mutate(start_julian_day=yday(start_date),
                   end_julian_day=yday(end_date),
                   year=year(start_date))

sampling<-sampling %>% #unless you group by sample year can be different for different rivers
          group_by(sample_name) %>%
          mutate(year=min(year)) %>%
          ungroup()

sample_number_map <- c(  
	 "1" = 10,  "7" = 11,  								 "8" = 12,
	 "9" = 13, "11" = 14, 								"15" = 15, "17" = 16,
	"18" = 17, "20" = 18, 								"23" = 19, "24" = 20,
	"25" = 21, "27" = 22, 								"30" = 23, "31" = 24,
	"32" = 25, "34" = 26, 	              "36" = 27, "37" = 28,
	"38" = 29, "40" = 30, 								"41" = 31, "41.8" = 32,
	"43" = 33, "45" = 34, 								"46" = 35, "47" = 36,
	"48" = 37, "49" = 38, 								"50" = 39, "51" = 40,
	"52" = 41, "53" = 42, 								"54" = 43, "55" = 44,
	"56" = 45, "57" = 46, 								"58" = 47, "59" = 48,
	"60" = 49, "61" = 50, 		  					"62" = 51, "63" = 52,
	"64" = 53, "65" = 54, 								"66" = 55, "67" = 56,
	"68" = 57, "69" = 58, 								"70" = 59, "71" = 60,
  "72" = 61, "73" = 62,                 "74" = 63, "75" = 64,
  "76" = 65, "77" = 66,                 "78" = 67, "79" = 68,
  "80" = 69, "81" = 70,                 "82" = 71, "83" = 72,
  "84" = 73, "85" = 74,                 "86" = 75, "87" = 76,
  "88" = 77, "89" = 78,                 "90" = 79, "91" = 80,
	"92" = 81, "93" = 82,                 "94" = 83
 )
sample_number_map <- as.list(sample_number_map)

sampling[['sample_number']] <- NA
for ( i in 1:nrow(sampling)) {
	idx <- as.character(sampling[i,'sample_name'])
	if (idx %in% names(sample_number_map)  ) {
		sampling[i,'sample_number'] <- sample_number_map[[idx]]
	}
}

dbWriteTable(conn=con, name='data_seasonal_sampling',value=data.frame(sampling),
						 overwrite=TRUE, row.names=FALSE)

## Embarassed to write code like this:  <3 !
with(data=shared_data, expr={
	sample_name_to_sample_number <- function(sample_name) {
		return(sample_number_map[sample_name])
	}
})

assign(x='sample_number_map', value= unlist(sample_number_map), envir=shared_data)

## End terrible... <3

#to define seasons need to group rivers
# sampling<-sampling %>%
#             group_by(sample_name,sample_number,order,seasonal) %>%
#             summarize(start_julian_day=min(start_julian_day),
#                       end_julian_day=max(end_julian_day),
#                       year=mean(year)) %>%
#             ungroup()

# sample_melt <- melt(
# 	data=sampling[,c('sample_name','order','seasonal','start_julian_day','end_julian_day','year')], 
# 	id.vars=c('sample_name','order','seasonal','year')
# )
# sample_melt[ sample_melt[['value']] < 20,'value'] <- 366
# pl_samples_by_name <- ggplot(
# 	data=sample_melt[sample_melt[['seasonal']],], 
# 	aes(x=value, y=year, colour=sample_name)
# ) + geom_line()
# 
# sample_melt <- melt(
# 	data=sampling[,c('sample_number','order','seasonal','start_julian_day','end_julian_day','year')], 
# 	id.vars=c('sample_number','order','seasonal','year')
# )
# sample_melt[ sample_melt[['value']] < 20,'value'] <- 366
# pl_samples_by_number <- ggplot(
# 	data=sample_melt[sample_melt[['seasonal']],], 
# 	aes(x=value, y=year, colour=factor(sample_number))
# ) + geom_line()

