#'Add section properties (stream width by section for each sample) to a data frame. Missing data are estimated based on log(width) ~ log(stream flow))
#'@return A data.frame appended with sample properties
#'@param data A data.frame created with createCoreData(), which must include river and section as a column 
#'@param basecolumns Logical: include default columns? river,sampleName,width, maximum_depth
#'@param columnsToAdd A character vector of columns to inlcude; can replace or add to baseColumns
#'@param estimatedWidth Logical: estimate widths for missing data? Default = T
#'@export


addSectionProperties <- 
function(data, defaultColumns = T, columnsToAdd = NULL, estimateWidthNA = T) {

  reconnect()
  
  if ( 'widthToUse' %in% names(data) ) data <- select(data, -widthToUse)
  if ( 'widthDataSource' %in% names(data) ) data <- select(data, -widthDataSource)
  
  whichDrainage <- "west"
  if (all(!unique(data$river) %in% c("west brook", "wb jimmy", 
                                     "wb mitchell", "wb obear"))) {
    whichDrainage <- "stanley"
  }
  
  if (defaultColumns == T) {
    chosenColumns <- c("river", "sample_name", "section", "width", "maximum_depth")
  }
  
  chosenColumns <- c(chosenColumns, columnsToAdd) %>% fillUnderscore()
  
  if (is.null(chosenColumns)) 
    stop("Must choose at least one column to add")
  
  rawWidths <- data.frame(tbl(conDplyr, "data_habitat")) %>% 
          dplyr::filter(drainage == whichDrainage & is.na(quarter)) %>% 
          select(one_of(chosenColumns)) %>% 
  #        mutate(sample_name = as.numeric(sample_name)) %>%
          distinct() %>% 
          collect(n = Inf)
  
  names(rawWidths) <- camelCase(names(rawWidths))
  
  samples <- data.frame(tbl(conDplyr, "data_seasonal_sampling")) %>%
    filter(drainage == whichDrainage) %>%
    mutate(date = as.Date(median_date)) %>%
  #  mutate(sample_name = as.numeric(sample_name)) %>%
    select(river, date, sample_name) %>%
    arrange(date)
  
  names(samples) <- camelCase(names(samples))
  
  flow <- data.frame(tbl(conDplyr, "data_daily_discharge")) %>%
    filter(date %in% samples$date) %>%
    select(date, river, discharge)
  
  allWidths <- 
    left_join(rawWidths,samples, by = c("river", "sampleName")) %>%
    left_join(., flow, by = c("date", "river")) %>%
 #   filter(!is.na(section)) %>%
    mutate( lDischarge = log(discharge + 0.0001), lWidth = log(width + 0.0001) ) %>%
    group_by( river, section ) %>%
    nest()
  
  widthModel <- function(df) {
    lm( lWidth ~ lDischarge, data = df )
  }
  
  allWidths <- allWidths %>% 
    mutate( mod = map(data, widthModel),
            pred = map2(data, mod, add_predictions)
          )  
  
  widths <- unnest(allWidths,pred) 
  widths$widthToUse <- widths$width
  if ( estimateWidthNA ) {
    widths$widthToUse <- ifelse( is.na(widths$width), exp(widths$pred), widths$width )
    widths$widthDataSource <- ifelse( is.na(widths$width), 'predicted', 'observed' )
  }  
    
#  ggplot(filter(widths,river == 'west brook'), aes(discharge,widthToUse, color = factor(is.na(width)))) +
#    geom_point( ) +
#    facet_wrap(~section,scales = 'free')
  
  data <- left_join(data, select(widths,river,section,sampleName,widthToUse,widthDataSource), by = c("river",'section','sampleName'))
  
  return(data)
}