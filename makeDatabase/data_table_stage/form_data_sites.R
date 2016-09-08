sites<-tbl(conDplyr,"raw_sites") %>%
  collect(n=Inf)

column_code <- list(
  drainage = function(drainage) return(drainage),
  river = function(river) return(river),
  area = function(area) return(area),
  section = function(section) return(section),
  quarter = function(quarter) return(as.integer(quarter)),
  quarter_length = function(quarter_length) return(as.numeric(quarter_length)),
  river_meter = function(river_meter) return(as.numeric(river_meter)),
  confluence_river_meter = function(confluence_river_meter){
    return(as.numeric(confluence_river_meter))}
)

sites <- pipeline_data_transformation(
  data=sites, pipeline=column_code)

dbWriteTable(con, 'data_sites', sites, row.names=FALSE,
             overwrite=TRUE, append=FALSE)

