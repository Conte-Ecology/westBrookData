tables<-c("raw_habitat_west_brook","raw_habitat_stanley")
habitat<-NULL
for(t in tables){
  habitat<-tbl(conDplyr,t) %>%
           collect(n=Inf) %>%
           bind_rows(habitat)
}

column_code <- list(
  drainage = function(drainage) return(drainage),
  river = function(river) return(river),
  sample_name = function(sample_name){
    return(as.character(as.numeric(sample_name)))},
  section = function(section) return(as.numeric(section)),
  quarter = function(quarter) return(as.numeric(quarter)),
  width = function(width) return(as.numeric(width)),
  maximum_depth = function(maximum_depth) return(as.numeric(maximum_depth)),
  distance_of_maximum_depth = function(distance_of_max_depth){
    return(as.numeric(distance_of_max_depth))},
  comment = function(comment) return(comment)
)

habitat <- pipeline_data_transformation(
  data=habitat, pipeline=column_code)

dbWriteTable(con, 'data_habitat', habitat, row.names=FALSE,
             overwrite=TRUE, append=FALSE)

