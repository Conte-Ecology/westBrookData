wb_boundary_detections <- tbl(conDplyr,"data_stationary_antenna") %>%
        filter(!is.na(detection_date),river=="west brook") %>%
        filter(river_meter<=4264|river_meter>=5524.25) %>%
        collect(n=Inf)

# boundary_antennas <- c('a1','a2','03','04','05','06','wb above')
# 
# boundary_detections <- data[data[['river']] %in% boundary_antennas,]

stanley_boundary_detections<-tbl(conDplyr,"data_stationary_antenna") %>%
                              filter(!is.na(detection_date),river=="mainstem",river_meter<=10) %>%
                              collect(n=Inf)
boundary_detections<-bind_rows(wb_boundary_detections,stanley_boundary_detections)

dbWriteTable(con, 'data_boundary_detections', boundary_detections, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)



