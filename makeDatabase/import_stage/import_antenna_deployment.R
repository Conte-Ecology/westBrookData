deploy<-read_excel(paste0(original_data_dir,"/antenna/antennaDeployment.xlsx")) %>%
  data.table() %>%
  setnames(tolower(names(.))) %>%
  setnames(c("river km","river-section-quarter"),
            c("river_meter","antenna_name")) %>%
  .[,":="(drainage=tolower(drainage),
          river=tolower(river),
          area=tolower(area),
          section = tolower(section),
          antenna_name = tolower(antenna_name),
          river_meter=as.numeric(river_meter))] %>%
  .[!is.na(river)] %>%
  setkey(river,river_meter) 


dbWriteTable(con, 'antenna_deployment', data.frame(deploy), row.names=FALSE,
             overwrite=TRUE, append=FALSE)


