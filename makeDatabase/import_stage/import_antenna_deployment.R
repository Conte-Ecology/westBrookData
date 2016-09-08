deploy<-read_excel(paste0(original_data_dir,"/antenna/antennaDeployment.xlsx")) %>%
  data.table() %>%
  setnames(tolower(names(.))) %>%
  setnames("river km","river_meter") %>%
  .[,":="(drainage=tolower(drainage),
          river=tolower(river),
          area=tolower(area),
          river_meter=as.numeric(river_meter))] %>%
  setkey(river,river_meter)

dbWriteTable(con, 'antenna_deployment', data.frame(deploy), row.names=FALSE,
             overwrite=TRUE, append=FALSE)


