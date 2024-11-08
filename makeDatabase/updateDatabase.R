importAll<-F #if false only updates files that have date modified>last update

do <- list(
  import_stage = c(
    "import_to_sql.R",
    "import_temps_from_sheds.R",
    "import_depths.R",
    "import_stanley_environmental.R",
    "import_family.R",
    "import_2011to2015_antenna.R",
    "import_old_allflex.R",
    "form_table_dead.R",
    "form_table_antenna.R",
    "form_table_captures.R",
    "form_table_untagged_captures.R",
    "form_table_family.R",
    "form_table_acoustic.R",
    
    "form_table_seasonal_sampling.R",
    "calculate_season_breaks.R",
    "form_table_season_map.R",
    "form_table_yoy_bins.R",
    
    "import_habitat.R",
    "import_sites.R",
    "import_antenna_deployment.R",
    "import_antenna_log.R"
  ),

  data_table_stage = c(
    "form_data_antenna.R",
    "form_data_boundary_detections.R",
    "form_data_family.R",

    "form_data_seasonal_sampling.R",
    "form_data_yoy_bins.R",
    "form_data_habitat.R",
    "form_data_sites.R",

    "form_data_tagged_captures.R",
    "form_data_untagged_captures.R",
    "form_data_acoustic.R",
    "form_data_antenna_log.R",
    "declare_data_errors.R",
    "fix_data_errors.R",

    "fix_tag_properties.R",


    "form_data_trap_captures.R",
    "form_data_smolts.R"
  ),
  environmental_data_stage = c(
    "form_data_hourly_temps.R",
    "form_data_daily_temps.R",

    "form_data_flow_extension.R",
    "form_data_daily_discharge.R"
  ),

  processing_stage = c(
    "form_data_by_tag.R",
    "form_antenna_status.R"
    # 		#"form_occasion_points.R",
    # 		#"form_occasion_rows.R",
    # 		#"form_sample_points.R",
    # 		#"form_sample_rows.R"
    # 	),
    # 	state_stage = c(
    # 		#"form_state_table.R"
  ),
  permissions_stage = c(
    "grant_permissions.R"
  )
)
source('/data/projects/westbrook/code/westBrookData/makeDatabase/shared_data.R')
for (stage in names(do)) {
  for (script in do[[stage]]) {
    temp <- new.env(parent=shared_data)
    temp[['shared_data']] <- shared_data
    with(
      data=temp,
      expr= {
        s <- file.path(westbrookDir,"code/westBrookData/makeDatabase",stage,script)
        cat(s,"\n")
        source(file=s, local=TRUE)
      }
    )
    rm(envir=temp, list='shared_data')
    rm(temp)
  }
}
