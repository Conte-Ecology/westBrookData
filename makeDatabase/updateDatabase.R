do <- list(
	import_stage = c(
		"import_to_sql.R",
		"import_high_res_env.R",
		"form_table_dead.R",
		"form_table_antenna.R",
		"form_table_captures.R",
		"form_table_untagged_captures.R",
		
		"form_table_seasonal_sampling.R",
		"calculate_season_breaks.R",
		"form_table_season_map.R",
		"form_table_yoy_bins.R"
		
#   	,"form_table_temperature_discharge.R"
	),
	
	data_table_stage = c(
		"form_data_antenna.R",
		"form_data_boundary_detections.R",
		
		"form_data_tagged_captures.R",
		"declare_data_errors.R",
		"fix_data_errors.R",
		"fix_tag_properties.R",
		
		"form_data_seasonal_sampling.R",
		"form_data_yoy_bins.R",
		"form_data_trap_captures.R",
		"form_data_smolts.R",
		"form_data_tag_history.R",
		"form_data_locations.R"
	),

	processing_stage = c(
	  "form_data_by_tag.R"
# 		"form_occasion_points.R",
# 		"form_occasion_rows.R",
# 		"form_sample_points.R",
# 		"form_sample_rows.R"
# 	),
# 	state_stage = c(
# 		"form_state_table.R"
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



