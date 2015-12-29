## R state:
library(RPostgreSQL)
library(lubridate)
library(getWBData)
library(integrator)#available at https://github.com/Conte-Ecology/data-integrator
library(parallel)
library(reshape2)
library(ggplot2)
library(data.table)
library(readxl)
library(waterData)

options(stringsAsFactors=FALSE)
options(check.names=FALSE)

options(mc.cores=1)
# Shared data:

westbrookDir<-"/data/projects/westbrook"

shared_data <- local(expr={
  
  date.format=c('mdy','mdyR','mdyT') #input for 'orders' in the function parse_date_time that matches input csv date format 
  
	reconnect() #creates connection 'con'

	dataInDir <- file.path(westbrookDir,'dataIn')
	dataOutDir <- file.path(westbrookDir,'dataOut')
	original_data_dir <- file.path(dataInDir,'originalData')
	adjusted_data_dir <- file.path(dataOutDir,'adjustedData')
	processed_data_dir <- file.path(dataOutDir,'processedData')
	standardizeFilesDir<-file.path(westbrookDir,"code/westBrookData/makeDatabase/standardizeFiles")
  
	tag_data_names <- c(
		"tags_antenna", "tags_dead", 
		"tags_salmon_wb", "tags_trout_wb", "tags_tribs_wb"
	)

	csv_files <- paste0(file.path(original_data_dir, c(tag_data_names,"yoy_bins")), '.csv')
	names(csv_files) <- c(tag_data_names,"yoy_bins")
	standardize_files <- paste0(file.path(standardizeFilesDir, c(tag_data_names,"yoy_bins")), '_standardize.R')
	names(standardize_files) <- c(tag_data_names,"yoy_bins")
  
	dbDropTable<-function(tableName){ #drops the table if it exists before writing the new version, really only matters for the first time the db is created
	  if(tableName %in% dbGetQuery(con,"SELECT table_name 
                              FROM information_schema.tables 
                              WHERE table_schema = 'public';")$table_name){
	    dbSendQuery(con,paste0("DROP TABLE ",tableName,";"))
	  }
	}

	return(environment(NULL)) 
})




