# title: Pull Canada Jaeger data from Movebank using API
# author: Autumn-Lynn Harrison, May 12, 2020 (used again fine 12/15/20)
# Notes: Allows to pull with duplicate time stamps...commented code at end of script original method with Move package that requires pre-filtering of duplicate timestamps at time of writing

library(data.table) # function fread

### Function to pull CSV file from URL
read.url <- function(url, ...){
  tmpFile <- tempfile()
  download.file(url, destfile = tmpFile, method = "curl")
  url.data <- fread(tmpFile, ...)
  return(url.data)
}

# POJA
studyid = as.character(973570814)
animalName="POJA-2019-CAN-01"
# no spaces between attributes
url = paste("https://www.movebank.org/movebank/service/direct-read?entity_type=event&study_id=", studyid, 
            "&attributes=individual_id,individual_local_identifier,tag_id,timestamp,location_long,location_lat,argos_lc,visible,algorithm_marked_outlier,manually_marked_outlier,argos_semi_major,argos_semi_minor,argos_error_radius,argos_orientation,argos_sensor_1,argos_sensor_2,argos_sensor_3,argos_sensor_4&user=MCP&password=C0nnect1v1ty",sep="")
po = read.url(url)
po$sp = "POJA"
po = po[po$individual_local_identifier==animalName,]

#DeploymentStartDate--Check once to make sure downloaded data do not include pre-deployment data
#url = paste("https://www.movebank.org/movebank/service/direct-read?entity_type=deployment&study_id=", studyid,
#            "&attributes=individual_id,deploy_on_timestamp&user=MCP&password=C0nnect1v1ty",sep="")
#depOnPo = read.url(url)
# OK

# PAJA
studyid = as.character(630339095)
animals=c("PAJA_CAN_01_2018", "PAJA_CAN_02_2018")
# no spaces between attributes
url = paste("https://www.movebank.org/movebank/service/direct-read?entity_type=event&study_id=", studyid, 
            "&attributes=individual_id,individual_local_identifier,tag_id,timestamp,location_long,location_lat,argos_lc,visible,algorithm_marked_outlier,manually_marked_outlier,argos_semi_major,argos_semi_minor,argos_error_radius,argos_orientation,argos_sensor_1,argos_sensor_2,argos_sensor_3,argos_sensor_4&user=MCP&password=C0nnect1v1ty",sep="")
pa = read.url(url)
pa$sp = "PAJA"
pa = pa[pa$individual_local_identifier %in% animals,]

#DeploymentStartDate--Check once to make sure downloaded data do not include pre-deployment data
#url = paste("https://www.movebank.org/movebank/service/direct-read?entity_type=deployment&study_id=", studyid,
#            "&attributes=individual_id,deploy_on_timestamp&user=MCP&password=C0nnect1v1ty",sep="")
#depOnPa = read.url(url)
## OK

# LTJA
studyid = as.character(300812056)
animals=c("LTJA-2019-CAN-01", "LTJA-2019-CAN-02")
# no spaces between attributes
url = paste("https://www.movebank.org/movebank/service/direct-read?entity_type=event&study_id=", studyid, 
            "&attributes=individual_id,individual_local_identifier,tag_id,timestamp,location_long,location_lat,argos_lc,visible,algorithm_marked_outlier,manually_marked_outlier,argos_semi_major,argos_semi_minor,argos_error_radius,argos_orientation,argos_sensor_1,argos_sensor_2,argos_sensor_3,argos_sensor_4&user=MCP&password=C0nnect1v1ty",sep="")
lt = read.url(url)
lt$sp = "LTJA"
lt = lt[lt$individual_local_identifier %in% animals,]

#DeploymentStartDate--Check once to make sure downloaded data do not include pre-deployment data
# url = paste("https://www.movebank.org/movebank/service/direct-read?entity_type=deployment&study_id=", studyid,
#            "&attributes=individual_id,deploy_on_timestamp&user=MCP&password=C0nnect1v1ty",sep="")
# depOnLT = read.url(url)

# OK

d = rbind(lt,pa,po)
save(d, file=("CanJaegerData_2020_1215.RData"))

########################
#library(move)
#library(xlsx)
#library(data.table)

# API method looping through study ids
# instead of doing a loop for all studies, do each separately to restrict to specific animals (just the small sample of Canadian birds) and save time

# start = "20141015000000" 
# finish = "20200507000000" 
# for (i in 1:length(ids)) {
#  studyid = as.character(ids[i])
#  url = paste("https://www.movebank.org/movebank/service/direct-read?entity_type=event&study_id=", studyid, "&timestamp_start=",start, "&attributes=individual_id,tag_id,timestamp,location_long,location_lat,visible,algorithm_marked_outlier,manually_marked_outlier&user=MCP&password=C0nnect1v1ty",sep="")
#  d = read.url(url)
#  d$sp = sp[i]
#  x = rbind(x,d)
# }

########################

## Using MOVE PACKAGE. Problem with duplicate time stamps
## Dave Douglas wrote to me with a solution for this
# originally included duplicate time stamps. Needed to filter these for download and mapping purposes
# lt = getMovebankData(study=300812056, animalName=c("LTJA-2019-CAN-01", "LTJA-2019-CAN-02"), login=loginStored, removeDuplicatedTimestamps=TRUE)
# pa = getMovebankData(study=630339095, animalName=c("PAJA_CAN_01_2018", "PAJA_CAN_02_2018"), login=loginStored)
# po = getMovebankData(study=973570814, animalName=c("POJA-2019-CAN-01"), login=loginStored)





