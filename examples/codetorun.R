rdsFileName <- list()
library(ROhdsiWebApi)

### update the parametes below
# baseUrl <-  "http://api.ohdsi.org:80/WebAPI"
# cohortIds <- c(1772958, 1772956,1772966)
# sourceKeys <- c('synpuf1k','synpuf5pct') #,
# conceptSetIds <- c(1861604, 1861603,1861606)
# cohortPathways <- c(22800)
# incidenceRateIds <- c(1747184)
# estimationId <- 273 #only one estimation id per project
# characterizationIds <- c(19)

##################
##### configuration (Achilles) ###########
rdsFileName[['configuration']] <-
 ROhdsiWebApi::getConfiguration(baseUrl = baseUrl,
                                   sourceKeys = sourceKeys
                                   )
##### data sources (Achilles) ###########
rdsFileName[['dataSources']] <-
 ROhdsiWebApi::getDataSource(baseUrl = baseUrl,
                                sourceKeys = sourceKeys
                                )
