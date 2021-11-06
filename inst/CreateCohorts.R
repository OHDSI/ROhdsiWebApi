# Copyright #CopyrightYear# Observational Health Data Sciences and Informatics
#
# This file is part of #packageName#
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

checkForInputFileEncoding <- function(fileName) {
  encoding <- readr::guess_encoding(file = fileName, n_max = min(1e+07))
  
  if (!encoding$encoding[1] %in% c("UTF-8", "ASCII")) {
    stop("Illegal encoding found in file ",
         basename(fileName),
         ". Should be 'ASCII' or 'UTF-8', found:",
         paste(paste0(encoding$encoding, " (", encoding$confidence, ")"), collapse = ", "))
  }
  invisible(TRUE)
}

.createCohorts <- function(connection,
                           cdmDatabaseSchema,
                           vocabularyDatabaseSchema = cdmDatabaseSchema,
                           cohortDatabaseSchema,
                           cohortTable,
                           oracleTempSchema = NULL,
                           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                           outputFolder) {
  if (!is.null(oracleTempSchema) && oracleTempSchema != "") {
    warning("The 'oracleTempSchema' argument is deprecated. Use 'tempEmulationSchema' instead.")
    tempEmulationSchema <- oracleTempSchema
  }
  
  # Create study cohort table structure:
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CreateCohortTable.sql",
                                           packageName = "#packageName#",
                                           dbms = attr(connection, "dbms"),
                                           tempEmulationSchema = tempEmulationSchema,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable)
  DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)
  
  #stats_start#
  # Insert rule names in cohort_inclusion table:
  pathToCsv <- system.file("cohorts", "InclusionRules.csv", package = "#packageName#")
  checkForInputFileEncoding(pathToCsv)
  inclusionRules <- readr::read_csv(pathToCsv, col_types = readr::cols()) 
  inclusionRules <- data.frame(cohort_definition_id = inclusionRules$cohortId,
                               rule_sequence = inclusionRules$ruleSequence,
                               name = inclusionRules$ruleName)
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = "#cohort_inclusion",
                                 data = inclusionRules,
                                 dropTableIfExists = FALSE,
                                 createTable = FALSE,
                                 tempTable = TRUE,
                                 tempEmulationSchema = tempEmulationSchema)
  #stats_end#
  
  # Instantiate cohorts:
  pathToCsv <- system.file("#fileName#", package = "#packageName#")
  checkForInputFileEncoding(pathToCsv)
  cohortsToCreate <- readr::read_csv(pathToCsv, col_types = readr::cols())
  for (i in 1:nrow(cohortsToCreate)) {
    writeLines(paste("Creating cohort:", cohortsToCreate$name[i]))
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = paste0(cohortsToCreate$name[i], ".sql"),
                                             packageName = "#packageName#",
                                             dbms = attr(connection, "dbms"),
                                             tempEmulationSchema = tempEmulationSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             vocabulary_database_schema = vocabularyDatabaseSchema,
                                             #stats_start#
                                             results_database_schema.cohort_inclusion = "#cohort_inclusion",  
                                             results_database_schema.cohort_inclusion_result = "#cohort_inc_result",  
                                             results_database_schema.cohort_inclusion_stats = "#cohort_inc_stats",  
                                             results_database_schema.cohort_summary_stats = "#cohort_summary_stats",  
                                             #stats_end#   
                                             target_database_schema = cohortDatabaseSchema,
                                             target_cohort_table = cohortTable,
                                             target_cohort_id = cohortsToCreate$cohortId[i])
    DatabaseConnector::executeSql(connection, sql)
  }
  
  # Fetch cohort counts:
  sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohort_database_schema.@cohort_table GROUP BY cohort_definition_id"
  sql <- SqlRender::render(sql,
                           cohort_database_schema = cohortDatabaseSchema,
                           cohort_table = cohortTable)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  counts <- DatabaseConnector::querySql(connection, sql)
  names(counts) <- SqlRender::snakeCaseToCamelCase(names(counts))
  counts <- merge(counts, data.frame(cohortDefinitionId = cohortsToCreate$cohortId,
                                     cohortName  = cohortsToCreate$name))
  readr::write_excel_csv(x = counts, file = file.path(outputFolder, "CohortCounts.csv"), na = "")
  
  #stats_start#
  # Fetch inclusion rule stats and drop tables:
  fetchStats <- function(tableName) {
    sql <- "SELECT * FROM #@table_name"
    sql <- SqlRender::render(sql, table_name = tableName)
    sql <- SqlRender::translate(sql = sql, 
                                targetDialect = attr(connection, "dbms"),
                                tempEmulationSchema = tempEmulationSchema)
    stats <- DatabaseConnector::querySql(connection, sql)
    names(stats) <- SqlRender::snakeCaseToCamelCase(names(stats))
    fileName <- file.path(outputFolder, paste0(SqlRender::snakeCaseToCamelCase(tableName), ".csv"))
    readr::write_csv(x = stats, path = fileName)
    
    sql <- "TRUNCATE TABLE #@table_name; DROP TABLE #@table_name;"
    sql <- SqlRender::render(sql, table_name = tableName)
    sql <- SqlRender::translate(sql = sql, 
                                targetDialect = attr(connection, "dbms"),
                                tempEmulationSchema = tempEmulationSchema)
    DatabaseConnector::executeSql(connection, sql)
  }
  fetchStats("cohort_inclusion")
  fetchStats("cohort_inc_result")
  fetchStats("cohort_inc_stats")
  fetchStats("cohort_summary_stats")
  #stats_end#
}

