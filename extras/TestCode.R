library(ROhdsiWebApi)
library(magrittr)
baseUrl <- Sys.getenv("baseUrl")

#######################################
# configuration information on WebApi##
#######################################

cdmSources <- getCdmSources(baseUrl = baseUrl)
View(cdmSources)

webApiVersion <- getWebApiVersion(baseUrl = baseUrl)
webApiVersion

priorityVocabulary <- getPriorityVocabularyKey(baseUrl = baseUrl)
priorityVocabulary

metaDataSpecifications <- getMetadataForAllSpecifications(baseUrl = baseUrl)


#############################
##concepts and concept sets##
#############################
conceptSetDefinition <- getConceptSetDefinition(conceptSetId = 10677, baseUrl = baseUrl)

conceptSetDefinitionAsTable <- convertConceptSetDefinitionToTable(conceptSetDefinition)
View(conceptSetDefinitionAsTable)

conceptIds <- resolveConceptSet(conceptSetDefinition = conceptSetDefinition, baseUrl = baseUrl)
concepts <- getConcepts(conceptIds = conceptIds, baseUrl = baseUrl)
sourceConcepts <- getSourceConcepts(conceptIds = conceptIds, baseUrl = baseUrl)
createConceptSetWorkbook(
  conceptSetIds = c(124, 125),
  fileName = "c:/temp/workbook.xlsx",
  baseUrl = baseUrl,
  included = TRUE,
  mapped = TRUE
)

##########
##cohort##
##########
cohortDefinition <- getCohortDefinition(cohortId = 5665, baseUrl = baseUrl)
cohortDefinition$name
cohortDefinition$description

cohortGenerationInformation <- getCohortGenerationInformation(cohortId = 5665, baseUrl = baseUrl)
conceptSetDefinition <- cohortDefinition$expression$ConceptSets[[1]]
conceptSetDefinitionAsTable <- convertConceptSetDefinitionToTable(conceptSetDefinition)

sql <- getCohortDefinitionSql(baseUrl = baseUrl, cohortId = 5665)

invokeCohortSetGeneration(baseUrl = baseUrl, sourceKeys = 'IBM_CCAE', cohortIds = c(5665, 5666))
cohortGenerationInformation <- getCohortGenerationInformation(cohortId = 5666, baseUrl = baseUrl)


cohortResults <-
  getCohortResults(cohortId = 5665,
                   baseUrl = baseUrl,
                   sourceKey = "CDM_JMDC_V1106")
cohortResults$inclusionRuleStats
cohortResults$summary
cohortResults$treemapData # needs to return a data frame.


# cohort characterization
cohortCharacterizationDefinition <- getCohortCharacterizationDefinition(characterizationId = 486, 
                                                                        baseUrl = baseUrl)
cohortName <- cohortCharacterizationDefinition$cohorts[[1]]$name
# can't get cohort definitions in cohort because 
# https://github.com/OHDSI/ROhdsiWebApi/issues/98

characterizationResults <- getCohortCharacterizationResults(characterizationId = 486, 
                                                            baseUrl = baseUrl, 
                                                            sourceKey = 'IBM_CCAE')

# incidence rate
incidenceRateDefinition <- getIncidenceRateDefinition(baseUrl = baseUrl, incidenceRateId = 429)
cohortDefinition <- incidenceRateDefinition$expression$targetCohorts[[1]]$expression
conceptSetDefinitionAsTable <- convertConceptSetDefinitionToTable(cohortDefinition$ConceptSets[[1]])

incidenceRateGenerationInformation <- getIncidenceRateGenerationInformation(baseUrl = baseUrl,
                                                                            incidenceRateId = 429)

# incidenceRateResults

# estimation
estimation <- getEstimationDefinition(estimationId = 124, baseUrl = baseUrl)
cohortId <- estimation$specification$cohortDefinitions[[1]]$id
### where is the cohort expression?
conceptSetDefinition <-
  getConceptSetDefinition(conceptSetId = estimation$specification$conceptSets[[1]]$id,
                          baseUrl = baseUrl)
conceptSetDefinitionAsTable <- convertConceptSetDefinitionToTable(conceptSetDefinition = conceptSetDefinition)



