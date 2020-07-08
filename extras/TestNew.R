baseUrl <- 'http://api.ohdsi.org:80/WebAPI'

## Post pathway
definition_pathway <- ROhdsiWebApi::getPathwayDefinition(82, baseUrl)
posted_pathway <- ROhdsiWebApi::postPathwayDefinition("rohdsiwebapi_test", definition_pathway, baseUrl, "rename")
posted_pathway

## Post characterization
definition_char <- ROhdsiWebApi::getCharacterizationDefinition(246, baseUrl)
posted_char <- ROhdsiWebApi::postCharacterizationDefinition("rohdsiwebapi_test", definition_char, baseUrl, "rename")
posted_char

## Post incidence rate
definition_ir <- ROhdsiWebApi::getIncidenceRateDefinition(1747266, baseUrl)
posted_ir <- ROhdsiWebApi::postIncidenceRateDefinition("rohdsiwebapi_test", definition_ir, baseUrl, "rename")
posted_ir

############################
### TIDY UP
############################
posted_pathway_def <- ROhdsiWebApi::getPathwayDefinition(posted_pathway$id, baseUrl)
ROhdsiWebApi::deletePathwayDefinition(posted_pathway_def$id, baseUrl)
purrr::walk(posted_pathway_def$targetCohorts, ~ROhdsiWebApi::deleteCohortDefinition(.x$id, baseUrl))
purrr::walk(posted_pathway_def$eventCohorts, ~ROhdsiWebApi::deleteCohortDefinition(.x$id, baseUrl))

posted_char_def <- ROhdsiWebApi::getCharacterizationDefinition(posted_char$id, baseUrl)
ROhdsiWebApi::deleteCharacterizationDefinition(posted_char_def$id, baseUrl)
purrr::walk(posted_char_def$expression$cohorts, ~ROhdsiWebApi::deleteCohortDefinition(.x$id, baseUrl))

posted_ir_def <- ROhdsiWebApi::getIncidenceRateDefinition(posted_ir$id, baseUrl)
ROhdsiWebApi::deleteIncidenceRateDefinition(posted_ir_def$id, baseUrl)
purrr::walk(posted_ir_def$expression$targetIds, ~ROhdsiWebApi::deleteCohortDefinition(.x, baseUrl))
purrr::walk(posted_ir_def$expression$outcomeIds, ~ROhdsiWebApi::deleteCohortDefinition(.x, baseUrl))
