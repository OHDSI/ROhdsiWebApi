###### TEST FNS
baseUrl <- 'http://api.ohdsi.org:80/WebAPI'

def_path <- ROhdsiWebApi::getPathwayDefinition(82, baseUrl)
out_path <- ROhdsiWebApi::postPathwayDefinition("test123", def_path, baseUrl, "rename")

def_char <- ROhdsiWebApi::getCharacterizationDefinition(246, baseUrl)
out_char <- ROhdsiWebApi::postCharacterizationDefinition("test123", def_char, baseUrl, "rename")

def_ir <- ROhdsiWebApi::getIncidenceRateDefinition(1747266, baseUrl)
out_ir <- ROhdsiWebApi::postIncidenceRateDefinition("test123", def_ir, baseUrl, "rename")


for(i in x:y){ROhdsiWebApi::deleteCohortDefinition(i,baseUrl)}