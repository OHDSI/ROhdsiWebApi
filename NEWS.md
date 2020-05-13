ROhdsiWebApi 0.1.0
=====================

Changes:
Note: This is a major release. It has few breaking changes. Significant new functionality and code rewrite has been performed.

1. Generic design pattern: Starting with this version, we have implemented a new generic design pattern.  
 - Functions that go from WebApi to R: will return a R-object (e.g. list, data frame). The function will handle the conversion of non R-objects like JSON objects into R-objcts in the background. The returned objects may be inspected in R.
 - Functions that go from R into WebApi: will start with a R-object (e.g. list, data frame). If the WebApi requires a JSON object, the function will handle the conversion of R-object into JSON in the background. 
More discussion [here.](https://github.com/OHDSI/ROhdsiWebApi/issues/37)
2. Consistent naming conventions: 
- References to 'Atlas' has been replaced by 'WebApi' - because this function is deisgned to interface directly with WebApi. [See issue #21](https://github.com/OHDSI/ROhdsiWebApi/issues/21)
- Analysis categories in WebApi such as concept sets, cohorts will use standard names as listed [here](https://github.com/OHDSI/ROhdsiWebApi/issues/86) These may now be used in string searches of objects returned by ROhdsiWebApi.

name | idField
-- | --
dataSource | dataSourceId
concept | conceptId
conceptSet | conceptSetId
cohort | cohortId
characterization | characterizationIds
pathway | pathwayId
incidenceRate | incidenceRateIds
person | personId
estimation | estimationId
prediction | predictionId

  - Enforcement of OHDSI R style guide. We have enforced the use of [OHDSI R style guide](https://ohdsi.github.io/MethodsLibrary/codeStyle.html#ohdsi_code_style_for_r) such as use of [camel case](https://github.com/OHDSI/ROhdsiWebApi/issues/22)
3. Breaking changes: This version has many breaking changes. 
- Renaming: because of renaming in function names/parameters/returned objects, this may break prior code.
- [Deprecated functions](https://github.com/OHDSI/ROhdsiWebApi/issues/91) and [here](https://github.com/OHDSI/ROhdsiWebApi/issues/66) for not following design pattern 
4. Added Vignette: 
- Working with cohorts
- Working with concept set
- Working with incidence rate
5. We have implemented a method to provide user with informative effor message using package [checkmate](https://github.com/OHDSI/ROhdsiWebApi/issues/56). This implementation will attempt to check many potential errors and return them together (instead of default behavior of only showing the first error message.) This implemenation is expected to improve usability and efficiency.

5. New functions: 
- [Incidence rate](https://github.com/OHDSI/ROhdsiWebApi/issues/59)
- [Patient profile](https://github.com/OHDSI/ROhdsiWebApi/issues/57)
- [Ability to post definitions](https://github.com/OHDSI/ROhdsiWebApi/issues/42)
- [Estimation specification](https://github.com/OHDSI/ROhdsiWebApi/issues/69)
- [Retrieve a list of definition metadata](https://github.com/OHDSI/ROhdsiWebApi/issues/40)
- [Delete definitions](https://github.com/OHDSI/ROhdsiWebApi/issues/38)