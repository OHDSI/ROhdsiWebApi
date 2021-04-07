
ROhdsiWebApi 1.2.0
==================

Changes:

1. CreatedDate in getDefinition now returns in proper date time format.



ROhdsiWebApi 1.2.0
==================

Changes:

1. Removed dependency on ParallelLogger.
2. Implemented support for security. Thank you @ablack3 @azimov and @anthonysena.
3. Removed function getConceptSetsFromCohortDefinition because out of scope of this package.
4. Added vignette.
5. Added additional tests.


ROhdsiWebApi 1.1.3
==================

Bug fixes:

1. Fixed bug with returning results for incidence rate.


ROhdsiWebApi 1.1.2
==================

Changes:

1. Replace read.csv with readr::read_csv, write.csv with readr::write_csv to ensure type stability and use of tibble.
2. Replaced jsonlite with RJSONIO to address issue with special characters "Sjögren".
3. Replaced used of data.frame(), tidyr::tibble().
4. Fixed bugs due to join on data frame with no row records.


ROhdsiWebApi 1.1.1
==================

Changes:

1.  Use BIGINT for cohort_definition_id.


ROhdsiWebApi 1.1.0
==================

Changes:

1.  New functions: getConceptSetsFromCohortDefinition.

Bug fixes:

1. https://github.com/OHDSI/ROhdsiWebApi/pull/144 was causing duplication of date matches.
2. RJSONIO::toJSON loss of precision #152.


ROhdsiWebApi 1.0.0
==================

Note: This is a major release. This release is expected to break implementations that rely on prior iterations of ROhdsiWebApi.

(last updated : 2020-06-12)

Changes:

1.  Generic design pattern: Starting with version 1.0.0, we have
    implemented a new generic design pattern.  

<!-- end list -->

  - Functions that go from WebApi to R: will return a R-object
    (e.g. list, data frame). The function will handle the conversion of
    non R-objects like JSON objects into R-objects in the background.
    The returned objects may be inspected in R. We have ensured that all
    returned objects are represented in the proper data types e.g. dates
    in are type date, instead of string.
  - Functions that go from R into WebApi: will start with a R-object
    (e.g. list, data frame). If the WebApi requires a JSON object, the
    function will handle the conversion of R-object into JSON in the
    background. More discussion
    [\#37](https://github.com/OHDSI/ROhdsiWebApi/issues/37)

<!-- end list -->

2.  Consistent naming conventions:

<!-- end list -->

  - References to ‘Atlas’ has been replaced by ‘WebApi’. This is because
    the package is designed to interface directly with WebApi.
    [\#21](https://github.com/OHDSI/ROhdsiWebApi/issues/21)
  - Analysis categories in WebApi such as concept sets, cohorts have
    been standardized
    [\#86](https://github.com/OHDSI/ROhdsiWebApi/issues/86) These may
    now be used in string searches of objects returned by ROhdsiWebApi.

<!-- end list -->

    ## # A tibble: 7 x 1
    ##   ` Category`     
    ##   <chr>           
    ## 1 ConceptSet      
    ## 2 Cohort          
    ## 3 IncidenceRate   
    ## 4 Estimation      
    ## 5 Prediction      
    ## 6 Characterization
    ## 7 Pathway

  - Enforcement of OHDSI R style guide. We have enforced the use of
    [OHDSI R style
    guide](https://ohdsi.github.io/MethodsLibrary/codeStyle.html#ohdsi_code_style_for_r)
    such as use of [camel
    case](https://github.com/OHDSI/ROhdsiWebApi/issues/22)

<!-- end list -->

3.  Function deprecation:

<!-- end list -->

  - We have made efforts to retain functions (no support) that do not
    follow design conventions.
  - Note: no support is provided for deprecated functions and may be
    purged without further notice.
  - See issues [\#91](https://github.com/OHDSI/ROhdsiWebApi/issues/91)
    and [\#66](https://github.com/OHDSI/ROhdsiWebApi/issues/66) for not
    following design pattern

<!-- end list -->

4.  Added Vignette

5.  We have implemented a method to provide user with informative error
    message using package
    [\#66](https://github.com/OHDSI/ROhdsiWebApi/issues/56). This
    implementation will attempt to check many potential errors and
    return them together (instead of default behavior of only showing
    the first error message.) This implementation is expected to improve
    usability and efficiency.

6.  New functions

<!-- end list -->

  - Get list of definitions, post definitions, search for definitions by
    id or string name.
  - Job management: execute/invoke, cancel, generation status
    information.
  - Names have been chosen so that they follow an easy to remember
    pattern and are intuitive.

Bug fixes:

  - This is a major change to package with significant code rewrite. No specific bug fixes were targeted in this release.
