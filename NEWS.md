News
================

# ROhdsiWebApi 1.0.0

Note: This is a major release. This release is expected to break
implementations that rely on prior iterations of ROhdsiWebApi.

### Changes:

1.  Generic design pattern: Starting with version 1.0.0, we have
    implemented a new generic design pattern.  

<!-- end list -->

  - Functions that go from WebApi to R: will return a R-object
    (e.g. list, data frame). The function will handle the conversion of
    non R-objects like JSON objects into R-objects in the background.
    The returned objects may be inspected in R.
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

``` r
library(magrittr) 
ROhdsiWebApi:::.getStandardCategories() %>% dplyr::pull(categoryStandard)
```

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
  - See issues [here](https://github.com/OHDSI/ROhdsiWebApi/issues/91)
    and [here](https://github.com/OHDSI/ROhdsiWebApi/issues/66) for not
    following design pattern

<!-- end list -->

4.  Added Vignette:

<!-- end list -->

  - Working with cohorts
  - Working with concept set
  - Working with incidence rate

<!-- end list -->

5.  We have implemented a method to provide user with informative effor
    message using package
    [checkmate](https://github.com/OHDSI/ROhdsiWebApi/issues/56). This
    implementation will attempt to check many potential errors and
    return them together (instead of default behavior of only showing
    the first error message.) This implementation is expected to improve
    usability and efficiency.

6.  New functions have been added to the following functional
    categories.

<!-- end list -->

  - [Incidence rate](https://github.com/OHDSI/ROhdsiWebApi/issues/59)
  - [Patient profile](https://github.com/OHDSI/ROhdsiWebApi/issues/57)
  - [Ability to post
    definitions](https://github.com/OHDSI/ROhdsiWebApi/issues/42)
  - [Estimation
    specification](https://github.com/OHDSI/ROhdsiWebApi/issues/69)
  - [Retrieve a list of definition
    metadata](https://github.com/OHDSI/ROhdsiWebApi/issues/40)
  - [Delete
    definitions](https://github.com/OHDSI/ROhdsiWebApi/issues/38)

## Bug fixes:

  - This is a major change to package with significant code rewrite.
  - No specific bug fixes were targeted in this release.
  - Future releases will target bug fixes.
