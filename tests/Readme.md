# Notes on testing ROhdsiWebApi

ROhdsiWebApi tests rely on the [httptest](https://enpiar.com/r/httptest/) package which caches data retrieved
from a webapi instance and "mocks" the webapi endpoint in subsequent tests.

Every file in the R/ folder has a corresponding test file in the tests/testthat/
folder. In addition cached data for each test file is stored in a corresponding 
folder inside tests/testthat/mocks/.

For example:

- Functions that deal with analysis generation are in R/Generation.R
- Test for those functions are in tests/testthat/test-Generation.R
- Cached webapi data needed for those tests are in tests/testthat/mocks/Generation/

To update test you will likely want to delete the cache for the specific file you 
are working on. For example if you want to add more tests for functions in Generation.R
you would delete the tests/testthat/mocks/Generation/ folder so that the cache is 
refreshed and includes data needed for your new tests.

To refresh all of the cached data delete tests/testthat/mocks/ and rerun all tests.

Not all of the functions use cached data (such as the authorization functions)
but it is preferred that new tests are called with `with_mock_dir` so they will use
cached data if available.
