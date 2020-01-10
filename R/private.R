# @file general
#
# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of R OHDSI webApi package.
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

# checks if url conforms with expected structure for base url
.checkBaseUrl <- function(baseUrl){
  
  valid_chars <- rex::rex(except_some_of(".", "/", " ", "-"))
  
  baseUrlRegEx <- rex::rex(
    start,
    
    # protocol identifier (optional) + //
    group(list("http", maybe("s")), "://"),
    
    # user:pass authentication (optional)
    maybe(non_spaces,
          maybe(":", zero_or_more(non_space)),
          "@"),
    
    #host name
    group(zero_or_more(valid_chars, zero_or_more("-")), one_or_more(valid_chars)),
    
    #domain name
    zero_or_more(".", zero_or_more(valid_chars, zero_or_more("-")), one_or_more(valid_chars)),
    
    #TLD identifier
    group(".", valid_chars %>% at_least(2)),
    
    # server port number (required)
    zero_or_more(":", digit %>% between(2, 5)),
    
    # resource path (optional)
    maybe("/", non_space %>% zero_or_more()),
    
    end
  )
  success <- as.logical(grepl(baseUrlRegEx, baseUrl))
  if (!success) {
    stop("Base URL not valid, should be like http://server.org:80/WebAPI")
  }
}