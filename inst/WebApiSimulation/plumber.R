# plumber.R

#* Get Webapi version info
#* @get /info
#* @serializer unboxedJSON
function() {
  list(version = '2.7.6')
}

#* Generate a Bearer token to use with WebAPI using db security
#* @param login 
#* @param password 
#* @post /user/login/db
#* @serializer unboxedJSON
function(login, password, res) {
  if(login == "testUser" && password == "testPassword"){
    res$headers <- list(bearer = "0000")
    return("success")
  } else {
    res$status <- 401 # unauthorized
    return("fail")
  }
}

