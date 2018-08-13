check_contributor <- function(contributor){
  
  if (sum(grepl("[[:digit:]]", contributor)) > 0){
    print("There seems to be a name with a possible typo. Please check and then proceed.")
  }else{
    print("Ok.")
  }
  
}