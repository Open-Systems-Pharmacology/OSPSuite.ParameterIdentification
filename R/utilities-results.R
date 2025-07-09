createPIResults <- function(optimResult, ciResult = NULL, configuration, piParameters, metadata = NULL) {

  out <- structure(
    list(optimResult, ciResult),
    class = "piResults"
  )
 return(out)
}

