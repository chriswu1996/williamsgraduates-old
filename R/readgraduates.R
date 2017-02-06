#' @title Read Graduates
#' @description  Scan for the names of graduates and create a list of names
#' @param year Sets the year of graduates to be scanned
#' @return A list of names of graduates from \code{year}
#' @usage readgraduates( year )
#' @import stringr
#' @export
readgraduates <- function( year ){

  # Check parameter
  if ( year < 2000 || year > 2015 ){
    warning("Invalid parameter \"year\", \"year\" must be between 2000-2015." )
    return()
  }

  # Read the data of the year
  input <- readLines(system.file("extdata", str_c(year,".txt"), package = "williamsgraduates"), warn = FALSE)

  # Combine lines that were not supposed to be separated
  for(i in 1:length(input)){
    if ( ! is.na(str_locate(input[i],", with")[1]) ){

      if ( is.na(str_locate(input[i],"Statistics")[1]) &&
           is.na(str_locate(input[i],"Spanish")[1]) &&
           is.na(str_locate(input[i],"Sociology")[1]) &&
           is.na(str_locate(input[i],"Russian")[1]) &&
           is.na(str_locate(input[i],"Religion")[1]) &&
           is.na(str_locate(input[i],"Psychology")[1]) &&
           is.na(str_locate(input[i],"Economy")[1]) &&
           is.na(str_locate(input[i],"Physics")[1]) &&
           is.na(str_locate(input[i],"Philosophy")[1]) &&
           is.na(str_locate(input[i],"Music")[1]) &&
           is.na(str_locate(input[i],"Mathematics")[1]) &&
           is.na(str_locate(input[i],"Japanese")[1]) &&
           is.na(str_locate(input[i],"History")[1]) &&
           is.na(str_locate(input[i],"Geosciences")[1]) &&
           is.na(str_locate(input[i],"German")[1]) &&
           is.na(str_locate(input[i],"French")[1]) &&
           is.na(str_locate(input[i],"Policy")[1]) &&
           is.na(str_locate(input[i],"English")[1]) &&
           is.na(str_locate(input[i],"Economics")[1]) &&
           is.na(str_locate(input[i],"Science")[1]) &&
           is.na(str_locate(input[i],"Literature")[1]) &&
           is.na(str_locate(input[i],"Classics")[1]) &&
           is.na(str_locate(input[i],"Chinese")[1]) &&
           is.na(str_locate(input[i],"Chemistry")[1]) &&
           is.na(str_locate(input[i],"Biology")[1]) &&
           is.na(str_locate(input[i],"Astrophysics")[1]) &&
           is.na(str_locate(input[i],"Astronomy")[1]) &&
           is.na(str_locate(input[i],"Art")[1]) &&
           is.na(str_locate(input[i],"Theatre")[1]) &&
           is.na(str_locate(input[i],"Studies")[1]) &&
           is.na(str_locate(input[i],"Neuroscience")[1]) &&
           is.na(str_locate(input[i],"Anthropology")[1]) ){

        input[i] = str_c(input[i]," ",input[i+1])
        input = input[-(i+1)]

      }
    }
  }
  input
}
