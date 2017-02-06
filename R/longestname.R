#' @title Longest First Name
#' @description Shows the name of graduating senior of \code{year} with the longest first name
#' @param year Sets the year of graduates to be scanned
#' @return the name of graduating senior with the longest first name
#' @usage longestname( year )
#' @import stringr
#' @export
longestname <- function( year ){

  # Check parameter
  if ( year < 2000 || year > 2015 ){
    warning("Invalid parameter \"year\", \"year\" must be between 2000-2015." )
    return()
  }

  # Finds the longest first name
  longest <- 0
  position <- 1
  data <- readgraduates(year)
  for ( i in 1:length(data)){
    if ( ! is.na(str_locate(data[i]," ")[2])){
      if ( str_locate(data[i]," ")[2] > longest ){
        position <- i
        longest <- str_locate(data[i]," ")[2]
      }
    }
  }

  data[position]
}
