#' @title Show Statistics
#' @description Uses the data from the read function and performs appropriate analysis and generates graphics associated with the data.
#' @param type The kind of analysis to be done. The possible options are \code{graduates}, \code{SCL}, \code{MCL}, \code{CL} and \code{summary}. \code{graduates}, \code{SCL}, \code{MCL} and \code{CL} displays graph of the total number of graduating seniors, those with Cum Laude or higher, those with Magna Cum Laude or hight and those with Summa Cum Laude respectively over the years. \code{summary} shows a graph summarizing the graduating seniors over the years.
#' @return Results from the kind of analysis specified by \code{type}.
#' @usage showstats(type)
#' @import ggplot2
#' @import stringr
#' @export
showstats <- function(type){

  # Load all names data in list
  names <- list( "2000"=readgraduates( 2000 ),"2001"=readgraduates( 2001 ),
                 "2002"=readgraduates( 2002 ),"2003"=readgraduates( 2003 ),
                 "2004"=readgraduates( 2004 ),"2005"=readgraduates( 2005 ),
                 "2006"=readgraduates( 2006 ),"2007"=readgraduates( 2007 ),
                 "2008"=readgraduates( 2008 ),"2009"=readgraduates( 2009 ),
                 "2010"=readgraduates( 2010 ),"2011"=readgraduates( 2011 ),
                 "2012"=readgraduates( 2012 ),"2013"=readgraduates( 2013 ),
                 "2014"=readgraduates( 2014 ),"2015"=readgraduates( 2015 ))

  data = data.frame( Year = rep( 2000:2015, each = 4 ),
                     Legend = c( "D: Summa Cum Laude","C: Magna Cum Laude","B: Cum Laude","A: Others" ),
                     Number = 0, ypos = 0 )
  # Number of Summa Cum Laude, Magna Cum Laude, Cum Laude and graduates
  scl <- data.frame( Year = 2000:2015, Number = 0)
  mcl <- data.frame( Year = 2000:2015, Number = 0)
  cl <- data.frame( Year = 2000:2015, Number = 0)
  graduates <- data.frame( Year = 2000:2015, Number = 0)
  for (i in 1:16){
    graduates[[2]][[i]] <- length( names[[i]] ) - 4
    for (j in 1:length(names[[i]])){
      if ( ! is.na(str_locate(names[[i]][[j]],"Magna Cum Laude")[1])){
        scl[[2]][[i]] <- j-2
        data[[3]][[(i-1)*4+1]] <- j-2
      }
      if ( ! is.na(str_locate(names[[i]][[j]],", Cum Laude")[1])){
        mcl[[2]][[i]] <- j-3
        data[[3]][[(i-1)*4+2]] <- j-3-scl[[2]][[i]]
      }
    }
    for (j in 1:length(names[[i]])){
      if ( ! is.na(str_locate(names[[i]][[j]],"Bachelor of Arts")[1])){
        cl[[2]][[i]] <- j-4
        data[[3]][[(i-1)*4+3]] <- j-4-mcl[[2]][[i]]
      }
    }
    data[[3]][[i*4]] <- length( names[[i]] )-cl[[2]][[i]]
    # ypos
    data[[4]][[(i-1)*4+1]] <- scl[[2]][[i]]
    data[[4]][[(i-1)*4+2]] <- mcl[[2]][[i]]
    data[[4]][[(i-1)*4+3]] <- cl[[2]][[i]]
    data[[4]][[i*4]] <- graduates[[2]][[i]]
  }

  #------------------------------------------------------------------------
  if(type=="graduates"){

    # Plots the graph
    ggplot( data = graduates, aes( x = Year, y = Number ) ) +
      geom_bar( stat= "identity", fill = "steelblue" ) +
      geom_text(aes(label=Number), vjust=-0.6, color="black", size=3.5)+
      ggtitle("Number of Graduating Seniors from 2000 to 2015")+
      theme_minimal()

  } else if(type=="SCL"){

    # Plots the graph
    ggplot( data = scl, aes( x = Year, y = Number ) ) +
      geom_bar( stat= "identity", fill = "steelblue" ) +
      geom_text(aes(label=Number), vjust=-0.6, color="black", size=3.5)+
      ggtitle("Number of Summa Cum Laude from 2000 to 2015")+
      theme_minimal()

  } else if(type=="MCL"){

    # Plots the graph
    ggplot( data = mcl, aes( x = Year, y = Number ) ) +
      geom_bar( stat= "identity", fill = "steelblue" ) +
      geom_text(aes(label=Number), vjust=-0.6, color="black", size=3.5)+
      ggtitle("Number of Magna Cum Laude and Above from 2000 to 2015")+
      theme_minimal()

  } else if(type=="CL"){

    # Plots the graph
    ggplot( data = cl, aes( x = Year, y = Number ) ) +
      geom_bar( stat= "identity", fill = "steelblue" ) +
      geom_text(aes(label=Number), vjust=-0.6, color="black", size=3.5)+
      ggtitle("Number of Cum Laude and Above from 2000 to 2015")+
      theme_minimal()

  } else if(type=="summary"){

    ggplot(data=data, aes(x=Year, y=Number, fill=Legend)) +
      geom_bar(stat="identity")+
      geom_text(aes(y=ypos, label=Number), vjust=-0.6,
                color="black", size=3.5)+
      scale_fill_brewer(palette="Paired")+
      theme_minimal()

  } else{
    warning("The specified type does not exist. Enter ?showstats or help(showstats) to view all the summary options available.")
  }
}
