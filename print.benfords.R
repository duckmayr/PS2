##
#' Print a table for Benford's Law test statistics
#'
#' This function prints Leemis' m, Cho-Gains' d, or both, with information
#' regarding the statistical reliability of the statistics, in a table format.
#' @param x a numeric vector or matrix for which the statistics are calculated
#' and printed or the output of benfords()
#'
#' @return In addition to printing the statistics with information on their
#' reliability in a table format, the function also invisibly returns this
#' information for future use (e.g. in write.csv.benfords())
#'
#' @author JB Duck-Mayr
#'
print.benfords <- function(x){
  # In case the user has passed the function raw data:
  if(!is.list(x)){
    x <- benfords(x)
  }
  # Now we can start building the elements of the eventual output
  stars <- c('', '*', '**', '***') # May need to use these, depending on values
  note <- "Signif. codes: 0 '***' 0.01 '**' 0.05 '*' 0.1 ' ' 1" # always prints
  out <- list(note=note) # We start with a list containing just the note
  if('M' %in% names(x)){# and add some content if Leemis' m is an element of x
    M.critvals <- c(0, 0.851, 0.967, 1.212) # the critical values for m
    # next we determine which element of M.critvals is the greatest element
    # less than or equal to m and store the corresponding element of stars
    M.stars <- stars[which(M.critvals == max(M.critvals[M.critvals <= x$M]))]
    out["Leemis' m"] <- paste0(x$M, M.stars) # then we paste the stars on
  }
  if('D' %in% names(x)){# and do the same thing for Cho-Gains' d if it's in x
    D.critvals <- c(0, 1.212, 1.330, 1.569)
    D.stars <- stars[which(D.critvals == max(D.critvals[D.critvals <= x$D]))]
    out["Cho-Gains' d"] <- paste0(x$D, D.stars)
  }
  # Then we paste the statistics together in a table format
  output <- paste(paste(names(out[-1]), collapse='\t\t\t'), 
                  paste(out[-1], collapse='\t\t'), sep ='\n')
  # And print out everything with the note and some extra spacing
  cat('\n', output, '\n\n', note, '\n\n')
  invisible(out)
}