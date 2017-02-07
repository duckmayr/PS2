
######################
##                  ##
##  Problem Set 2   ##
##  JB Duck-Mayr    ##
##                  ##
######################

## benfords() calculates Leemis' M and/or Cho-Gains' D.
## The function's arguments are x, a data vector or matrix,
## and an option type, taking the values 'm', 'd', or 'both'.
## If 'm' is specified for type, the function returns only Leemis' M.
## If 'd' is specified for type, the function returns only Cho-Gains' D.
## If 'both' or nothing is specified for type, both statistics are returned.

benfords <- function(x, type='both'){
  # First we need the distribution of first digits that are ones, twos, etc.
  # It will be convenient for this purpose to convert the data to character
  x <- as.character(x) 
  digit.table <- table(factor(regmatches(x, regexpr('[1-9]', x)), levels=1:9))
  # The we calculate a vector that is used in both Leemis' M and Cho-Gains' D
  common.vector <- digit.table/length(x) - log10(1 + 1/1:9)
  # If type is either 'm' or 'both', we calculate and store Leemis' M
  if(type != 'd'){
    M <- max(common.vector)
  }
  # If type is either 'd' or 'both', we calculate and store Cho-Gains' D
  if(type != 'm'){
    D <- sqrt(sum(common.vector^2))
  }
  # Then we return a list always containing digit.distribution
  return(switch(type, d = list(D=D, digit.distribution=digit.table),
                m = list(M=M, digit.distribution=digit.table),
                both = list(M=M, D=D, digit.distribution=digit.table)))
}

## print.benfords() prints the Benford's law statistics in a table format
## with information regarding statistical reliability.
## The function takes as input either raw data or output from benfords()

print.benfords <- function(x){
  # In case the user has passed the function raw data:
  if(!is.list(x)){
    x <- benfords(x)
  }
  # Now we can start building the elements of the eventual output
  stars <- c('', '*', '**', '***') # May need to use these, depending on values
  note <- "Signif. codes: 0 '***' 0.01 '**' 0.05 '*' 0.1 ' ' 1" # always prints
  out <- list() # We start with an empty list
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
  output <- paste(paste(names(out), collapse='\t\t\t'), 
                  paste(out, collapse='\t\t'), sep ='\n')
  # And print out everything with the note and some extra spacing
  cat(c('\n', output, '\n\n', note, '\n\n'))
}