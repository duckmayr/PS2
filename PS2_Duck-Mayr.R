
######################
##                  ##
##  Problem Set 2   ##
##  JB Duck-Mayr    ##
##                  ##
######################

## The following function will calculate Leemis' M and Cho-Gains' D.
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

