##
#' Calculate Benford's Law test statistics
#'
#' This function calculates Leemis' m, Cho-Gains' d, or both for numeric data.
#' @param x a numeric vector or matrix for which the statistics are calculated. 
#' @param type a character vector specifying which statistics to compute;
#' for type = 'm' only Leemis' m is calculated, for type = 'd' only Cho-Gains'
#' d is calculated, and for type = 'both' (the default) both are calculated.
#'
#' @return A list with elements digit.distribution, a table giving the number
#' of elements of x with each first significant digit, M (present only if
#' type is 'm' or 'both'), which gives Leemis' m statistic for x, and
#' D (present only if type is 'd' or 'both'), which gives Cho-Gains' d
#' statistic for x.
#'
#' @author JB Duck-Mayr
#'
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
  # Then return a list containing digit.distribution and the appropriate stats
  return(switch(type, d = list(D=D, digit.distribution=digit.table),
                m = list(M=M, digit.distribution=digit.table),
                both = list(M=M, D=D, digit.distribution=digit.table)))
}