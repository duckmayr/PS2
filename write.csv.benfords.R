##
#' Write a CSV containing Benford's Law test statistics
#'
#' This function writes a CSV file containing the output of print.benfords.
#' @param x a numeric vector or matrix for which the statistics are calculated
#' and written to a CSV, output from benfords(), or output from print.benfords()
#' @param filename
#'
#' @return The function writes a CSV to the destination filename with three
#' columns: Leemis' m, Cho-Gains' d, and Note. The CSV will have one row other
#' than the header; the values in the first two columns will be character
#' representations of the statistics' value along with asterisks appended as
#' appropriate to indicate statistical reliability, or "NA" if x is output
#' from benfords where only 'm' or 'd' was specified, and the value for the
#' 'Note' column explains the asterisks' meaning.
#'
#' @author JB Duck-Mayr
#'
write.csv.benfords <- function(x, filename){
  if(!('note' %in% names(x))){# If x is not output from print.benfords, call it
    x <- print.benfords(x)
  }
  header <- "Leemis_m,Cho-Gains_d,Note" # header row (column names)
  # we need to comma separate the values and account for the possibility that
  # x is output from benfords() where only one test was specified
  body <- paste(ifelse("Leemis' m" %in% names(x), x$`Leemis' m`, 'NA'),
                ifelse("Cho-Gains' d" %in% names(x), x$`Cho-Gains' d`, 'NA'),
                x$note, sep=',')
  sink(filename) # this opens a connection to filename
  cat(header, '\n', body, '\n', sep='') # this creates the content for filename
  sink() # this closes the connection to filename
}