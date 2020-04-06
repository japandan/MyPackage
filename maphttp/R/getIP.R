#' An apache access_log parser
#'
#' This function reads an apache access_log file and returns
#' the IP addresses and the reformated access date & time.
#' It is designed to be used with a Geolocator package to determine that
#' country of origin of each website access and the time.
#'
#' @param logfile Defaults to "access_log".
#' @keywords httpd apache access_log
#' @export
#' @examples
#' getIP("logdelta")
#' getIP("maphttp/logdelta")
#'

getIP <- function(logfile="access_log"){

  library(stringr)
  library(lubridate)

  con = file(logfile, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    #print(line)
    IP <- str_extract(line,"\\d+.\\d+.\\d+.\\d+")
    ## strip off the GMT offset and brackets
    ToD <- str_extract(line,"\\[(.*)\\+0000\\]")
    AccessDateTime <- substring(ToD, 2, nchar(ToD)-7)
    #print(AccessDateTime)
    DateTimeVar<-dmy_hms(AccessDateTime)
    #print(DateTimeVar)
    #print(as.numeric(DateTimeVar))
    print(paste(IP,",", DateTimeVar))

  }

  close(con)

}
