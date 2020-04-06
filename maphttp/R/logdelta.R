#' logdelta will return the difference of two files
#'
#' A file name "logdelta" that can be passed to a function is returned.
#' Data is saved in "logdelta" which are the last n lines of logfile2.
#' @param logfile1 This is the old access_log from apache webserver
#' @param logfile2 This is the current logfile from apache webserver
#' @keywords access_log apache httpd
#' @examples
#' logdelta("old_access_log", "access_log")
#' logdelta("maphttp/old_access_log", "maphttp/access_log")
#
# This will return the IP, timestamps of the delta, or the new entries
# when run repeatedly.
# After running, you should copy the access_log to old_access_log
# This will allow you to run a script every few minutes to plot new access
#
logdelta <- function(logfile1="old_access_log",logfile2="access_log"){
  con1 = file(logfile1, "r")
  con2 = file(logfile2, "r")
  lines1<-length(readLines(con1))
  lines2<-length(readLines(con2))
  delta<-lines2-lines1
  close(con1)
  close(con2)
  print(paste(logfile1,"has", lines1,"records"))
  print(paste(logfile2,"has", lines2,"records"))
  print(paste("There are ", delta,"new lines in", logfile2))
  # continue reading the second file after the first file is done..
  deltalines<-scan(logfile2, what="", skip=lines1-1,nlines=delta,sep="\n",quiet=TRUE)
  write(deltalines,"logdelta")
  return("logdelta")

}
