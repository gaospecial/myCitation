# pkg.env = new.env()
# pkg.env$remDr = start_browser()

#' Start a browser
#'
#' @param browser only support chrome
#' @param chromever the version of your Chrome
#'
#' @return
#' @export
#'
#' @examples
start_browser = function(browser = "chrome", chromever = "96.0.4664.45", port = 4445L, startpage = getOption("scholar_site")){
  # cleanup 4445 port
  killtask_by_port(port)
  library(RSelenium)
  ## just check the browser version of Chrome and use same version of Driver
  # binman::list_versions("chromedriver")
  ## rsDriver() can download server and driver automatically
  driver<- RSelenium::rsDriver(browser = browser,
                    port = port,
                    chromever = chromever,
                    geckover = NULL,
                    phantomver = NULL)
  remDr <- driver[["client"]]
  remDr$navigate(startpage)
  return(remDr)
}

killtask_by_port = function(port = 4445){
  task = system("netstat -aon", intern = TRUE)
  task = strsplit(task[grep(paste0("0.0.0.0:", port),task)], split = "\\s+")
  if (length(task)<1) return(NULL)
  pid = as.integer(task[[1]][[6]])
  tools::pskill(pid)
}


