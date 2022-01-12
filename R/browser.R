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
start_browser = function(browser = "firefox", chromever = "96.0.4664.45", geckover = "0.30.0", port = 4445L, startpage = getOption("scholar_site")){
  # cleanup 4445 port
  library(RSelenium)
  ## just check the browser version of Chrome and use same version of Driver
  # binman::list_versions("chromedriver")
  ## rsDriver() can download server and driver automatically
  driver<- RSelenium::rsDriver(browser = browser,
                    port = port,
                    chromever = chromever,
                    geckover = geckover,
                    phantomver = NULL)
  remDr <- driver[["client"]]
  remDr$navigate(startpage)
  return(remDr)
}

#' get page source
#'
#' @param url
#' @param browser
#' @param retry
#'
#' @return
#' @export
#'
#' @examples
get_pagesource = function(url, browser, retry = 3, wait = 3){
  if (retry == 0){
    warning("Failed in get: ", url, "\n")
    return(NULL)
  }
  browser$navigate(url)
  pageSource = browser$getPageSource()[[1]]
  if (str_detect(pageSource, "请完成安全验证")){
    invisible(readline(prompt="Please pass the verification, and press [enter] to continue"))
    pageSource = get_pagesource(url = url, browser = browser, retry - 1)
  } else if (str_detect(pageSource, "网络错误")) {
    Sys.sleep(wait)
    pageSource = get_pagesource(url = url, browser = browser, retry - 1)
  }
  return(pageSource)
}

killtask_by_port = function(port = 4445){
  task = system("netstat -aon", intern = TRUE)
  task = strsplit(task[grep(paste0("0.0.0.0:", port),task)], split = "\\s+")
  if (length(task)<1) return(NULL)
  pid = as.integer(task[[1]][[6]])
  tools::pskill(pid)
}

view_html_in_rstudio = function(source){
  file = tempfile(fileext = ".html")
  writeLines(source, file)
  rstudioapi::viewer(file)
}

