# fetch one's publication from Google Scholar, etc

fetch_publication <- function(scholar_id, source = c("google")){
  publication = switch(
    source,
    google = fetch_publication_by_google_scholar(scholar_id),
  )


}

fetch_publication_by_google_scholar = function(id){
  profile = scholar::get_profile(id)
  citation_history = scholar::get_citation_history(id)
  publication = get_publications(id, browser = browser)
  return(profile)
}

get_publications <- function(id, browser = NULL) {

  ## Ensure we're only getting one scholar's publications
  id <- scholar::tidy_id(id)

  ## Define the cache path
  cache.dir <- file.path(tempdir(), "r-scholar")
  R.cache::setCacheRootPath(cache.dir)

  ## Clear the cache if requested
  if (flush) R.cache::saveCache(NULL, key=list(id, cstart))

  ## Check if we've cached it already
  data <- R.cache::loadCache(list(id, cstart))


  ## If not, get the data and save it to cache
  if (is.null(data)) {

    ## Build the URL
    url = build_scholar_publication_url(id)

    ## Load the page
    pageSource <- get_scholar_resp(url, browser = browser)
    data = parse_scholar_publications(pageSource)

    ## Check if we've reached pagesize articles. Might need
    ## to search the next page
    if (cstart >= I(cstop)) {
      return(data)
    }

    if (nrow(data) > 0 && nrow(data)==pagesize) {
      data <- rbind(data, get_publications(id, cstart=cstart+pagesize, pagesize=pagesize, browser = browser))
    }

    ## Save it after everything has been retrieved.
    if (cstart == 0) {
      R.cache::saveCache(data, key=list(id, cstart))
    }
  }

  return(data)
}


#' Recursively try to GET a Google Scholar Page storing session cookies
#'
#' see \code{\link{scholar-package}} documentation for details about Scholar
#' session cookies.
#'
#' @param url URL to fetch
#' @param attempts_left The number of times to try and fetch the page
#'
#' @return an \code{httr::\link{response}} object
#' @seealso \code{httr::\link{GET}}
#' @export
get_scholar_resp <- function(url, attempts_left = 5, browser = NULL) {

  stopifnot(attempts_left > 0)

  if (is.null(browser)){
    resp <- httr::GET(url, handle = scholar::scholar_handle())

    # On a successful GET, return the response
    if (httr::status_code(resp) == 200) {
      return(resp)
    } else if(httr::status_code(resp) == 429){
      stop("Response code 429. Google is rate limiting you for making too many requests too quickly.")
    } else if (attempts_left == 1) { # When attempts run out, stop with an error
      stop("Cannot connect to Google Scholar. Is the ID you provided correct?")
    } else { # Otherwise, sleep a second and try again
      Sys.sleep(1)
      get_scholar_resp(url, attempts_left - 1)
    }
  } else {
    browser$navigate(url)
    currentUrl = browser$getCurrentUrl()[[1]]
    if (currentUrl != url) verified = menu(c("Yes", "No"), title = "Are you pass the verification?")
    if (verified) pageSource = browser$getPageSource()[[1]]
    return(pageSource)
  }

}

#' Build Google scholar publication url
#'
#' @param id
#' @param cstart
#' @param cstop
#' @param pagesize
#' @param sortby
#'
#' @return
#'
#' @examples
build_scholar_publication_url = function(id, cstart = 0, cstop = Inf, pagesize=20, sortby="citation") {
  ## Make sure pagesize is not greater than max allowed by Google Scholar
  if (pagesize > 100) {
    warning("pagesize: ", pagesize, " exceeds Google Scholar maximum. Setting to 100.")
    pagesize <- 100
  }

  site <- getOption("scholar_site")

  stopifnot(sortby == "citation" | sortby == "year")

  if(sortby == "citation"){
    url_template <- paste0(site, "/citations?hl=en&user=%s&cstart=%d&pagesize=%d")
  }

  if(sortby == "year"){
    url_template <- paste0(site, "/citations?hl=en&user=%s&cstart=%d&pagesize=%d&sortby=pubdate")
  }

  url <- sprintf(url_template, id, cstart, pagesize)
  return(url)
}


#' Parse Google scholar publications
#'
#' @param pageSource Html page source of a Google scholar profile page
#'
#' @return
#' @export
#'
#' @examples
parse_scholar_publications = function(pageSource){
  page = pageSource %>% rvest::read_html()
  cites <- page %>% rvest::html_nodes(xpath="//tr[@class='gsc_a_tr']")

  title <- cites %>% rvest::html_nodes(".gsc_a_at") %>% rvest::html_text()
  pubid <- cites %>% rvest::html_nodes(".gsc_a_at") %>%
    rvest::html_attr("href") %>% stringr::str_extract(":.*$") %>% stringr::str_sub(start=2)
  doc_id <- cites %>% rvest::html_nodes(".gsc_a_ac") %>% rvest::html_attr("href") %>%
    stringr::str_extract("cites=.*$") %>% stringr::str_sub(start=7)
  cited_by <- suppressWarnings(cites %>% rvest::html_nodes(".gsc_a_ac") %>%
                                 rvest::html_text() %>%
                                 as.numeric(.) %>% replace(is.na(.), 0))
  year <- cites %>% rvest::html_nodes(".gsc_a_y") %>% rvest::html_text() %>%
    as.numeric()
  authors <- cites %>% rvest::html_nodes("td .gs_gray") %>% rvest::html_text() %>%
    as.data.frame(stringsAsFactors=FALSE) %>%
    dplyr::filter(dplyr::row_number() %% 2 == 1)  %>% .[[1]]

  ## Get the more complicated parts
  details <- cites %>% rvest::html_nodes("td .gs_gray") %>% rvest::html_text() %>%
    as.data.frame(stringsAsFactors=FALSE) %>%
    dplyr::filter(dplyr::row_number() %% 2 == 0) %>% .[[1]]


  ## Clean up the journal titles (assume there are no numbers in
  ## the journal title)
  first_digit <- as.numeric(regexpr("[\\[\\(]?\\d", details)) - 1
  journal <- stringr::str_trim(stringr::str_sub(details, end=first_digit)) %>%
    stringr::str_replace(",$", "")

  ## Clean up the numbers part
  numbers <- stringr::str_sub(details, start=first_digit) %>%
    stringr::str_trim() %>% stringr::str_sub(end=-5) %>% stringr::str_trim() %>% stringr::str_replace(",$", "")

  ## Put it all together
  data <- dplyr::tibble(title=title,
                        author=authors,
                        journal=journal,
                        number=numbers,
                        cites=cited_by,
                        year=year,
                        cid=doc_id,
                        pubid=pubid)
  return(data)
}
