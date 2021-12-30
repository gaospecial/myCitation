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
  publication = scholar::get_publications(id)
  return(profile)
}

get_publications <- function(id, cstart = 0, cstop = Inf, pagesize=100, flush=FALSE, sortby="citation", remDr = remDr) {

  ## Make sure pagesize is not greater than max allowed by Google Scholar
  if (pagesize > 100) {
    warning("pagesize: ", pagesize, " exceeds Google Scholar maximum. Setting to 100.")
    pagesize <- 20
  }

  ## Ensure we're only getting one scholar's publications
  id <- tidy_id(id)

  ## Define the cache path
  cache.dir <- file.path(tempdir(), "r-scholar")
  setCacheRootPath(cache.dir)

  ## Clear the cache if requested
  if (flush) saveCache(NULL, key=list(id, cstart))

  ## Check if we've cached it already
  data <- loadCache(list(id, cstart))

  site <- getOption("scholar_site")

  ## If not, get the data and save it to cache
  if (is.null(data)) {

    ## Build the URL

    stopifnot(sortby == "citation" | sortby == "year")

    if(sortby == "citation"){
      url_template <- paste0(site, "/citations?hl=en&user=%s&cstart=%d&pagesize=%d")
    }

    if(sortby == "year"){
      url_template <- paste0(site, "/citations?hl=en&user=%s&cstart=%d&pagesize=%d&sortby=pubdate")
    }

    url <- sprintf(url_template, id, cstart, pagesize)

    ## Load the page
    pageSource <- get_scholar_resp(url, remDr = remDr)
    page = stringi::stri_unescape_unicode(pageSource) %>%  read_html()
    cites <- page %>% html_nodes(xpath="//tr[@class='gsc_a_tr']")

    title <- cites %>% html_nodes(".gsc_a_at") %>% html_text()
    pubid <- cites %>% html_nodes(".gsc_a_at") %>%
      html_attr("href") %>% str_extract(":.*$") %>% str_sub(start=2)
    doc_id <- cites %>% html_nodes(".gsc_a_ac") %>% html_attr("href") %>%
      str_extract("cites=.*$") %>% str_sub(start=7)
    cited_by <- suppressWarnings(cites %>% html_nodes(".gsc_a_ac") %>%
                                   html_text() %>%
                                   as.numeric(.) %>% replace(is.na(.), 0))
    year <- cites %>% html_nodes(".gsc_a_y") %>% html_text() %>%
      as.numeric()
    authors <- cites %>% html_nodes("td .gs_gray") %>% html_text() %>%
      as.data.frame(stringsAsFactors=FALSE) %>%
      filter(row_number() %% 2 == 1)  %>% .[[1]]

    ## Get the more complicated parts
    details <- cites %>% html_nodes("td .gs_gray") %>% html_text() %>%
      as.data.frame(stringsAsFactors=FALSE) %>%
      filter(row_number() %% 2 == 0) %>% .[[1]]


    ## Clean up the journal titles (assume there are no numbers in
    ## the journal title)
    first_digit <- as.numeric(regexpr("[\\[\\(]?\\d", details)) - 1
    journal <- str_trim(str_sub(details, end=first_digit)) %>%
      str_replace(",$", "")

    ## Clean up the numbers part
    numbers <- str_sub(details, start=first_digit) %>%
      str_trim() %>% str_sub(end=-5) %>% str_trim() %>% str_replace(",$", "")

    ## Put it all together
    data <- dplyr::tibble(title=title,
                       author=authors,
                       journal=journal,
                       number=numbers,
                       cites=cited_by,
                       year=year,
                       cid=doc_id,
                       pubid=pubid)

    ## Check if we've reached pagesize articles. Might need
    ## to search the next page
    if (cstart >= I(cstop)) {
      return(data)
    }

    if (nrow(data) > 0 && nrow(data)==pagesize) {
      data <- rbind(data, get_publications(id, cstart=cstart+pagesize, pagesize=pagesize, remDr = remDr))
    }

    ## Save it after everything has been retrieved.
    if (cstart == 0) {
      saveCache(data, key=list(id, cstart))
    }
  }

  return(data)
}


get_scholar_resp = function(url, attempts_left = 1, remDr = remDr){
  stopifnot( attempts_left > 0)

  remDr$navigate(url)
  pageSource = remDr$getPageSource()[[1]]

  return(pageSource)
}
