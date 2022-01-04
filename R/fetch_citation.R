# fetch citation of publication(s)

scholar_cites_url = function(cid, total = 0){
  if (total == 0) return("")
  if (total <= 10) return(build_scholar_cites_url(cid))
  if (total > 10) starts = seq(0, total, by = 10)
  urls = sapply(starts, build_scholar_cites_url, cid = cid)
  return(urls)
}

build_scholar_cites_url = function(cid, start = 0){
  site <- getOption("scholar_site")
  url_template <- paste0(site, "/scholar?hl=en&start=%d&cites=%s")
  url <- sprintf(url_template, start, cid)
  return(url)
}

parse_scholar_cites = function(pageSource){
  require(rvest)
  page = pageSource %>% read_html()
  cites <- page %>% html_nodes('div.gs_r.gs_or.gs_scl')

  title <- cites %>% html_nodes("h3") %>% html_text()
  url = cites %>% html_nodes("h3 a") %>% html_attr("href")
  cid <- cites %>% html_nodes(".gs_or_cit.gs_or_btn.gs_nph + a") %>%
    html_attr("href") %>% str_extract("cites=[0-9]+") %>% str_remove("cites=")
  cited_time = cites %>% html_nodes(".gs_or_cit.gs_or_btn.gs_nph + a") %>%
    html_text() %>% str_remove("Cited by ") %>% as.integer()
  json_list = lapply(url, query_zotero_translation_server) %>%
    unlist(recursive = FALSE)
  title = sapply(json_list, function(x) x$title)
  journal = sapply(json_list, function(x) x$publicationTitle)
  authors = sapply(json_list, function(x) "NA")
  numbers = sapply(json_list, function(x) paste0(x$volume,"(",x$issue,"): ", x$pages))
  date = sapply(json_list, function(x) x$date)
  year = sapply(date, function(x) lubridate::year(as.Date(x)))
  ## Put it all together
  data <- dplyr::tibble(title=title,
                        author=authors,
                        journal=journal,
                        number=numbers,
                        cites=cited_time,
                        year=year,
                        date = date,
                        cid=cid)
  return(data)
}


query_zotero_translation_server = function(fulltextUrl){
  server = "http://139.129.128.105:1969/web"
  response = httr::POST(
    url = server,
    body = fulltextUrl,
    httr::add_headers(.headers = c(`Content-Type` = 'text/plain')),
    httr::accept_json()
  )

  json_data = httr::content(response, as = "parsed")
  return(json_data)
}

parse_zotero_json = function(json){
  if (length(json) == 1){
    title = json[[1]]$title
    journal = json[[1]]$publicationTitle
    year = json[[1]]$date
  }

}
