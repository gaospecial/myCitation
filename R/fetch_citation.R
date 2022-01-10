# fetch citation of publication(s)

scholar_cites_url = function(cid, total = 0){
  if (total == 0) return("")
  if (total <= 10) return(build_scholar_cites_url(cid))
  if (total > 10) starts = seq(0, total, by = 10)
  urls = sapply(starts, build_scholar_cites_url, cid = cid)
  return(urls)
}

#' build Google Scholar cites URL
#'
#' @param cid Google cite id
#' @param start start, i.e. 10, 20, 30
#'
#' @return a valid URL
#' @export
#'
#' @examples
build_scholar_cites_url = function(cid, start = 0){
  site <- getOption("scholar_site")
  url_template <- paste0(site, "/scholar?hl=en&start=%d&cites=%s")
  url <- sprintf(url_template, start, cid)
  return(url)
}

#' parse Google Scholar cite page source
#'
#' @param pageSource HTML source
#'
#' @importFrom stringr str_remove str_extract
#' @importFrom rvest read_html html_node html_nodes html_text html_element html_attr
parse_scholar_cites = function(pageSource){
  page = pageSource %>% rvest::read_html()
  cites <- page %>% rvest::html_nodes('div.gs_r.gs_or.gs_scl')
  title <- cites %>% rvest::html_node("h3") %>% rvest::html_text()
  url = cites %>% rvest::html_node("h3") %>% rvest::html_element("a") %>% rvest::html_attr("href")
  line2 = cites %>% rvest::html_node('div.gs_a') %>% rvest::html_text()
  authors = line2 %>% stringr::str_remove("-.*") %>% trimws(whitespace = "[\\h\\v]")
  journal_and_year = line2 %>% stringr::str_remove(".*?-") %>%
    stringr::str_remove("-.*") %>% trimws(whitespace = "[\\h\\v]")
  journal = journal_and_year %>% stringr::str_remove("[0-9]{4}") %>%
    trimws(whitespace = "[\\h\\v]") %>%
    stringr::str_remove(",$")
  year = journal_and_year %>% stringr::str_extract("[0-9]{4}")
  cid <- cites %>% rvest::html_node(".gs_or_cit.gs_or_btn.gs_nph + a") %>%
    rvest::html_attr("href") %>% stringr::str_extract("cites=[0-9]+") %>% stringr::str_remove("cites=")
  cited_time = cites %>% rvest::html_node(".gs_or_cit.gs_or_btn.gs_nph + a") %>%
    rvest::html_text() %>% stringr::str_extract("Cited by [0-9]+") %>% stringr::str_extract("[0-9]+")
  ## Put it all together
  data <- dplyr::tibble(title=title,
                        author=authors,
                        journal=journal,
                        year=year,
                        cites=cited_time,
                        cid=cid,
                        fulltextUrl = url)
  return(data)
}

if (FALSE) {
  json_list = lapply(url, query_zotero_translation_server) %>%
    unlist(recursive = FALSE)
  title = sapply(json_list, function(x) x$title)
  journal = sapply(json_list, function(x) x$publicationTitle)
  authors = sapply(json_list, function(x) "NA")
  numbers = sapply(json_list, function(x) paste0(x$volume,"(",x$issue,"): ", x$pages))
  date = sapply(json_list, function(x) x$date)
  year = sapply(date, function(x) lubridate::year(as.Date(x)))
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
