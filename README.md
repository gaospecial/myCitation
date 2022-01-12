
<!-- README.md is generated from README.Rmd. Please edit that file -->

# myCitation

<!-- badges: start -->
<!-- badges: end -->

The goal of myCitation is to …

## Installation

You can install the development version of myCitation from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gaospecial/myCitation")
```

## Example

This is a basic example which shows you how to solve a common problem:

### Start a browser

Use Chrome or Firefox client to overcome access restriction on server
side.

``` r
library(myCitation)
## basic example code

chromever = "96.0.4664.45"
geckover = "0.30.0"
port = 4445L
killtask_by_port(port)
remDr <- RSelenium::remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444L,
  browserName = "firefox"
)
remDr$open()
driver<- RSelenium::rsDriver(browser = "firefox",
                  port = port,
                  chromever = NULL,
                  geckover = geckover,
                  phantomver = NULL)
browser <- driver[["client"]]
```

### Publications

Access Google Scholar profile page.

``` r
scholar::set_scholar_mirror("https://xs2.dailyheadlines.cc/")

id = "WU7fIIwAAAAJ"
profile_url = build_scholar_publication_url(id)
browser$navigate(profile_url)
```

``` r
profilePageSource = browser$getPageSource()[[1]]
```

Parse HTML page to get publication list.

``` r
library(stringr)
library(dplyr)
publication = parse_scholar_publications(profilePageSource)
publication
usethis::use_data(publication, overwrite = TRUE)
```

### Citations

#### Build Citation URLs

From publications to citations.

``` r
url = lapply(1:nrow(publication), function(i){
  build_scholar_cites_url(cid = publication$cid[[i]], total = publication$cites[[i]])
})
citation_url = list(pubid = publication$pubid, url = url) %>%
  as_tibble() %>% 
  tidyr::unnest(url) %>%
  na.omit()
```

#### Fetch Citation Page Sources

This is a time-consuming step.

``` r
citationPageSource = vector("list", nrow(citation_url))
# wait randomly
wait = rnorm(100,30,20) %>% round() %>% abs()
for (i in 1:nrow(citation_url)){
  currentUrl = citation_url$url[[i]]
  pageSource = get_pagesource(currentUrl, browser = browser, retry = 3)
  citationPageSource[[i]] = pageSource
  Sys.sleep(sample(wait, 1))
}
```

#### From Citation Page Sources to Tibble

``` r
library(dplyr)
source = lapply(citationPageSource, parse_scholar_cites)
citation_data = list(pubid = citation_url$pubid, data = source) %>%
  as_tibble() %>%
  tidyr::unnest(cols = "data")

usethis::use_data(citation_data, overwrite = TRUE)
```

``` r
unique_fulltextUrl = unique(citation_data$fulltextUrl)
```

``` r
zotero_json = lapply(unique_fulltextUrl, function(x){
  result = tryCatch(query_zotero_translation_server(x),
           error = function(e) "Error")
  Sys.sleep(1)
  return(result)
})
```

``` r
d = lapply(zotero_json, parse_zotero_json)
zotero_data = list(fulltextUrl = unique_fulltextUrl, data = d) %>%
  as_tibble() %>%
  tidyr::unnest(cols = "data") %>%
  tidyr::drop_na(title, publicationTitle, fulltextUrl)
usethis::use_data(zotero_data, overwrite = TRUE)
```
