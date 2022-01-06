
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

``` r
library(myCitation)
## basic example code

scholar::set_scholar_mirror("https://sc.panda321.com")
chromever = "96.0.4664.45"
geckover = "0.30.0"
port = 4445L
killtask_by_port(port)
driver<- RSelenium::rsDriver(browser = "chrome",
                  port = port,
                  chromever = chromever,
                  geckover = geckover,
                  phantomver = NULL)
browser <- driver[["client"]]

id = "WU7fIIwAAAAJ"
profile_url = build_scholar_publication_url(id)
browser$navigate(profile_url)
```

### Publications

``` r
data("profilePageSource")
publication = parse_scholar_publications(profilePageSource)
publication
```

### Citations

#### Build Citation URLs

``` r
citation_url = lapply(1:nrow(publication), function(i){
  scholar_cites_url(cid = publication$cid[[i]], total = publication$cites[[i]])
})

citation_url2 = unlist(citation_url, recursive = FALSE) 
citation_url2 = citation_url2[str_starts(citation_url2,"http")]
```

#### Fetch Citation Page Sources

``` r
get_pagesource = function(url, browser, retry = 3 ){
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
    Sys.sleep(3)
    pageSource = get_pagesource(url = url, browser = browser, retry - 1) 
  }
  return(pageSource)
}
```

``` r
citations = lapply(citation_url2, function(url){
  get_pagesource(url)
  Sys.sleep(1)
})
```

``` r
old_result = lapply(citations, function(x){
  tryCatch(parse_scholar_cites(x),
           error = function(e) e)
})

new_result = lapply(citation_pageSource, function(x){
  tryCatch(parse_scholar_cites(x),
           error = function(e) e)
})

old_success = lapply(old_result, function(x) {
  if (!is_tibble(x)){
    return(FALSE)
  } else if (nrow(x) == 0){
    return(FALSE)
  } else {
    return(TRUE)
  }
}) %>% unlist

table(old_success)

new_success = lapply(new_result, function(x) {
  if (!is_tibble(x)){
    return(FALSE)
  } else if (nrow(x) == 0){
    return(FALSE)
  } else {
    return(TRUE)
  }
}) %>% unlist

table(new_success)


if ( identical(old_success, new_success)){
  warning("new result is equal to old one.")
} else {
  citations = citation_pageSource
  usethis::use_data(citations, overwrite = TRUE)
  success = new_success
}
```

``` r
data("citations")
citation_pageSource = vector("list", length(citation_url2))
```

``` r
wait = rnorm(100,30,20) %>% round() %>% abs()
for (i in seq_along(citation_url2)){
  is_success = success[[i]]
  currentUrl = citation_url2[[i]]
  if (is_success){
    citation_pageSource[[i]] = citations[[i]]
  } else {
    pageSource = get_pagesource(currentUrl, browser = browser, retry = 3)
    citation_pageSource[[i]] = pageSource
    Sys.sleep(sample(wait, 1))
  }
}
```

#### From Citation Page Sources to Tibble

``` r
while (length(failed_index) > 5) {
  
}

citation_data = lapply(citations, parse_scholar_cites) %>%
  bind_rows()
```
