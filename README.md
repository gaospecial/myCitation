
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


browser = start_browser()
id = "WU7fIIwAAAAJ"
profile_url = build_scholar_publication_url(id)
browser$navigate(profile_url)
```

### Publications

``` r
devtools::load_all()
profilePageSource = data("profilePageSource")
publication = parse_scholar_publications(profilePageSource)
publication
```

### Citations

``` r
citation_url = lapply(1:nrow(publication), function(i){
  scholar_cites_url(cid = publication$cid[[i]], total = publication$cites[[i]])
})

citation_url2 = unlist(citation_url, recursive = FALSE)
```

``` r
data = lapply(citation_url2, function(url){
  browser$navigate(url)
  pageSource = brwoser$getPageSource()[[1]]
  parse_scholar_cites(pageSource)
})
```
