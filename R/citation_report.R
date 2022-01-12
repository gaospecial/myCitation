# report who, when, where cite one's publication(s)



#' A fancy data table
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
DT_table = function(df){
  df %>% dplyr::mutate_if(is.numeric, round, digits = 3) %>%
    DT::datatable(extensions = c("Buttons"),
                  colnames = var2header(colnames(.)),
                  rownames = FALSE,
                  options = list(
                    dom = 'Bfrtip',
                    scrollX = TRUE,
                    buttons = list(
                      'pageLength',
                      list(extend="excel",
                           filename=paste0(substitute(df)),
                           header=TRUE)
                    )
                  ))
}

var2header = function(x){
  gsub("_"," ",x)
}

my_kable_classic = function(df){
  df %>%
    knitr::kable(digits = 3, col.names = var2header(colnames(.))) %>%
    kableExtra::kable_classic(font_size = 10, full_width = FALSE)
}
