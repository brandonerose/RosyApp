#' @import RosyUtils
#' @title make_parcats
#' @export
make_parcats<-function(
    DF,
    variables,
    marginal_histograms = T
){
  if(!is_something(variables))return(h3("Nothing to return!")) # stop("Provide variable names of at least length 1!")
  colnames(DF) <-  colnames(DF) %>% lapply(function(col) {
    x<- attr(DF[[col]], "label")
    ifelse(is.null(x),col,x)
  }) %>% unlist()
  parcats::parcats(
    easyalluvial::alluvial_wide(DF),
    marginal_histograms = marginal_histograms,
    data_input = DF,
    labelfont=list(size = 10),
    arrangement = "freeform"
  )
}
