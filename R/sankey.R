#' @import RosyUtils
#' @title make_parcats
#' @export
make_parcats<-function(
    DF,
    remove_missing = F,
    marginal_histograms = T
){
  if(remove_missing){
    DF <- na.omit(DF)
  }
  colnames(DF) <-  colnames(DF) %>% lapply(function(col) {
    x<- attr(DF[[col]], "label")
    ifelse(is.null(x),col,x)
  }) %>% unlist()
  parcats::parcats(
    easyalluvial::alluvial_wide(DF,NA_label = "*Missing*"),
    marginal_histograms = marginal_histograms,
    data_input = DF,
    labelfont=list(size = 12,color = "black"),
    tickfont=list(size = 12,color = "black"),
    arrangement = "freeform"
  )
}
