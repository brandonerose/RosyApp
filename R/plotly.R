#' @import RosyUtils
#' @title plotly_bar
#' @export
plotly_bar<-function(DF,x_col,y_col,name){
  fig <- plotly::plot_ly(
    DF,
    x = x_col,
    y = y_col,
    type = 'bar',
    name = name,
    text=x_col,
    textposition="inside",
    insidetextanchor="middle",
    insidetextfont=list(color="black")
  ) %>% plotly::layout(
    xaxis = list(
      title = list(
        # text=paste0(name," (",prettyNum(sum(b$n_applicants),","),")"),
        font=list(
          size=12,
          color="black"
        )),
      tickfont=list(
        size=12,
        color="black"
      )),
    yaxis = list(title = '',
                 tickfont=list(
                   size=10,
                   color="black"
                 )
    ),
    barmode = 'stack',
    annotations = list(
      x = x_col,
      y = y_col,
      xanchor="left",
      xref="x",
      yref="y",
      text = paste0((x_col/sum(x_col)*100) %>% round(1),"%"),
      showarrow = F,
      arrowhead = NULL,
      arrowsize = NULL,
      font=list(size=12,
                color="black"),
      textangle=0
    )
  ) %>%
    plotly::config(scrollZoom=F, displaylogo = F,
                   modeBarButtonsToRemove = c(
                     "zoom2d",
                     "pan2d",
                     "select2d",
                     "lasso2d",
                     # "zoomIn2d",
                     # "zoomOut2d",
                     "autoScale2d",
                     # "resetScale2d",
                     "hoverclosest",
                     "hoverCompareCartesian",
                     "toggleHover"
                   )) %>%
    plotly::layout(showlegend = F,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T)
    ) %>% plotly::style(hoverinfo = 'none')
  fig
}
#' @title plotly_parcats
#' @export
plotly_parcats<-function(DF, remove_missing = T,line_shape_curved = T, numeric_to_factor = T,quantiles = 5,more_descriptive_label = F){
  line_shape <- "linear"
  the_labels <- get_labels(DF)
  if(line_shape_curved)line_shape <- "hspline"
  if(remove_missing){
    DF <- na.omit(DF)
  }else{
    DF <- DF %>% lapply(function(col){
      OUT <- col
      if(is.factor(OUT))levels(OUT)<- c(levels(OUT),"*Missing*")
      OUT[which(is.na(OUT))] <- "*Missing*"
      return(OUT)
    }) %>% as.data.frame()
  }
  if(nrow(DF)==0){return(plotly::plotly_empty())}
  DF <- DF %>% lapply(function(col){
    OUT <- col
    if(!is.factor(OUT)){
      if(is.numeric(OUT)){
        if(numeric_to_factor){
          OUT <- OUT %>% numeric_to_cats(method = "quantile", quantiles = quantiles, more_descriptive_label = more_descriptive_label)
        }
      }else{
        OUT <- as.factor(OUT)
      }
    }
    return(OUT)
  }) %>% as.data.frame()
  adj_margins_l <- adjust_margins(max(nchar(as.character(DF[[1]]))))
  adj_margins_r <- adjust_margins(max(nchar(as.character(DF[[ncol(DF)]]))))
  color_palette <- RColorBrewer::brewer.pal(12, "Paired")  # Using 3 colors for "High", "Medium", "Low"
  color_palette_vec <- color_palette %>% sample(size = length(levels(DF[[1]])),replace =  length(levels(DF[[1]])))
  fig <- plotly::plot_ly(
    # data = DF,
    # x = x_col,
    # y = y_col,
    type = 'parcats',
    dimensions = lapply(colnames(DF), function(col) {
      categoryarray <- as.character(unique(DF[[col]]))
      if(is.factor(DF[[col]])){
        categoryarray <- categoryarray[match( levels(DF[[col]]), categoryarray)] %>% drop_nas()
      }
      out_list <-  list(
        values = DF[[col]],
        label = the_labels[which(colnames(DF)==col)],
        categoryorder = "array",
        categoryarray = categoryarray %>% rev()
      )
      return(out_list)
    }),
    line = list(
      shape = line_shape,
      color = color_palette_vec[as.integer(DF[[1]])]  # Use the numeric representation for colors
      # colorscale = colorscale,  # Define the new colorscale
      # cmin =min(as.numeric(DF[[1]])),  # Set min for color scaling
      # cmax = max(as.numeric(DF[[1]]))   # Set max for color scaling
    ),
    arrangement = "freeform",
    # name = name,
    # text=x_col,
    # textposition="inside",
    # insidetextanchor="middle",
    # insidetextfont=list(color="black"),
    labelfont = list(
      size=14,
      color="black"
    ),
    tickfont = list(
      size=12,
      color="black"
    )
  ) %>%
    plotly::config(
      # scrollZoom=T,
      displaylogo = F
    ) %>%
    plotly::layout(
      showlegend = F,
      margin = list(
        l = adj_margins_l,
        r = adj_margins_r
      )
      #l = 100, r = 100)
    ) %>% plotly::style(hoverinfo = 'none')
  return(fig)
}
adjust_margins <- function(max_label_length, tick_font_size = 12, base_margin = 20) {
  extra_margin <- max_label_length * tick_font_size * 0.4  # Adjust multiplier as needed
  out <- base_margin + extra_margin
  return(out)
}
#' @title plotify
#' @export
plotify<-function(GG){
  GG %>% plotly::ggplotly(tooltip = "text") %>%
    plotly::config(scrollZoom=F, displaylogo = F,
                   modeBarButtonsToRemove = c(
                     "zoom2d",
                     "pan2d",
                     "select2d",
                     "lasso2d",
                     # "zoomIn2d",
                     # "zoomOut2d",
                     "autoScale2d",
                     # "resetScale2d",
                     "hoverClosestCartesian",
                     "hoverCompareCartesian"
                   )) %>%
    plotly::layout(hoverlabel = list(align = "left"),
                   xaxis = list(
                     tickfont=list(
                       size=10,
                       color="black"
                     )),
                   yaxis = list(
                     tickfont=list(
                       size=10,
                       color="black"
                     )
                   )
    )
}
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
