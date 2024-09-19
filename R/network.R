#' @import RosyUtils
#' @title create_node_edge
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
create_node_edge <- function(){
  is_DiagrammeR <- type =="DiagrammeR"
  is_visNetwork <- type =="visNetwork"
  node_df <- NULL
  edge_df <- NULL
  instruments <- DB$redcap$instruments
  fontcolor <- "black"
  instrument_color <- "#FF474C"
}
node_aes_names_DiagrammR <- function(){
  list(
    shape = NULL,
    style = NULL,
    penwidth = NULL,
    color = NULL,
    fillcolor = NULL,
    image = NULL,
    fontname = NULL,
    fontsize = NULL,
    fontcolor = NULL,
    peripheries = NULL,
    height = NULL,
    width = NULL,
    x = NULL,
    y = NULL,
    group = NULL,
    tooltip = NULL,
    xlabel = NULL,
    URL = NULL,
    sides = NULL,
    orientation = NULL,
    skew = NULL,
    distortion = NULL,
    gradientangle = NULL,
    fixedsize = NULL,
    labelloc = NULL,
    margin = NULL
  ) %>% names()
}
edge_aes_names_DiagrammR <- function(){
  list(
    style = NULL,
    penwidth = NULL,
    color = NULL,
    arrowsize = NULL,
    arrowhead = NULL,
    arrowtail = NULL,
    fontname = NULL,
    fontsize = NULL,
    fontcolor = NULL,
    len = NULL,
    tooltip = NULL,
    URL = NULL,
    label = NULL,
    labelfontname = NULL,
    labelfontsize = NULL,
    labelfontcolor = NULL,
    labeltooltip = NULL,
    labelURL = NULL,
    edgetooltip = NULL,
    edgeURL = NULL,
    dir = NULL,
    headtooltip = NULL,
    headURL = NULL,
    headclip = NULL,
    headlabel = NULL,
    headport = NULL,
    tailtooltip = NULL,
    tailURL = NULL,
    tailclip = NULL,
    taillabel = NULL,
    tailport = NULL,
    decorate = NULL
  )
}
node_aes_names_visNetwork <- function(){
  list(
    id = NULL,
    shape = NULL,
    size = NULL,
    title = NULL,
    value = NULL,
    x = NULL,
    y = NULL,
    label = NULL,
    level = NULL,
    group = NULL,
    hidden = NULL,
    image = NULL,
    mass = NULL,
    physics = NULL,
    borderWidth = NULL,
    borderWidthSelected = NULL,
    brokenImage = NULL,
    labelHighlightBold = NULL,
    color = NULL,
    opacity = NULL,
    fixed = NULL,
    font = NULL,
    icon = NULL,
    shadow = NULL,
    scaling = NULL,
    shapeProperties = NULL,
    heightConstraint = NULL,
    widthConstraint = NULL,
    margin = NULL,
    chosen = NULL,
    imagePadding = NULL,
    ctxRenderer = NULL
  ) %>% names()
}
edge_aes_names_visNetwork <- function(){
  list(
    title = NULL,
    value = NULL,
    label = NULL,
    length = NULL,
    width = NULL,
    dashes = NULL,
    hidden = NULL,
    hoverWidth = NULL,
    id = NULL,
    physics = NULL,
    selectionWidth = NULL,
    selfReferenceSize = NULL,
    selfReference = NULL,
    labelHighlightBold = NULL,
    color = NULL,
    font = NULL,
    arrows = NULL,
    arrowStrikethrough = NULL,
    smooth = NULL,
    shadow = NULL,
    scaling = NULL,
    widthConstraint = NULL,
    chosen = NULL,
    endPointOffset = NULL
  )
}
