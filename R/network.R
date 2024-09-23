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
  c(
    "shape",
    "style",
    "penwidth",
    "color",
    "fillcolor",
    "image",
    "fontname",
    "fontsize",
    "fontcolor",
    "peripheries",
    "height",
    "width",
    "x",
    "y",
    "group",
    "tooltip",
    "xlabel",
    "URL",
    "sides",
    "orientation",
    "skew",
    "distortion",
    "gradientangle",
    "fixedsize",
    "labelloc",
    "margin"
  )
}
edge_aes_names_DiagrammR <- function(){
  c(
    "style",
    "penwidth",
    "color",
    "arrowsize",
    "arrowhead",
    "arrowtail",
    "fontname",
    "fontsize",
    "fontcolor",
    "len",
    "tooltip",
    "URL",
    "label",
    "labelfontname",
    "labelfontsize",
    "labelfontcolor",
    "labeltooltip",
    "labelURL",
    "edgetooltip",
    "edgeURL",
    "dir",
    "headtooltip",
    "headURL",
    "headclip",
    "headlabel",
    "headport",
    "tailtooltip",
    "tailURL",
    "tailclip",
    "taillabel",
    "tailport",
    "decorate"
  )
}
node_aes_names_visNetwork <- function(){
  c(
    "id",
    "shape",
    # The types with the label inside of it are:
    # ... ellipse, circle, database, box, text
    # The ones with the label outside of it are:
    # ... image, circularImage, diamond, dot, star, triangle, triangleDown, hexagon, square and icon
    "size",
    "title",
    "value",
    "x",
    "y",
    "label",
    "level",
    "group",
    "hidden",
    "image",
    "mass",
    "physics",
    "borderWidth",
    "borderWidthSelected",
    "brokenImage",
    "labelHighlightBold",
    "color",
    "opacity",
    "fixed",
    "font",
    "icon",
    "shadow",
    "scaling",
    "shapeProperties",
    "heightConstraint",
    "widthConstraint",
    "margin",
    "chosen",
    "imagePadding",
    "ctxRenderer"
  )
}
edge_aes_names_visNetwork <- function(){
  c(
    "title",
    "value",
    "label",
    "length",
    "width",
    "dashes",
    "hidden",
    "hoverWidth",
    "id",
    "physics",
    "selectionWidth",
    "selfReferenceSize",
    "selfReference",
    "labelHighlightBold",
    "color",
    "font",
    "arrows",
    "arrowStrikethrough",
    "smooth",
    "shadow",
    "scaling",
    "widthConstraint",
    "chosen",
    "endPointOffset"
  )
}
