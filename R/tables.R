#' @title make_table1
#' @export
make_table1<-function(
    df_table,
    group = "no_choice",
    variables,
    render.missing = F
){
  has_group <- group!="no_choice"
  # if(any(!x))warning(paste0(x,collapse = ", ")," <- not in the form you specified")
  if(!is_something(variables))return(h3("Nothing to return!")) # stop("Provide variable names of at least length 1!")
  # caption  <- "Basic stats"
  # footnote <- "ᵃ Also known as Breslow thickness"
  if(has_group){
    df_table$group <-  df_table[[group]] %>% factor()
    df_table <- df_table[which(!is.na(df_table$group)),] %>% clone_attr(from = df_table)
    variables<-variables[which(variables!=group)]
  }
  forumla <- paste0(variables, collapse = " + ")
  if(has_group)forumla <- paste0(forumla, " | group")
  forumla <- as.formula(paste0("~",forumla))
  if(render.missing){
    table1::table1(forumla,data=df_table)
  }else{
    table1::table1(forumla,data=df_table,render.missing=NULL)
  }
}
save_table1 <- function(table1,filepath){
  table1 %>%
    table1::t1flex() %>%
    flextable::bg(bg="white",part = "all") %>%
    flextable::save_as_image(
      path = filepath
    )
}
clone_attr <- function(to,from){
  units_vec <- from %>% lapply(function(col){attr(col,"units")}) %>% unlist()
  label_vec <- from %>% lapply(function(col){attr(col,"label")}) %>% unlist()
  to_cols <- colnames(to)
  if(is_something(units_vec)){
    for(i in 1:length(units_vec)){
      x <- units_vec[i]
      col <- names(x)
      if(col%in%to_cols){
        attr(to[[col]],"units") <- as.character(x)
      }
    }
  }
  if(is_something(label_vec)){
    for(i in 1:length(label_vec)){
      x <- label_vec[i]
      col <- names(x)
      if(col%in%to_cols){
        attr(to[[col]],"label") <- as.character(x)
      }
    }
  }
  return(to)
}
