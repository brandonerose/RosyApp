#' @import RosyUtils
#' @import golem
#' @import shiny
#' @import shinydashboard
#' @title dbSidebar
#' @export
dbSidebar <- function(...){
  shinydashboardPlus::dashboardSidebar(
    minified = F,
    collapsed = F,
    TCD_SBH(),
    sidebarMenu(
      id="sb1",
      ...,
      backend_menu_item()
    ),
    TCD_SBF()
  )
}
#' @title backend_menu_item
#' @export
backend_menu_item <- function(){
  if(golem::app_prod())return(NULL)
  return(
    menuItem(
      text="Backend",
      tabName = "backend",
      icon =shiny::icon("gear")
    )
  )
}
#' @title dbBody
#' @export
dbBody <- function(...){
  dashboardBody(
    tabItems(
      ...,
      tabItem(
        "backend",
        fluidRow(
          box(
            title = h1("Input List"),
            width = 12,
            mod_list_ui("input_list")
          ),
          box(
            title = h1("Values List"),
            width = 12,
            mod_list_ui("values_list")
          )
        )
      )
    )
  )
}
#' @title dbHeader
#' @export
dbHeader <- function(...){
  shinydashboardPlus::dashboardHeader(
    title = tagList(
      span(class = "logo-lg", pkg_name),
      tags$a(
        href="https://thecodingdocs.com",
        target="_blank",
        tags$img(src = "www/logo.png", width="100%")
      )
    ),
    ...
  )
}
#' @title dbControlbar
#' @export
dbControlbar <- function(...){
  shinydashboardPlus::dashboardControlbar(
    TCD_SBH(),
    div(style="text-align:center",p(paste0(pkg_name,' Version: ',pkg_version))),
    div(style="text-align:center",p(paste0('Pkg Updated: ',pkg_date))),
    ...,
    TCD_SBF(),
    fluidRow(
      column(
        12,
        p("This app is still in development."),
        p("Consider donating for more."),
        p("Contact with issues."),
        p("Consider using R package."),
        align="center"
      )
    )
  )
}
#' @title mod_list_ui
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @export
mod_list_ui <- function(id) {
  ns <- NS(id)
  tagList(
    listviewer::jsoneditOutput(ns("values_list")),
  )
}
#' @title mod_list_server
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @export
mod_list_server <- function(id,values){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$values_list <- listviewer::renderJsonedit({
      if(!is_something(values))return(NULL)
      values %>% shiny::reactiveValuesToList() %>% listviewer::jsonedit() %>% return()
    })
  })
}
#' @title TCD_SBH
#' @export
TCD_SBH <- function(){
  shinydashboard::sidebarMenu(
    shiny::div(
      style="text-align:center",
      tags$a(
        href="https://thecodingdocs.com",
        target="_blank",
        tags$img(src = "www/logo.png", width="50%")
      )
    )
  )
}
#' @title TCD_SBF
#' @export
TCD_SBF <- function(){
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem(
      text="Donate!",
      icon = shiny::icon("dollar"),
      href="https://account.venmo.com/u/brandonerose"
    ),
    shinydashboard::menuItem(
      text="TheCodingDocs.com",
      icon = shiny::icon("stethoscope"),
      href="https://thecodingdocs.com"
    ),
    shinydashboard::menuItem(
      text="GitHub Code",
      icon = shiny::icon("github"),
      href="https://github.com/brandonerose/"
    ),
    shinydashboard::menuItem(
      text="TheCodingDocs",
      icon = shiny::icon("twitter"),
      href="https://twitter.com/TheCodingDocs"
    ),
    shinydashboard::menuItem(
      text="BRoseMDMPH",
      icon = shiny::icon("twitter"),
      href="https://twitter.com/BRoseMDMPH"
    ),
    shinydashboard::menuItem(
      text="Brandon Rose, MD, MPH",
      icon = shiny::icon("user-doctor"),
      href="https://www.thecodingdocs.com/founder"
    )
  )
  # p(paste0('Version: ',pkg_version)) %>% shiny::div(style="text-align:center"),
  # p(paste0('Last Update: ',pkg_date)) %>% shiny::div(style="text-align:center"),
}
#' @title TCD_NF
#' @export
TCD_NF <- function(){
  shinydashboardPlus::dashboardFooter(
    left = fluidRow(
      shiny::actionButton(
        inputId='ab1',
        label="Donate!",
        icon = shiny::icon("dollar"),
        onclick ="window.open('https://account.venmo.com/u/brandonerose', '_blank')") ,
      shiny::actionButton(
        inputId='ab2',
        label="TheCodingDocs.com",
        icon = shiny::icon("stethoscope"),
        onclick ="window.open('https://thecodingdocs.com', '_blank')") ,
      shiny::actionButton(
        inputId='ab3',
        label="GitHub Code",
        icon = shiny::icon("github"),
        onclick =paste0("window.open('https://github.com/brandonerose/",pkg_name,"', '_blank')")
      ),
      shiny::actionButton(
        inputId='ab4',
        label="TheCodingDocs",
        icon = shiny::icon("twitter"),
        onclick ="window.open('https://twitter.com/TheCodingDocs', '_blank')"
      ) ,
      shiny::actionButton(
        inputId='ab5',
        label="BRoseMDMPH",
        icon = shiny::icon("square-twitter"),
        onclick ="window.open('https://twitter.com/BRoseMDMPH', '_blank')"
      ) ,
      shiny::actionButton(
        inputId='ab6',
        label="Brandon Rose, MD, MPH",
        icon = shiny::icon("user-doctor"),
        onclick ="window.open('https://www.thecodingdocs.com/founder', '_blank')"
      )
    ) %>% shiny::div(style="text-align:center"),
    right = NULL
  )
}
#' @title TCD_SF
#' @export
TCD_SF <- function(){
  shiny::div(
    class = "sticky_footer",
    fluidRow(
      shiny::actionButton(
        inputId='ab1',
        label="Donate!",
        icon = shiny::icon("dollar"),
        onclick ="window.open('https://account.venmo.com/u/brandonerose', '_blank')") ,
      shiny::actionButton(
        inputId='ab2',
        label="TheCodingDocs.com",
        icon = shiny::icon("stethoscope"),
        onclick ="window.open('https://thecodingdocs.com', '_blank')") ,
      shiny::actionButton(
        inputId='ab3',
        label="GitHub Code",
        icon = shiny::icon("github"),
        onclick =paste0("window.open('https://github.com/brandonerose/",pkg_name,"', '_blank')")
      ),
      shiny::actionButton(
        inputId='ab4',
        label="TheCodingDocs",
        icon = shiny::icon("twitter"),
        onclick ="window.open('https://twitter.com/TheCodingDocs', '_blank')"
      ) ,
      shiny::actionButton(
        inputId='ab5',
        label="BRoseMDMPH",
        icon = shiny::icon("square-twitter"),
        onclick ="window.open('https://twitter.com/BRoseMDMPH', '_blank')"
      ) ,
      shiny::actionButton(
        inputId='ab6',
        label="Brandon Rose, MD, MPH",
        icon = shiny::icon("user-doctor"),
        onclick ="window.open('https://www.thecodingdocs.com/founder', '_blank')"
      )
    )
  )
}
#' @title make_DT_table
#' @export
make_DT_table<-function(DF,editable = F,selection="single",paging = TRUE,scrollY = F,searching = T){
  if(!is_something(DF)){
    return(h3("No data available to display."))
  }
  DF %>% DT::datatable(
    selection = selection,
    editable = editable,
    rownames = F,
    fillContainer = T,
    # extensions = 'Buttons',
    options = list(
      columnDefs = list(list(className = 'dt-center',targets = "_all")),
      paging = paging,
      pageLength = 50,
      fixedColumns = FALSE,
      ordering = TRUE,
      scrollY = scrollY,
      scrollX = T,
      # autoWidth = T,
      searching = searching,
      dom = 'Bfrtip',
      # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      scrollCollapse = F,
      stateSave = F
    ),
    class = "cell-border",
    filter = 'top',
    escape =F
  ) %>% DT::formatStyle(
    colnames(DF),
    color = "#000"
  )
}
