#' app to get details on events in packages
#' @param af ArtifSet S4 instance
#' @import shiny
#' @examples
#' af = make_demo_ArtifSet()
#' if (interactive()) {
#'  browse_events(af)
#' }
#' @export
#browse_events = function(af) {
#  stopifnot(inherits(af, "ArtifSet"))
  library(shiny)
  library(bbsBuildArtifacts)
  af <<- make_demo_ArtifSet()
  ec <<- collect_events(af, event_class="errors")
  wc <<- collect_events(af, event_class="warnings")
  get_packnames = function(type) {
   if (type == "errors") return(names(ec$events))
   else if (type == "warnings") return(names(wc$events))
   else if (type == "wontinstall") return(names(wc$noparse_paths))  # should be same in ec,wc
   else stop("unrecognized type")
   }

  ui = fluidPage(
   sidebarLayout(
    sidebarPanel(
     helpText("bbsBuildArtifacts event browser -- very limited slice of data collected 10 December 2021 for demonstration purposes"),
     radioButtons("eventtype", "event type", choices=c("errors", "warnings", "wontinstall")),
     #radioButtons("host", "host", choices=c("nebbiolo2", "machv2", "tokay2")),
     uiOutput("abc")
     ),
    mainPanel(
     tabsetPanel(
      tabPanel("linux", verbatimTextOutput( "errtxt_neb" )),
      tabPanel("windows", verbatimTextOutput( "errtxt_tok" )),
      tabPanel("macos", verbatimTextOutput( "errtxt_mac" ))
      )
     )
    )
   )

