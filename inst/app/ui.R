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
  build_hosts <<- c(linux="nebbiolo2", macos="machv2", windows="tokay2")

  ui = fluidPage(
   sidebarLayout(
    sidebarPanel(
     helpText("event browser"),
     radioButtons("eventtype", "event type", choices=c("errors", "warnings", "wontinstall")),
     radioButtons("phase", "phase", choices=c("install", "buildsrc", "checksrc", "buildbin"),
       selected="checksrc"),
     uiOutput("pack_selector"),
#     actionButton("Reset", "Reset packs."),
     actionButton("stopBtn", "Stop app.")
     ),
    mainPanel(
     tabsetPanel(id="curtab",
      tabPanel("linux", id="linux", verbatimTextOutput( "errtxt_lin" )),
      tabPanel("windows", id="windows", verbatimTextOutput( "errtxt_win" )),
      tabPanel("macos", id="macos", verbatimTextOutput( "errtxt_mac" ))
      )
     )
    )
   )

