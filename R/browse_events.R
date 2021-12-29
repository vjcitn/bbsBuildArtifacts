#' app to get details on events in packages
#' @param af ArtifSet S4 instance
#' @param build_hosts named character(3) names of elements must be 'linux', 'macos', 'windows',
#' element values are the host names used in the construction of html in package build report.
#' @import shiny
#' @importFrom shinyBS popify bsButton
#' @examples
#' af = make_demo_ArtifSet()
#' if (interactive()) {
#'  browse_events(af)
#' }
#' @export
browse_events = function(af, build_hosts=c(linux="nebbiolo2", macos="machv2", windows="tokay2")) {
  stopifnot(inherits(af, "ArtifSet"))
  stopifnot(all(names(build_hosts) %in% c("linux", "macos", "windows")))
#
# SERVER
#
  server = function(input, output, session) {
    output$initlabel = renderUI({tags$span(
       popify(bsButton("pointlessButton1", "Event browser", style = "primary", size = "large"),
         "A package may produce multiple events in different classes."))
	})
    output$curpackname = renderText( paste("Package: ", input$curpack, sep="") )
    output$pack_selector = renderUI({
     curhost = build_hosts[ input$curtab ]
     eventmap = c(errors="ERROR", warnings="WARNINGS", 
             wontinstall="wontinstall", skipped="skipped", timeout="TIMEOUT")
     cur_event_class = eventmap[input$eventtype]
#
# the 'selected' parameter below is intended to make this 'sticky' as we
# move across tabs -- we'll recompute the pack list for each host, and
# select the 'current' one if present, as we switch tabs
#
     selectInput("curpack", "packs", choices=packnames_with_events(af=af, host=curhost,
           phase=input$phase, event_class=cur_event_class), selected=input$curpack)
     })
     get_package_data = reactive({
       validate(need(nchar(input$eventtype)>0, "waiting"))
       validate(need(nchar(input$curpack)>0, "waiting"))
       validate(need(nchar(input$phase)>0, "waiting"))
       make_BBS_package_data( af, input$curpack )
       })
     get_event_txt = reactive({
       function(HOST) {
         pd = get_package_data()
         cat(host_data_by_phase( pd, HOST, input$phase ), sep="\n")
         }
      })
       
    output$errtxt_lin = renderPrint({  get_event_txt()(build_hosts["linux"]) })  
    output$errtxt_win = renderPrint({  get_event_txt()(build_hosts["windows"]) })  
    output$errtxt_mac = renderPrint({  get_event_txt()(build_hosts["macos"]) })
    observeEvent(input$stopBtn, {
       stopApp(returnValue=NULL)   # could return information here
      })
    output$evfreq = renderTable(event_freqs(af))
   }

  ui = fluidPage(
   sidebarLayout(
    sidebarPanel(
     uiOutput("initlabel"),
     div(tableOutput("evfreq"), style="font-size:75%"),
     radioButtons("eventtype", "event type", choices=c("errors", "warnings", "wontinstall")),
     radioButtons("phase", "phase", choices=c("install", "buildsrc", "checksrc", "buildbin"),
       selected="checksrc"),
     uiOutput("pack_selector"),
#     actionButton("Reset", "Reset packs."),
     actionButton("stopBtn", "Stop app.")
     ),
    mainPanel(
     tabsetPanel(id="curtab",
      tabPanel("linux", id="linux", verbatimTextOutput("curpackname"), verbatimTextOutput( "errtxt_lin" )),
      tabPanel("windows", id="windows", verbatimTextOutput( "errtxt_win" )),
      tabPanel("macos", id="macos", verbatimTextOutput( "errtxt_mac" ))
      )
     )
    )
   )
  runApp(list(server=server, ui=ui))
}

#readLines("/Users/vincentcarey/report/affyPara/raw-results/nebbiolo2/install-out.txt")

