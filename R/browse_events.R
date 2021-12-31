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
#
# note -- source code modified so there is only one ui.R and server.R to maintain
# for both basic usage and shinyapps.io deployment
#
  stopifnot(inherits(af, "ArtifSet"))
  stopifnot(all(names(build_hosts) %in% c("linux", "macos", "windows")))
  fromPkg <<- TRUE  # should these be cleaned up on exit?  should we warn if found on startup?
                    # could have a kill_globals parameter
  build_hosts <<- build_hosts
  af <<- af
  runApp(appDir = system.file("app", package="bbsBuildArtifacts"))
}
  

browse_events_old = function(af, build_hosts=c(linux="nebbiolo2", macos="machv2", windows="tokay2")) {
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
    output$curpackname2 = renderText( paste("Package: ", input$curpack, sep="") )  # need unique output names
    output$curpackname3 = renderText( paste("Package: ", input$curpack, sep="") )
    output$curpackname4 = renderText( paste("Package: ", input$curpack, sep="") )
    output$pack_selector = renderUI({
     if (!("about app" %in% names(build_hosts))) build_hosts = c(build_hosts, `about app`=as.character(build_hosts[1]))
     if (!("about pkg" %in% names(build_hosts))) build_hosts = c(build_hosts, `about pkg`=as.character(build_hosts[1]))
     # above is a hack to ensure that looking at 'about' tabs doesn't change the list of packages offered
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
    output$pkg_data = renderUI({
       tags$a(href=paste0("http://bioconductor.org/packages/", input$curpack), target="_blank", "Landing page")
       })
   }

  ui = fluidPage(
   sidebarLayout(
    sidebarPanel(
     uiOutput("initlabel"),
     div(tableOutput("evfreq"), style="font-size:75%"),
     radioButtons("eventtype", "event type", choices=c("errors", "warnings", "wontinstall")),
     radioButtons("phase", "phase", choices=c("install", "buildsrc", "checksrc", "buildbin"),
       selected="checksrc"),
     helpText("Note: changing phase will alter the set of packages to be viewed and reset panel contents."),
     uiOutput("pack_selector"),
#     actionButton("Reset", "Reset packs."),
     actionButton("stopBtn", "Stop app.")
     ),
    mainPanel(
     tabsetPanel(id="curtab",
      tabPanel("linux", id="linux", verbatimTextOutput("curpackname"), verbatimTextOutput( "errtxt_lin" )),
      tabPanel("windows", id="windows",verbatimTextOutput("curpackname2"), verbatimTextOutput( "errtxt_win" )),
      tabPanel("macos", id="macos", verbatimTextOutput("curpackname3"), verbatimTextOutput( "errtxt_mac" )),
      tabPanel("about pkg", id="pkg", verbatimTextOutput("curpackname4"), htmlOutput( "pkg_data" )),
      tabPanel("about app", helpText("This shiny app is intended to help investigation of adverse events in the Bioconductor Build System.  The reports focus on packages exhibiting problems in different phases of build and check for different hosts."))
      )
     )
    )
   )
  runApp(list(server=server, ui=ui))
}

#readLines("/Users/vincentcarey/report/affyPara/raw-results/nebbiolo2/install-out.txt")

