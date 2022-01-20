# determine whether we are being called from package or not
sc = sys.call(which=1)
sc2 = sys.call(which=2)
 print(as.character(sc))
 print(as.character(sc2))
if (!is.null(sc)) {
 chk = as.character(sc)[1]
# if not, need to use the demo data
 if (chk == "runApp") {
  message("running app manually")
  library(shiny)
  library(bbsBuildArtifacts)
  af <<- make_demo_ArtifSet()
  build_hosts <<- c(linux="nebbiolo2", macos="machv2", windows="tokay2")
  }
 }
  ui = fluidPage(
   sidebarLayout(
    sidebarPanel(
     helpText("Bioconductor build system event browser"),
     verbatimTextOutput("state"),
     verbatimTextOutput("bbsbavers"),
     div(tableOutput("evfreq"), style="font-size:75%"),
     radioButtons("eventtype", "event type", choices=c("errors", "warnings", "wontinstall")),
     helpText("use wontinstall with phase 'install'"),
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
#      tabPanel("timings", id="timing", helpText("Summaries per phase/host are followed by details for longest times"),
#                  verbatimTextOutput("timingSummaries"), verbatimTextOutput("timings")),
      tabPanel("about pkg", id="pkg", verbatimTextOutput("curpackname4"), 
               verbatimTextOutput("pkg_raw_info"), htmlOutput( "pkg_data" )),
      tabPanel("about app", helpText("This shiny app is intended to help investigation of adverse events in the Bioconductor Build System.  The reports focus on packages exhibiting problems in different phases of build and check for different hosts."), helpText("ArtifSet in use:"),  verbatimTextOutput("afdata"))
      )
     )
    )
   )

