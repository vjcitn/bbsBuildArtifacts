  library(shiny)
  library(bbsBuildArtifacts)
  af <<- make_demo_ArtifSet()
  build_hosts <<- c(linux="nebbiolo2", macos="machv2", windows="tokay2")
  ui = fluidPage(
   sidebarLayout(
    sidebarPanel(
     helpText("Bioconductor build system event browser -- limited demonstration"),
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

