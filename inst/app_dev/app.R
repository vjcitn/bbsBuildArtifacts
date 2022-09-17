# FOR DEVEL BRANCH

  library(shiny)
  library(bbsBuildArtifacts)

 VERSION = "3.16"
 if (!file.exists("report")) {
  ur = build_report_tgz_url(version=VERSION, type="bioc")
  download.file(ur, "curdev.tgz")
  untar("curdev.tgz")
  }
  bdb = readLines("./report/BUILD_STATUS_DB.txt")
  bb = strsplit(bdb, "#")
  hh = unique(sapply(bb, "[", 2))
  hnmap = c(nebbiolo1="linux", nebbiolo2="linux", lconway="macos",
     merida1="macos", palomino3="windows", palomino4="windows")
  osnames = hnmap[hh]
  names(hh) = as.character(osnames)
  af = setup_artifacts(extracted=".", version=VERSION, hostnames=hh)
  build_hosts <<- slot(af, "hostnames")
  bpl = BiocPkgTools::biocPkgList(version=VERSION)

#
# SERVER
#
 server = function(input, output, session) {
 library(shiny)
 library(bbsBuildArtifacts)

     eventmap = c(errors="ERROR", warnings="WARNINGS", 
             wontinstall="wontinstall", skipped="skipped", timeout="TIMEOUT")

  #  timerep = time_report(af)
  #  output$timings = renderPrint( timerep )
  #  output$timingSummaries = renderPrint( summary(timerep) )
    output$curemail = renderText({ 
         validate(need(is.character(input$curpack), "waiting..."))
         paste("Maintainer: ", bbsBuildArtifacts::maint4pkg(pkg=input$curpack, version=VERSION, bpl=bpl))
         })
    output$state = renderText( sprintf("Bioc version %s, tarball produced %s\n", slot(af, "version"),
       slot(af, "tarball_date")) )
    output$idio = renderPrint( {
         idiosync_status(af, phase.in=input$phase, state=eventmap[input$eventtype])
         } )
           
    output$afdata = renderPrint( af )
    output$initlabel = renderUI({tags$span(
       popify(bsButton("pointlessButton1", "Event browser", style = "primary", size = "large"),
         "A package may produce multiple events in different classes."))
	})
    output$bbsbavers = renderText( paste("bbsBuildArtifacts version: ", packageVersion("bbsBuildArtifacts")) )
    output$curpackname = renderText( paste("Package: ", input$curpack, sep="") )
    output$curpackname2 = renderText( paste("Package: ", input$curpack, sep="") )
    output$curpackname3 = renderText( paste("Package: ", input$curpack, sep="") )
    output$curpackname4 = renderText( paste("Package: ", input$curpack, sep="") )
    output$pack_selector = renderUI({
     if (!("about app" %in% names(build_hosts))) build_hosts = c(build_hosts, `about app`=as.character(build_hosts[1]))
     if (!("about pkg" %in% names(build_hosts))) build_hosts = c(build_hosts, `about pkg`=as.character(build_hosts[1]))
     # above is a hack to ensure that looking at 'about' tabs doesn't change the list of packages offered

     curhost = build_hosts[ input$curtab ]
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
       validate(need(nchar(input$eventtype)>0, "waiting; are hosts properly identified to app?"))
       validate(need(nchar(input$curpack)>0, "waiting; are hosts properly identified to app?"))
       validate(need(nchar(input$phase)>0, "waiting; are hosts properly identified to app?"))
       make_BBS_package_data( af, input$curpack )
       })
     get_event_txt = reactive({
       function(HOST) {
         pd = get_package_data()
         cat(host_data_by_phase( pd, HOST, input$phase ), sep="\n")
         }
      })
       
    output$errtxt_lin = renderPrint({  
              if ("linux" %in% names(af@hostnames)) ans = get_event_txt()(build_hosts["linux"]) 
              else ans = "no linux host for this build"
              ans
              })  
    output$errtxt_win = renderPrint({  
              if ("windows" %in% names(af@hostnames)) ans = get_event_txt()(build_hosts["windows"]) 
              else ans = "no windows host for this build"
              ans
              })  
    output$errtxt_mac = renderPrint({  
              if ("macos" %in% names(af@hostnames)) ans = get_event_txt()(build_hosts["macos"]) 
              else ans = "no macos host for this build"
              ans
              })
    observeEvent(input$stopBtn, {
       stopApp(returnValue=NULL)   # could return information here
      })
    output$evfreq = renderTable({
      #ho = build_hosts[c("linux", "windows", "macos")]
      #rmap = names(ho)
      #names(rmap) = ho
      event_freqs(af) # , hostmap=rmap)
      })
    output$pkg_data = renderUI({
       tags$a(href=paste0("http://bioconductor.org/packages/", input$curpack), target="_blank", "Landing page")
       })
    output$pkg_raw_info = renderPrint({
       validate(need(nchar(input$curpack)>0, "select a package"))
       bbsBuildArtifacts:::make_raw_info(af, input$curpack)
       })


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
      tabPanel("linux", id="linux", 
          verbatimTextOutput("curpackname"), 
          verbatimTextOutput("curemail"), 
          verbatimTextOutput( "errtxt_lin" )),
      tabPanel("windows", id="windows",
          verbatimTextOutput("curpackname2"), 
          verbatimTextOutput( "errtxt_win" )),
      tabPanel("macos", id="macos", 
          verbatimTextOutput("curpackname3"), 
          verbatimTextOutput( "errtxt_mac" )),
#      tabPanel("timings", id="timing", helpText("Summaries per phase/host are followed by details for longest times"),
#                  verbatimTextOutput("timingSummaries"), verbatimTextOutput("timings")),
      tabPanel("idiosync", id="idio", helpText("platform-specific problems"), verbatimTextOutput("idio")),
      tabPanel("about pkg", id="pkg", verbatimTextOutput("curpackname4"), 
               verbatimTextOutput("pkg_raw_info"), htmlOutput( "pkg_data" )),
      tabPanel("about app", helpText("This shiny app is intended to help investigation of adverse events in the Bioconductor Build System.  The reports focus on packages exhibiting problems in different phases of build and check for different hosts."), helpText("ArtifSet in use:"),  verbatimTextOutput("afdata"))
      )
     )
    )
   )

shinyApp(ui=ui, server=server)
