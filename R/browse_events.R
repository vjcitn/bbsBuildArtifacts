#' app to get details on events in packages
#' @param af ArtifSet S4 instance
#' @param build_hosts named character(3) names of elements must be 'linux', 'macos', 'windows',
#' element values are the host names used in the construction of html in package build report.
#' @import shiny
#' @examples
#' af = make_demo_ArtifSet()
#' if (interactive()) {
#'  browse_events(af)
#' }
#' @export
browse_events = function(af, build_hosts=c(linux="nebbiolo2", macos="machv2", windows="tokay2")) {
  stopifnot(inherits(af, "ArtifSet"))
  stopifnot(all(names(build_hosts) %in% c("linux", "macos", "windows")))
  server = function(input, output, session) {
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
     get_err_txt = reactive({
      function(HOST) {
       validate(need(nchar(input$eventtype)>0, "waiting"))
       validate(need(nchar(input$curpack)>0, "waiting"))
       validate(need(nchar(input$phase)>0, "waiting"))
       dat = bbsBuildArtifacts::package_by_host_data( paths(af)[input$curpack], host=HOST )
       if (input$eventtype=="wontinstall") return(cat(dat$bld_txt, sep="\n")) # a readLines result
       lk = dat$parsed_chks
       if (is.na(lk[[1]])) return(cat("no parsed check output available\n"))
       if (input$eventtype=="errors") cat(lk$errors)
       else if (input$eventtype=="warnings") cat(lk$warnings)
       }
      })
    output$errtxt_lin = renderPrint({  get_err_txt()(build_hosts["linux"]) })  
    output$errtxt_win = renderPrint({  get_err_txt()(build_hosts["windows"]) })  
    output$errtxt_mac = renderPrint({  get_err_txt()(build_hosts["macos"]) })
    observeEvent(input$stopBtn, {
       stopApp(returnValue=NULL)   # could return information here
      })
   }

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
  runApp(list(server=server, ui=ui))
}

#readLines("/Users/vincentcarey/report/affyPara/raw-results/nebbiolo2/install-out.txt")

get_inst_error_text_by_host = function(pkname, af, host) {
  stopifnot(host %in% c("nebbiolo2", "tokay2", "machv2")) # FIXME
  pa = paths(af)[pkname]
  stopifnot(length(pa)==1)
  readLines(paste0(pa, "/raw-results/", host, "/install-out.txt"))
}

get_build_error_text_by_host = function(pkname, af, host) {
  stopifnot(host %in% c("nebbiolo2", "tokay2", "machv2")) # FIXME
  pa = paths(af)[pkname]
  stopifnot(length(pa)==1)
  readLines(paste0(pa, "/raw-results/", host, "/buildsrc-out.txt"))
}
