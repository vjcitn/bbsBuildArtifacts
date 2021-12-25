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
  ec = collect_events(af, event_class="errors")
  wc = collect_events(af, event_class="warnings")
  get_packnames = function(type) {
   if (type == "errors") return(names(ec$events))
   else if (type == "warnings") return(names(wc$events))
   else if (type == "wontinstall") return(names(wc$noparse_paths))  # should be same in ec,wc
   else stop("unrecognized type")
   }
  server = function(input, output, session) {
    output$abc = renderUI({
     selectInput("curpack", "packs", choices=get_packnames(input$eventtype))
     })
    output$def = renderPlot(plot(1, main=input$eventtype))
#    output$errtxt_old = renderPrint({
#      validate(need(nchar(input$eventtype)>0, "waiting"))
#      validate(need(nchar(input$curpack)>0, "waiting"))
#      dat = bbsBuildArtifacts:::package_by_host_data( paths(af)[input$curpack], host=input$host )
#      if (input$eventtype=="errors") cat(dat$parsed_chks$errors)
#      else if (input$eventtype=="warnings") cat(dat$parsed_chks$warnings)
#      else if (input$eventtype=="wontinstall") cat(dat$bld_txt)
#      })
    get_err_txt = reactive({
      function(HOST) {
       validate(need(nchar(input$eventtype)>0, "waiting"))
       validate(need(nchar(input$curpack)>0, "waiting"))
       dat = bbsBuildArtifacts:::package_by_host_data( paths(af)[input$curpack], host=HOST )
       if (input$eventtype=="errors") cat(dat$parsed_chks$errors)
       else if (input$eventtype=="warnings") cat(dat$parsed_chks$warnings)
       else if (input$eventtype=="wontinstall") cat(dat$bld_txt, sep="\n") # a readLines result
       }
      })
    output$errtxt_lin = renderPrint({  get_err_txt()(build_hosts["linux"]) })  
    output$errtxt_win = renderPrint({  get_err_txt()(build_hosts["windows"]) })  
    output$errtxt_mac = renderPrint({  get_err_txt()(build_hosts["macos"]) })
   }

  ui = fluidPage(
   sidebarLayout(
    sidebarPanel(
     helpText("event browser"),
     radioButtons("eventtype", "event type", choices=c("errors", "warnings", "wontinstall")),
     uiOutput("abc")
     ),
    mainPanel(
     tabsetPanel(
      tabPanel("linux", verbatimTextOutput( "errtxt_lin" )),
      tabPanel("windows", verbatimTextOutput( "errtxt_win" )),
      tabPanel("macos", verbatimTextOutput( "errtxt_mac" ))
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
