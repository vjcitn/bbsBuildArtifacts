#' app to get details on events in packages
#' @param af ArtifSet S4 instance
#' @import shiny
#' @export
browse_events = function(af) {
  stopifnot(inherits(af, "ArtifSet"))
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
    output$errtxt = renderPrint({
      validate(need(nchar(input$eventtype)>0, "waiting"))
      validate(need(nchar(input$curpack)>0, "waiting"))
      print(input$eventtype)
      print(input$curpack)
      bbsBuildArtifacts:::package_by_host_data( paths(af)[input$curpack], host=input$host )[[input$eventtype]] 
      })
    }

  ui = fluidPage(
   sidebarLayout(
    sidebarPanel(
     helpText("event browser"),
     radioButtons("eventtype", "event type", choices=c("errors", "warnings", "wontinstall")),
     radioButtons("host", "host", choices=c("nebbiolo2", "machv2", "tokay2")),
     uiOutput("abc")
     ),
    mainPanel(
     tabsetPanel(
      tabPanel("stuf", verbatimTextOutput( "errtxt" ))
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
