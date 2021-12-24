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
    output$errtxt_old = renderPrint({
      validate(need(nchar(input$eventtype)>0, "waiting"))
      validate(need(nchar(input$curpack)>0, "waiting"))
      dat = bbsBuildArtifacts:::package_by_host_data( paths(af)[input$curpack], host=input$host )
      if (input$eventtype=="errors") cat(dat$parsed_chks$errors)
      else if (input$eventtype=="warnings") cat(dat$parsed_chks$warnings)
      })
    get_err_txt = reactive({
      function(HOST) {
       validate(need(nchar(input$eventtype)>0, "waiting"))
       validate(need(nchar(input$curpack)>0, "waiting"))
       dat = bbsBuildArtifacts:::package_by_host_data( paths(af)[input$curpack], host=HOST )
       if (input$eventtype=="errors") cat(dat$parsed_chks$errors)
       else if (input$eventtype=="warnings") cat(dat$parsed_chks$warnings)
       }
      })
    output$errtxt_neb = renderPrint({  get_err_txt()("nebbiolo2") })  # EVENTUALLY LINUX
    output$errtxt_tok = renderPrint({  get_err_txt()("tokay2") })  # EVENTUALL WINDOWS
    output$errtxt_mac = renderPrint({  get_err_txt()("machv2") })  # EVENTUALL MAC
   }

  ui = fluidPage(
   sidebarLayout(
    sidebarPanel(
     helpText("event browser"),
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
