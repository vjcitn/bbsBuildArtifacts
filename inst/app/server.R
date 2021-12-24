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
#  af <<- make_demo_ArtifSet()
#  ec = collect_events(af, event_class="errors")
#  wc = collect_events(af, event_class="warnings")
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
    output$errtxt_neb = renderPrint({  get_err_txt()("nebbiolo2") })  # EVENTUALLY LINUX
    output$errtxt_tok = renderPrint({  get_err_txt()("tokay2") })  # EVENTUALL WINDOWS
    output$errtxt_mac = renderPrint({  get_err_txt()("machv2") })  # EVENTUALL MAC
   }
