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
       validate(need(nchar(input$eventtype)>0, "no entry with this set of choices"))
       validate(need(nchar(input$curpack)>0, "no entry with this set of choices"))
       validate(need(nchar(input$phase)>0, "no entry with this set of choices"))
       validate(need(dir.exists(paths(af)[input$curpack]), "no entry with this set of choices"))
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

