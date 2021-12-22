#' scan a slice of an ArtifSet for events produced in R CMD check of source
#' @importFrom BiocParallel bpparam bplapply
#' @param af instance of ArtifSet
#' @param event_class character(1) "warnings" (default) or "errors"
#' @param BPPARAM instance of a BiocParallel BiocParallelParam subclass
#' @param which_to_use vector of elements in ArtifSet (can be package names or numeric indices), defaults to NULL
#' in which case all elements are examined for events
#' @export
collect_events = function(af, event_class="warnings", BPPARAM=BiocParallel::bpparam(), which_to_use=NULL) {
  stopifnot(event_class %in% c("warnings", "errors"))
  if (is.null(which_to_use)) pa = paths(af)
  else pa = paths(af)[which_to_use]
  out1 = BiocParallel::bplapply(pa, function(x) package_by_host_data(x))
  out2 = lapply(out1, function(x) try(x$parsed_chks[[1]][[event_class]], silent=TRUE))
  noparse = which(sapply(out2, inherits, "try-error"))
  if (length(noparse)>0) out = out2[-noparse]
  else out=out2
  clean = which(sapply(out, function(x) length(x)==0))
  noparse_paths = pa[noparse]
  cleanpaths = pa[clean]
  out = out[-union(noparse,clean)]
  ans = list(events=out, noparse_paths=noparse_paths, cleanpaths=cleanpaths)
  class(ans) = c("event_collection", class(ans))
  ans
}

#' short report of an event collection
#' @param x event_collection instance
#' @param \dots not used
#' @export
print.event_collection = function(x, ...) {
  cat("bbsBuildArtifacts event_collection instance\n")
  cat(sprintf("%d events, %d no-parse paths.\n", length(x$events), length(x$noparse)))
  invisible(NULL)
}

#' app to get details on events in packages
#' @param ec event_collection S3 instance
#' @param af ArtifSet S4 instance
#' @import shiny
#' @export
browse_event_collection = function(ec, af) {
  pknames = list(epacks = names(ec$events),
                 noppacks = names(ec$noparse_paths))
  get_packnames = function(type) {
   if (type == "install") return(pknames$noppacks)
   return(pknames$epacks)
   }
  server = function(input, output, session) {
    output$abc = renderUI({
     selectInput("curpack", "packs", choices=get_packnames(input$type))
     })
    output$def = renderPlot(plot(1, main=input$type))
    output$errtxt = renderPrint( get_inst_error_text_by_host( input$curpack, af, input$host ) )
    }

  ui = fluidPage(
   sidebarLayout(
    sidebarPanel(
     helpText("event browser"),
     radioButtons("type", "type", choices=c(check_error="check", install_error="install",
                build_error="build")),
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
