  #library(shiny)
  #library(bbsBuildArtifacts)


#
# SERVER
#
  server = function(input, output, session) {
  #  timerep = time_report(af)
  #  output$timings = renderPrint( timerep )
  #  output$timingSummaries = renderPrint( summary(timerep) )
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
     get_package_data = reactive({
       validate(need(nchar(input$eventtype)>0, "waiting"))
       validate(need(nchar(input$curpack)>0, "waiting"))
       validate(need(nchar(input$phase)>0, "waiting"))
       make_BBS_package_data( af, input$curpack, hosts=build_hosts )
       })
     get_event_txt = reactive({
       function(HOST) {
         pd = get_package_data()
         cat(host_data_by_phase( pd, HOST, input$phase ), sep="\n")
         }
      })
       
    output$errtxt_lin = renderPrint({  get_event_txt()(build_hosts["linux"]) })  
    output$errtxt_win = renderPrint({  get_event_txt()(build_hosts["windows"]) })  
    output$errtxt_mac = renderPrint({  get_event_txt()(build_hosts["macos"]) })
    observeEvent(input$stopBtn, {
       stopApp(returnValue=NULL)   # could return information here
      })
    output$evfreq = renderTable({
      ho = build_hosts[c("linux", "windows", "macos")]
      rmap = names(ho)
      names(rmap) = ho
      event_freqs(af, hostmap=rmap)
      })
    output$pkg_data = renderUI({
       tags$a(href=paste0("http://bioconductor.org/packages/", input$curpack), target="_blank", "Landing page")
       })
    output$pkg_raw_info = renderPrint({
       validate(need(nchar(input$curpack)>0, "select a package"))
       bbsBuildArtifacts:::make_raw_info(af, input$curpack)
       })


   }

