# Objective -- a data structure that will provide information
# on BBS processing of a Bioc package ... possibly universal
# (for software, annotation, experiment, ...)

# Canonical data from BBS for a software package-
#
# /Users/vincentcarey/report/SummarizedExperiment
# ├── index.html
# ├── machv2-buildbin.html
# ├── machv2-buildsrc.html
# ├── machv2-checksrc.html
# ├── machv2-install.html
# ├── nebbiolo2-buildsrc.html
# ├── nebbiolo2-checksrc.html
# ├── nebbiolo2-install.html
# ├── raw-results
# │   ├── info.dcf
# │   ├── machv2
# │   │   ├── buildbin-out.txt
# │   │   ├── buildbin-summary.dcf
# │   │   ├── buildsrc-out.txt
# │   │   ├── buildsrc-summary.dcf
# │   │   ├── checksrc-out.txt
# │   │   ├── checksrc-summary.dcf
# │   │   ├── install-out.txt
# │   │   └── install-summary.dcf
# │   ├── nebbiolo2
# │   │   ├── buildsrc-out.txt
# │   │   ├── buildsrc-summary.dcf
# │   │   ├── checksrc-out.txt
# │   │   ├── checksrc-summary.dcf
# │   │   ├── install-out.txt
# │   │   └── install-summary.dcf
# │   └── tokay2
# │       ├── buildbin-out.txt
# │       ├── buildbin-summary.dcf
# │       ├── buildsrc-out.txt
# │       ├── buildsrc-summary.dcf
# │       ├── checksrc-out.txt
# │       ├── checksrc-summary.dcf
# │       ├── install-out.txt
# │       └── install-summary.dcf
# ├── tokay2-buildbin.html
# ├── tokay2-buildsrc.html
# ├── tokay2-checksrc.html
# └── tokay2-install.html
# 

setOldClass("package_version")
setOldClass("rcmdcheck")
setOldClass("POSIXct")

hosts = function(bbspd) slot(bbspd, "hosts")

setGeneric("host_data_by_phase", function(bbspd, host, phase) standardGeneric("host_data_by_phase"))
setMethod("host_data_by_phase", c("BBS_package_data", "character", "character"),
    function(bbspd, host, phase) {
      stopifnot(phase %in% valid_phases())
      stopifnot(host %in% hosts(bbspd))
      slot(slot(bbspd, "host_data")[[host]], phase)
    })
      

setClass("BBS_raw_pkg_info", slots=c(name="character", last_commit_date="POSIXct", version = "package_version",
  commit_tag="character", branch="character", maint_email = "character"))

setClass("BBS_package_data",
 slots=c(
  package_name = "character",
  bioc_version = "package_version",
  raw_info = "BBS_raw_pkg_info",   # from raw-results/info.dcf
  hosts = "character",
  platforms = "character",
  host_data = "list"
 )
)

make_BBS_package_data = function(afset, packagename, 
   hosts=c(linux="nebbiolo2", macos="machv2", windows="tokay2")) {
 perhost = lapply(hosts, function(host) make_pkg_data_for_host( afset=afset, host=host, 
              packagename=packagename))
 names(perhost) = as.character(hosts)
 platforms = names(hosts)
 new("BBS_package_data", package_name=packagename,
   bioc_version = package_version(slot(afset, "version")),
   raw_info = make_raw_info(afset=afset, packagename=packagename),
   hosts=hosts,
   platforms=platforms,
   host_data = perhost)
}

setMethod("show", "BBS_package_data", function(object) {
 cat(sprintf("BBS_package_data for package '%s' version %s\n",
            slot(object, "package_name"), as.character(slot(object, "bioc_version"))))
})

setClass("BBS_pkg_data_for_host",
 slots=c(
  package_name = "character",
  host = "character",
  buildbin = "character",
  buildsrc = "character",
  checksrc = "list",
  install = "character"
 )
)

valid_phases = function() c("install", "buildbin", "buildsrc", "checksrc")

get_out_txt = function(afset, packagename, host, phase) {
 path = paths(afset)[packagename]
 stopifnot(dir.exists(path))
 stopifnot(phase %in% valid_phases())
 target = sprintf(paste0(path, "/raw-results/", host, "/", phase, "-out.txt"))
 if (phase == "checksrc") {
   if (!file.exists(target)) chk_dat = list(errors=NA_character_, warnings=NA_character_, notes=NA_character_)
   else {
    tmp = rcmdcheck::check_details(rcmdcheck::parse_check(target))
    chk_dat = list(errors=tmp$errors, warnings=tmp$warnings, notes=tmp$notes)
   }
   return(chk_dat)
 }
 if (!file.exists(target)) return(paste0("no ", phase, "-out.txt available for ", packagename))
 readLines(target)
}
 
 

make_pkg_data_for_host = function(afset, host, packagename) {
 new("BBS_pkg_data_for_host", 
      package_name=packagename,
      host=host,
      buildbin = get_out_txt(afset=afset, packagename=packagename, host=host, phase="buildbin"),
      buildsrc = get_out_txt(afset=afset, packagename=packagename, host=host, phase="buildsrc"),
      checksrc = get_out_txt(afset=afset, packagename=packagename, host=host, phase="checksrc"),
      install = get_out_txt(afset=afset, packagename=packagename, host=host, phase="install")
   )
}


setMethod("show", "BBS_pkg_data_for_host", function(object) {
 cat("BBS_pkg_data_for_host instance:\n")
 cat(sprintf(" package %s, host %s\n", slot(object, "package_name"), slot(object, "host")))
})
 
 


make_raw_info = function(afset, packagename) {
    afpath = paths(afset)[packagename]
    stopifnot(dir.exists(afpath))
    infopa = paste0(afpath, "/raw-results/info.dcf")
    infomat = read.dcf(infopa)
    last_commit_date = as_datetime(gsub(" \\(.*", "", infomat[1, 
        "git_last_commit_date"]))
    commit_tag = infomat[1, "git_last_commit"]
    branch = infomat[1, "git_branch"]
    version = package_version(infomat[1, "Version"])
    maint_email = infomat[1, "MaintainerEmail"]
    new("BBS_raw_pkg_info", name=packagename, version=version, last_commit_date = last_commit_date, commit_tag = commit_tag, 
        branch = branch, maint_email = maint_email)
}

setMethod("show", "BBS_raw_pkg_info", function(object) {
 cat(sprintf("BBS_raw_pkg_info instance:\n package %s %s, branch %s\n", 
      slot(object, "name"), slot(object, "version"), slot(object, "branch")))
 cat(sprintf(" last commit date: %s\n", slot(object, "last_commit_date")))
})