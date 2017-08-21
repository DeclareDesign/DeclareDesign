## this function is from lazyeval version git version, commit c155c3d
freeze_environment <- function(x) {
  list2env(as.list(x, all.names = TRUE), parent = parent.env(x))
}

from_package <- function(func, package) {
  func_package <-
    tryCatch(
      getNamespaceName(environment(func)),
      error = function(e)
        NULL
    )
  ifelse(is.null(func_package), FALSE, func_package == package)
}

#' @importFrom rlang quos lang_fn lang_modify eval_tidy
#' @export
wrap_step_ <- function(...) {
  ## this function allows you to put any R expression
  ## such a dplyr call mutate
  ## into the causal order, i.e.
  ## declare_design(pop(), po, declare_step(mutate(q = 5)))

  step_call <- quos(...)[[1]]

  arg_names <- names(formals(lang_fn(step_call)))

  declare_step_function_internal <- function(data) {
    if (".data" %in% arg_names) {
      step_call <- lang_modify(step_call, .data = data)
    } else if ("data" %in% arg_names) {
      step_call <- lang_modify(step_call, data = data)
    }
    eval_tidy(step_call)
  }

  attributes(declare_step_function_internal) <-
    list(call = match.call(), type = "declare_step")

  return(declare_step_function_internal)

}





#' #' Print version of R and packages to improve reproducibility
#' #'
#' #' @param versions A versions object created by \code{get_versions()}.
#' #'
#' #' @export
#' print_versions <- function(versions = NULL){
#'
#'   if(is.null(versions)){
#'     versions <- get_versions()
#'   }
#'
#'   cat("This document was compiled with the R package DeclareDesign version ", versions$DeclareDesign,
#'       " using ", versions$R, " with the ", versions$OS, " operating system and the R packages: ", sep = "")
#'
#'   versions$packages[, 2] <- paste0("(", versions$packages[, 2], ")")
#'   versions$packages <- apply(versions$packages, 1, function(x) paste(x, collapse = " "))
#'
#'   if(length(versions$packages) == 1)
#'     cat(versions$packages)
#'   if(length(versions$packages) == 2)
#'     cat(paste(versions$packages, collapse = " and "))
#'   else
#'     cat(paste(versions$packages[1:(length(versions$packages)-1)], collapse = "; "), "; and ", versions$packages[length(versions$packages)], sep = "")
#'
#'   cat(".")
#'
#' }
#'
#' get_versions <- function(packages = NULL){
#'   installed <- installed.packages()[, c("Package", "Version")]
#'
#'   if(is.null(packages)){
#'     packages <- loadedNamespaces()
#'   }
#'   if(!("DeclareDesign" %in% packages)){
#'     packages <- c("DeclareDesign", packages)
#'   }
#'
#'   packages <- installed[installed[,1] %in% packages  &
#'                           !(installed[,1] %in% c("base", "datasets", "graphics",
#'                                                  "grDevices", "methods", "stats", "tools", "utils")), ]
#'   DeclareDesign_ver <- packages[packages[,1] %in% "DeclareDesign"][2]
#'   packages <- packages[!(packages[,1] %in% "DeclareDesign"), ]
#'
#'   if (.Platform$OS.type == "windows") {
#'     running <- win.version()
#'   } else if (nzchar(Sys.which("uname"))) {
#'     uname <- system("uname -a", intern = TRUE)
#'     os <- sub(" .*", "", uname)
#'     running <- switch(os, Linux = if (file.exists("/etc/os-release")) {
#'       tmp <- readLines("/etc/os-release")
#'       t2 <- if (any(grepl("^PRETTY_NAME=", tmp))) sub("^PRETTY_NAME=",
#'                                                       "", grep("^PRETTY_NAME=", tmp, value = TRUE)[1L]) else if (any(grepl("^NAME",
#'                                                                                                                            tmp))) sub("^NAME=", "", grep("^NAME=", tmp,
#'                                                                                                                                                          value = TRUE)[1L]) else "Linux (unknown distro)"
#'       sub("\"(.*)\"", "\\1", t2)
#'     } else if (file.exists("/etc/system-release")) {
#'       readLines("/etc/system-release")
#'     }, Darwin = {
#'       ver <- readLines("/System/Library/CoreServices/SystemVersion.plist")
#'       ind <- grep("ProductUserVisibleVersion", ver)
#'       ver <- ver[ind + 1L]
#'       ver <- sub(".*<string>", "", ver)
#'       ver <- sub("</string>$", "", ver)
#'       ver1 <- strsplit(ver, ".", fixed = TRUE)[[1L]][2L]
#'       sprintf("OS X %s (%s)", ver, switch(ver1, `6` = "Snow Leopard",
#'                                           `7` = "Lion", `8` = "Mountain Lion", `9` = "Mavericks",
#'                                           `10` = "Yosemite", `11` = "El Capitan", "unknown"))
#'     }, SunOS = {
#'       ver <- system("uname -r", intern = TRUE)
#'       paste("Solaris", strsplit(ver, ".", fixed = TRUE)[[1L]][2L])
#'     }, uname)
#'   }
#'
#'   return(list(packages = packages, DeclareDesign = DeclareDesign_ver, R = R.version.string, OS = running))
#'
#' }
