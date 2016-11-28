#' Create a Bootstrap Table
#'
#' Creates a bootstrap table.
#' 
#' @param x object of dispatch
#' @param ... arguments to methods
#' @seealso \code{\link{as.bootstrap.modelname}}
#' @export
#' @return data.frame
#' @examples
#' \dontrun{
#' options('project') <- 'model'
#' 1044 %>% as.bootstrap
#' }
as.bootstrap <- function(x,...)UseMethod('as.bootstrap')
as.bootstrap.bootstrap <- function(x,...)x
as.bootstrap.numeric  <- function(x,...)as.bootstrap(as.character(x),...)
as.bootstrap.character <- function(x,...){
  class(x) <- if(file.exists(x)) 'filepath' else 'modelname'
  as.bootstrap(x,...)
}

#' Create a Bootstrap Table from Filepath
#'
#' Creates a bootstrap table from a PsN bootstrap results csv filepath.
#' 
#' @param x object of dispatch
#' @param skip number of lines to skip in bootstrap_results.csv
#' @param check.names passed to bootstrap reader
#' @param lo the PsN bootstrap lower confidence limit (\%)
#' @param hi the PsN bootstrap upper confidence limit (\%)
#' @param ... arguments to methods
#' @import magrittr
#' @export
#' @return data.frame

as.bootstrap.filepath <- function(x,skip=28,check.names=FALSE,lo='2.5',hi='97.5',...){
  x <- x %>% 
    read.csv(skip=skip,check.names=check.names,as.is=TRUE,...)
  y <- x
  x <- x[c(1:8),] 
  row.names(x) <- text2decimal(x[,1])
  x <- x[,-1]
  x <- t(x)
  x <- data.frame(x,stringsAsFactors = F,check.names=F)
  suppressWarnings(x[] <- lapply(x,as.numeric))
  x <- x[,names(x) %in% c(lo,hi)]
  x  
}

#' Create a Bootstrap Table from Modelname in Project Context
#'
#' Creates a bootstrap table from a modelname in Project Context.
#' 
#' Assumes project has been identified, model directory exists, and PsN bootstrap method has been run for the model.  Scavenges for the last file matching pattern.  
#' 
#' @param x object of dispatch
#' @param skip number of lines to skip in bootstrap_results.csv
#' @param check.names passed to bootstrap reader
#' @param lo the PsN bootstrap lower confidence limit (\%)
#' @param hi the PsN bootstrap upper confidence limit (\%)
#' @param ... arguments to methods
#' @seealso \code{\link{as.bootstrap.filename}}

#' @export
#' @return data.frame


as.bootstrap.modelname <- function(
  x,
  project = if(is.null(opt)) getwd() else opt, 
  opt = getOption('project'),
  rundir = file.path(project,x),
  pattern='bootstrap_results.csv',
  bootcsv = dir(rundir,pattern = pattern,recursive=TRUE,full.names=TRUE),
  ...
){
  if(!length(bootcsv))stop(
    'no file found with pattern ',
    pattern,
    '. Perhaps set arg project= or options(project=)'
  )
  file <- rev(bootcsv)[[1]]
  class(file) <- 'filepath'
  as.bootstrap(file)
}

