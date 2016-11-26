as.bootstrap <- function(x,...)UseMethod('as.bootstrap')
as.bootstrap.bootstrap <- function(x,...)x
as.bootstrap.numeric  <- function(x,...)as.bootstrap(as.character(x),...)
as.bootstrap.character <- function(x,...){
  class(x) <- if(file.exists(x)) 'filepath' else 'modelname'
  as.bootstrap(x,...)
}
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
as.bootstrap.modelname <- function(
  x,
  project = if(is.null(opt)) getwd() else opt, 
  opt = getOption('project'),
  rundir = file.path(project,x),
  pattern='bootstrap_results.csv',
  files = dir(rundir,pattern = pattern,recursive=TRUE,full.names=TRUE),
  ...
){
  if(!length(files))stop(
    'no file found with pattern ',
    pattern,
    '. Perhaps set arg project= or options(project=)'
  )
  file <- rev(files)[[1]]
  class(file) <- 'filepath'
  as.bootstrap(file)
}

