#' Coerce to NONMEM Control Object
#' 
#' Coerces to NONMEM control stream object.
#' @param x object of dispatch
#' @param ... dots
#' @return nmctl
#' @export
 
as.nmctl <-
function(x,...)UseMethod('as.nmctl')

#' Coerce NONMEM Control Object to character
#' 
#' Coerces NONMEM control stream object to character.
#' @param x object of dispatch
#' @param ... dots
#' @return nmctl
#' @export
as.character.nmctl <-
function(x,...){
	if(length(x)==0) return(character(0))
	x[] <- lapply(x,as.character) # to accommodate novel underlying object types
	order <- sapply(x,length)
	recnums <- 1:length(x)
	record <- rep(recnums,order)
	flag <- runhead(record)
	content <- as.character(unlist(x))
	nms <- toupper(names(x))
	content[flag] <- paste(paste0('$',nms),content[flag])
	content[flag] <- sub(' $','',content[flag])
	content
}

#' Coerce nmctl to list
#' 
#' Coerces nmctl to list.
#' @param x nmctl
#' @param ... dots
#' @return list
#' @export
as.list.nmctl <-
function(x,...)unclass(x)

#' Coerce character to nmctl
#' Coerces chacter to nmctl.
#' @param x character
#' @param pattern pattern to identify record declarations
#' @param head subpattern to identify declaration type
#' @param tail subpattern remaining
#' @param parse whether to convert thetas to initList objects
#' @param ... dots
#' @return list
#' @export
as.nmctl.character <-
function(
	x,
	pattern='^ *\\$([^ ]+)( .*)?$',
	head='\\1',
	tail='\\2',
  parse=FALSE,
	...
){
  if(length(x) == 1){
    class(x) <- if(file.exists(x)) 'filename' else 'modelname'
      return(as.nmctl(x,...))
  }
	flag <- contains(pattern,x)
	nms <- sub(pattern,head,x)
	nms <- nms[flag]
	nms <- tolower(nms)
	content <- sub(pattern,tail,x)
	content[flag] <- sub('^ ','',content[flag])
	content <- split(content,cumsum(flag))
	content[['0']] <- NULL	
	names(content) <- nms
	class(content) <- c('nmctl',class(content))
	thetas <- names(content)=='theta'
	omegas <- names(content)=='omega'
	sigmas <- names(content)=='sigma'
	if(parse)content[thetas] <- lapply(content[thetas],as.initList)
	if(parse)content[omegas] <- lapply(content[omegas],as.initList)
	if(parse)content[sigmas] <- lapply(content[sigmas],as.initList)
	content
}

#' Format nmctl
#' 
#' Format nmctl.
#' 
#' Coerces to character.
#' @param x nmctl
#' @param ... dots
#' @return character
#' @export
format.nmctl <-
function(x,...)as.character(x,...)

#' Print nmctl
#' 
#' Print nmctl.
#' 
#' Formats and prints.
#' @param x nmctl
#' @param ... dots
#' @return character
#' @export
print.nmctl <-
function(x,...)print(format(x,...))

#' Read nmctl
#' 
#' Read nmctl.
#' 
#' Reads nmctl from a connection.
#' @param con nmctl connection
#' @param parse whether to convert thetas to initList objects
#' @param ... dots
#' 
#' @return character
#' @export
read.nmctl <-
function(con,parse=FALSE,...)as.nmctl(readLines(con,...),parse=parse,...)

#' Write nmctl
#' 
#' Write nmctl.
#' 
#' writes (formatted) nmctl to file.
#' @param x nmctl
#' @param file passed to write()
#' @param ncolumns passed to write() 
#' @param append passed to write()
#' @param sep passed to write()
#' @param ... dots
#' @return used for side effects.
#' @export

write.nmctl <-
function(x, file='data',ncolumns=1,append=FALSE, sep=" ",...){
	out <- format(x)
	write(
		out,
		file=file,
		ncolumns=ncolumns,
		append=append, 
		sep=sep,
		...
	)
}

#' Subset nmctl
#' 
#' Subsets nmctl.
#' @param x nmctl
#' @param ... dots
#' @param drop passed to subset
#' @return nmctl
#' @export
`[.nmctl` <- function (x, ..., drop = TRUE){
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}
#' Select nmctl Element
#' 
#' Selects nmctl element.
#' @param x nmctl
#' @param ... dots
#' @param drop passed to element select
#' @return element
#' @export

`[[.nmctl` <- function (x, ..., drop = TRUE)NextMethod("[[")

#' Convert Filename to nmctl
#' 
#' Converts filename to nmctl.
#' 
#' @param x filename
#' @param parse convert thetas to initList
#' @param ... dots
#' @return nmctl
#' @export 

as.nmctl.filename <- function(x, parse, ...)read.nmctl(con=x,parse=parse,...)

#' Convert Modelname to nmctl
#' 
#' Converts modelname to nmctl.
#' 
#' @param x modelname
#' @param verbose whether to display messages
#' @param project path to project directory
#' @param opt alternative specification of project
#' @param rundir model specific run directory
#' @param ctlfile path to model control stream
#' @param ext extension (with dot) for control stream
#' @param parse convert thetas to initList
#' @param ... dots
#' @return nmctl
#' @export 

as.nmctl.modelname <- function(
  x,
  verbose=TRUE,
  project = if(is.null(opt)) getwd() else opt, 
  opt = getOption('project'),
  rundir = file.path(project,x), 
  ctlfile = file.path(rundir,paste0(x,ext)),
  ext = '.ctl',
  parse = TRUE,
  ...
){
  if(verbose)message('converting ',ctlfile)
  read.nmctl(ctlfile,parse=parse,...)
}

#' Extract nmctl record type
#' 
#' Extracts nmctl record type.
#' 
#'@param x nmctl
#'@param ... dots
#'@return nmctltype (list)
#'@export
as.nmctlType <- function(x,...)UseMethod('as.nmctlType')

#' Extract nmctl record type from nmctl
#' 
#' Extracts nmctl record type from nmctl.
#' 
#'@param x nmctl
#'@param type theta omega or sigma
#'@param ... dots
#'@return nmctltype (list)
#'@export
as.nmctlType.nmctl <- function(x,type,...){
  y <- x[names(x) %in% type ]
  attr(y,'type') <- type
  class(y) <- 'nmctlType'
  y
}

#' Coerce to paramComments
#' 
#' Coerces to paramComments
#' 
#' @param x object of dispatch
#' @param ... dots
#' @export
as.paramComments <- function(x,...)UseMethod('as.paramComments')

#' Convert nmctlType to paramComments
#' 
#' Converts nmctlType to paramComments
#' 
#' @param x nmctlType
#' @param ... dots
#' @return data.frame
#' @export
#' 
as.paramComments.nmctlType <- function(x,...){
  type <- attr(x,'type')
  y <- list()
  prior <- 0
  for(i in seq_along(x)){
    this <- x[[i]]
    y[[i]] <- as.paramComments(this,type=type, prior=prior)
    prior <- prior + ord(this)
  }
  y <- do.call(rbind,y)
  class(y) <- union('paramComments',class(y))
  y
}

#' Convert nmctl to paramComments
#' 
#' Converts nmctl to paramComments
#' 
#' @param x nmctl
#' @param fields data items to scavenge from control stream comments
#' @param expected parameters known from NONMEM output
#' @param na string to use for NA values when writing default metafile
#' @param ... dots
#' @return data.frame
#' @export
#' 
as.paramComments.nmctl <- function(x,fields,expected,na, ...){
  t <- x %>% as.nmctlType('theta') %>% as.paramComments
  o <- x %>% as.nmctlType('omega') %>% as.paramComments
  s <- x %>% as.nmctlType('sigma') %>% as.paramComments
  y <- rbind(t,o,s)
  y <- cbind(y[,'par',drop=F], .renderComments(
    y$comment,fields=fields, na=na, ...))
  y <- data.frame(stringsAsFactors=F,par=expected) %>% left_join(y)
  class(y) <- union('paramComments',class(y))
  y
}

.renderComments <- function(x, fields, cumulative = NULL,na, ...){
  if(length(fields) < 1) return(cumulative)
  col <- fields[[1]]
  dat <- sub('^([^;]*);?(.*)$','\\1',x)
  rem <- sub('^([^;]*);?(.*)$','\\2',x)
  out <- data.frame(stringsAsFactors=F, col = dat)
  out$col[is.defined(out) & out == ''] <- na
  names(out)[names(out) == 'col'] <- col
  cum <- if(is.null(cumulative)) out else cbind(cumulative,out)
  .renderComments(x=rem,fields=fields[-1],cumulative=cum, na=na)
}


#' Convert initList to paramComments
#' 
#' Converts initList to paramComments
#' 
#' @param x initlist
#' @param type parameter type: theta, omega,or sigma
#' @param prior number of prior parameters of this type (imporant for numbering)
#' @param ... dots
#' @return data.frame
#' @export
#' 

as.paramComments.initList <- function(x, type, prior,...){
  block <- attr(x,'block')
  com <- lapply(x,function(i)attr(i,'comment'))
  com <- sapply(com, function(i){ # ensure single string
    if(length(i) == 0) return('')
    i[[1]]
  })
  stopifnot(length(com) == length(x))
  if(block > 0) stopifnot(block == ord(as.halfmatrix(x)))
  block <- block > 0
  dex <- if(block)as.data.frame(as.halfmatrix(com)) else data.frame(
    row = seq_along(com), col=seq_along(com), x=com
  )
  dex$row <- padded(dex$row + prior,2)
  dex$col <- padded(dex$col + prior,2)
  dex$par <- type
  dex$par <- paste(sep='_',dex$par,dex$row)
  if(type %in% c('omega','sigma'))dex$par <- paste(sep='_', dex$par, dex$col)
  dex %<>% rename(comment = x)
  dex %<>% select(par,comment)
  class(dex) <- union('paramComments',class(dex))
  dex
}

#' Identify the order of an initList
#' 
#' Identifies the order of an initList.
#' 
#' Essentially the length of the list, or the length of the diagonal of a matrix (if BLOCK was defined).
#' @param x initList
#' @param ... dots
#' @return numeric
#' @export

ord.initList <- function(x,...){
  block <- attr(x,'block')
  len <- length(x)
  if(is.null(block)) return(len)
  if(block == 0) return(len)
  return(block)
}



 
  
  