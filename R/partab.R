#' Create Model Parameter Table in Project Context.
#'
#' Creates a model parameter table in project context.
#' 
#' x can be numeric or character model name, assuming project is identified by argument or option.
#' @param x object of dispatch
#' @param ... arguments to methods
#' @seealso \code{\link{as.partab.modelname}}
#' @examples
#' options('project') <- '../project/model'
#' 1001 %>% as.partab
#' @export
as.partab <- function(x,...)UseMethod('as.partab')
#' Create Model Parameter Table from partab
#'
#' Creates a model parameter table from a partab object.
#' 
#' Just returns the object unmodified.
#' @param x partab
#' @param ... arguments to methods
#' @export
as.partab.partab <- function(x,...)x
#' Create Model Parameter Table from Number.
#'
#' Creates a model parameter table from a number.
#' 
#' Just coerces to character and calls as.partab again.
#' @param x numeric
#' @param ... arguments to methods
#' @export
as.partab.numeric  <- function(x,...)as.partab(as.character(x),...)
#' Create Model Parameter Table from Character
#'
#' Creates a model parameter table from a character string.
#' 
#' Reclassifies x as a modelname and calls as.partab again.
#' @param x numeric
#' @param ... arguments to methods
#' @export
as.partab.character <- function(x,...){
  class(x) <-  'modelname'
  as.partab(x,...)
}

val_name <- function(x, xpath, param, moment,...){
  tokenpath <- paste0('//',xpath,'/val')
  valpath   <- paste0(tokenpath,'/@name')
  dat <- data.frame(
    stringsAsFactors=FALSE,
    par = x %>% xpath(valpath) %>% padded(2),
    x = x %>% xpath(tokenpath) %>% as.numeric
  )
  dat %<>% mutate(par = paste(sep='_',param,par))
  names(dat)[names(dat) == 'x'] <- moment
  dat
}
row_col <- function(x, xpath, param, moment,...){
  tokenpath <- paste0('//',xpath,'/row/col')
  dat <- x %>% xpath(tokenpath) %>% as.halfmatrix %>% as.data.frame
  dat %<>% mutate(par = paste(sep='_',param,row %>% padded(2),col %>% padded(2)))
  dat %<>% mutate(offdiag = as.integer(row != col))
  dat %<>% select(par,x, offdiag)
  names(dat)[names(dat) == 'x'] <- moment
  dat
}

#' Create a Parameter Table from Model name.
#'
#' Creates a parameter table from a model name. Pass the project argument or set 
#' the project option.  
#' 
#' Normally you can just call the generic.  Suitable defaults are supplied, but much customization is supported by means of arguments documented here and in called functions.
#' @import magrittr
#' @import dplyr
#' @param x a model name (numeric or character)
#' @param verbose set FALSE to suppress messages
#' @param lo the PsN bootstrap lower confidence limit (\%)
#' @param hi the PsN bootstrap upper confidence limit (\%)
#' @param project parent directory of model directories
#' @param opt alternative argument for setting project
#' @param rundir specific model directory
#' @param metafile metadata for parameter table; will be created if missing (edit and re-run)
#' @param xmlfile path to xml file
#' @param ctlfile path to control stream
#' @param bootcsv path to PsN bootstrap_results.csv
#' @param strip.namespace whether to strip e.g. nm: from xml elements for easier xpath syntax
#' @param skip number of lines to skip in bootstrap_results.csv
#' @param check.names passed to bootstrap reader
#' @param digits limits numerics to significant digits if specified
#' @param ci combine bootstrap lo and hi into an enclosed interval
#' @param sep separator for bootstrap interval
#' @param open first character for bootstrap interval
#' @param close last character for bootstrap interval
#' @param format format numerics as character
#' @param fields metadata fields to read from control stream if no metafile
#' @param relative transform standard errors to relative standard errors: rse replaces se
#' @param percent if relative is true, express as percent (else ignore): prse replaces se
#' @param quote quote csv values when creating template metafile
#' @param na how to encode NA in metafile
#' @param na.strings what to read as NA in metafile 
#' @param as.is passed to read.csv for reading metafile
#' @param nonzero limit random effects to those with nonzero estimates
#' @param ... passed to other functions
#' @seealso \code{\link{as.xml_document.modelname}}
#' @seealso \code{\link{as.bootstrap.modelname}}
#' @seealso \code{\link{as.nmctl.modelname}}
#' @seealso \code{\link{write.csv}}
#' @seealso \code{\link{read.csv}}

#' @return object of class partab, data.frame
#' @export


as.partab.modelname <- function(
  x,
  verbose=TRUE,
  lo='5',
  hi='95',
  project = if(is.null(opt)) getwd() else opt, 
  opt = getOption('project'),
  rundir = file.path(project,x), 
  metafile = file.path(rundir,paste0(x,'.meta')),
  xmlfile = file.path(rundir,paste0(x,'.xml')),
  ctlfile = file.path(rundir,paste0(x,'.ctl')),
  bootcsv,
  strip.namespace=TRUE,
  skip=28,
  check.names=FALSE,  
  digits = numeric(0),
  ci = FALSE,
  open = '(',
  close = ')',
  sep = ', ',
  format = ci,
  fields = c('symbol','label','unit'),
  relative = FALSE,
  percent=relative,
  quote = FALSE,
  na = '.',
  na.strings = na,
  as.is = TRUE,
  nonzero = TRUE,
  ...
){
  if(verbose)message('searching ',rundir)
  # SCAVENGE XML
  y <- x %>% as.xml_document(strip.namespace=strip.namespace,verbose=verbose,project=project,file=xmlfile,...)
  # SCAVENGE BOOTSTRAPS
  args <- list(
    x = x, skip=skip,check.names=check.names,lo=lo,hi=hi,
    verbose=verbose,project=project
  )
  if(!missing(bootcsv)) args <- c(args,list(bootcsv=bootcsv))
  args <- c(args,list(...))
  z <- try(do.call(as.bootstrap,args))
  #z <- try(x %>% as.bootstrap(skip=skip,check.names=check.names,lo=lo,hi=hi,verbose=verbose,project=project,bootcsv=bootcsv,...))
  theta   <- y %>% val_name('theta',  'theta','estimate')
  thetase <- y %>% val_name('thetase','theta','se')
  sigma   <- y %>% row_col('sigma',   'sigma','estimate')
  sigmase <- y %>% row_col('sigmase', 'sigma','se')
  omega   <- y %>% row_col('omega',   'omega','estimate')
  omegase <- y %>% row_col('omegase', 'omega','se')
  theta %<>% left_join(thetase,by='par')
  omega %<>% left_join(omegase,by=c('par','offdiag'))
  sigma %<>% left_join(sigmase,by=c('par','offdiag'))
  theta %<>% mutate(offdiag = 0)
  param <- rbind(theta,omega,sigma)
  if(inherits(z,'data.frame')){
    z <- z[-1,] # drop ofv
    need <- nrow(param) - sum(param$offdiag)
    if(nrow(z) < need){
      message('not as many bootstrap estimates as parameters')
    }else{
      z <- z[1:need,]
      names(z) <- c('lo','hi')
      if(verbose)message(
        'matching:\n',
        paste(
          paste(
            sep=':',
            param %>% 
              filter(offdiag==0) %$% 
              par,
            row.names(z)
          ),
          '\n'
        )
      )
      i <- param$offdiag==0
      param$lo[i] <- z$lo
      param$hi[i] <- z$hi
    }
  }else{
    param$lo <- NA_real_
    param$hi <- NA_real_
  }
  param %<>% select(-offdiag)
  if(nonzero){
    param %<>% filter(!(estimate == 0 & par %contains% 'omega|sigma'))
  }
  if(relative){
    param %<>% mutate(se = se / estimate) # rename rse below
    if(percent){
    param %<>% mutate(se = se * 100) # rename prse below
    }
  }
  if(length(digits)){
    param %<>% mutate(estimate = estimate %>% signif(digits))
    param %<>% mutate(se = se %>% signif(digits))
    param %<>% mutate(lo = lo %>% signif(digits))
    param %<>% mutate(hi = hi %>% signif(digits))
  }
  if(format){
    param %<>% mutate(estimate = estimate %>% as.character)
    param %<>% mutate(se = se %>% as.character)
    param %<>% mutate(lo = lo %>% as.character)
    param %<>% mutate(hi = hi %>% as.character)
  }
  if(ci){
    param %<>% mutate(ci = paste(sep=sep, lo, hi) %>% enclose(open,close))
    param %<>% select(-lo, -hi)
  }
  if(relative && percent) param %<>% rename(prse = se)
  if(relative && !percent) param %<>% rename(rse = se)
  # CREATE META
  if(!file.exists(metafile)){
    # SCAVENGE CTL
    x %>%
      as.nmctl(verbose=verbose,rundir = rundir,ctlfile=ctlfile,...) %>%
      as.paramComments(fields=fields,expected=param$par, na=na) %>%
      write.csv(row.names=F,quote=quote, file=metafile, na=na, ...)
    message('edit contents of ',metafile)
  }
  if(verbose)message('merging ',metafile)
  # SCAVENGE META
  meta <- read.csv(na.strings=na.strings, as.is=as.is,metafile)
  param %<>% left_join(meta,by='par')
  class(param) <- union('partab', class(param))
  param
}
