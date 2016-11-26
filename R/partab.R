as.partab <- function(x,...)UseMethod('as.partab')
as.partab.partab <- function(x,...)x
as.partab.numeric  <- function(x,...)as.partab(as.character(x),...)
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

as.partab.modelname <- function(
  x,
  verbose=TRUE,
  lo='2.5',
  hi='97.5',
  strip.namespace=TRUE,
  skip=28,
  check.names=FALSE,  
  project = if(is.null(opt)) getwd() else opt, 
  opt = getOption('project'),
  rundir = file.path(project,x), 
  file = file.path(rundir,paste0(x,'.meta')), 
  ...
){
  y <- x %>% as.xml_document(strip.namespace=strip.namespace,...)
  z <- try(x %>% as.bootstrap(skip=skip,check.names=check.names,lo=lo,hi=hi,...))
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
  }
  param %<>% select(-offdiag)
  if(!file.exists(file)){
    d <- data.frame(par=param$par,symbol=NA_character_,label=NA_character_)
    write.csv(d,file=file,row.names=F,quote=F,na='.')
    message('edit contents of ',file)
  }
  if(verbose)message('merging ',file)
  meta <- read.csv(na.strings=c('.','','NA'), as.is=T,file)
  param %<>% left_join(meta,by='par')
  param
}

