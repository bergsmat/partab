#' Harvest Model Item Definitions in Project Context
#'
#' Havests model item definitions in project context.
#' 
#' x can be numeric or character model name, assuming project is identified by argument or option.
#' @param x object of dispatch
#' @param ... arguments to methods
#' @seealso \code{\link{as.definitions.modelname}}
#' @export
as.definitions <- function(x,...)UseMethod('as.definitions')

#' Create Model Item Definitions from definitions
#'
#' Creates a model item definitions from a definitions object.
#' 
#' Just returns the object unmodified.
#' @inheritParams as.definitions
#' @describeIn as.definitions definitions method
#' @export
as.definitions.definitions <- function(x,...)x
#' Create Model Item Definitions from Number.
#'
#' Creates a model item definitions from a number.
#' 
#' Just coerces to character and calls as.definitions again.
#' @inheritParams as.definitions
#' @describeIn as.definitions numeric method
#' @export
as.definitions.numeric  <- function(x,...)as.definitions(as.character(x),...)
#' Create Model Item Definitions from Character
#'
#' Creates a model item definitions from a character string.
#' 
#' Reclassifies x as a modelname and calls as.definitions again.
#' @inheritParams as.definitions
#' @describeIn as.definitions character method
#' @export
as.definitions.character <- function(x,...){
  class(x) <-  'modelname'
  as.definitions(x,...)
}
#' Create Item Definitions from Model Name
#'
#' Creates item definitions from a model name. Scavenges definitions optionally 
#' from the control stream and optionally from the definitions file. Optionally
#' writes the result to the definitions file. Always returns a data.frame with 
#' at least the column 'item' but possibly no rows. 
#' 
#' @import magrittr
#' @import dplyr
#' @param x a model name (numeric or character)
#' @param verbose set FALSE to suppress messages
#' @param project parent directory of model directories
#' @param opt alternative argument for setting project
#' @param rundir specific model directory
#' @param ctlfile path to control stream (pass length-zero argument to ignore)
#' @param metafile path to definitions file (pass length-zero argument to ignore)
#' @param fields metadata fields to read from control stream if no metafile
#' @param read whether to read the definitions file
#' @param write whether to write the definitions file
#' @param ... passed to other functions
#' @seealso \code{\link{as.xml_document.modelname}}
#' @seealso \code{\link{as.bootstrap.modelname}}
#' @seealso \code{\link{as.nmctl.modelname}}
#' @seealso \code{\link{write.csv}}
#' @seealso \code{\link{read.csv}}
#' @aliases definitions
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='partab'))
#' 1001 %>% as.definitions
#' @return object of class definitions, or path to metafile if write = TRUE.
#' @export
as.definitions.modelname <- function(
  x,
  verbose=FALSE,
  opt = getOption('project'),
  project = if(is.null(opt)) getwd() else opt, 
  rundir = file.path(project,x), 
  ctlfile = file.path(rundir,paste0(x,'.ctl')),
  metafile = file.path(rundir,paste0(x,'.def')),
  fields = c('symbol','label','unit'),
  read = length(metafile) == 1,
  write = FALSE,
  ...
){
  m1 <- data.frame(item=character(0),stringsAsFactors=FALSE)
  m2 <- data.frame(item=character(0),stringsAsFactors=FALSE)
  
  if(length(ctlfile) == 1 & file.exists(ctlfile)){
    if(verbose)message('searching ',ctlfile)
    m1 <- x %>%
      as.nmctl(verbose=verbose,rundir = rundir,ctlfile=ctlfile,...) %>%
      as.itemComments(fields=fields,...)
  }
  if(length(metafile) == 1 & file.exists(metafile) & read){
    if(verbose)message('searching ',metafile)
    m2 <- metafile %>% as.csv(...)
  }
  y <- full_join(m1,m2,by = intersect(names(m1),names(m2)))
  y <- unique(y)
  dups <- y$item[duplicated(y$item)]
  if(length(dups))warning('found conflicting metadata for ',paste(dups,collapse=', '))
  class(y) <- union('definitions', class(y))
  if(write & length(metafile) == 1) {
    y %>% as.csv(metafile,...)
    return(metafile)
  }
  y
}
