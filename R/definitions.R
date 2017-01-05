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
#' Create a Item Definitions from Model Name
#'
#' Creates a item definitions from a model name. Pass the project argument or set 
#' the project option.  
#' 
#' Normally you can just call the generic.  Suitable defaults are supplied, but much customization is supported by means of arguments documented here and in called functions.
#' @import magrittr
#' @import dplyr
#' @param x a model name (numeric or character)
#' @param verbose set FALSE to suppress messages
#' @param project parent directory of model directories
#' @param opt alternative argument for setting project
#' @param rundir specific model directory
#' @param ctlfile path to control stream
#' @param fields metadata fields to read from control stream if no metafile
#' @param unique drop duplicate definitions
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
#' @return object of class definitions, data.frame
#' @export
as.definitions.modelname <- function(
  x,
  verbose=FALSE,
  opt = getOption('project'),
  project = if(is.null(opt)) getwd() else opt, 
  rundir = file.path(project,x), 
  ctlfile = file.path(rundir,paste0(x,'.ctl')),
  fields = c('symbol','label','unit'),
  unique = TRUE,
  ...
){
  if(verbose)message('searching ',ctlfile)
  y <- x %>%
      as.nmctl(verbose=verbose,rundir = rundir,ctlfile=ctlfile,...) %>%
      as.itemComments(fields=fields,...) 
  if(unique) y <- y[!duplicated(y),]
  class(y) <- union('definitions', class(y))
  
  y
}