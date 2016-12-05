#' Create an xml_document in a Project Context
#'
#' Creates an xml_document in a project context.
#' @param x object of dispatch
#' @param ... arguments to methods
#' @seealso \code{\link{xpath}}
#' @return xml_document
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='partab'))
#' 1001 %>% as.xml_document
#' @export

as.xml_document <- function(x,...)UseMethod('as.xml_document')
as.xml_document.xml_document <- function(x,...)x
as.xml_document.numeric  <- function(x,...)as.xml_document(as.character(x),...)
as.xml_document.character <- function(x,...){
  class(x) <- if(file.exists(x)) 'filepath' else 'modelname'
  as.xml_document(x,...)
}

#' Create xml_document From Filepath
#'
#' Creates an xml_document from filepath
#' @import xml2
#' @param x filepath
#' @param strip.namespace whether to strip e.g. nm: from xml elements
#' @param ... passed arguments
#' @return xml_document
#' @describeIn as.xml_document filepath method
#' @export
as.xml_document.filepath <- function(x,strip.namespace=TRUE,...){
  if(!strip.namespace)return(read_xml(x))
  x <- readLines(x)
  x <- paste(x,collapse=' ')
  x <- gsub('<[a-zA-Z]+:','<',x)
  x <- gsub('</[a-zA-Z]+:','</',x)
  x <- gsub(' +[a-zA-Z]+:',' ',x)
  read_xml(x)
}

#' Create xml_document From modelname
#'
#' Creates an xml_document from modelname
#' @param x filename
#' @param project parent directory of model directories
#' @param opt alternative argument for setting project
#' @param rundir specific model directory
#' @param file actual xml storage location; overrides others if specified directly
#' @param ... passed arguments
#' @return xml_document
#' @describeIn as.xml_document modelname method
#' @export

as.xml_document.modelname <- function(
  x,
  project = if(is.null(opt)) getwd() else opt, 
  opt = getOption('project'),
  rundir = file.path(project,x), 
  file = file.path(rundir,paste0(x,'.xml')), 
  ...
){
  class(file) <- 'filepath'
  if(!file.exists(file))stop(
    'file not found:', 
    file,
    '. Perhaps set arg project= or options(project=)'
  )
  as.xml_document(file)
}

#' Evaluate xpath Expression
#'
#' Evaluates an xpath expression.
#' 
#' The resulting nodeset is scavenged for text, and coerced to best of numeric or character.
#' @param x xml_document
#' @param ... passed arguments
#' @export
#' @examples
#' library(magrittr)
#' options(project = system.file('project/model',package='partab'))
#' 1001 %>% xpath('//etashrink/row/col')
xpath <- function(x,...)UseMethod('xpath')

#' Evaluate xpath Expression in Default Context
#'
#' Coerces x to xml_document and evaluates.
#' @param x xml_document
#' @param ... passed arguments
#' @return vector
#' @describeIn xpath default method
#' @export
xpath.default <- function(x,...)xpath(as.xml_document(x),...)

#' Evaluate xpath Expression in Document Context
#'
#' Evaluates an xpath expression for a given document.
#' 
#' The resulting nodeset is scavenged for text, and coerced to best of numeric or character.
#' @import magrittr
#' @import xml2
#' @param x xml_document
#' @param xpath xpath expression to evaluate
#' @param ... passed arguments
#' @return vector
#' @describeIn xpath xml_document method
#' @export
xpath.xml_document <- function(x, xpath,...){
  x %>%
    xml_find_all(xpath) %>%
    xml_text %>%
    as.best
}

