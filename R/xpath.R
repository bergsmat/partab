as.xml_document <- function(x,...)UseMethod('as.xml_document')
as.xml_document.xml_document <- function(x,...)x
as.xml_document.numeric  <- function(x,...)as.xml_document(as.character(x),...)
as.xml_document.character <- function(x,...){
  class(x) <- if(file.exists(x)) 'filepath' else 'modelname'
  as.xml_document(x,...)
}
as.xml_document.filepath <- function(x,strip.namespace=TRUE,...){
  if(!strip.namespace)return(read_xml(x))
  x <- readLines(x)
  x <- paste(x,collapse=' ')
  x <- gsub('<[a-zA-Z]+:','<',x)
  x <- gsub('</[a-zA-Z]+:','</',x)
  x <- gsub(' +[a-zA-Z]+:',' ',x)
  read_xml(x)
}
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

xpath <- function(x,...)UseMethod('xpath')
xpath.default <- function(x,...)xpath(as.xml_document(x),...)
xpath.xml_document <- function(x, xpath,...){
  x %>%
    xml_find_all(xpath) %>%
    xml_text %>%
    as.best
}

