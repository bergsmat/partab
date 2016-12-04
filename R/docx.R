#' Coerce to docx
#'
#' Coerces to docx
#' @param x object of dispatch
#' @param ... arguments to methods
#' @return docx
#' @export

as.docx <- function(x,...)UseMethod('as.docx')
#' Coerce docx to docx
#' 
#' Coerces docx to docx.
#' 
#' Returns the object.
#' @param x docx
#' @param ... dots
#' @return docx
#' @export
as.docx.docx <- function(x,...)x
#' Coerce data.frame to docx
#' 
#' Coerces data.frame to docx
#' @param x docx
#' @param title passed to docx
#' @param template passed to docx
#' @param empty_template passed to docx
#' @param list.definition passed to docx
#' @param ... passed to addFlexTable
#' @seealso \code{\link{docx}}
#' @seealso \code{\link{addFlexTable}}
#' @seealso \code{\link{FlexTable}}
#' @return docx
#' @export
as.docx.data.frame <- function(
  x,
  title='untitled',
  template,
  empty_template= FALSE,
  list.definition = getOption("ReporteRs-list-definition"),
  ...
){
  require(ReporteRs)
  doc <- docx(
    title=title,
    template=template,
    empty_template = empty_template,
    list.definition = list.definition
  )
  doc %<>% addFlexTable(as.flextable(x,...),...)
}
#' Coerce to flextable
#' 
#' Coerces to flextable.
#' @param x object
#' @param ... passed to methods
#' @seealso \code{\link{docx}}
#' @seealso \code{\link{addFlexTable}}
#' @seealso \code{\link{FlexTable}}
#' @export
as.flextable <- function(x,...)UseMethod('as.flextable')
#' Coerce data.frame to flextable
#' 
#' Coerces data.frame to flextable
#' @param x data.frame
#' @param ... passed to flextable if valid argument
#' @seealso \code{\link{docx}}
#' @seealso \code{\link{addFlexTable}}
#' @seealso \code{\link{FlexTable}}
#' @return flextable
#' @export
as.flextable.data.frame <- function(x,...){
  require(ReporteRs)
  dots <- list(...)
  nms <- names(dots)
  valid <- nms %in% c(
    'numrow','numcol','header.columns','add.rownames','body.cell.props',
    'body.par.props','body.text.props','header.cell.props','header.par.props',
    'header.text.props'
  )
  dots <- dots[valid]
  args <- c(
    list(data=x),
    dots
  )
  do.call(FlexTable,args)
}
#' Coerce partab to flextable
#' 
#' Coerces partab to flextable.
#' 
#' At present, just calls the data.frame method.
#' @param x data.frame
#' @param ... passed to flextable if valid argument
#' @seealso \code{\link{docx}}
#' @seealso \code{\link{addFlexTable}}
#' @seealso \code{\link{FlexTable}}
#' @return flextable
#' @export
as.flextable.partab <- function(x,...)as.flextable.data.frame(x,...)

#' Coerce to file
#' 
#' Coerces to file.
#' @param x object
#' @param ... passed to methods
#' @export
as.file <- function(x,...)UseMethod('as.file')

#' Coerce docx to File
#' 
#' Coerces docx to file.
#' @param x docx
#' @param ... passed to writeDoc
#' @seealso \code{\link{docx}}
#' @seealso \code{\link{writeDoc}}
#' @export
as.file.docx <- function(x,file,...)writeDoc(doc=x,file=file,...)
