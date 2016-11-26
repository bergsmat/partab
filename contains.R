`contains` <-
function(pattern,text,...){
	hits <- regexpr(pattern,text,...)
	hits >=0
}
`%contains%` <- function(x,y)contains(y,x)
`text2decimal` <-
function (x) as.numeric(sub("^[^0-9.+-]*([0-9.eE+-]+).*$", "\\1", as.character(x)))

aug <- function(`_data`,...){
	extras <-  eval(substitute(list(...)), `_data`, parent.frame())
	nms <- names(extras)
	for(name in nms)`_data`[[name]] <- extras[[name]]
	`_data`
}
is.defined <- function(x)!is.na(x)
pool <- function(x,y)list(x=setdiff(x,y),y=setdiff(y,x),both=intersect(x,y))
parens <- function(x,...)paste0('(',x,')')
padded<-function (x, width = 4, ...) 
  sprintf(paste0("%0", width, ".0f"), x)