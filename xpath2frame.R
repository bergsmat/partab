.xpath2vector <- function(x, doc, atomic=FALSE, namespaces = FALSE, ...){
  stopifnot(length(x)==1)
  if(
    length(doc) == 1 &
    inherits(doc,'character')
  )doc <- .parse(doc, asText = !file, error = NULL, namespaces = namespaces, ...)
  # result <- xpathSApply(doc, x, fun = fun)
  # if(length(result)==0)result <- numeric(0)
  # if(local)free(doc)
  # result
  result <- if(atomic)
    doc %>% xml_find_first(x) 
  else 
    doc %>% xml_find_all(x) %>% xml_text
  
  if(inherits(result,'xml_missing')) result <- numeric(0)
  result
}

xmlValue.XMLAttributeValue <- function(x,...)as.best(x)

.unstrsplit <- function(x,split,...){
  if(!is.list(x)) x <- list(x)
  split <- rep(split,length.out=length(x))
 sapply(seq_along(x),function(i)paste(x[[i]],collapse=split[[i]],...),...)
}

.xmlTerminal <- function(x, depth,...){
  x <- strsplit(x,'/')[[1]]
  x <- x[x!='']
  dimension <- length(x)
  stopifnot(depth <= dimension)
  depth == dimension  
}

.token <- function (x, depth, ...){
  x <- strsplit(x,'/')[[1]]
  x <- x[x!='']
  x[depth]
}

.xmlCount <- function(x, doc, depth, namespaces = FALSE, ...){
  stopifnot(length(x)==1,depth >= 1)
  if(
    length(doc) == 1 &
    inherits(doc,'character')
  )doc <- .parse(doc, asText = !file, error = NULL, namespaces = namespaces, ...)
  x <- strsplit(x,'/')[[1]]
  node <- x != ''
  node <- cumsum(node)
  x <- x[node <= depth]
  x <- .unstrsplit(x,'/')
  x <- paste0('count',parens(x))
  y <- .xpath2vector(x=x,doc=doc,fun=xml_find_first,...)
  stopifnot(length(y) == 1)
  y
}

.xmlPredicate <- function(x, depth, i, ...){
  stopifnot(length(x)== 1, depth >=1)
  x <- strsplit(x,'/')[[1]]
  node <- x != '' # up to 2 leading '' are present
  node <- cumsum(node)
  index <- match(depth, node) # actual position corresponding to nth term
  pred <- function(subscript){
    y <- x
    y[index] <- paste0(y[index], '[', subscript, ']')
    y
  }
  preds <- lapply(i, pred)
  preds <- sapply(preds, .unstrsplit, split='/')
  preds
}
      
.xpath2index <- function(x, doc, depth=1, namespaces = FALSE, ...){
  # enumerate myself, convert to df with token as col name
  if(
    length(doc)==1 & 
    inherits(doc,'character')
  )doc <- .parse(doc)
  # local <- !inherits(doc,'XMLInternalDocument')
  # file <- local && is.character(doc) && file.exists(doc)
  # if(local) doc <- .parse(doc, asText = !file, error = NULL, namespaces = namespaces, ...)
  dim <- .xmlCount(x = x, doc = doc, atomic=TRUE, depth = depth,...)
  col <- data.frame(seq_len(dim))
  names(col) <- .token(x, depth = depth)
  # if I am terminal, return
  if(.xmlTerminal(x=x, depth=depth,...))return(col)
  # collect and enumerate children
  families <- .xmlPredicate(x, depth = depth, i = seq_len(dim), ...)
  children <- lapply(families, .xpath2index, doc = doc, depth = depth + 1, ...)
  enumeration <- sapply(children, nrow)
  # rbind children
  reunion <- do.call(rbind, children)
  # rep(myself, times=enumeration)
  col <- col[rep(rownames(col), times = enumeration),,drop=FALSE]
  # cbind(myself, children)
  stopifnot(nrow(col) == nrow(reunion))
  reunion <- cbind(col, reunion)
  # return
  # if(local)free(doc)
  reunion
}
.newcolname <- function(base,existing){
	stopifnot(length(base)==1)
	nm <- base
	count <- 0
	while(nm %in% existing){
		count <- count + 1
		nm <- paste(base,count,sep='.')
	}
	return(nm)
}
		
	
.xmlSimplify <- function(x, stack=FALSE, ignore=character(0),nodebase='node',...){
  nms <- lapply(x, unique)
  stat <- sapply(nms, function(nm)length(nm) == 1 && length(nm[is.defined(nm)]) == 1)
  stat <- setdiff(names(x)[stat] , ignore)
  x <- x[,setdiff(names(x) , stat),drop=FALSE]
  if(!stack) return(x)
  nms <- nms[setdiff(names(nms) , stat)]
  semi <- sapply(nms, function(nm)length(nm) == 2 && length(nm[is.defined(nm)]) == 1)
  semi <- setdiff(names(x)[semi] , ignore)
  if(!length(semi))return(x)
  node <- .newcolname(base=nodebase,existing=names(x))
  x[node] <- NA
  x <- shuffle(x, names(x)[names(x) %contains% paste0('^',nodebase,'\\b')])
  for(column in semi){
    disposable <- all( is.na( x[ node ][ is.defined( x[ column ] ) ] ) )
    if(disposable) x[ node ][ is.defined( x[ column ] ) ] <- column
    if(disposable) x[column] <- NULL
  }
  if(any(semi %in% names(x)))x <- .xmlSimplify(
  	x, 
  	stack = stack, 
  	ignore = c(ignore,node), 
  	nodebase = nodebase, 
  	...
  )
  x
}

.xpath2frame <- function(x, doc, fun=xmlValue, simplify = TRUE, namespaces = FALSE, ...){
  stopifnot(length(x)==1)
  if(
    length(doc)==1 & 
    inherits(doc,'character')
  )doc <- .parse(doc)
  # local <- !inherits(doc,'XMLInternalDocument')
  # file <- local && is.character(doc) && file.exists(doc)
  # if(local) doc <- .parse(doc, asText = !file, error = NULL, namespaces = namespaces,...)
  # Technically, namespaces should now be irrelevant, as the document is already parsed.
  index <- .xpath2index(x, doc, namespaces=namespaces,...)
  value <- .xpath2vector(x, doc, fun=xml_find_all, namespaces=namespaces, ...)
  stopifnot(nrow(index) == length(value))
  result <- cbind(index, value=value, stringsAsFactors = FALSE)
  #stopifnot(ncol(result) >= 1)
  #names(result)[length(names(result))] <- 'value'
  #if(!nrow(result))stop('no rows; names(index):',paste(collapse=',',names(index)),'; class(value):',paste(collapse=',',class(value)),'; doc:',substr(saveXML(doc),0,250))
  #if(local)free(doc)
  if(simplify) result <- .xmlSimplify(result)
  return(result)
}

xpath2frame <- function(x, doc, simplify = TRUE, sort = TRUE, nodebase = 'node', namespaces = FALSE, ...){
  if(
    length(doc) == 1 &
    inherits(doc,'character')
  ) doc <- .parse(doc)
  result <- lapply(x,.xpath2frame, doc = doc, simplify = FALSE, ...)
  #if(local)free(doc)
  result <- metaMerge(result)
  #if(! 'value' %in% names(result))stop('expected colname "value" but got ',paste(collapse=', ',names(result)))
  result <- shuffle(result, 'value',after=NULL)
  if(simplify) result <- .xmlSimplify(result, stack = TRUE, nodebase = nodebase, ...)
  try(silent = T, result <- arrange_(result, .dots = setdiff(names(result), 'value')))
  #if(sort) result <- sort(result)
  return(result)
}

# .parse <- function(doc, asText, error, namespaces,...){
# 	doc <- xmlParse(doc, asText = asText, error=error,...)
# 	if(!namespaces){
# 		removeXMLNamespaces(xmlRoot(doc), all = TRUE)
# 		doc <- xmlParse(saveXML(doc), asText = TRUE, error = function(...){})
# 	}
# 	doc
# }

.parse <- function(doc, asText, error, namespaces,...){
  doc <- readLines(doc)
  doc <- paste(doc,collapse=' ')
  doc <- gsub('nm:','',doc)
  read_xml(doc)
}

xlog <- function(
  x, 
  xpath=c(
    '/output/nonmem/problem/problem_title',
    '/output/nonmem/problem/estimation/estimation_method',
    '/output/nonmem/problem/estimation/estimation_title',
    '/output/nonmem/problem/estimation/termination_status',
    '/output/nonmem/problem/estimation/final_objective_function',
    '/output/nonmem/problem/estimation/theta/val',
    '/output/nonmem/problem/estimation/omega/row/col',
    '/output/nonmem/problem/estimation/sigma/row/col'
  ),
  opt = getOption('project'),
  project = if(is.null(opt)) getwd() else opt, 
  rundir = file.path(project,x), 
  file= file.path(rundir,paste0(x,'.xml')), 
  namespaces = TRUE,
  
  includeFile = NULL,
  simplify = TRUE,
  sort = TRUE,
  nodebase = 'node',
  ...
){
  stopifnot(xor(missing(x), missing(file)))
  mrun <- missing(x)
  if(mrun) includeFile <- TRUE
  if(is.null(includeFile)) includeFile <- FALSE
  exists <- file.exists(file)
  if(any(!exists))warning(
    'One or more files does not exist, e.g. ', 
    file[!exists][[1]],
    '. Perhaps set arg project= or options(project=)',
    immediate.=T
  )
  file <- file[exists]
  if(!mrun) x <- x[exists]
  result <- lapply(
    file, 
    function(doc) xpath2frame(
      x=xpath, 
      doc=doc, 
      simplify = FALSE, 
      sort = FALSE, 
      nodebase = nodebase,
      namespaces = namespaces,
      ...
    )
  )
  fnm <- .newcolname(base = 'file', existing=names(result))
  rnm <- .newcolname(base = 'run', existing=names(result))
  identify <- function(i){
    dat <- result[[i]]
    if(mrun || includeFile) dat[fnm] <- if(nrow(dat)) file[[i]] else character(0)
    if(!mrun) dat[rnm] <- if(nrow(dat)) x[[i]] else character(0)
    dat
  }
  result <- lapply(seq_along(result), identify)
  result <- metaMerge(result)
  if(simplify) result <- .xmlSimplify(result, stack = TRUE, ignore = c(fnm,rnm), nodebase = nodebase, ...)
  result <- shuffle(result, intersect(c(rnm,fnm) , names(result)))
  if('value' %in% names(result)) result <- shuffle(result, 'value',after=NULL)  
  #result <- as.keyed(result, key = names(result) %-% 'value')
  try(silent=T,result <- arrange_(result, .dots= setdiff(names(result), 'value')))
  # if(sort) result <- sort(result)
  return(result)
}

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



# 'model/CONTROL5.xml' %>% as.xml_document
# '1360' %>% as.xml_document
# '1360' %>% as.xml_document %>% xpath('//etashrink/row/col')
# 1360 %>% as.xml_document %>% xpath('//etashrink/row/col')
# 1360 %>% xpath('//etashrink/row/col')
# 1360 %>% xpath('//etashrink/*/descendant::*')













