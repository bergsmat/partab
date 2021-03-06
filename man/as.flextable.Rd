% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docx.R
\name{as.flextable}
\alias{as.flextable}
\alias{as.flextable.data.frame}
\alias{as.flextable.partab}
\title{Coerce to flextable}
\usage{
as.flextable(x, ...)

\method{as.flextable}{data.frame}(x, ...)

\method{as.flextable}{partab}(x,
  body.par.props = ReporteRs::parProperties(padding = 5),
  header.par.props = ReporteRs::parProperties(padding = 5),
  inner.vertical = ReporteRs::borderProperties(style = "none"),
  inner.horizontal = ReporteRs::borderProperties(width = 1),
  outer.vertical = ReporteRs::borderProperties(width = 0),
  outer.horizontal = ReporteRs::borderProperties(width = 2), footer = TRUE,
  ...)
}
\arguments{
\item{x}{object}

\item{...}{passed to methods}

\item{body.par.props}{passed to FlexTable}

\item{header.par.props}{passed to FlexTable}

\item{inner.vertical}{passed to setFlexTableBorders}

\item{inner.horizontal}{passed to setFlexTableBorders}

\item{outer.vertical}{passed to setFlexTableBorders}

\item{outer.horizontal}{passed to setFlexTableBorders}

\item{footer}{passed to setFlexTableBorders}
}
\value{
flextable

flextable
}
\description{
Coerces to flextable.

Coerces data.frame to flextable

Coerces partab to flextable.
}
\details{
Calls the data.frame method and adds remaining arguments. Pass zero-length values (e.g. NULL) to suppress.
}
\section{Methods (by class)}{
\itemize{
\item \code{data.frame}: data.frame method

\item \code{partab}: partab method
}}
\seealso{
\code{\link[ReporteRs]{docx}}

\code{\link[ReporteRs]{addFlexTable}}

\code{\link[ReporteRs]{FlexTable}}

\code{\link[ReporteRs]{docx}}

\code{\link[ReporteRs]{addFlexTable}}

\code{\link[ReporteRs]{FlexTable}}

\code{\link[ReporteRs]{docx}}

\code{\link[ReporteRs]{addFlexTable}}

\code{\link[ReporteRs]{FlexTable}}
}

