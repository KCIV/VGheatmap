% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ConvertIns.R
\name{ConvertIns}
\alias{ConvertIns}
\title{ConvertIns}
\usage{
ConvertIns(dataset, mapname, env = globalenv())
}
\arguments{
\item{dataset}{The Data you wish to convert.}

\item{mapname}{The map name you wish to graph the data against. This is used to get the conversion for the data set. (conversion is map specific)}

\item{env}{set the enviroment.}
}
\value{
Data sets you wish to graph.
}
\description{
Will take in a data set, run a conversion based on the map name given, then return a series of datasets to then be used for graphing.
}

