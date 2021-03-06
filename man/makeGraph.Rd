% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeGraph.R
\name{makeGraph}
\alias{makeGraph}
\title{Make FS graph.}
\usage{
makeGraph(featureMatrix, metadata, colOrder, metaStrength = 1000)
}
\arguments{
\item{featureMatrix}{matrix of feature-sample edge strength.}

\item{metadata}{metadata \code{data.frame} for metanodes and metaedges.}

\item{colOrder}{see in \code{\link{makeMetaNodes}}}

\item{metaStrength}{see \code{strength} in \code{\link{makeMetaNodes}}}
}
\value{
igraph graph object
}
\description{
\code{colnames} of \code{featureMatrix} corresponds to feature
names and \code{rownames} corresponds to names of samples. If name of some
row does not correspond to some value in the metadata \code{data.frame}
column that correspond to the last name in \code{colOrder}, that row will be
excluded from consideration. If any value in the metadata \code{data.frame}
column that correspond to the last name in \code{colOrder} is not found in
\code{rownames(featureMatrix)} error will be thrown.
}
