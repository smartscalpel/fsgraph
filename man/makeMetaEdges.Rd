% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeGraph.R
\name{makeMetaEdges}
\alias{makeMetaEdges}
\title{Create metaedges from two consecutive columns}
\usage{
makeMetaEdges(idx, columns, df, vertex, strength)
}
\arguments{
\item{idx}{index of the first column}

\item{columns}{vector of column names}

\item{df}{metadata data.frame}

\item{vertex}{data.frame of metavertices}

\item{strength}{value of the metaedge}
}
\value{
edges data.frame
}
\description{
Create metaedges from two consecutive columns
}
\seealso{
\code{\link[igraph]{graph_from_data_frame}}
}
