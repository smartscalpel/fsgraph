#' Function creates two dataframes for metanodes and edges between them.
#'
#' Metanodes are sample and nodes describing them. Unique name in each column in
#' \code{df} defines metanode of type, specified by column name. Metanodes
#' should form a forrest. Connections between metanodes defined by order of
#' columns in \code{colOrder}. Metanodes which names are in the same row in
#' neighbour column in \code{colOrder} will be connected by edge of the strength
#' from \code{strength} argument.
#' The column wihch name is the first in \code{colOrder} will contains roots of
#' trees in the forrest. Second column will be connected to first, third to
#' second and so on.
#'
#' If \code{colOrder} is not provided order of columns in \code{df} will be
#' calculated by ordering columns according number of distinct values in it in
#' increasing order.
#'
#' @param df metadata data frame, describing relationship between metanodes.
#' @param colOrder vector of column names. Metanodes defined by unique names in
#' each column will be connected in order defined by \code{colOrder}.
#' @param strength strength of edge between two metadata. \code{strength} could
#' be ether one value, applied to each edge, or vector of length
#' \code{length(colOrder)-1} for each pair of metanodes types.
#'
#' @return list with two \code{data.frames} vertex and edges.
#' @export
#'
#' @examples
makeMetaNodes<-function(df,colOrder,strength=1e3){
  if(missing(colOrder)){
    t<-apply(df,2,function(.x){length(table(.x))})
    colOrder<-names(df)[order(t,decreasing = FALSE)]
  }
  v<-data.frame()
  e<-data.frame()
  return(list(vertex=v,edges=e))
}

#' Make FS graph.
#'
#' @param featureMatrix matrix of feature-sample edge strength.
#' @param metadata metadata \code{data.frame} for metanodes and metaedges.
#' @param colOrder see in \code{\link{makeMetaNodes}}
#' @param metaStrength see \code{strength} in \code{\link{makeMetaNodes}}
#'
#' @return igraph graph object
#' @export
#'
#' @import igraph
#' @examples
makeGraph<-function(featureMatrix,metadata,colOrder,metaStrength=1e3){
  g<-empty_graph()
  return(g)
}
