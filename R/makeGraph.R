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
#' increasing order by function \code{\link{getMetaColOrder}}.
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
#' @import plyr
#' @seealso \code{\link{getMetaColOrder}} for ordering columns.
#'
#' @examples
makeMetaNodes <- function(df, colOrder, strength = 1e3) {
  if (missing(colOrder)) {
    colOrder <- getMetaColOrder(df)
  }
  if (length(strength) == 1) {
    strength <- rep(strength, length(colOrder) - 1)
  } else if (length(strength) > length(colOrder) - 1) {
    strength <- strength[1:(length(colOrder) - 1)]
  }
  v <- ldply(colOrder, makeMetaVertex, df)
  e <-
    ldply(1:(length(colOrder) - 1), makeMetaEdges, colOrder, df, v, strength)
  return(list(vertex = v, edges = e))
}

#' Create metavertices from metadata column.
#'
#' @param column column to take
#' @param df data.frame to analyse
#'
#' @return data.frame of vertices
#' @export
#'
#' @seealso \code{\link[igraph]{graph_from_data_frame}}
#' @examples
makeMetaVertex <- function(column, df) {
  val <- as.character(unique(df[, column]))
  tp <- column
  if (tolower(tp) == 'feature') {
    tp <- 'cFeature'
  }
  vd <- data.frame(
    ID = paste0(tp, seq_along(val)),
    Name = val,
    Type = tp,
    stringsAsFactors = FALSE
  )
  return(vd)
}

#' Create metaedges from two consecutive columns
#'
#' @param idx index of the first column
#' @param columns vector of column names
#' @param df metadata data.frame
#' @param vertex  data.frame of metavertices
#' @param strength value of the metaedge
#'
#' @return edges data.frame
#' @export
#'
#' @seealso \code{\link[igraph]{graph_from_data_frame}}
#' @examples
makeMetaEdges <- function(idx, columns, df, vertex, strength) {
  if (idx >= length(columns)) {
    warning(paste(
      'Index is outside of the column vector:',
      idx,
      ' - ',
      length(columns) - 1,
      '\n'
    ))
    return(data.frame(
      Source = 'A',
      Target = 'B',
      value = -1e-3,
      stringsAsFactors = FALSE
    )[FALSE, ])
  }
  cSource <- columns[idx]
  cTarget <- columns[idx + 1]
  sdf <- unique(df[, c(cSource, cTarget)])
  sidx <- match(sdf[, cSource], vertex$Name)
  tidx <- match(sdf[, cTarget], vertex$Name)
  val <- data.frame(
    Source = as.character(vertex$ID[sidx]),
    Target = as.character(vertex$ID[tidx]),
    value = strength[idx],
    stringsAsFactors = FALSE
  )
  return(val)
}

#' Get column order for metadata.
#'
#' @param df metadata \code{data.frame}
#'
#' @return vector of column names
#' @export
#'
#' @examples
#' md<-data.frame(class=c(rep('A',4),rep('B',4)),
#' subclass=paste0(c(rep('A',4),
#' rep('B',4)),rep(c(1,2),4)),
#' sample=paste0('sample',1:8))
#' getMetaColOrder(md)
getMetaColOrder <- function(df) {
  t <- apply(df, 2, function(.x) {
    length(table(.x))
  })
  colOrder <- names(df)[order(t, decreasing = FALSE)]
  return(colOrder)
}

#' Make data.frames for FS graph.
#'
#'
#' @description  \code{colnames} of \code{featureMatrix} corresponds to feature
#' names and \code{rownames} corresponds to names of samples. If name of some
#' row does not correspond to some value in the metadata \code{data.frame}
#' column that correspond to the last name in \code{colOrder}, that row will be
#' excluded from consideration. If any value in the metadata \code{data.frame}
#' column that correspond to the last name in \code{colOrder} is not found in
#' \code{rownames(featureMatrix)} error will be thrown.
#'
#' @param featureMatrix matrix of feature-sample edge strength.
#' @param metadata metadata \code{data.frame} for metanodes and metaedges.
#' @param colOrder see in \code{\link{makeMetaNodes}}
#' @param metaStrength see \code{strength} in \code{\link{makeMetaNodes}}
#'
#' @return igraph graph object
#' @export
#'
#' @import igraph reshape2 RColorBrewer
#' @examples
makeGraphDFs <-
  function(featureMatrix,
           metadata,
           colOrder,
           metaStrength = 1e3) {
    if (length(metaStrength) == 1) {
      metaStrength <- rep(metaStrength, length(colOrder) - 1)
    } else if (length(metaStrength) > length(colOrder) - 1) {
      metaStrength <- metaStrength[1:(length(colOrder) - 1)]
    }
    if (missing(colOrder)) {
      colOrder <- getMetaColOrder(metadata)
    }
    snames <- rownames(featureMatrix)
    fnames <- colnames(featureMatrix)
    sidx <- match(metadata[, rev(colOrder)[1]], snames)
    if (any(is.na(sidx))) {
      idx <- which(is.na(sidx))
      stop(
        paste(
          "Not all samples from metadata correspond to rows in featureMatrix: (",
          paste('"', metadata[idx, rev(colOrder)[1]], '"', collapse = ', '),
          ')\n'
        )
      )
    }
    snames <- snames[sidx]
    featureMatrix <- featureMatrix[sidx, ]
    l <-
      makeMetaNodes(df = metadata,
                    colOrder = colOrder,
                    strength = metaStrength)
    v <- l$vertex
    v <- rbind(v,
               data.frame(
                 ID = paste0('f', 1:dim(featureMatrix)[2]),
                 Name = fnames,
                 Type = 'feature',
                 stringsAsFactors = FALSE
               ))
    tp <- unique(v$Type)
    cl <- brewer.pal(length(tp), 'Set3')
    v$color <- cl[match(v$Type, tp)]
    v$size <- match(v$Type, rev(tp))

    e <- l$edges
    sID <- v$ID[match(snames, v$Name)]
    fID <- v$ID[match(fnames,v$Name)]
    rownames(featureMatrix) <- sID
    colnames(featureMatrix) <- fID
    mfm<-melt(featureMatrix, varnames = c('Source', 'Target'))
    e <- rbind(e, mfm[mfm$value>0,])
    return(list(vertex = v, edges = e))
  }

#' Make FS graph.
#'
#'
#' @description  \code{colnames} of \code{featureMatrix} corresponds to feature
#' names and \code{rownames} corresponds to names of samples. If name of some
#' row does not correspond to some value in the metadata \code{data.frame}
#' column that correspond to the last name in \code{colOrder}, that row will be
#' excluded from consideration. If any value in the metadata \code{data.frame}
#' column that correspond to the last name in \code{colOrder} is not found in
#' \code{rownames(featureMatrix)} error will be thrown.
#'
#' @param featureMatrix matrix of feature-sample edge strength.
#' @param metadata metadata \code{data.frame} for metanodes and metaedges.
#' @param colOrder see in \code{\link{makeMetaNodes}}
#' @param metaStrength see \code{strength} in \code{\link{makeMetaNodes}}
#'
#' @return igraph graph object
#' @export
#'
#' @import igraph reshape2 RColorBrewer
#' @examples
makeGraph<-function(featureMatrix,
                    metadata,
                    colOrder,
                    metaStrength = 1e3){
  l<-makeGraphDFs(featureMatrix=featureMatrix,
                   metadata=metadata,
                   colOrder=colOrder,
                   metaStrength = metaStrength)
  g <- graph_from_data_frame(d = l$edges,
                             directed = FALSE,
                             vertices = l$vertex)
  return(g)

}
