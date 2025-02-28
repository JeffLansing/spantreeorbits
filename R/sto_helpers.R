require(magrittr)
require(stringr)

#' @importFrom utils capture.output
NULL

#' Title Find the index of a row in a matrix
#'
#' @param mx a 2d array of values
#' @param row a 1d row of values
#'
#' @return The index of the requested row in the array,
#' or 0 if not found.
#' @export
#'
#' @examples
#' wh <- which_row(matrix(c(1,2,3,4), ncol = 2), c(2,4))
#' stopifnot(wh == 2)
#'
which_row <- function(mx, row) {
  max(which(rowSums(mx == row[col(mx)]) == ncol(mx)), 0)
}

#' Title Convert an edgelist to a set of unordered pairs
#'
#' @param info the edgelist to be converted
#'
#' @return a set of unordered pairs (i.e., 2-element sets)
#' @export
#'
#' @examples
#' s <- get_set(matrix(c(1,2,3,4,5,6), ncol = 2))
#' stopifnot(length(s) == 3)
#'
get_set <- function(info) {
  sel <- info %>% apply(1,function(r) {sets::set(sets::as.set(r))})
  Reduce(sets::set_union, sel)
}

#' get_perms
#'
#' @param actions A matrix of permutations, one per row
#'
#' @return A character vector of the permutations as cycles
#' @export
#'
#' @examples
#' mx <- rbind(c(1,2,3),c(1,3,2))
#' v <- get_perms(mx)
#' stopifnot(nrow(mx) == length(v))
#'
get_perms <- function(actions) {
  options("comma" = TRUE)
  lapply(1:nrow(actions), function(k) {
    capture.output(
      actions[k,] %>% permutations::as.word() %>% permutations::as.cycle() %>% print()
    ) %>% stringr::str_remove('^....')
  }) %>% unlist()
}

#' edgelist_to_adjmat
#' Convert an edgelist representation of a graph into an
#' adjacency matrix representation of the same graph
#' @param edgelist An edgelist
#'
#' @return An adjacency list
#' @export
#'
#' @examples
#' el <- matrix(c(1,1,1,2,3,4), ncol = 2)
#' amx <- edgelist_to_adjmat(el)
#' stopifnot(rowSums(amx) == colSums(amx))
#'
edgelist_to_adjmat <- function(edgelist) {
  ext <- max(edgelist)
  mx <- matrix(0,ext,ext)
  mx[edgelist] <- 1
  mx[edgelist[,c(2,1)]] <- 1
  mx
}

#' as_incidence_matrix
#'The incidence matrix imxof a graph has a row for each vertex
#'and a column for each edge, with imx\[v,e\] ==1 if vertex v
#'is incident upon edge e, and 0 otherwise.
#' @param elist The edges of the graph
#' @param vertices The vertices of the graph
#'
#' @return The incidence matrix of the graph
#' @export
#'
#' @examples
#' el <- matrix(c(1,1,1,2,3,4), ncol = 2)
#' vs <- 1:4
#' amx <- edgelist_to_adjmat(el)
#' imx <- as_incidence_matrix(el, vs)
#' stopifnot(amx %*% imx == 1)
#'
as_incidence_matrix <- function(elist, vertices) {
  imx <- elist[,1:2] %>% apply(1, function(r) {
    vals <- rep(0,length(vertices))
    vals[which(vertices == r[1])] <- 1
    vals[which(vertices == r[2])] <- 1
    vals
  })
  imx
}

#' as_es
#' A helper function to retrieve edge indices from an incidence matrix
#' @param mx An incidence matrix
#'
#' @return A list of edge indices
#'
as_es <- function(mx) {
  apply(mx,1, function(x) x[x != 0]) %>%
    unlist() %>% unique() %>% sort()
}

#' depth_first
#' A way of detecting cycles in a graph
#' @param ajmx An adjacency matrix of a graph
#'
#' @return The longest path from the initial vertex
#' @export
#'
#' @examples
#' el <- matrix(c(1,1,1,2,3,4), ncol = 2)
#' amx <- edgelist_to_adjmat(el)
#' df <- depth_first(amx)
#' stopifnot(length(df) == 4)
#'
depth_first <- function(ajmx) {
  i <- 1
  len <- nrow(ajmx)

  dfs <- function(i, vs, visited) {
    vs <- c(vs, i)
    visited[[i]] <- TRUE
    for(j in len:1) {
      if(!visited[j] && ajmx[i,j] == 1) {
        vv <- dfs(j, vs, visited)
        vs <- vv[[1]]
        visited <- vv[[2]]
      }
    }
    list(vs, visited)
  }
  dfs(1, c(), rep(FALSE, len))[[1]]
}

#' edgelist
#' A refinement of the base function "matrix(ncol = 2)"
#'
#' @param edgevec a vector which is a general edgelist (of an undirected graph)
#' in column-order
#'
#' @return an edgelist in canonical form, which means (for an undirected
#' graph only) that the vertex index in the first column is less than or equal
#' to the vertex index in the second column.
#' @export
#'
#' @examples
#' require(magrittr)
#' pg <- igraph::make_graph("Petersen")
#' elist <- pg %>% igraph::as_edgelist()
#' ncelist <- elist
#' identical(ncelist, elist) #expect TRUE
#' for(i in c(2,3,5,7,11,13)) {
#'   tmp <- elist[i,1]
#'   ncelist[[i,1]] <- elist[i,2] # put second first
#'   ncelist[[i,2]] <- tmp # put first second
#' }
#' identical(ncelist, elist) #expect FALSE
#' ncpg <- ncelist %>% igraph::graph_from_edgelist(directed = FALSE)
#' igraph::is_isomorphic_to(ncpg, pg) #expect TRUE
#' celist <- ncelist %>% as.numeric() %>% edgelist()
#' identical(celist, elist) #expect TRUE
#'
edgelist <- function(edgevec) {
  len <- length(edgevec)
  if(len %% 2 == 1) stop("length of edgevec must be even")
  dim <- len/2 # so that the edgevec can be divided into 2 parts
  wh <- edgevec[1:dim] > edgevec[1:dim + dim] # find the indices which are uncanonical
  reord <- 1:len + c(wh * dim, wh * -dim) # create an index vector for reordering
  `dim<-`(edgevec[reord], c(dim, 2)) #canonicalize the edge vector and create a matrix
}

#' canonicalize
#' Canonicalize an edgelist
#' @param el an edge list
#'
#' @return the edgelist in canonical form
#'
#' @export
#'
#' @examples
#' el <- rbind(c(4,3),c(2,1))
#' cc <- canonicalize(el)
#' stopifnot(cc[1,1] <= cc[1,2] & cc[1,1] <= cc[2,1])
#'
canonicalize <- function(el) {
  mx <- edgelist(el)
  mx <- mx[order(mx[,1]),]
  mxi <- apply(mx, c(1,2), as.integer)
  mxi
}


#' gen_span_trees
#' Generate the spanning trees of a polygon
#' @param elist The edgelist of the polygon
#' @param vertices The vertices of the polygon (x,y,z coordinates)
#' @param cmb A superset of the spanning trees, for example:
#' the combinations of nrow(edgelist) things taken length(vertices) -1
#' at a time
#'
#' @return A matrix of spanning trees, one per row
#' @export
#'
#' @examples
#' cmb <- combn(6,3)
#' tcmb <- t(cmb)
#' scmb <- t(apply(cmb, 2, function(cl) setdiff(1:6, cl)))
#' tinfo <- gen_span_trees(rhombic_dodecahedron$nghmap[,1:2], letters[1:4], tcmb)
#' sinfo <- gen_span_trees(rhombic_dodecahedron$nghmap[,3:4], letters[1:4], scmb)
#' stopifnot(identical(tinfo$is, sinfo$is))
#'
gen_span_trees <- function(elist, vertices, cmb) {
  en <- nrow(elist)
  vn <- length(vertices)

  imx <- elist %>% as_incidence_matrix(c(1:vn))
  vm <- vn - 1
  u <- diag(c(1:en)) %*% t(imx[1:vm,])

  is <- c()
  mxs <- list()
  eis <- list()
  for(i in 1:nrow(cmb)) {
    mx <- u[cmb[i,],]
    dt <- det(mx)
    if(dt != 0){
      adjmat <- elist[cmb[i,],] %>% edgelist_to_adjmat()
      len <- depth_first(adjmat) %>% length()
      if(len == vn) {
        is <- c(is, i)
        mxs[[1 + length(mxs)]] <- mx
        eis[[1 + length(eis)]] <- mx %>% as_es()
      }
    }
  }

  if(length(is) > 0) {
    sts = abind::abind(eis, along = 2) %>% t()
    mxs = abind::abind(mxs, along = 3)
  } else {
    sts <- NULL
    mxs <- NULL
  }

  val <- list(
    elist = elist,
    u = u,
    sts = sts,
    mxs = mxs,
    sels = cmb[is,],
    is = is
  )
  val
}

#' get_orbits_of_trees
#'
#' @param sts A matrix of pairs of aligned spanning trees for a polyhedon and its dual,
#' represented as a set of edge labels
#' @param ops The elements of a permutation group, regarded as symmetry operators
#' @param lf The number of edges in a spanning tree of the polyhedron.
#' @param rt The number of edges in a spanning tree of the polyhedron.
#' @param orb_only = TRUE If false the multiplication table is also returned,
#'
#' @return A list of orbits referring to the spanning trees by their row indices
#' @export
#'
#' @examples
#' cmb <- combn(6,3)
#' tcmb <- t(cmb)
#' scmb <- t(apply(cmb, 2, function(cl) setdiff(1:6, cl)))
#' tinfo <- gen_span_trees(rhombic_dodecahedron$nghmap[,1:2], letters[1:4], tcmb)
#' sinfo <- gen_span_trees(rhombic_dodecahedron$nghmap[,3:4], letters[1:4], scmb)
#' ops <- rbind(c(1,2,3,4,5,6), c(5,6,2,1,4,3))
#' orbits <- get_orbits_of_trees(cbind(tinfo$sts, sinfo$sts), ops, 3, 3)
#' stopifnot(length(orbits) == 5)
#'
get_orbits_of_trees <- function(sts, ops, lf, rt, orb_only = TRUE) {
  aemap <- c()
  stsx <- apply(sts,1,digest::digest)
  p <- 1; q <- lf; r <- q + 1; s <- q + rt;
  for(k in 1:nrow(ops)) {
    op <- ops[k,]
    for(j in 1:nrow(sts)) {
      st <- sts[j,]
      pr <- op[st] %>% as.numeric()
      sprx <- c(pr[p:q] %>% sort(), pr[r:s] %>% sort()) %>% digest::digest()
      i <- match(sprx, stsx)
      if(!is.na(i)) {
        aemap <- c(aemap, c(j,i,k))
      }
    }
  }
  aemapx <- matrix(aemap, byrow = T, ncol = 3)
  edges <- aemapx %>% data.frame() %>% dplyr::filter(X1 != X2) %>%
    dplyr::distinct(X1, X2) %>% apply(c(1,2), as.character)
  gr <- edges %>% igraph::graph_from_edgelist(directed = T)
  sgrs <- igraph::decompose(gr, mode = 'weak')

  orbits <- vector(mode = 'list', length = length(sgrs))
  for(i in 1:length(sgrs)) {
    sgr <- sgrs[[i]]
    vs <- igraph::V(sgr)
    orbit <- vs$name %>% as.integer() %>% sort()
    orbits[[i]] <- orbit
  }
  if(orb_only) {
    orbits
  } else {
    list(
      orbits = orbits,
      map = aemapx
    )
  }
}
