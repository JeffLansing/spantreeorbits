options(rgl.useNULL=FALSE)
suppressPackageStartupMessages(library(rgl))
options(rgl.useNULL=TRUE)
options(rgl.printRglwidget=FALSE)

#' map_trees_to_poly
#' Map spanning trees onto a polyhedron
#' @param verts The vertices of the polyhedron (x,y,z coordinates)
#' @param oids The indices of the rows of the trees to map
#' @param elist The left (or right) half of a nghmap structure
#' @param st_lf A list of spanning trees
#' @param st_rt Another list of spanning trees which is aligned with it
#'
#' @return
#' @export
#'
#' @examples
#' cmb <- combn(6,3)
#' tcmb <- t(cmb)
#' scmb <- t(apply(cmb, 2, function(cl) setdiff(1:6, cl)))
#' tinfo <- gen_span_trees(rhombic_dodecahedron$nghmap[,1:2], letters[1:4], tcmb)
#' sinfo <- gen_span_trees(rhombic_dodecahedron$nghmap[,3:4], letters[1:4], scmb)
#' map <- map_trees_to_poly(tetrahedron$verts, 1,
#'   rhombic_dodecahedron$nghmap[,1:2], tinfo$sts, sinfo$sts)
#'
map_trees_to_poly <- function(verts, oids, elist, st_lf, st_rt) {
  ixs <- lapply(oids, function(k) {
    el <- elist[st_lf[k,],]
    el %>% apply(c(2,1), c) %>% `dim<-`(c(1,nrow(el)*2))
  }) %>% abind::abind(along = 1)

  jxs <- lapply(oids, function(k) {
    el <- elist[st_rt[k,],]
    el %>% apply(c(2,1), c) %>% `dim<-`(c(1,nrow(el)*2))
  }) %>% abind::abind(along = 1)

  mpsa <- lapply(oids, function(k) {
    elist[st_lf[k,],] %>% apply(1, function(rw) {
      rbind(verts[rw[1],], verts[rw[2],]) %>% colMeans()
    }) %>% t()
  }) %>% abind::abind(along = 3)

  mpsb <- lapply(oids, function(k) {
    elist[st_rt[k,],] %>% apply(1, function(rw) {
      rbind(verts[rw[1],], verts[rw[2],]) %>% colMeans()
    }) %>% t()
  }) %>% abind::abind(along = 3)

  list(ixs = ixs, jxs = jxs, mpsa = mpsa, mpsb = mpsb,
       verts = verts, st_lf = st_lf, st_rt = st_rt)
}

#' display_pairs
#' Use rgl to display pairs of polyhedra, typically a polyhedron
#' and its dual
#' @param map1 A map of a tree to a polyhedron
#' @param map2 A map of a tree to a polyhedron
#' @param oids Indices
#' @param shared = TRUE Whether mouse actions are shared between the pair
#' @param theta = 20 An initial value for the theta display angle
#' @param phi = 10 An initial value for the phi display angle
#' @param umx = NULL A 3d rotation matrix. If present overrides theta and phi.
#'
#' @return named zero length integer vector: 'rglHighlevel' int(0)
#' @export
#'
#' @examples
display_pairs <- function(map1, map2, oids, shared = TRUE, theta = 20, phi = 10, umx = NULL) {
  len <- length(oids)
  if(len %% 2 == 1) len <- len + 1
  rc <- c(len/2, 4)
  ht <- ifelse(len/2 == 1, 350, 700)
  open3d(windowRect = c(50, 50, 750, ht))
  if(is.null(umx)) {
    view3d(theta, phi, zoom = 0.85)
  } else {
    view3d(userMatrix = umx, zoom = 0.85)
  }
  par3d(font = 2, FOV = 10) #0 is isomorphic view
  material3d(color = 'white', alpha = 0.1)
  mfrow3d(rc[1], rc[2], sharedMouse = shared)
  for(i in 1:length(oids)) {
    k <- oids[i]
    next3d()
    bgplot3d({
      plot.new()
      title(main = str_c('tree ', k), line = 2)
    })
    text3d(map1$verts, texts = map1$texts, adj = 1.1)
    segments3d(map1$verts[map1$ixs[i,],], col="red")
    segments3d(map1$verts[map1$jxs[i,],], col="forestgreen")
    text3d(map1$mpsa[,,i], texts = str_c(map1$st_lf[k,]), col = "red")
    text3d(map1$mpsb[,,i], texts = str_c(map1$st_rt[k,]), col = "forestgreen")
    next3d()
    bgplot3d({
      plot.new()
      title(main = str_c('dual ', k), line = 2)
    })
    text3d(map2$verts, texts = map2$texts, adj = 1.1)
    segments3d(map2$verts[map2$ixs[i,],], col="red")
    segments3d(map2$verts[map2$jxs[i,],], col="forestgreen")
    text3d(map2$mpsa[,,i], texts = str_c(map2$st_lf[k,]), col = "red")
    text3d(map2$mpsb[,,i], texts = str_c(map2$st_rt[k,]), col = "forestgreen")
  }
  highlevel(integer()) # To trigger display as rglwidget
}

#' display_split_pairs
#' Use rgl to display pairs of polyhedrasplit into small graphs
#' and its dual
#' @param map1 A map of a tree to a polyhedron
#' @param map2 A map of a tree to a polyhedron
#' @param oids Indices
#' @param shared = TRUE Whether mouse actions are shared between the pair
#' @param theta = 20 An initial value for the theta display angle
#' @param phi = 10 An initial value for the phi display angle
#' @param umx = NULL A 3d rotation matrix. If present overrides theta and phi.
#'
#' @return named zero length integer vector: 'rglHighlevel' int(0)
#' @export
#'
#' @examples
display_split_pairs <- function(map1, map2, oids, shared = TRUE, theta = 20, phi = 10, umx = NULL) {
  len <- length(oids)
  if(len %% 2 == 1) len <- len + 1
  rc <- c(len, 4)
  ht <- ifelse(rc[1] == 1, 350, 700)
  open3d(windowRect = c(50, 50, 750, ht))
  if(is.null(umx)) {
    view3d(theta, phi, zoom = 0.85)
  } else {
    view3d(userMatrix = umx, zoom = 0.85)
  }
  par3d(font = 2, FOV = 10) #0 is isomorphic view
  material3d(color = 'white', alpha = 0.1)
  mfrow3d(rc[1], rc[2], sharedMouse = shared)
  for(i in 1:length(oids)) {
    k <- oids[i]
    next3d()
    bgplot3d({
      plot.new()
      title(main = str_c('tree ', k), line = 3)
    })
    text3d(map1$verts, texts = map1$texts, adj = 0)
    segments3d(map1$verts[map1$ixs[i,],], col="red")
    # text3d(map1$mpsa[,,i], texts = str_c(map1$st_lf[k,]), col = "red")
    next3d()
    bgplot3d({
      plot.new()
      title(main = str_c('co-tree ', k), line = 3)
    })
    text3d(map1$verts, texts = map1$texts, adj = 0)
    segments3d(map1$verts[map1$jxs[i,],], col="forestgreen")
    # text3d(map1$mpsb[,,i], texts = str_c(map1$st_rt[k,]), col = "forestgreen")
    next3d()
    bgplot3d({
      plot.new()
      title(main = str_c('dual ', k), line = 3)
    })
    text3d(map2$verts, texts = map2$texts, adj = 0)
    segments3d(map2$verts[map2$ixs[i,],], col="red")
    # text3d(map2$mpsa[,,i], texts = str_c(map2$st_lf[k,]), col = "red")
    next3d()
    bgplot3d({
      plot.new()
      title(main = str_c('co-dual ', k), line = 3)
    })
    text3d(map2$verts, texts = map2$texts, adj = 0)
    segments3d(map2$verts[map2$jxs[i,],], col="forestgreen")
    # text3d(map2$mpsb[,,i], texts = str_c(map2$st_rt[k,]), col = "forestgreen")
  }
  highlevel(integer()) # To trigger display as rglwidget
}

#' display_tree_pairs
#'
#' @param poly a polyhedron
#' @param dual the dual of that polyhedron
#' @param ortho the polyhedron which results from applying
#' the ortho operator to either the polyhedron on to the dual
#' (ortho is the dual of expand)
#' @param trees a set of spanning trees which are compatible
#' with the three polyhedra (in matrix form)
#' @param choices a choosing of trees to display (as a vector
#' of indices of the trees in the matrix.
#'
#' @return a zero-length integer vector
#' @export
#'
#' @examples
#' library(spantreepairs)
#' library(rgl)
#' display_tree_pairs(tetrahedron, dual_tetrahedron, rhombic_dodecahedron, gens, c1,3,15,16)
display_tree_pairs <- function(poly, dual, ortho, trees, choices, split = FALSE) {
  nghmap <- ortho$nghmap
  l <- nghmap[,1:2] %>% max() -1
  r <- nghmap[,3:4] %>% max() -1
  tl <- trees[,1:l]
  tr <- trees[,(l+1):(l+r)]
  linfo <- gen_span_trees(nghmap[,1:2], letters[1:(l+1)], tl)
  rinfo <- gen_span_trees(nghmap[,3:4], letters[1:(r+1)], tr)

  verts1 <- poly$verts
  verts2 <- dual$verts
  elist1 <- nghmap[,1:2]
  elist2 <- nghmap[,3:4]
  sts1 <- linfo$sts
  sts2 <- rinfo$sts

  poly_map <- map_trees_to_poly(verts1,choices, elist1, sts1, sts2) %>%
    append(list(texts = poly$texts))
  dual_map <- map_trees_to_poly(verts2, choices, elist2, sts2, sts1) %>%
    append(list(texts = dual$texts))

  if(split) {
    display_split_pairs(poly_map, dual_map, choices)
  } else {
    display_pairs(poly_map, dual_map, choices)
  }
}

#' get_axes
#' Get the labeled rotation axes for a polyhedron
#'
#' @param verts The geometrically opposite vertices of a polyhedron
#' @param texts The labels for those vertices
#'
#' @return A vector of vertex indices
#' @export
#'
#' @examples
#'
get_axes <- function(verts, texts = str_c('x', 1:nrow(verts))) {
  vt <- verts %>% `row.names<-`(texts)
  vti <- (verts * -1) %>% `row.names<-`(str_c(texts, 'i'))
  both <- rbind(vt, vti)
  pairs <- both[order(both[,1], both[,2], both[,3]),] %>%
    rownames() %>%str_remove("i") %>% matrix(byrow = T, ncol = 2) %>%
    apply(1,sort) %>% t() %>% unique()
  wideaxes <- pairs[order(pairs[,1]),] %>% apply(1, function(rw) {
    ixs <- c(which(texts ==rw[1]),
             which(texts ==rw[2]))
    ixs
  }) %>% t()
  c(t(wideaxes))
}

#' display_poly
#' Display a polygon in various ways.
#' @param poly The polygon
#' @param dual = NULL The dual of the polygon, or optionally a contained polygon
#' @param codual = NULL Used for the dual of the contained polygon
#' @param scale = 1 A scale factor for any contained polygons
#' @param coscale = scale A scale factor for the second contained polygon
#' @param labels = TRUE Whether to displayed the vertex labels of the contained
#' polygons.
#' @param zoom = 0,8 An initial value  for the zoom factor
#' @param theta = 20 An initial value for the theta display angle
#' @param phi = 10 An initial value for the phi display angle
#' @param umx = NULL A 3d rotation matrix. If present overrides theta and phi.
#' @param show_segments = TRUE Whether or not to show the segments of the containing poly
#' @param show_labels = TRUE Whether or not to show the vertex labels of the containing poly
#' @param show_axes = FALSE Whether or not to show the rotation axes of the containing poly
#' @param show_arrows = FALSE Whether or not to show the segments of the containing poly as arrows
#' @param colorize = FALSE Whether or not to color the segments of the containing poly with scico colors
#' @param palette = "hawaii" The name of the scico pallette to use, if colorize is true
#'
#' @return named zero length integer vector: 'rglHighlevel' int(0)
#' @export
#'
#' @examples
#'
display_poly <- function(poly, dual = NULL, codual = NULL,
                         scale = 1, coscale = scale,
                         labels = TRUE, jitter = FALSE, zoom = 0.8,
                         theta = 20, phi = 10, umx = NULL,
                         show_segments = TRUE, show_labels = TRUE,
                         show_axes = FALSE, show_arrows =FALSE,
                         colorize = FALSE, palette = "hawaii") {
  open3d(windowRect = c(50, 50, 750, 700))
  if(is.null(umx)) {
    view3d(theta, phi, zoom = zoom)
  } else {
    view3d(userMatrix = umx, zoom = zoom)
  }
  par3d(font = 2, FOV = 10) #0 is isomorphic view
  material3d(color = 'white', alpha = 0.1)
  mfrow3d(1, 1, sharedMouse = FALSE)
  next3d()
  bgplot3d({
    plot.new()
  })
  if(show_axes) {
    axes <- poly$verts[get_axes(poly$verts, poly$texts),] * scale * 1.25
    clrs <- scico(nrow(axes)/2, palette = palette)
    for(j in seq(1, nrow(axes) - 1, 2)) {
      seg <- axes[c(j, j + 1),]
      segments3d(seg, col=clrs[(j+1)/2], lwd=1.0)
    }
  }
  if(colorize) {
    clrs <- scico(nrow(poly$segments)/2, palette = palette)
    for(j in seq(1, nrow(poly$segments) - 1, 2)) {
      seg <- poly$segments[c(j, j + 1),]
      segments3d(seg, col=clrs[(j+1)/2], lwd=1.5)
    }
  } else if(show_segments) {
    segments3d(poly$segments, col="black", lwd=1)
  } else if(show_arrows) {
    edges <- poly$edges
    stopifnot(NROW(edges)> 0)
    for(j in 1:nrow(edges)) {
      edge <- edges[j,]
      arrow3d(poly$verts[edge[1],], poly$verts[edge[2],], type = 'lines', s = 1/10)
    }
  }
  if(show_labels) {
    if(jitter) {
      text3d(poly$verts, texts = poly$texts, adj = -0.1)
    } else {
      text3d(poly$verts, texts = poly$texts)
    }
  }

  if(!is.null(dual)) {
    segments3d(dual$segments * scale, col="red", lwd=1.5)
    if(labels) {
      text3d(dual$verts * scale, texts = dual$texts, col="red", adj = 1.1)
    }
  }
  if(!is.null(codual)) {
    segments3d(codual$segments * coscale, col="forestgreen", lwd=1.5)
    if(labels) {
      text3d(codual$verts * coscale, texts = codual$texts, col="forestgreen", adj = 1.1)
    }
  }
  highlevel(integer()) # To trigger display as rglwidget
}
