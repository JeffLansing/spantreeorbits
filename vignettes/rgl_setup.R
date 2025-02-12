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
  nr <- nrow(map1$ixs)
  rc <- c(nr, 2)
  open3d(windowRect = c(50, 50, 750, 350))
  if(is.null(umx)) {
    view3d(theta, phi, zoom = 0.85)
  } else {
    view3d(userMatrix = umx, zoom = 0.85)
  }
  par3d(font = 2, FOV = 10) #0 is isomorphic view
  material3d(color = 'white', alpha = 0.1)
  mfrow3d(rc[1], rc[2], sharedMouse = shared)
  for(i in 1:length(oids)) {
    next3d()
    bgplot3d({
      plot.new()
      title(main = str_c('tree ', oids[i]), line = 2)
    })
    text3d(map1$verts, texts = map1$texts, adj = 1.1)
    segments3d(map1$verts[map1$ixs,], col="red")
    segments3d(map1$verts[map1$jxs,], col="forestgreen")
    text3d(map1$mpsa[,,i], texts = str_c(map1$st_lf[oids[i],]), col = "red")
    text3d(map1$mpsb[,,i], texts = str_c(map1$st_rt[oids[i],]), col = "forestgreen")
    next3d()
    bgplot3d({
      plot.new()
      title(main = str_c('tree ', oids[i]), line = 2)
    })
    text3d(map2$verts, texts = map2$texts, adj = 1.1)
    segments3d(map2$verts[map2$ixs,], col="red")
    segments3d(map2$verts[map2$jxs,], col="forestgreen")
    text3d(map2$mpsa[,,i], texts = str_c(map2$st_lf[oids[i],]), col = "red")
    text3d(map2$mpsb[,,i], texts = str_c(map2$st_rt[oids[i],]), col = "forestgreen")
  }
  highlevel(integer()) # To trigger display as rglwidget
}

#' display_poly
#' Display a polygon in various ways.
#' @param poly The polygon
#' @param dual = NULL The dual of the polygon, or optionally a contained polygon
#' @param codual = NULL Used for the dual of the contained polygon
#' @param scale = 1 A scale factor for any contained polygons
#' @param labels = TRUE Whether to displayed the vertex labels of the contained
#' polygons.
#' @param zoom = 0,8 An initial value  fro the zoom factor
#' @param theta = 20 An initial value for the theta display angle
#' @param phi = 10 An initial value for the phi display angle
#' @param umx = NULL A 3d rotation matrix. If present overrides theta and phi.
#'
#' @return named zero length integer vector: 'rglHighlevel' int(0)
#' @export
#'
#' @examples
#'
display_poly <- function(poly, dual = NULL, codual = NULL,
                         scale = 1, labels = TRUE, zoom = 0.8,
                         theta = 20, phi = 10, umx = NULL) {
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
  segments3d(poly$segments, col="black", lwd=1)
  text3d(poly$verts, texts = poly$texts)#, adj = 1.1)
  if(!is.null(dual)) {
    segments3d(dual$segments * scale, col="red", lwd=1)
    if(labels) {
      text3d(dual$verts * scale, texts = dual$texts, col="red", adj = 1.1)
    }
  }
  if(!is.null(codual)) {
    segments3d(codual$segments * scale, col="forestgreen", lwd=1)
    if(labels) {
      text3d(codual$verts * scale, texts = codual$texts, col="forestgreen", adj = 1.1)
    }
  }
  highlevel(integer()) # To trigger display as rglwidget
}
