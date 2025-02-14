library(magrittr)
library(stringr)

#' @importFrom magrittr %>%
NULL
#' @importFrom stringr str_c
NULL
#' @importFrom stringr str_remove_all
NULL
#' @importFrom stringr str_replace
NULL

#' Title Get a Deltoidal Tetracontaoctahedron
#' Translate the expanded_cuboctahedron into its dual polyhedron
#'
#' @return a list describing a deltoidal_tetracontaoctahedron
#' @export
#'
#' @examples
#' dtc <- get_deltoidal_tetracontaoctahedron()
#' stopifnot(nrow(dtc$vertices) == 50)
#'
#' @include expanded_cuboctahedron.R
get_deltoidal_tetracontaoctahedron <- function() {

  xc <- spantreeorbits:::expanded_cuboctahedron

  co_texts <- str_c('o', 1:12)
  rad_texts <- c(str_c('f', 1:6), str_c('v', 1:8))

  verts <- rbind(
    xc$cu_faces %>% apply(1, function(rw) {
      xc$verts[rw,] %>% colMeans()
    }) %>% t(),
    xc$oct_faces %>% apply(1, function(rw) {
      xc$verts[rw,] %>% colMeans()
    }) %>% t()
  )

  faces <- rbind(
    c(1, 20, 15, 23),
    c(1, 23, 9, 24),
    c(1, 24, 17, 19),
    c(1, 19, 7, 20),
    c(2, 25, 10, 26),
    c(2, 26, 16, 21),
    c(2, 21, 8, 22),
    c(2, 22, 18, 25),
    c(3, 30, 8, 27),
    c(3, 27, 11, 28),
    c(3, 28, 7, 29),
    c(3, 29, 12, 30),
    c(4, 33, 14, 34),
    c(4, 34, 9, 31),
    c(4, 31, 13, 32),
    c(4, 32, 10, 33),
    c(5, 38, 13, 35),
    c(5, 35, 15, 36),
    c(5, 36, 11, 37),
    c(5, 37, 16, 38),
    c(6, 41, 18, 42),
    c(6, 42, 12, 39),
    c(6, 39, 17, 40),
    c(6, 40, 14, 41),
    c(7, 19, 45, 29),
    c(7, 29, 3, 28),
    c(7, 28, 43, 20),
    c(7, 20, 1, 19),
    c(8, 30, 46, 22),
    c(8, 22, 2, 21),
    c(8, 21, 44, 27),
    c(8, 27, 3, 30),
    c(9, 34, 49, 24),
    c(9, 24, 1, 23),
    c(9, 23, 47, 31),
    c(9, 31, 4, 34),
    c(10, 25, 50, 33),
    c(10, 33, 4, 32),
    c(10, 32, 48, 26),
    c(10, 26, 2, 25),
    c(11, 36, 43, 28),
    c(11, 28, 3, 27),
    c(11, 27, 44, 37),
    c(11, 37, 5, 36),
    c(12, 29, 45, 39),
    c(12, 39, 6, 42),
    c(12, 42, 46, 30),
    c(12, 30, 3, 29),
    c(15, 35, 47, 23), #4 semi-redundant faces here
    c(16, 26, 48, 38),
    c(17, 40, 49, 24),
    c(18, 41, 50, 25)
  )

  texts <- c(str_c('f', 1:6), str_c('o', 1:12), str_c('d', 1:24), str_c('v', 1:8))

  segs <- list()
  for(i in 1:nrow(faces)) {
    face <- faces[i,]
    for(j in 1:4) {
      b <- j
      e <- ifelse(j < 4, j+1, 1)
      seg <- c(face[b], face[e])
      segs[[1+length(segs)]] <- seg
      if(j == 4) break
    }
  }
  segx <- Reduce(rbind, segs)
  segix <- get_set(segx) %>% as.list() %>% unlist()
  segments <- verts[segix,] #192 of these
  rownames(segments) <- segix

  edges <- texts[segix] %>% matrix(byrow = T, ncol = 2)
  g <- edges %>% igraph::graph_from_edgelist(directed = F)

  nghmap1 <- lapply(19:42, function(k) {
    txt <- texts[k]
    ngh <- igraph::neighbors(g, txt)
    names(ngh)
  }) %>% abind::abind(along = 2) %>% t() %>% `[`(TRUE,c(2,3,1,4))

  nghmap2 <- cbind(
    nghmap1[,1:2] %>% apply(c(1,2), function(x) {
      which(co_texts == x)
    }),
    nghmap1[,3:4] %>% apply(c(1,2), function(x) {
      which(rad_texts == x)
    })
  )

  # In Conway's operators, this is aaaC or aaaO (See Polyhedralisme )
  deltoidal_tetracontaoctahedron <- list(
    info = c(48,96,50) %>% `names<-`(c('facces', 'edges', 'vertices')),
    verts = verts,
    nghmap = nghmap2,
    faces = faces,
    texts = texts,
    segments = segments
  )
  deltoidal_tetracontaoctahedron
}

#'  Deltoidal Tetracontaoctahedron
#' @details A deltoidal_tetracontaoctahedron data structure.
#' \idescribe{
#'  \item{verts}{The vertex coordinates}
#'  \item{nghmap}{A map from the edges of a contained rhombic_dodecahedron to the edges of a contained cuboctahedron}
#'  \item{texts}{The vertex labels}
#'  \item{segments}{The edges of the deltoidal_tetracontaoctahedron}
#'  \item{faces}{The faces}
#' }
deltoidal_tetracontaoctahedron <- get_deltoidal_tetracontaoctahedron()
save(deltoidal_tetracontaoctahedron, file = "data/deltoidal_tetracontaoctahedron.rda")
