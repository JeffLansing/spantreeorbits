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

#' Title Get a Cuboctrahedron
#' Translate the cuboctahedron coordinates from the dmcooey library into
#' R code
#'
#' @return a list describing a cuboctahedron
#' @export
#'
#' @examples
#' rad <- get_rhombic_dodecahedron()
#' stopifnot(nrow(rad$vertices) == 14)
#'
#' @include sto_helpers.R
get_rhombic_dodecahedron <- function() {
  data <- readLines("http://dmccooey.com/polyhedra/RhombicDodecahedron.txt")
  code <- c()
  for(i in 3:4) code <- c(code, data[i] %>% str_replace("=.+=", '=') %>%
                            str_c(';'))
  tmp <- c()
  for(i in 6:19) tmp <- c(tmp, data[i] %>% str_replace("V.+= ", "c"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 22:33) tmp <- c(tmp, data[i] %>% str_replace("\\{", "c(") %>%
                             str_replace("\\}", ") + 1"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("faces <- rbind(", ., ")"))
  eval(parse(text=code))

  texts <- c(str_c('f',1:6), str_c('s',1:4), str_c('t',1:4))[c(1:6,8,12,11,7,13,9,10,14)]

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
  segments <- verts[segix,]
  rownames(segments) <- segix

  edges <- texts[segix] %>% matrix(byrow = T, ncol = 2)
  g <- edges %>% igraph::graph_from_edgelist(directed = F)

  nghmap <- lapply(1:6, function(k) {
    txt <- texts[k]
    ngh <- igraph::neighbors(g, txt)
    vals <- names(ngh) %>% sort() %>% str_remove_all("(s|t)") %>% as.numeric()
    c(sort(vals[1:2]), sort(vals[3:4]))
  }) %>% abind::abind(along = 2) %>% t() %>% `[`(TRUE,c(3,4,1,2))

  lapply(1:6, function(k) {
    txt <- texts[k]
    ngh <- igraph::neighbors(g, txt)
    names(ngh)
  })

  rhombic_dodecahedron <- list(
    info = c(12,24,14) %>% `names<-`(c('facces', 'edges', 'vertices')),
    verts = verts,
    nghmap = nghmap,
    faces = faces,
    texts = texts,
    segments = segments
  )
  rhombic_dodecahedron
}

#'  Rhombic_Dodecahedron
#' @details A rhombic_dodecahedron data structure.
#' \describe{
#'  \item{verts}{The vertex coordinates}
#'  \item{nghmap}{A map from the edges of a contained tetrahedron to
#' the edges of its dual}
#'  \item{texts}{The vertex labels}
#'  \item{segments}{The edges of the rhombic_dodecahedron}
#'  \item{faces}{The faces}
#' }
#'
rhombic_dodecahedron <- get_rhombic_dodecahedron()
save(rhombic_dodecahedron, file = "data/rhombic_dodecahedron.rda")

