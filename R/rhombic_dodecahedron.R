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
#' @importFrom stringr str_replace_all
NULL

#' Title Get a Cuboctrahedron
#' Translate the cuboctahedron coordinates from the polyhedronisme OBJ output into
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
  path <- system.file("extdata", "polyhedronisme-daC.obj", package = "spantreeorbits", mustWork = TRUE)
  data <- readLines(con = path)
  code <- c()
  tmp <- c()
  for(i in 4:17) tmp <- c(tmp, data[i] %>% str_replace("v ", "c(") %>%
                            str_replace_all(" ", ", ") %>% str_c(., ')'))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 32:43) tmp <- c(tmp, data[i] %>% str_extract_all("( \\d+)") %>% unlist() %>%
                             str_c(collapse = ", ") %>% str_c("c(", ., ")"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("faces <- rbind(", ., ")"))
  eval(parse(text=code))

  vorder <- c(1,6,9,11,13,14,4,7,10,2,8,5,3,12)

  verts <- verts %>% zapsmall() %>% `[`(vorder, TRUE)
  faces <- faces %>% apply(1, function(rw) order(vorder)[rw]) %>% t()

  texts <- c(str_c('f', 1:6), str_c('v', 1:8))

  segs <- list()
  nc <- 4
  for(i in 1:nrow(faces)) {
    face <- faces[i,]
    for(j in 1:nc) {
      b <- j
      e <- ifelse(j < nc, j+1, 1)
      seg <- c(face[b], face[e])
      segs[[1+length(segs)]] <- seg
      if(j == nc) break
    }
  }
  segx <- Reduce(rbind, segs)
  segix <- get_set(segx) %>% as.list() %>% unlist()
  segments <- verts[segix,]

  edges <- texts[segix] %>% matrix(byrow = T, ncol = 2)
  g <- edges %>% igraph::graph_from_edgelist(directed = F)

  tetmap <- list("v1" = "t1", "v2" = "t2", "v3" = "t3", "v4" = "t4",
                 "v5" = "s1", "v6" = "s2", "v7" = "s3", "v8" = "s4")

  nghmap <- lapply(1:6, function(k) {
    txt <- texts[k]
    ngh <- igraph::neighbors(g, txt)
    vals <- tetmap[names(ngh)] %>% unlist() %>% sort() %>% str_remove_all("(s|t)") %>% as.numeric()
    c(sort(vals[1:2]), sort(vals[3:4]))
  }) %>% abind::abind(along = 2) %>% t() %>% `[`(TRUE,c(3,4,1,2))

  rhombic_dodecahedron <- list(
    info = c(12,24,14) %>% `names<-`(c('faces', 'edges', 'vertices')),
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

