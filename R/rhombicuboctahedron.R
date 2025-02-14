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

#' Title Get a Rhombicuboctahedron
#' Translate the rhombicuboctahedron coordinates from the dmcooey library into
#' R code
#'
#' @return a list describing a rhombicuboctahedron
#' @export
#'
#' @examples
#' co <- get_rhombicuboctahedron()
#' stopifnot(nrow(co$vertices) == 24)
#'
#' @include sto_helpers.R
get_rhombicuboctahedron <- function() {

  data <- readLines("http://dmccooey.com/polyhedra/Rhombicuboctahedron.txt")

  code <- data[3] %>% str_replace("=.+=", '=') %>% str_c(';')
  tmp <- c()
  for(i in 5:28) tmp <- c(tmp, data[i] %>% str_replace("V.+= ", "c"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 31:48) tmp <- c(tmp, data[i] %>% str_replace("\\{", "c(") %>%
                             str_replace("\\}", ") + 1"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("cu_faces <- rbind(", ., ")"))
  tmp <- c()
  for(i in 49:56) tmp <- c(tmp, data[i] %>% str_replace("\\{", "c(") %>%
                             str_replace("\\}", ") + 1"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("oct_faces <- rbind(", ., ")"))
  eval(parse(text=code))

  texts <- c(str_c('p', 1:8), str_c('q', 1:8), str_c('r', 1:8))

  segs <- list()
  for(i in 1:nrow(cu_faces)) {
    face <- cu_faces[i,]
    for(j in 1:4) {
      b <- j
      e <- ifelse(j < 4, j+1, 1)
      seg <- c(face[b], face[e])
      segs[[1+length(segs)]] <- seg
      if(j == 4) break
    }
  }
  for(i in 1:nrow(oct_faces)) {
    face <- oct_faces[i,]
    for(j in 1:3) {
      b <- j
      e <- ifelse(j < 3, j+1, 1)
      seg <- c(face[b], face[e])
      segs[[1+length(segs)]] <- seg
      if(j == 3) break
    }
  }

  segx <- Reduce(rbind, segs)
  segix <- get_set(segx) %>% as.list() %>% unlist()
  segments <- verts[segix,]

  rhombicuboctahedron <- list(
    info = c(26,48,24) %>% `names<-`(c('facces', 'edges', 'vertices')),
    verts = verts,
    texts = texts,
    segments = segments,
    cu_faces = cu_faces,
    oct_faces = oct_faces
  )
  rhombicuboctahedron
}

#'  Rhombicuboctahedron
#' @details A rhombicuboctahedron data structure.
#' \describe{
#'  \item{verts}{The vertex coordinates}
#'  \item{texts}{The vertex labels}
#'  \item{segments}{The edges of the rhombicuboctahedron}
#'  \item{cu_faces}{The quadrilateral faces}
#'  \item{oct_faces}{The triangular faces}
#' }
#'
rhombicuboctahedron <- get_rhombicuboctahedron()
save(rhombicuboctahedron, file = "data/rhombicuboctahedron.rda")


