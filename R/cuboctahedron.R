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
#' co <- get_cuboctahedron()
#' stopifnot(nrow(co$vertices) == 12)
#'
get_cuboctahedron <- function() {

  get_set <- function(info) {
    sel <- apply(info,1,function(r) {sets::set(sets::as.set(r))})
    Reduce(sets::set_union, sel)
  }

  data <- readLines("http://dmccooey.com/polyhedra/Cuboctahedron.txt")
  code <- c()
  for(i in 3:3) code <- c(code, data[i] %>% str_replace("=.+=", '=') %>%
                            str_c(';'))
  tmp <- c()
  for(i in 5:16) tmp <- c(tmp, data[i] %>% str_replace("V.+= ", "c"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 19:24) tmp <- c(tmp, data[i] %>% str_replace("\\{", "c(") %>%
                             str_replace("\\}", ") + 1"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("cu_faces <- rbind(", ., ")"))
  tmp <- c()
  for(i in 25:32) tmp <- c(tmp, data[i] %>% str_replace("\\{", "c(") %>%
                             str_replace("\\}", ") + 1"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("oct_faces <- rbind(", ., ")"))
  eval(parse(text=code))

  texts <- c(str_c('o', 1:4),c('t1','s1','t2','s2','s3','s4','t3','t4'))

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

  cuboctahedron <- list(
    verts = verts,
    texts = texts,
    segments = segments,
    cu_faces = cu_faces,
    oct_faces = oct_faces
  )
  cuboctahedron
}

##'  Title Cuboctahedron
##' @details A cuboctahedron data structure.
##' \itemize{
##'  \item{"verts"}{The vertex coordinates}
##'  \item{"texts"}{The vertex labels}
##'  \item{"segments"}{The edges of the cuboctahedron}
##'  \item{"cu_faces"}{The quadrilateral faces}
##'  \item{"oct_faces"}{The triangular faces}
##' }
##'
cuboctahedron <- get_cuboctahedron()
save(cuboctahedron, file = "data/cuboctahedron.rda")


