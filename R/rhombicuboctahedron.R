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
#' Translate the rhombicuboctahedron coordinates from the polyhedronisme OBJ
#' output into R code
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
  path <- system.file("extdata", "polyhedronisme-eC.obj", package = "spantreeorbits", mustWork = TRUE)
  data <- readLines(con = path)
  code <- c()
  tmp <- c()
  for(i in 4:27) tmp <- c(tmp, data[i] %>% str_replace("v ", "c(") %>%
                            str_replace_all(" ", ", ") %>% str_c(., ')'))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 56:81) tmp <- c(tmp, data[i] %>% str_extract_all("( \\d+)") %>% unlist() %>%
                             str_c(collapse = ", ") %>% str_c("c(", ., ")"))
  cnt <- str_count(tmp, ',') + 1
  code <- c(code, tmp[which(cnt == 4)] %>% str_c(collapse = ", ") %>%
              str_c("cu_faces <- rbind(", ., ")"))
  code <- c(code, tmp[which(cnt == 3)] %>% str_c(collapse = ", ") %>%
              str_c("oct_faces <- rbind(", ., ")"))

  eval(parse(text=code))

  verts <- verts %>% zapsmall()

  texts <- str_c('r', 1:24)

  segs <- list()
  nc <- 4
  for(i in 1:nrow(cu_faces)) {
    face <- cu_faces[i,]
    for(j in 1:nc) {
      b <- j
      e <- ifelse(j < nc, j+1, 1)
      seg <- c(face[b], face[e])
      segs[[1+length(segs)]] <- seg
      if(j == nc) break
    }
  }
  nc <- 3
  for(i in 1:nrow(oct_faces)) {
    face <- oct_faces[i,]
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
  edges <- segix %>% matrix(byrow = T, ncol = 2)

  rhombicuboctahedron <- list(
    info = c(26,48,24) %>% `names<-`(c('faces', 'edges', 'vertices')),
    verts = verts,
    cu_faces = cu_faces,
    oct_faces = oct_faces,
    texts = texts,
    segments = segments,
    edges = edges
  )
  rhombicuboctahedron
}

#'  Rhombicuboctahedron
#' @details A rhombicuboctahedron data structure.
#' \describe{
#'  \item{info}{The numbers of faces, edges, and vertices}
#'  \item{verts}{The vertex coordinates}
#'  \item{texts}{The vertex labels}
#'  \item{segments}{The edges of the rhombicuboctahedron}
#'  \item{edges}{An edgelist as indices of vertices}
#'  \item{cu_faces}{The quadrilateral faces}
#'  \item{oct_faces}{The triangular faces}
#' }
#'
rhombicuboctahedron <- get_rhombicuboctahedron()
save(rhombicuboctahedron, file = "data/rhombicuboctahedron.rda")


