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
#' @importFrom stringr str_count
NULL

#' Title Get a Cuboctrahedron
#' Translate the cuboctahedron coordinates from the polyhedronisme OBJ output into
#' R code
#'
#' @return a list describing a cuboctahedron
#' @export
#'
#' @examples
#' co <- get_cuboctahedron()
#' stopifnot(nrow(co$vertices) == 12)
#'
#' @include sto_helpers.R
get_cuboctahedron <- function() {
  path <- system.file("extdata", "polyhedronisme-aC.obj", package = "spantreeorbits", mustWork = TRUE)
  data <- readLines(con = path)
  code <- c()
  tmp <- c()
  for(i in 4:15) tmp <- c(tmp, data[i] %>% str_replace("v ", "c(") %>%
                            str_replace_all(" ", ", ") %>% str_c(., ')'))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 32:45) tmp <- c(tmp, data[i] %>% str_extract_all("( \\d+)") %>% unlist() %>%
                             str_c(collapse = ", ") %>% str_c("c(", ., ")"))
  cnt <- str_count(tmp, ',') + 1
  code <- c(code, tmp[which(cnt == 4)] %>% str_c(collapse = ", ") %>%
              str_c("cu_faces <- rbind(", ., ")"))
  code <- c(code, tmp[which(cnt == 3)] %>% str_c(collapse = ", ") %>%
              str_c("oct_faces <- rbind(", ., ")"))

  eval(parse(text=code))

  verts <- verts %>% zapsmall()

  texts <- str_c('o', 1:12)

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

  edges <- cbind(rep(1:12, each = 2),
                 c(2,9,4,11,1,7,3,5,2,6,7,12,4,8,6,9,3,10,8,11,1,12,5,10))

  cuboctahedron <- list(
    info = c(14,24,12) %>% `names<-`(c('faces', 'edges', 'vertices')),
    verts = verts,
    cu_faces = cu_faces,
    oct_faces = oct_faces,
    texts = texts,
    segments = segments,
    edges = edges
  )
  cuboctahedron
}

#'  Cuboctahedron
#' @details A cuboctahedron data structure.
#' \describe{
#'  \item{verts}{The vertex coordinates}
#'  \item{texts}{The vertex labels}
#'  \item{segments}{The edges of the cuboctahedron}
#'  \item{cu_faces}{The quadrilateral faces}
#'  \item{oct_faces}{The triangular faces}
#'  \item{edges}{An edgelist as indices of vertices}
#' }
#'
cuboctahedron <- get_cuboctahedron()
save(cuboctahedron, file = "data/cuboctahedron.rda")


