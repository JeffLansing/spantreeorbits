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

#' Title Get an Octahedron
#' Translate the octahedron coordinates from the polyhedronisme OBJ output into
#' R code
#'
#' @return a list describing an octahedron
#' @export
#'
#' @examples
#' oct <- get_octahedron()
#' stopifnot(nrow(oct$vertices) == 6)
#'
#' @include sto_helpers.R
get_octahedron <- function() {
  path <- system.file("extdata", "polyhedronisme-O.obj", package = "spantreeorbits", mustWork = TRUE)
  data <- readLines(con = path)
  code <- c()
  tmp <- c()
  for(i in 4:9) tmp <- c(tmp, data[i] %>% str_replace("v ", "c(") %>%
                           str_replace_all(" ", ", ") %>% str_c(., ')'))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 20:27) tmp <- c(tmp, data[i] %>% str_extract_all("( \\d+)") %>% unlist() %>%
                             str_c(collapse = ", ") %>% str_c("c(", ., ")"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("faces <- rbind(", ., ")"))
  eval(parse(text=code))

  verts <- verts %>% zapsmall()

  texts <- str_c('f', 1:6)

  segs <- list()
  nc <- 3
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

  edges <- cbind(rep(1:6, each = 2), c(2,4,3,5,1,6,3,5,1,6,2,4))

  octahedron <- list(
    info = c(8,12,6) %>% `names<-`(c('faces', 'edges', 'vertices')),
    verts = verts,
    faces = faces,
    texts = texts,
    segments = segments,
    edges = edges
  )
  octahedron
}

#'  Octahedron
#' @details An octahedron data structure.
#' \describe{
#'  \item{verts}{The vertex coordinates}
#'  \item{texts}{The vertex labels}
#'  \item{segments}{The edges of the octahedron}
#'  \item{faces}{The faces}
#'  \item{edges}{An edgelist as indices of vertices}
#' }
#'
octahedron <- get_octahedron()
save(octahedron, file = "data/octahedron.rda")
