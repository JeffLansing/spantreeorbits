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
#' Translate the octahedron coordinates from the dmcooey library into
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
  data <- readLines("http://dmccooey.com/polyhedra/Octahedron.txt")
  code <- c()
  for(i in 3:3) code <- c(code, data[i] %>% str_replace("=.+=", '=') %>%
                            str_c(';'))
  tmp <- c()
  for(i in 5:10) tmp <- c(tmp, data[i] %>% str_replace("V.+= ", "c"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 13:20) tmp <- c(tmp, data[i] %>% str_replace("\\{", "c(") %>%
                             str_replace("\\}", ") + 1"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("faces <- rbind(", ., ")"))

  eval(parse(text=code))

  texts <- str_c('f', 1:6)

  segs <- list()
  for(i in 1:nrow(faces)) {
    face <- faces[i,]
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


  octahedron <- list(
    verts = verts,
    faces = faces,
    texts = texts,
    segments = segments
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
#' }
#'
octahedron <- get_octahedron()
save(octahedron, file = "data/octahedron.rda")
