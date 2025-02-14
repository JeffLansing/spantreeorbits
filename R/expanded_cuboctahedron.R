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

#' Title Get an Expanded Cuboctahedron
#' Translate the expanded_cuboctahedron coordinates from the dmcooey library into
#' R code
#'
#' @return a list describing an expanded_cuboctahedron
#' @export
#'
#' @examples
#' xco <- get_expanded_cuboctahedron()
#' stopifnot(nrow(xco$vertices) == 48)
#'
#' @include sto_helpers.R
get_expanded_cuboctahedron <- function() {
  data <- readLines("http://dmccooey.com/polyhedra/ExpandedCuboctahedron.txt")
  code <- c()
  for(i in 3:6) code <- c(code, data[i] %>% str_c(';'))
  tmp <- c()
  for(i in 21:68) tmp <- c(tmp, data[i] %>% str_replace("V.+= ", "c"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 71:112) tmp <- c(tmp, data[i] %>% str_replace("\\{", "c(") %>%
                              str_replace("\\}", ") + 1"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("cu_faces <- rbind(", ., ")"))
  tmp <- c()
  for(i in 113:120) tmp <- c(tmp, data[i] %>% str_replace("\\{", "c(") %>%
                               str_replace("\\}", ") + 1"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("oct_faces <- rbind(", ., ")"))
  eval(parse(text=code))

  texts <- str_c('x', 1:48)

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
  rownames(segments) <- segix

  expanded_cuboctahedron <- list(
    verts = verts,
    texts = texts,
    segments = segments,
    cu_faces = cu_faces,
    oct_faces = oct_faces
  )
  expanded_cuboctahedron
}

#'  Expanded Cuboctahedron
#' @details An expanded_cuboctahedron data structure.
#' \describe{
#'  \item{verts}{The vertex coordinates}
#'  \item{texts}{The vertex labels}
#'  \item{segments}{The edges of the expanded_cuboctahedron}
#'  \item{faces}{The faces}
#' }
"expanded_cuboctahedron"

expanded_cuboctahedron <- get_expanded_cuboctahedron()

save(expanded_cuboctahedron, file = "data/expanded_cuboctahedron.rda")

