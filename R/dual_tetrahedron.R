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

#' Title Get a Dual Tetrahedron
#' Translate the dual tetrahedron coordinates from the polyhedronisme OBJ output into
#' R code
#'
#' @return a list describing a dual_tetrahedron
#' @export
#'
#' @examples
#' dtet <- get_dual_tetrahedron()
#' stopifnot(nrow(dtet$vertices) == 4)
#'
#' @include sto_helpers.R
get_dual_tetrahedron <- function() {
  path <- system.file("extdata", "polyhedronisme-dT.obj", package = "spantreeorbits", mustWork = TRUE)
  data <- readLines(con = path)
  code <- c()
  tmp <- c()
  for(i in 4:7) tmp <- c(tmp, data[i] %>% str_replace("v ", "c(") %>%
                           str_replace_all(" ", ", ") %>% str_c(., ')'))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 14:17) tmp <- c(tmp, data[i] %>% str_extract_all("( \\d+)") %>% unlist() %>%
                             str_c(collapse = ", ") %>% str_c("c(", ., ")"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("faces <- rbind(", ., ")"))
  eval(parse(text=code))

  verts <- verts %>% zapsmall()

  texts <- str_c('s', 1:4)

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

  dual_tetrahedron <- list(
    info = c(4,6,4) %>% `names<-`(c('faces', 'edges', 'vertices')),
    verts = verts,
    faces = faces,
    texts = texts,
    segments = segments
  )
  dual_tetrahedron
}

#'  Dual Tetrahedron
#' @details A dual tetrahedron data structure.
#' \describe{
#'  \item{verts}{The vertex coordinates}
#'  \item{texts}{The vertex labels}
#'  \item{segments}{The edges of the dual tetrahedron}
#'  \item{faces}{The faces}
#' }
#'
dual_tetrahedron <- get_dual_tetrahedron()
save(dual_tetrahedron, file = "data/dual_tetrahedron.rda")




