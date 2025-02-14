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

#' Title Get a Cube
#' Translate the cube coordinates from the dmcooey library into
#' R code
#'
#' @return a list describing a cube
#' @export
#'
#' @examples
#' cu <- get_cube()
#' stopifnot(nrow(cu$vertices) == 8)
#'
#' @include sto_helpers.R
get_cube <- function() {
  data <- readLines("http://dmccooey.com/polyhedra/Cube.txt")
  code <- c()
  tmp <- c()
  for(i in 3:10) tmp <- c(tmp, data[i] %>% str_replace("V.+= ", "c"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 13:18) tmp <- c(tmp, data[i] %>% str_replace("\\{", "c(") %>%
                             str_replace("\\}", ") + 1"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("faces <- rbind(", ., ")"))

  eval(parse(text=code))

  texts <- str_c('v', 1:8)

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

  cube <- list(
    verts = verts,
    faces = faces,
    texts = texts,
    segments = segments
  )
  cube
}

#'  Title Cube
#' @details A cube data structure.
#' \describe{
#'  \item{verts}{The vertex coordinates}
#'  \item{texts}{The vertex labels}
#'  \item{segments}{The edges of the cube}
#'  \item{faces}{The faces}
#' }
#'
cube <- get_cube()
save(cube, file = "data/cube.rda")




