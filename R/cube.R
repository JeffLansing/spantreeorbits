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
#' @importFrom stringr str_extract_all
NULL

#' Title Get a Cube
#' Translate the cube coordinates from the polyhedronisme OBJ output into
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
  path <- system.file("extdata", "polyhedronisme-C.obj", package = "spantreeorbits", mustWork = TRUE)
  data <- readLines(con = path)
  code <- c()
  tmp <- c()
  for(i in 4:11) tmp <- c(tmp, data[i] %>% str_replace("v ", "c(") %>%
                            str_replace_all(" ", ", ") %>% str_c(., ')'))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 20:24) tmp <- c(tmp, data[i] %>% str_extract_all("( \\d+)") %>% unlist() %>%
                             str_c(collapse = ", ") %>% str_c("c(", ., ")"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("faces <- rbind(", ., ")"))
  eval(parse(text=code))

  vorder <- c(1,5,7,3,6,2,4,8)

  verts <- verts %>% zapsmall() %>% `[`(vorder, TRUE)
  faces <- faces %>% apply(1, function(rw) order(vorder)[rw]) %>% t()

  texts <- str_c('v', 1:8)

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

  cube <- list(
    info = c(6,12,8) %>% `names<-`(c('facces', 'edges', 'vertices')),
    verts = verts,
    faces = faces,
    texts = texts,
    segments = segments
  )
  cube
}

#'  Cube
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




