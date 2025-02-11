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

#' Title Get a Tetrahedron
#' Translate the tetrahedron coordinates from the dmcooey library into
#' R code
#'
#' @return a list describing a tetrahedron
#' @export
#'
#' @examples
#' tet <- get_tetrahedron()
#' stopifnot(nrow(tet$vertices) == 4)
#'
get_tetrahedron <- function() {

  data <- readLines("http://dmccooey.com/polyhedra/Tetrahedron.txt")
  code <- c()
  for(i in 3:3) code <- c(code, data[i] %>% str_replace("=.+=", '=') %>%
                            str_c(';'))
  tmp <- c()
  for(i in 5:8) tmp <- c(tmp, data[i] %>% str_replace("V.+= ", "c"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 11:14) tmp <- c(tmp, data[i] %>% str_replace("\\{", "c(") %>%
                             str_replace("\\}", ") + 1"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("faces <- rbind(", ., ")"))

  eval(parse(text=code))

  texts <- str_c('t', 1:4)

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

  tetrahedron <- list(
    verts = verts,
    faces = faces,
    texts = texts,
    segments = segments
  )
  tetrahedron
}

#' get_dual
#' Convert a tetrahedron into its dual
#' @param tetrahedron The polyhedron produced by the get_tetrahedron function
#'
#' @return The dual polyhedron
get_dual <- function(tetrahedron) {
  X <- diag(3) %>% `[<-`(3,3,-1) # a rotation matrix
  dual <- tetrahedron
  dual$verts <- tetrahedron$verts %*% X
  dual$segments <- tetrahedron$segments %*% X
  dual$texts <- tetrahedron$texts %>% str_replace_all('t', 's')
  dual
}

##'  Title Tetrahedron
##' @details A tetrahedron data structure.
##' \itemize{
##'  \item{"verts"}{The vertex coordinates}
##'  \item{"texts"}{The vertex labels}
##'  \item{"segments"}{The edges of the tetrahedron}
##'  \item{"faces"}{The faces}
##' }
##'
tetrahedron <- get_tetrahedron()
save(tetrahedron, file = "data/tetrahedron.rda")

dual <- get_dual(tetrahedron)
save(dual, file = "data/dual_tetrahedron.rda")


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


