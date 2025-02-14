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

#' Title Get a Deltoidal Icositetrahedron
#' Translate the deltoidal_icositetrahedron coordinates from the dmcooey library into
#' R code
#'
#' @return a list describing a deltoidal_icositetrahedron
#' @export
#'
#' @examples
#' dit <- get_deltoidal_icositetrahedron()
#' stopifnot(nrow(diy$vertices) == 6)
#'
#' @include sto_helpers.R
get_deltoidal_icositetrahedron <- function() {
  data <- readLines("http://dmccooey.com/polyhedra/DeltoidalIcositetrahedron.txt")
  code <- c()
  for(i in 3:4) code <- c(code, data[i] %>% str_replace("=.+=", '=') %>%
                            str_c(';'))
  tmp <- c()
  for(i in 6:31) tmp <- c(tmp, data[i] %>% str_replace("V.+= ", "c"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 34:57) tmp <- c(tmp, data[i] %>% str_replace("\\{", "c(") %>%
                             str_replace("\\}", ") + 1"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("faces <- rbind(", ., ")"))
  eval(parse(text=code))

  texts <- c(str_c('f', 1:6), str_c('o', 1:12), str_c('v', 1:8))

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

  edges <- texts[segix] %>% matrix(byrow = T, ncol = 2)
  g <- edges %>% igraph::graph_from_edgelist(directed = F)

  nghmap <- lapply(7:18, function(k) {
    txt <- texts[k]
    ngh <- igraph::neighbors(g, txt)
    names(ngh) %>% sort() %>% str_remove_all("(f|v)") %>% as.numeric()
  }) %>% abind::abind(along = 2) %>% t() %>% `[`(TRUE,c(3,4,1,2))

  deltoidal_icositetrahedron <- list(
    info = c(24,48,26) %>% `names<-`(c('facces', 'edges', 'vertices')),
    verts = verts,
    nghmap = nghmap,
    texts = texts,
    segments = segments,
    faces = faces
  )
  deltoidal_icositetrahedron
}

#'  Deltoidal Icositetrahedron
#' @details A deltoidal_icositetrahedron data structure.
#' \describe{
#'  \item{verts}{The vertex coordinates}
#'  \item{nghmap}{A map from the edges of a contained rhombic_dodecahedron to
#' the edges of its dual cuboctahedron}
#'  \item{texts}{The vertex labels}
#'  \item{egments}{The edges of the deltoidal_icositetrahedron}
#'  \item{"faces}{The faces}
#' }
#'
deltoidal_icositetrahedron <- get_deltoidal_icositetrahedron()
save(deltoidal_icositetrahedron, file = "data/deltoidal_icositetrahedron.rda")
