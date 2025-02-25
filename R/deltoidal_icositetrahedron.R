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
#' Translate the deltoidal_icositetrahedron coordinates from the polyhedronisme OBJ output into
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
  path <- system.file("extdata", "polyhedronisme-deC.obj", package = "spantreeorbits", mustWork = TRUE)
  data <- readLines(con = path)
  code <- c()
  tmp <- c()
  for(i in 4:29) tmp <- c(tmp, data[i] %>% str_replace("v ", "c(") %>%
                            str_replace_all(" ", ", ") %>% str_c(., ')'))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 56:79) tmp <- c(tmp, data[i] %>% str_extract_all("( \\d+)") %>% unlist() %>%
                             str_c(collapse = ", ") %>% str_c("c(", ., ")"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("faces <- rbind(", ., ")"))
  eval(parse(text=code))

  vorder <- c(1,14,20,23,25,26,2,3,5,4,9,15,11,19,13,22,7,17,10,12,6,8,16,18,21,24)

  verts <- verts %>% zapsmall() %>% `[`(vorder, TRUE)
  faces <- faces %>% apply(1, function(rw) order(vorder)[rw]) %>% t()

  texts <- c(str_c('f', 1:6), str_c('o', 1:12), str_c('v', 1:8))

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

  edges <- texts[segix] %>% matrix(byrow = T, ncol = 2)
  g <- edges %>% igraph::graph_from_edgelist(directed = F)

  nghmap <- lapply(7:18, function(k) {
    txt <- texts[k]
    ngh <- igraph::neighbors(g, txt)
    vals <- names(ngh) %>% sort() %>% str_remove_all("(f|v)") %>% as.numeric()
    c(sort(vals[1:2]), sort(vals[3:4]))
  }) %>% abind::abind(along = 2) %>% t() %>% `[`(TRUE,c(3,4,1,2))

  deltoidal_icositetrahedron <- list(
    info = c(24,48,26) %>% `names<-`(c('facces', 'edges', 'vertices')),
    verts = verts,
    nghmap = nghmap,
    faces = faces,
    texts = texts,
    segments = segments
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
