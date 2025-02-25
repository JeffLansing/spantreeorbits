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

#' Title Get a Deltoidal Tetracontaoctahedron
#' Translate the deltoidal_tetracontaoctahedron coordinates from the
#' polyhedronisme OBJ output into R code
#'
#' @return a list describing a deltoidal_tetracontaoctahedron
#' @export
#'
#' @examples
#' dtc <- get_deltoidal_tetracontaoctahedron()
#' stopifnot(nrow(dtc$vertices) == 50)
#'
#' @include sto_helpers.R
get_deltoidal_tetracontaoctahedron <- function() {
  path <- system.file("extdata", "polyhedronisme-deaC.obj", package = "spantreeorbits", mustWork = TRUE)
  data <- readLines(con = path)
  vorder <- c(6,9,15,12,23,35,27,42,31,46,19,39,3,4,8,11,5,14,2,17,28,13,25,24,37,32,
              16,29,43,20,7,33,40,10,21,36,1,34,44,47,49,50,26,38,45,18,41,30,22,48)
  code <- c()
  tmp <- c()
  for(i in 4:53) tmp <- c(tmp, data[i] %>% str_replace("v ", "c(") %>%
                            str_replace_all(" ", ", ") %>% str_c(., ')'))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
  tmp <- c()
  for(i in 104:151) tmp <- c(tmp, data[i] %>% str_extract_all("( \\d+)") %>% unlist() %>%
                               str_c(collapse = ", ") %>% str_c("c(", ., ")"))
  code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("faces <- rbind(", ., ")"))
  eval(parse(text=code))

  verts <- verts %>% zapsmall() %>% `[`(vorder, TRUE)
  faces <- faces %>% apply(1, function(rw) order(vorder)[rw]) %>% t()

  texts <- c(str_c('o', 1:12), str_c('r', 1:24), str_c('f', 1:6), str_c('v', 1:8))

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
  segments <- verts[segix,] #192 of these

  edges <- texts[segix] %>% matrix(byrow = T, ncol = 2)
  g <- edges %>% igraph::graph_from_edgelist(directed = F)

  radnames <- lapply(1:14, function(k) {
    ch <- ifelse(k < 7, 'f', 'v')
    j <- ifelse(k < 7, k, k-6)
    str_c(ch, j)
  }) %>% unlist() %>% c(str_c('o', 1:12))
  radmap <- lapply(1:26, function(k) {
    if(k < 15) str_c('p', k) else str_c('o', k - 14)
  }) %>% `names<-`(radnames)

  nghmap <- lapply(13:36, function(k) {
    txt <- texts[k]
    ngh <- igraph::neighbors(g, txt)
    vals <- radmap[names(ngh)] %>% unlist() %>% sort() %>% str_remove_all("(o|p)") %>% as.numeric()
    c(sort(vals[1:2]), sort(vals[3:4]))
  }) %>% abind::abind(along = 2) %>% t() %>% `[`(TRUE,c(3,4,1,2))

  # In Conway's operators, this is aaaC or aaaO (See Polyhedralisme . Also note e = aa)
  deltoidal_tetracontaoctahedron <- list(
    info = c(48,96,50) %>% `names<-`(c('facces', 'edges', 'vertices')),
    verts = verts,
    nghmap = nghmap,
    faces = faces,
    texts = texts,
    segments = segments
  )
  deltoidal_tetracontaoctahedron
}

#'  Deltoidal Tetracontaoctahedron
#' @details A deltoidal_tetracontaoctahedron data structure.
#' \describe{
#'  \item{verts}{The vertex coordinates}
#'  \item{nghmap}{A map from the edges of a contained rhombic_dodecahedron to the edges of a contained cuboctahedron}
#'  \item{texts}{The vertex labels}
#'  \item{segments}{The edges of the deltoidal_tetracontaoctahedron}
#'  \item{faces}{The faces}
#' }
deltoidal_tetracontaoctahedron <- get_deltoidal_tetracontaoctahedron()
save(deltoidal_tetracontaoctahedron, file = "data/deltoidal_tetracontaoctahedron.rda")
