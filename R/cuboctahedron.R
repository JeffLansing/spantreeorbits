library(magrittr)
library(stringr)

#' Get Set
#'Converts an edgelist of a graph into a set of two-element sets (i.e., a set
#'of unordered pairs).
#' @param info An edgelist of a graph
#'
#' @return a set of two-element sets
#'
#' @examples
#' mx <- matrix(c(1,2,3,4), ncol = 2)
#' get_set(mx)
#'
get_set <- function(info) {
  sel <- apply(info,1,function(r) {sets::set(sets::as.set(r))})
  Reduce(sets::set_union, sel)
}

data <- readLines("http://dmccooey.com/polyhedra/Cuboctahedron.txt")
code <- c()
for(i in 3:3) code <- c(code, data[i] %>% str_replace("=.+=", '=') %>%
                          str_c(';'))
tmp <- c()
for(i in 5:16) tmp <- c(tmp, data[i] %>% str_replace("V.+= ", "c"))
code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("verts <- rbind(", ., ");"))
tmp <- c()
for(i in 19:24) tmp <- c(tmp, data[i] %>% str_replace("\\{", "c(") %>%
                           str_replace("\\}", ") + 1"))
code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("cu_faces <- rbind(", ., ")"))
tmp <- c()
for(i in 25:32) tmp <- c(tmp, data[i] %>% str_replace("\\{", "c(") %>%
                           str_replace("\\}", ") + 1"))
code <- c(code, tmp %>% str_c(collapse = ", ") %>% str_c("oct_faces <- rbind(", ., ")"))
eval(parse(text=code))

texts <- c(str_c('o', 1:4),c('t1','s1','t2','s2','s3','s4','t3','t4'))

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

edges <- texts[segix] %>% matrix(byrow = T, ncol = 2)
g <- edges %>% igraph::graph_from_edgelist(directed = F)

nghmap <- lapply(1:4, function(k) {
  txt <- texts[k]
  ngh <- igraph::neighbors(g, txt)
  vals <- names(ngh) %>% str_remove_all("(s|t)") %>% as.numeric()
  c(sort(vals[1:2]), sort(vals[3:4]))
}) %>% abind::abind(along = 2) %>% t() %>% `[`(TRUE,c(1,3,2,4))

cuboctahedron <- list(
  verts = verts,
  nghmap = nghmap,
  texts = texts,
  segments = segments,
  cu_faces = cu_faces,
  oct_faces = oct_faces
)

save(cuboctahedron, file = "data/cuboctahedron.rda")


