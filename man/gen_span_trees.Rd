% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sto_helpers.R
\name{gen_span_trees}
\alias{gen_span_trees}
\title{gen_span_trees
Generate the spanning trees of a polygon}
\usage{
gen_span_trees(elist, vertices, cmb)
}
\arguments{
\item{elist}{The edgelist from the polygon}

\item{vertices}{The vertices of the tree (symbolic names)}

\item{cmb}{A superset of the spanning trees, for example:
the combinations of nrow(edgelist) things taken length(vertices) -1
at a time}
}
\value{
A matrix of spanning trees, one per row
}
\description{
gen_span_trees
Generate the spanning trees of a polygon
}
\examples{
cmb <- combn(6,3)
tcmb <- t(cmb)
scmb <- t(apply(cmb, 2, function(cl) setdiff(1:6, cl)))
tinfo <- gen_span_trees(rhombic_dodecahedron$nghmap[,1:2], letters[1:4], tcmb)
sinfo <- gen_span_trees(rhombic_dodecahedron$nghmap[,3:4], letters[1:4], scmb)
stopifnot(identical(tinfo$is, sinfo$is))

}
