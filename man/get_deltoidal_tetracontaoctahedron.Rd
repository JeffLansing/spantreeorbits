% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deltoidal_tetracontaoctahedron.R
\name{get_deltoidal_tetracontaoctahedron}
\alias{get_deltoidal_tetracontaoctahedron}
\title{Title Get a Deltoidal Tetracontaoctahedron
Translate the expanded_cuboctahedron into its dual polyhedron}
\usage{
get_deltoidal_tetracontaoctahedron()
}
\value{
a list describing a deltoidal_tetracontaoctahedron
}
\description{
Title Get a Deltoidal Tetracontaoctahedron
Translate the expanded_cuboctahedron into its dual polyhedron
}
\examples{
dtc <- get_deltoidal_tetracontaoctahedron()
stopifnot(nrow(dtc$vertices) == 50)

}
