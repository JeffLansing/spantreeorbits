
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spantreeorbits

<!-- badges: start -->
<!-- badges: end -->

The goal of spantreeorbits is to use algebra to generate the orbits of
the spanning trees of some polyhedra.

## Installation

You can install the development version of spantreeorbits like so:

``` r
install.packages("devtools")
devtools::install_github("JeffLansing/spantreeorbits")
```

## Example

Consider the tetrahedron, a self-dual polyhedron with 4 vertices, 4
faces, and 6 edges. (See:
<https://dmccooey.com/polyhedra/Tetrahedron.html>)

We will use some external tools to assemble some data concerning this
polyhedron. These are [Sage](https://www.sagemath.org/) and
[GAP](https://www.gap-system.org/), which will be accessed using [Sage
Cells](https://sagecell.sagemath.org/). To run the code shown in any
Sage Cell, click the *Evaluate* button below the cell.

<script src="https://sagecell.sagemath.org/static/embedded_sagecell.js" data-external="1"></script>
<script>sagecell.makeSagecell({"inputLocation": ".sage"});</script>
<link rel="stylesheet" type="text/css" href="https://sagecell.sagemath.org/static/sagecell_embed.css"  data-external="1">
<div class="sage">
  <script type="text/x-sage">
tet = graphs.TetrahedralGraph()
tet.relabel({0:1,1:2,2:3,3:4})
st = tet.spanning_trees()
for i,t in enumerate(st):
    print(t.edges(labels=False))
  </script>
</div>
<script src="https://sagecell.sagemath.org/static/embedded_sagecell.js" data-external="1"></script>
<script>sagecell.makeSagecell({"inputLocation": ".sage"});</script>
<link rel="stylesheet" type="text/css" href="https://sagecell.sagemath.org/static/sagecell_embed.css"  data-external="1">
<div class="sage">
  <script type="text/x-sage">
t4 = graphs.trees(4)
for i,t in enumerate(t4):
    t.relabel({0:1,1:2,2:3,3:4})
    print(t.edges(labels=False))
print(*reps, sep = "', '")
  </script>
</div>

So there are 16 different spanning trees for the tetrahedron, and there
will be between 16 and 2 orbits. ([This
document](https://doi.org/10.1016/j.tcs.2024.114593), in Table 1,
suggests that there will be exactly 2 orbits.)

Each spanning tree of the tetrahedron has 3 edges, and the tetrahedron
is self dual, so we will look for some more data pertinent to these two
sets of 3 edges. We will use GAP for this.

<script src="https://sagecell.sagemath.org/static/embedded_sagecell.js" data-external="1"></script>
<div id="cell2">
<script type="text/x-sage">
s6 := SymmetricGroup(6);
ll6 := LowLayerSubgroups(s6,2);;Display(List(ll6, Order));
Print(StructureDescription(ll6[21]), ", ", StructureDescription(ll6[22]), "\n");
rc21 := RightCosets(s6, ll6[21]);;
lrc := List(rc21, x -> ListPerm(Representative(x),6));;
for p in lrc do Print(p,"\n");; od;;
</script>
</div>
<script>
sagecell.makeSagecell({
inputLocation: '#cell2',
languages: ['gap'],
//Focus the editor
callback: function() {$('#cell2').find(".CodeMirror").get(0).CodeMirror.focus();}
});
</script>

When we evaluate this GAP code, we see that the symmetric group S6 has 2
subgroups of order 36, called S3 X S3. Further, looking at the right
cosets in S6 of either of these subgroups, it appears that there could
be some interesting relation with the 2 sets of 3 edges mentioned above.
We use the R code in this package to explore that relation.
