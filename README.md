Deprecation notice
##################

NB: this library is deprecated by https://github.com/igarnier/prbnmcn-clustering

Functorial k-clustering implementation in OCaml.

Provided algorithms:
* k-means (voronoi iteration + various initialisation methods)
* k-medoids (voronoi iteration or PAM + various initialisation methods)
* agglomerative hierarchical clustering

Some example code is available here:

https://github.com/igarnier/clustering/wiki/An-example-with-k-means

For some simple, typical distance measures used in maths, look at the gromov library.

Some examples are provided in lib_test/gaussian.ml (relies on owl, vplot and gromov).
The library also provides sequential (and soon parallel) multi-start wrappers,
as well as ways to assess the tradeoff between the quality of clustering
and overfitting using provided cost functions.

BUILDING & INSTALLING

Building and installing the package relies on having the opam package manager
installed.
1. Do `opam install jbuilder batteries parmap`.
2. Type `jbuilder build` at the root to build the package.
3. Type `jbuilder install` to install the package in your .opam directory.
4. (optional)
   To build the example in lib_test/gaussian.ml, you will need to `opam install owl`. Then,
   `jbuilder runtest` should build and run the example. The results should be available
   in _build/default/lib_test/dataset.png (to see the clusters as they have been generated)
   and _build/default/lib_test/result.png (to see the clusters as inferred by k-means)

TODOS:
1. Parmap doesn't work super well: all child processes run on the same CPU. Try with
   Netmcore.
2. Compare quality of clustering as well as performance with scikit-learn's implementation
