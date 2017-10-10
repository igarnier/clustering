A functorial k-means clustering implementation in OCaml.
A standard example is provided in lib_test/gaussian.ml (relies on Owl).

BUILDING & INSTALLING

Building and installing the package relies on having the opam package manager
installed.
1. Do `opam install jbuilder batteries`.
2. Type `jbuilder build` at the root to build the package.
3. Type `jbuilder install` to install the package in your .opam directory.
4. (optional)
   To build the example in lib_test/gaussian.ml, you will need to `opam install owl`. Then,
   `jbuilder runtest` should build and run the example. The results should be available
   in _build/default/lib_test/dataset.png (to see the clusters as they have been generated)
   and _build/default/lib_test/result.png (to see the clusters as inferred by k-means)
