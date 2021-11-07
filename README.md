# CanonicalExpression
A Canonical Expressions is a kind of binary expression used especially in the implicative calculus of propositions.

The theory of canonical expressions is describe in a paper : Pierres Lescanne "Zaionc paradox revisited" https://arxiv.org/abs/2104.06723

This repository contains three Haskell programs.
1. Constants.hs contains precomputed values for Stam algorithm for randomly generating restricted growth strings, plus other plus other constants used in the code.
2. CanonicalExpression.hs contains several functions for binary trees, restricted growth strings and canonical expressions, plus functions used in the Monte-Carlo approach.
3. MainCanonicalExpression.hs contains utilities for building programs running in background

This repository contains three files of data computed with those programs
1. RatioIntuitionisticVsTautology.txt (ratio of cheap intuitinistic theorems vs cclassical theorems)
2. RatioSimple.txt (ratio of simple intuitionistic theorems over all the canonical expressions)
3. RatioNotNonCLassSimpl.txt (ratio of simple intuitionistc theorms over non simple antilogies, the evaluation of Genitrini et al.)
