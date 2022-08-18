# Phylogenetic combinatorics in Haskell

I implemented some function to enumerate basic phylogenetic quantities I encounter in my work with `Haskell`. 
These include _coalescent histories_ and _ancestral configurations_.

The code is naive without crazy optimizations or tricks.

### Usage

To use this, you can run `ghci Main.hs` and experiment with various functions. Alternatively, you can import the modules ino your own projects.

To define a tree, you can use a standard string format, where the tree structure is defined with parentheses and nodes are labeled with alphanmeric characers. For example, you would run `matchingRootConfigurations $ parseString "(((a,b),(c,d)),e)"`. All unlabeled nodes will be labeled automatically as `x1`, `x2`, and so on.

### References 

1 - [Rosenberg, N. A. (2007). Counting coalescent histories.](https://rosenberglab.stanford.edu/papers/Rosenberg2007-JCB.pdf)

2 - [Alimpiev, E., & Rosenberg, N. A. (2021). A lattice structure for ancestral configurations arising from the relationship between gene trees and species trees.](https://arxiv.org/pdf/2111.10456)
