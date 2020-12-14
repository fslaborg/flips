![Flips Banner](GitHubBanner.png)

# Flips : **F**# **LI**near **P**rogramming **S**ystem

> Full Documenation can be found [here](https://flipslibrary.com/).

Flips is an F# library for modeling and solving Linear Programming (LP) and Mixed-Integer Programming (MIP) problems. 
It is inspired by the work of the PuLP library for Python and the excellent Gurobi Python library. It builds on the 
work of the outstanding [Google OR-Tools library] and the [OPTANO library].

F# is a great language to work with but many of the existing APIs for modeling Optimization problems are heavily 
influenced by Object-Oriented concepts. While there is nothing wrong with OO, this is an attempt to take a 
functional-first approach to the problem.

This library tries to make the modeling of Optimization Models (LP/MIP) clean and simple. The idea was to make it 
straightforward for an Operation Researcher or Optimization domain expert to express their ideas in F#. These 
practitioners are used to working with Mathematical constructs like Sets, Sigma-notation, and summations. 
Reducing the mental distance between the mathematical formulation of problems and the F# representation was a key 
design goal.

F# developers should also find it comfortable to use this library. Over time I will be adding tutorials and training 
material on how to model Optimization Problems using this library. With a little training any F# developer will be 
able to add the powerful tool of Optimization to their repertoire.

## Installation

To use Flips, simply add the [nuget package] to whatever project you are working on. The library comes with the [CBC solver] 
for Mixed-Integer Programming and the [Google GLOPS solver] for Linear Programming which are both free and open source.

Flips also supports the [Gurobi] and [IBM CPLEX] commercial solvers through the use of the excellent [OPTANO library]. You 
will need to get a separate license to use these libraries. The installation of these commercial libraries is not covered 
in this documentation since installation can depend on deployment and use case. Please refer to these vendors for commercial 
support in using their product. Flips currently only supports the latest version of each of these libraries.

[here]: http://matthewcrews.com/flips/#/
[Google OR-Tools library]: https://github.com/google/or-tools
[OPTANO library]: https://optano.com/en/modeling/
[nuget package]: https://www.nuget.org/packages/Flips/
[CBC solver]: https://github.com/coin-or/Cbc
[Google GLOPS solver]: https://github.com/google/or-tools
[Gurobi]: https://www.gurobi.com/
[IBM CPLEX]: https://www.ibm.com/products/ilog-cplex-optimization-studio/details
