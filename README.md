# Flips
## **F**# **LI**near **P**rogramming **S**ystem

### Introduction

Flips is an F# library for modeling Linear Programming and Mixed-Integer Programming problems. It is inspired by the work of the PuLP library for Python and the excellent Gurobi Python library. F# is a great language to work with but many of the existing APIs for modeling Optimization problems were heavily influenced by Object-Oriented concepts. While there is nothing wrong with OO, this is an attempt to take a functional-first approach to the problem.

This library tries to making the modeling of Optimization Models clean and simple. The idea was to make it as straightforward as possible for an Operation Researcher or an Optimization domain expert to express their ideas in F#. These practictioners are used to working with Mathematical constructs like Sets, Sigma-notation, and summations. Reducing the mental distance between the mathematical formulation of problems and the F# representation was a primary goal.

### Simple Example Problem

Creating and solving an Optimization model is composed of the following steps:

1. Defining your parameters
2. Creating your Decision Variables
3. Formulating your Objective Function
4. Adding Constraints
5. Solving the Model

Let us work an example problem to see how this works. Let us say that we are managing a Food Truck and we need to figure out what ingredients we need to pack for the day. Let us say that we sell Hamburgers and Hotdogs. Each Hamburger we sell provides us $1.50 in profit. Each Hotdog we sell provides $1.20 in profit. We only have enough Hamburger buns for up to 300 Hamburgers and only 200 buns for Hot Dogs. The ingredients for a single Hamburger weight 0.5 kg and the ingredients for a Hot Dot weigh 0.4 kg. Our Food Truck can only up to 800 kg.

We now have what we need for formulate an Optimization Model. Let's look at how this works in code.

```fsharp


```