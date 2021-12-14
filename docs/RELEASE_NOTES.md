### 2.4.7 - Tuesday, December 14th, 2021
* Updated OPTANO.Modeling to 3.9.2.537

### 2.4.6 - Monday, Oct 18th, 2021
* Updated OPTANO.Modeling to 3.8.1.532

### 2.4.5 - Tuesday, May 4th, 2021
* Updated Google ORTools to 9

### 2.4.4 - Thursday, December 31st, 2020
* Bug fixes for LinearExpression
* Updated to Google.ORTools 8.1.8487
* Code cleaning

### 2.4.3 - Wednesday, December 23rd, 2020
* Fixed code comments

### 2.4.2 - Thursday, November 16th, 2020
* Added Settings.basic type for default values for the solver
* Added functions for adjusting the settings for the solver
* Added Solution.evaluateObjective function for getting the value of an objective after solving
* Deprecated the Solution.ObjectiveValue field

### 2.4.1 - Wednesday, November 18th, 2020
* Fixed writing of LP files to have correct variable names
* Added writing of MPS files
* Added SourceLink

### 2.4.0 - Monday, November 16th, 2020
* Implemented interfaces for SliceMaps to be used with Solution.getValues

### 2.3.7 - Friday, November 13th, 2020
* Fixed SMap addition

### 2.3.6 - Friday, November 13th, 2020
* Fixed XML Comments
* Fixed writing of LP file for OPTANO based solvers

### 2.3.5 - Friday, November 13th, 2020
* Target netstandard2.0 for FSharp 5.0 Notebooks compatability

### 2.3.4 - Monday, October 26th, 2020
* Updated Google OR Tools
* Bug fix for SMap4

### 2.3.2 - Thursday, October 1st, 2020
* Removed unnecessary printfn

### 2.3.1 - Thursday, October 1st, 2020
* Bug fix for Optano Solve

### 2.3.0 - Wednesday, September 30th, 2020
* Added Multi-Objective function models 
* Bug fix for DecisionBuilder<'Measure>

### 2.2.0 - Friday, September 25th, 2020
* Massive rewrite of the SliceMaps

### 2.1.0 - Wednesday, August 12th, 2020
* Speed improvement for formulating models

### 2.0.3 - Friday, August 7th, 2020
* Fixed the adding of an empty Constraint list

### 2.0.2 - Thursday, August 6th, 2020
* Made Decision type summable within SliceMaps

### 2.0.1 - Wednesday, July 15th, 2020
* Fixed bug in Decision Builder index

### 2.0.0 - Monday, July 13th, 2020
* Include the ability to model with Units of Measure

### 1.3.0 - Monday, July 6th, 2020
* Added:
    * GLOP
    * Optano
    * LP File export for Optano
    * Gurobi

### 1.2.3 - Wednesday, June 10th, 2020
* Cleaner implementation of the DecisionBuilder

### 1.2.2 - Wednesday, June 10th, 2020
* Added overloads for Decision and float

### 1.2.1 - Wednesday, June 10th, 2020

### 1.2.0 - Thursday, May 28th, 2020
* Added a Computation Expression for creating Decisions 
* Made the creation of Decision function return a Decision instead of a Linear Expression 
* Made extracting values from the solution more streamlined

### 1.1.0 - Monday, May 18th, 2020
* Added:
    * Projections and reKey function
    * sumAll function
    * subtraction operator
    * custom equality to LinearExpression
    * LinearExpression to NoComparison
* Updated:
    * delta requirements for Scalar

### 1.0.0 - Thursday, May 7th, 2020
* Initial release
