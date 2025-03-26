# Window slider with weighting

The project is a window slider based on the r-language and aims at calculating the reference limits of this indicator for different time periods from a set of existing datasets.

## Table of Contents
- [Introduction](#Introduction)
- [Installation](#Installation)
- [Instructions](#Instructions)
- [Usage](#Usage)
- [Licence](#Licence)

## Introduction
The program provides various distribution functions, such as Gaussian distribution, triangular distribution, trapezoidal distribution, and more, with freely adjustable parameters. Users can use these distribution functions to weight the data. The program will calculate the weighted data to generate a reference limit chart.

## Installation

**Method 1:** Use the function `runGithub()` from the package shiny:

```R
if("shiny" %in% rownames(installed.packages())){
  library(shiny)} else{install.packages("shiny")
  library(shiny)}
runGitHub("reflimR_Sliding", "ddddxx1")
```

**Method 2 (not recommended):** Download the Zip-File from the Shiny App (https://github.com/ddddxx1/reflimR_Sliding). Extract the file and set your working direction to the path of the folder. The package shiny must be installed before using the Shiny App:

```R
# Test if shiny is installed:
if("shiny" %in% rownames(installed.packages())){
  library(shiny)} else{install.packages("shiny")
  library(shiny)}
```

And then start the app with the following code:

```R
runApp("app.R")
```



## Instructions
Please put app.R and main.R in the same directory.
The application will automatically download and call the required libraries.

## Usage
Users can run the shiny app directly, which provides a user interface for the user. The user can intuitively select the distribution function or modify the parameters in the programme. shiny will show the user the weight distribution at each step and the final reference limit line graph.
Alternatively, you can call the run function in main.R, which integrates all the functions and automatically calculates the reference limit line graph.

## License
This project is licensed under the GPL 3.0 License. You can freely use, modify, and distribute this software, provided that you adhere to the terms of the license.