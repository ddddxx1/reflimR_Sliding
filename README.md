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
```bash
# Clone this repository
git clone https://github.com/ddddxx1/Praxisprojekt.git

# Go to the project directory
cd Praxisprojekt
```
## Instructions
Please put shiny.R and main.R in the same directory, and set the R working directory to this with `setwd("your work directory")`.
The application will automatically download and call the required libraries.

## Usage
Users can run the shiny app directly, which provides a user interface for the user. The user can intuitively select the distribution function or modify the parameters in the programme. shiny will show the user the weight distribution at each step and the final reference limit line graph.
Alternatively, you can call the run function in main.R, which integrates all the functions and automatically calculates the reference limit line graph.

## Licence
This project is licensed under the MIT License.