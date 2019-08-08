# consensusMIBCweb
Temporary: A webapp version of the consensusMIBC R package. 

Runs a single-sample consensus classification of a given dataset.

## Installation

To run this version of the app, please install shiny and its dependencies:

```{r}
install.packages(c("shiny", "tidyverse", "readxl", "devtools"))
devtools::install_github("cit-bioinfo/consensusMIBC")
```

## Run the app

You can run the app using:

```{r}
library(shiny)
runGitHub("consensusMIBCweb", "csgroen")
```
