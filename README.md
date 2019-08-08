# consensusMIBCweb
Temporary: A webapp version of the consensusMIBC R package. 

Runs a single-sample consensus classification of a given dataset.

## Installation

To run this version of the app, please install shiny and its dependencies in R:

```{r}
install.packages(c("shiny", "tidyverse", "readxl", "devtools", "shinyWidgets"))
devtools::install_github("cit-bioinfo/consensusMIBC")
```

## Run the app

You can run the app by pasting the following code in R:

```{r}
library(shiny)
runGitHub("consensusMIBCweb", "csgroen")
```
