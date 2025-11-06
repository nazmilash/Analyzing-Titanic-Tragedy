.PHONY: help deps render serve open clean

R_LIB := $(HOME)/R/library
PKGS := rmarkdown knitr dplyr ggplot2 stargazer

help:
    @echo "Targets: deps render serve open clean"

deps:
    sudo apt update
    sudo apt install -y pandoc libcurl4-openssl-dev libssl-dev libxml2-dev build-essential || true
    mkdir -p $(R_LIB)
    Rscript -e '.libPaths(c("$(R_LIB)", .libPaths())); install.packages(c($(PKGS)), repos="https://cran.rstudio.com/", Ncpus = parallel::detectCores())'

render:
    Rscript -e '.libPaths(c("$(R_LIB)", .libPaths())); rmarkdown::render("Titanic_Exercise Solved.Rmd", output_format="html_document")'

serve:
    python3 -m http.server 8000 --directory /workspaces/Analyzing-Titanic-Tragedy &

open:
    "$$BROWSER" "http://localhost:8000/Titanic_Exercise%20Solved.html"

clean:
    rm -f *.html titanic_temp.csv
	