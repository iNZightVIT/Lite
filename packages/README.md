# shiny_packages
## R v3.1.2
A repository for all of the R Packages used by the shiny docker container.

Essentially, this should replace the R package library. i.e.,
```
cd path/to/R/
rm -rf library
git clone git@github.com:iNZightVIT/shiny_packages.git
mv shiny_packages library
```

Of course, this assumes that the correct version of R is being used for the build (stated above).
