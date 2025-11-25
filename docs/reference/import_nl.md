# Import NetLogo Experiment

Import NetLogo Experiment from export_nl

## Usage

``` r
import_nl(tarfile, targetdir, new_session = FALSE)
```

## Arguments

- tarfile:

  Path to tarfile that contains files to run NetLogo experiment

- targetdir:

  Path to folder where the experiments gets extracted

- new_session:

  If TRUE, opens a new RStudio Session with an Rproj

## Value

The status value returned by the external command, invisibly.

## Details

Imports NetLogo experiments that were saved with `export_nl`. If the
folder comes with an .Rproj file (which is recommended because relative
paths enhance the reproducability of your analysis), `import_nl` opens
this project and loads the nl object in your R environment.

## Examples

``` r
if (FALSE) { # \dontrun{

infile <- "/home/user/test.zip"
targetdirectory <- "/home/user/test"
import_nl(infile, targetdirectory)
} # }
```
