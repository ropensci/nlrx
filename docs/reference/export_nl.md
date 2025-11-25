# Export NetLogo Experiment

Export NetLogo Experiment as zip file

## Usage

``` r
export_nl(nl, path = dirname(getnl(nl, "modelpath")), tarfile)
```

## Arguments

- nl:

  nl object

- path:

  Path to folder that contains files to run NetLogo experiment

- tarfile:

  Path to folder where the experiments gets stored as zip file

## Value

The status value returned by the external command, invisibly.

## Details

Exports your folder that contains data and scripts for NetLogo + nlrx
analyses as a zip file. Furthermore, `export_nl` takes your nl object
and saves it in the zipped folder. This enables other person to run the
same experiment as you.

## Examples

``` r
if (FALSE) { # \dontrun{

# Load nl object from testdata:
nl <- nl_lhs
path <- getwd() # adjust path to your needs, path should point to a directory with model data
outfile <- tempfile(fileext = ".zip") # adjust file path to your needs
export_nl(nl, path = path, tarfile = outfile)
} # }
```
