# Create NetLogo documentation

Create NetLogo documentation

## Usage

``` r
nldoc(
  modelfiles,
  outpath,
  infotab = TRUE,
  gui = TRUE,
  bs = TRUE,
  output_format = "html",
  number_sections = TRUE,
  theme = "journal",
  date = as.Date(Sys.time()),
  toc = TRUE
)
```

## Arguments

- modelfiles:

  vector of filepaths to model files

- outpath:

  Path to folder where rendered documentation should be created

- infotab:

  TRUE/FALSE, if TRUE infotab section will be included in the
  documentation

- gui:

  TRUE/FALSE, if TRUE a table with GUI elements from the model will be
  included in the documentation

- bs:

  TRUE/FALSE, if TRUE a table with behavior space experiments will be
  included in the documentation

- output_format:

  either "html", "pdf" or "docx"

- number_sections:

  TRUE/FALSE, if TRUE sections in the documentation will be numbered

- theme:

  markdown theme, supported themes are "journal", "cerulean", "flatly",
  "readable", "spacelab", "united", "cosmo"

- date:

  date that is printed in the documentation header

- toc:

  TRUE/FALSE, if TRUE the documentation contains a table of contents -
  only for html and pdf output format

## Details

nldoc reads model code from the provided model files. The code is then
split into several groups (code, gui, behavior space). The procedures
then finds noxygen commands within the NetLogo model code. For a
complete list of noxygen commands type ?nldoc These commands are
translated into a markdown documentation file. If needed, tables of gui
elements and behavior space experiments are added to the markdown file.
Finally, the document is rendered in the specified format.

## Examples

``` r
if (FALSE) { # \dontrun{

# List model files (.nls subfiles are also supported)
modelfiles <- c("https://raw.githubusercontent.com/nldoc/nldoc_pg/master/WSP.nlogo",
                "https://raw.githubusercontent.com/nldoc/nldoc_pg/master/WSP.nls")

# Define output directory:
outdir <- tempdir()  # adjust path to your needs

# Create documentation:
nldoc(modelfiles = modelfiles,
      infotab=TRUE,
      gui=TRUE,
      bs=TRUE,
      outpath = outdir,
      output_format = "html",
      number_sections = TRUE,
      theme = "cosmo",
      date = date(),
      toc = TRUE)
} # }
```
