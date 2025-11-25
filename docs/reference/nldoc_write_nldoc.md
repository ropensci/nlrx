# Write NetLogo documentation

Write NetLogo documentation

## Usage

``` r
nldoc_write_nldoc(
  noxygen,
  noxygen_it,
  noxygen_gui,
  noxygen_bs,
  outpath,
  output_format,
  number_sections,
  theme,
  date,
  toc
)
```

## Arguments

- noxygen:

  list with parsed and processed noxygen tags from NetLogo model code

- noxygen_it:

  list with parsed and processed infotab strings

- noxygen_gui:

  list with parsed and processed noxygen tags from NetLogo GUI elements

- noxygen_bs:

  list with parsed and processed noxygen tags from NetLogo behavior
  space experiments

- outpath:

  Path to folder where rendered documentation should be created

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

## Value

list containing NetLogo GUI elements

## Details

The procedure uses parsed and processed noxygen tags to create and
render a markdown documentation in the specified format.
