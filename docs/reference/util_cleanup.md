# Delete temporary files

Delete temporary files

## Usage

``` r
util_cleanup(
  nl,
  cleanup.csv = TRUE,
  cleanup.xml = TRUE,
  cleanup.bat = TRUE,
  cleanup.files
)
```

## Arguments

- nl:

  nl object

- cleanup.csv:

  TRUE/FALSE, if TRUE temporary created csv output files will be deleted
  after gathering results.

- cleanup.xml:

  TRUE/FALSE, if TRUE temporary created xml output files will be deleted
  after gathering results.

- cleanup.bat:

  TRUE/FALSE, if TRUE temporary created bat/sh output files will be
  deleted after gathering results.

- cleanup.files:

  vector with paths to temporary created files (csv, xml, bat)
