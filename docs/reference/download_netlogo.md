# Download NetLogo

Auxiliary function to download NetLogo

## Usage

``` r
download_netlogo(to, version, os = NA, extract = FALSE)
```

## Arguments

- to:

  Path to folder where the downloaded file is saved.

- version:

  Character string naming which NetLogo Version to download (see
  Details)

- os:

  operation system ("win", "mac", "unix") decides which version of
  netlogo (msi, dmg, tgz) is downloaded. If set to NA (default) os will
  be detected automatically
  ([`util_get_os()`](https://docs.ropensci.org/nlrx/reference/util_get_os.md))

- extract:

  TRUE/FALSE, if TRUE downloaded archive is extracted to subfolder of
  `to` (only unix)

## Details

Retrieve supported Versions for Download and Usage (parameter
`version`): supported_netlogo_versions()

## Examples

``` r
if (FALSE) { # \dontrun{
dlpath <- tempdir()  # adjust path to your needs
try(download_netlogo(dlpath, "6.0.3"))
} # }
```
