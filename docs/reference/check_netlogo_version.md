# Check supported NetLogo versions

Auxiliary function to check supported NetLogo version numbers

## Usage

``` r
check_netlogo_version(version, throw_error = FALSE)
```

## Arguments

- version:

  Character string naming which NetLogo Version to check

- throw_error:

  TRUE/FALSE, if TRUE an error is thrown if the version is not supported

## Value

logical, if netlogo versions is supported by nlrx

## Examples

``` r
if (FALSE) { # \dontrun{
check_netlogo_version("6.2.0")
check_netlogo_version("1.0")
check_netlogo_version("1.0", TRUE)
} # }
```
