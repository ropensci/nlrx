# Submission

This is a major update of nlrx (0.4.6 -> 0.5.0).

nlrx now supports NetLogo 7 and newer. NetLogo 7 replaced the model file format and the
way simulations are invoked, so the simulation backend was redesigned around the
'logolink' package (on CRAN), which writes the BehaviorSpace XML and executes NetLogo.

Support for NetLogo versions below 7.0.0 has been removed, which makes this a breaking
release: the previous execution backend and a number of arguments that depended on it
are gone. All breaking changes are listed in NEWS.md, and the error message for an older
NetLogo version points users to nlrx 0.4.6, which continues to support NetLogo 5.3.1 to
6.4.0.

# Test environments

* local: Ubuntu 24.04, R 4.6.1 -- 0 errors | 0 warnings | 0 notes
* TODO before submission: add the GitHub Actions R-CMD-check results (Ubuntu, macOS,
  Windows; R release and oldrel) once they have run on the release branch.

# R CMD check results

0 errors | 0 warnings | 0 notes

# System requirements

Running simulations requires a local NetLogo (>= 7.0.0) and Java (>= 11) installation,
which is not available on the check machines. Examples that execute NetLogo are therefore
wrapped in \dontrun{}, and the tests that execute simulations are skipped unless a
NetLogo installation is present. The checks above exercise everything that does not
require NetLogo, including the generation of BehaviorSpace definitions.

# Package dependencies

The dependency check reports "Imports includes 23 non-default packages". The number of
imports is essentially unchanged from the previous release: 'logolink', 'cli', 'janitor'
and 'xml2' were added and 'furrr' was dropped. The imports reflect the scope of the
package, which covers simulation design (including sensitivity analysis and optimization
backends) as well as spatial and network output processing.

# Reverse dependencies

There are currently no reverse dependencies. Checked against the full CRAN package
database (Depends, Imports, Suggests, LinkingTo, Enhances).
