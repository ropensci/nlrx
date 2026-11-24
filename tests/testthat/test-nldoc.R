testthat::context("nldoc tests")
testthat::test_that("nldoc", {

  testthat::context("nldoc")

  if (rmarkdown::pandoc_available())
  {
    modelfiles <- c("WSP.nlogox",
                    "WSP.nls")


    # Create html documentation:
    nldoc_dir <- tempdir()
    nldoc(modelfiles = modelfiles,
          infotab=TRUE,
          gui=TRUE,
          bs=TRUE,
          outpath = nldoc_dir,
          output_format = "html",
          number_sections = TRUE,
          theme = "cosmo",
          date = date(),
          toc = TRUE)

    nldoc_html <- file.path(nldoc_dir, "nldoc.html")
    testthat::expect_true(file.exists(nldoc_html))

    ## Read html back in:
    #nldoc_readin <- XML::readHTMLList(nldoc_html)
    #testthat::expect_equal(length(nldoc_readin), 8)

    # Create docx documentation:
    nldoc_dir <- tempdir()
    nldoc(modelfiles = modelfiles,
          infotab=TRUE,
          gui=TRUE,
          bs=TRUE,
          outpath = nldoc_dir,
          output_format = "docx",
          number_sections = TRUE,
          theme = "cosmo",
          date = date(),
          toc = TRUE)

    nldoc_docx <- file.path(nldoc_dir, "nldoc.docx")
    testthat::expect_true(file.exists(nldoc_docx))


    testthat::context("nldoc_network")

    nw <- nldoc_network(modelfiles)
    testthat::expect_match(class(nw), "igraph")
    testthat::expect_equal(length(nw), 10)

    # Test nldoc without gui, infotab and bs:
    nldoc_dir <- tempdir()
    nldoc(modelfiles = modelfiles,
          infotab=FALSE,
          gui=FALSE,
          bs=FALSE,
          outpath = nldoc_dir,
          output_format = "html",
          number_sections = TRUE,
          theme = "cosmo",
          date = date(),
          toc = TRUE)

    nldoc_html <- file.path(nldoc_dir, "nldoc.html")
    testthat::expect_true(file.exists(nldoc_html))

    ## Read html back in:
    #nldoc_readin <- XML::readHTMLList(nldoc_html)
    #testthat::expect_equal(length(nldoc_readin), 3)
  }
})

testthat::test_that("nldoc documents models without noxygen annotations", {

  ## The test model without any noxygen tags:
  ## (the first tag shares its line with the opening of the code section)
  model <- readLines("WSP.nlogox")
  model <- sub(";`@.*$", "", model[!grepl("^;`@", model)])
  plain <- file.path(tempfile("nldoc_test"), "Plain Model.nlogox")
  dir.create(dirname(plain))
  writeLines(model, plain)

  outdir <- tempfile("nldoc_out")
  dir.create(outdir)
  testthat::expect_message(
    suppressWarnings(utils::capture.output(
      nldoc(modelfiles = plain, outpath = outdir, infotab = TRUE, gui = FALSE, bs = FALSE)
    )),
    regexp = "No noxygen annotations"
  )
  md <- readLines(file.path(outdir, "nldoc.md"))
  ## The file name is the fallback title, empty sections are left out:
  testthat::expect_true("title: \"Plain Model\"" %in% md)
  testthat::expect_true("author: \"Unknown\"" %in% md)
  testthat::expect_false(any(md %in% c("# Global", "# Procedures")))
  testthat::expect_true("# Info Tab" %in% md)

  ## A model with only a header keeps its title:
  header <- file.path(dirname(plain), "Header Model.nlogox")
  writeLines(sub("<code><![CDATA[", "<code><![CDATA[;`@model Header Title\n;`@author Someone\n",
                 model, fixed = TRUE), header)
  outdir <- tempfile("nldoc_out")
  dir.create(outdir)
  suppressWarnings(utils::capture.output(
    nldoc(modelfiles = header, outpath = outdir, infotab = FALSE, gui = FALSE, bs = FALSE)
  ))
  md <- readLines(file.path(outdir, "nldoc.md"))
  testthat::expect_true("title: \"Header Title\"" %in% md)
  testthat::expect_true("author: \"Someone\"" %in% md)
  testthat::expect_false(any(md %in% c("# Global", "# Procedures")))
})
