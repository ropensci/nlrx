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

testthat::test_that("nldoc documents the GUI elements and BehaviorSpace experiments of NetLogo 7 models", {

  ## The test model with a sub-experiment added to its experiment:
  model <- readLines("WSP.nlogox")
  model <- sub("</constants>",
               paste0("</constants><subExperiments><subExperiment>",
                      "<steppedValueSet variable=\"wolf-reproduce\" first=\"1\" step=\"1\" last=\"3\"></steppedValueSet>",
                      "</subExperiment></subExperiments>"),
               model, fixed = TRUE)
  modelfile <- tempfile(fileext = ".nlogox")
  writeLines(model, modelfile)

  ## GUI elements (the widget parser of report_model_parameters()):
  gui <- nldoc_table_gui(modelfile)
  testthat::expect_identical(gui, util_read_nlogox_widgets(modelfile))
  testthat::expect_identical(gui$`model-version`$validvalues, c("sheep-wolves", "sheep-wolves-grass"))

  ## BehaviorSpace experiments:
  bs <- nldoc_table_bs(modelfile)
  testthat::expect_length(bs, 1)
  testthat::expect_identical(bs[[1]]$name, "wolf-sheep-experiment")
  testthat::expect_identical(bs[[1]]$definitions[c("Setup", "Post run", "Time limit", "Exit condition", "Repetitions")],
                             c("Setup" = "setup", "Post run" = "ca", "Time limit" = "500",
                               "Exit condition" = "not any? turtles", "Repetitions" = "10"))
  testthat::expect_identical(bs[[1]]$definitions[["Metrics"]], "count sheep; count wolves; grass")
  testthat::expect_true(all(c("wolf-gain-from-food", "wolf-reproduce", "model-version") %in% bs[[1]]$parameters$name))
  testthat::expect_identical(bs[[1]]$parameters$type[bs[[1]]$parameters$name == "wolf-reproduce"], "enumerated")
  testthat::expect_length(bs[[1]]$subexperiments, 1)
  testthat::expect_identical(bs[[1]]$subexperiments[[1]]$values, "from: 1; to: 3; by: 1")

  ## Both end up in the documentation:
  outdir <- tempfile("nldoc_out")
  dir.create(outdir)
  suppressWarnings(suppressMessages(
    nldoc(modelfiles = modelfile, outpath = outdir, infotab = FALSE, gui = TRUE, bs = TRUE)
  ))
  md <- readLines(file.path(outdir, "nldoc.md"))
  testthat::expect_true(all(c("# GUI elements", "# Behavior Space Experiments",
                              "#### Parameters {-}", "#### Sub-experiment 1 {-}") %in% md))
  testthat::expect_true(any(grepl("|  model-version  |  CHOOSER  |  sheep-wolves  |  validvalues: sheep-wolves, sheep-wolves-grass  |",
                                  md, fixed = TRUE)))
  testthat::expect_true("| Time limit | 500 |" %in% md)
  testthat::expect_true("| wolf-reproduce | sequence | from: 1; to: 3; by: 1 |" %in% md)
})
