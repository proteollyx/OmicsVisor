# ─────────────────────────────────────────────────────────
# OmicsVisor - Tests: the startup notice and the Disclaimer tab
#
# The notice is gated on localStorage so a returning browser is not nagged.
# The gate must fail *towards showing* the notice: if storage is unavailable
# the client reports "no" and the disclaimer appears.
# ─────────────────────────────────────────────────────────

app_src <- function() paste(readLines(file.path(OV_ROOT, "app.R"), warn = FALSE),
                            collapse = "\n")

test_that("the notice is gated on the client flag, not shown every session", {
  s <- app_src()
  expect_false(grepl("observeEvent(TRUE, {", s, fixed = TRUE),
               info = "still firing unconditionally on every session")
  expect_true(grepl("observeEvent(input$ov_notice_seen", s, fixed = TRUE))
  expect_true(grepl('identical(input$ov_notice_seen, "no")', s, fixed = TRUE))
})

test_that("the browser-side gate is present and versioned", {
  s <- app_src()
  expect_true(grepl("omicsvisor_notice_v1", s, fixed = TRUE))
  expect_true(grepl("Shiny.setInputValue('ov_notice_seen'", s, fixed = TRUE))
  expect_true(grepl("ovMarkNoticeSeen", s, fixed = TRUE))
  # acknowledging must write the flag back
  expect_true(grepl('sendCustomMessage("ovMarkNoticeSeen"', s, fixed = TRUE))
})

test_that("the gate fails towards showing the notice", {
  s <- app_src()
  # both try/catch blocks must default to 'no' rather than 'yes'
  expect_true(grepl("catch (e) { seen = 'no'; }", s, fixed = TRUE),
              info = "blocked localStorage must still show the notice")
  expect_true(grepl("var seen = 'no';", s, fixed = TRUE))
})

test_that("the notice text states the substantive limitation", {
  s <- app_src()
  for (phrase in c("does not perform quality control",
                   "Disclaimer tab",
                   "sensitive or personally identifiable"))
    expect_true(grepl(phrase, s, fixed = TRUE), info = phrase)
})


# ── Disclaimer tab ───────────────────────────────────────────────────────────

test_that("the Disclaimer tab renders without literal markdown", {
  html <- as.character(disclaimer_ui("disclaimer_module"))
  # p("**bold**") emits the asterisks verbatim; use tags instead
  expect_false(grepl("**", html, fixed = TRUE),
               info = "markdown asterisks are not rendered by p()")
  expect_true(grepl("<strong>", html, fixed = TRUE))
  expect_true(grepl("<ol>", html, fixed = TRUE))
})

test_that("the Disclaimer tab is no longer described as beta", {
  html <- as.character(disclaimer_ui("disclaimer_module"))
  expect_false(grepl("beta", html, ignore.case = TRUE))
})

test_that("the Disclaimer tab carries licence, DOI and version", {
  html <- as.character(disclaimer_ui("disclaimer_module"))
  expect_true(grepl("MIT licence", html, fixed = TRUE))
  expect_true(grepl("10.5281/zenodo.22660844", html, fixed = TRUE))
  expect_true(grepl("ASSETS.md", html, fixed = TRUE))
  # the footer reads version.R, so it cannot go stale
  expect_true(grepl(ov_version, html, fixed = TRUE))
  expect_true(grepl("Max Delbr", html))
})

test_that("the Disclaimer tab still covers the essential caveats", {
  html <- as.character(disclaimer_ui("disclaimer_module"))
  for (phrase in c("quality control", "as is", "responsib",
                   "personally identifiable", "Technology Platform Proteomics"))
    expect_true(grepl(phrase, html, ignore.case = TRUE), info = phrase)
})


# ── Behavioural: does the gate actually gate? ────────────────────────────────
# The tests above read app.R as text, which proves the wiring is present but
# not that it works. These drive the real server function with shinyalert
# mocked, so the modal's firing can be asserted directly.
# Each case builds the app in its own test_that: sourcing app.R twice inside
# one mocked-binding scope interferes with the mock.

# Instantiating the whole app server emits plotly's "plotly_click event ... is
# not registered" warning, because the volcano plot has not rendered yet. It is
# unrelated to the notice; muffle only that message so a genuine warning still
# surfaces.
without_plotly_noise <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("plotly_click", conditionMessage(w), fixed = TRUE))
      invokeRestart("muffleWarning")
  })
}

build_app_server <- function() {
  env <- new.env(parent = globalenv()); captured <- NULL
  env$shinyApp <- function(ui, server, ...) {
    captured <<- list(ui = ui, server = server); invisible(NULL)
  }
  withr::with_dir(OV_ROOT, sys.source(file.path(OV_ROOT, "app.R"), envir = env))
  captured$server
}

test_that("the notice fires when the browser reports it has not been seen", {
  calls <- new.env(); calls$n <- 0L; calls$title <- NA_character_
  local_mocked_bindings(
    shinyalert = function(title = "", ...) {
      calls$n <- calls$n + 1L; calls$title <- title; invisible(NULL)
    },
    .package = "shinyalert"
  )
  without_plotly_noise(testServer(build_app_server(), {
    session$setInputs(ov_notice_seen = "no")
    session$flushReact()
  }))
  expect_equal(calls$n, 1L)
  expect_match(calls$title, "Please read")
})

test_that("the notice stays hidden when the browser has already seen it", {
  calls <- new.env(); calls$n <- 0L
  local_mocked_bindings(
    shinyalert = function(...) { calls$n <- calls$n + 1L; invisible(NULL) },
    .package = "shinyalert"
  )
  without_plotly_noise(testServer(build_app_server(), {
    session$setInputs(ov_notice_seen = "yes")
    session$flushReact()
  }))
  expect_equal(calls$n, 0L)
})
