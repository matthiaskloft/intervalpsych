pkgbuild::clean_dll()
pkgbuild::compile_dll()
roxygen2::roxygenize()
callr::rcmd(
  paste(
    "R CMD INSTALL --preclean --no-multiarch --with-keep-source",
    here::here()
  )
)


#devtools::install(quick = FALSE, upgrade = FALSE)
devtools::install(quick=TRUE, upgrade = FALSE)
