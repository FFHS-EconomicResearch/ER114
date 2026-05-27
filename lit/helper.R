print_bib_rmd <- function(bib, .opts = list(), start = 1, stop = NULL, decreasing = FALSE) {

  bib <- sort(bib, decreasing = FALSE)

  if (!length(bib)) {
    return(bib)
  }

  if (identical(class(bib), "bibentry")) {
    bib <- as.BibEntry(bib)
  }

  keys <- unlist(bib$key)
  ind <- keys %in% names(.cites$indices)

  if (!any(ind)) {
    message("You haven't cited any references in this bibliography yet.")
    return()
  }

  if (length(.opts$bib.style)) {
    bibstyle <- .opts$bib.style
  } else {
    bibstyle <- .BibOptions$bib.style
  }

  if (length(.opts$cite.style)) {
    citestyle <- .opts$cite.style
  } else {
    citestyle <- .BibOptions$cite.style
  }

  if (length(.opts$style)) {
    style <- .opts$style
  } else {
    style <- .BibOptions$style
  }

  bib <- bib[[ind]] # gets citations to print

  if (bibstyle == citestyle) {
    if (bibstyle == "numeric") {
      if (length(bib) == length(.cites$labs)) {
        bib <- bib[[names(.cites$labs)]]
        .opts$sorting <- "none"
        bib$.index <- structure(.cites$labs, names = NULL)
      }
    } else {
      bib$.index <- .cites$labs[keys[ind]]
    }
  }

  if (length(.opts)) {
    old.opts <- BibOptions(.opts)
    on.exit(BibOptions(old.opts), add = TRUE)
  }

  if (style == "yaml") {
    cat("\n---\nnocite:", sQuote(paste0(paste0("@", names(.cites$indices)),
                                        collapse = ", ")))
    cat("\n...  \n\n")
  }

  if (is.null(stop)) {
    stop <- length(bib)
  }
  bib <- bib[start:stop]

  # --- NEUE LOGIK: Schrittweise Text-Filterung (DOI > URL > ISBN) ---
  # Zeilenumbrüche für den Druck vorübergehend abschalten
  old_width <- options(width = 10000)
  on.exit(options(old_width), add = TRUE)

  # Wir fangen den fertig generierten Text ab
  out <- capture.output(print(bib))

  for (i in seq_along(out)) {
    has_doi <- grepl("DOI:", out[i])
    has_url <- grepl("URL:", out[i])

    if (has_doi) {
      # 1. Priorität: DOI ist vorhanden.
      # Lösche URL (inkl. "visited on..."), ISBN und ISSN.
      out[i] <- gsub(" URL: \\S+( \\([^)]+\\))?\\.?", "", out[i])
      out[i] <- gsub(" ISBN: [0-9\\-X]+\\.?", "", out[i])
      out[i] <- gsub(" ISSN: [0-9\\-X]+\\.?", "", out[i])

    } else if (has_url) {
      # 2. Priorität: Keine DOI, aber URL ist vorhanden.
      # Lösche nur ISBN und ISSN.
      out[i] <- gsub(" ISBN: [0-9\\-X]+\\.?", "", out[i])
      out[i] <- gsub(" ISSN: [0-9\\-X]+\\.?", "", out[i])
    }
  }

  # Bereinigten Text in das Dokument ausgeben
  cat(out, sep = "\n")
  # ------------------------------------------------------------------
}

Zitiere <- function(bib, key, ...) {
  gsub(" and ", " und ", Citet(bib, key, ...))
}

environment(print_bib_rmd) <- asNamespace("RefManageR")
