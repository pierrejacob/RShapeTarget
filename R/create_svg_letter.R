#' @rdname create_svg_letter
#' @name create_svg_letter
#' @aliases create_svg_letter
#' @title Create SVG file with one letter
#' @description
#' This function creates a SVG file with one letter in it, and calls Inkscape
#' to convert it into a path. It is assumed that Inkscape is configured to export
#' SVG files WITHOUT relative coordinates (File > Inkscape Preferences > Allow relative coordinates).
#' @param letter a character that should belong to \code{letters} or \code{LETTERS}.
#' @return None.
#' @details
#' Inkscape must be installed for this function to work. Essentially it calls Inkscape from the command line
#' with the following command:
#' \code{inkscape FILENAME --select=ID --verb=ObjectToPath --verb=FileSave --verb=FileClose}
#' @export
create_svg_letter <- function(letter){
  if (!(letter %in% letters || letter %in% LETTERS)){
    stop("argument is not a letter")
  }
  svg <- xmlTreeParse(system.file(package = "RLetter", "data/a.svg"), useInternalNodes=TRUE, addAttributeNamespaces=TRUE, fullNamespaceInfo=FALSE)
  root <- xmlRoot(svg)
  xmlValue(root[["g"]][["text"]][["tspan"]]) <- letter
  if (is.na(str_locate(string=letter, pattern="[a-z]")[,1])){
    file_name <- suppressWarnings(normalizePath(paste0(system.file(package = "RLetter", "data/"), letter, "_uppercase_path.svg")))
  } else {
    file_name <- suppressWarnings(normalizePath(paste0(system.file(package = "RLetter", "data/"), letter, "_path.svg")))
  }
  print(file_name)
  saveXML(doc=root, file=file_name)
  id <- xmlAttrs(root[["g"]][["text"]])[["id"]]
  inkscape_command <- paste0('inkscape ', file_name,
                             ' --select=', id, ' --verb=ObjectToPath --verb=FileSave --verb=FileClose')
  system(inkscape_command, intern = TRUE)
}
