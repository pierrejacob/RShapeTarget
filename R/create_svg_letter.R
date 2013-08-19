#'@export
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
