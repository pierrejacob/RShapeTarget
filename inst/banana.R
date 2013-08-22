file_name <- "/home/pierre/Dropbox/RLetter/RShapeTarget/data/canada.svg"
file_name <- "/home/pierre/Dropbox/RLetter/RShapeTarget/data/a_path.svg"
file_name <- "/home/pierre/Dropbox/RLetter/RShapeTarget/data/banana.svg"

# root <- xmlRoot(xmlTreeParse(file=file_name2, useInternalNodes=TRUE))
# xmlGetAttr(root[["g"]][["g"]][["path"]], "d")
# xmlName(root[["g"]][["g"]][["path"]])
# xmlAttrs(root[["g"]][["g"]][["path"]])
# top <- xmlInternalTreeParse(file=file_name2, useInternalNodes=TRUE)
# els = xpathApply(doc=top, path="//svg:path[@d]", fun=xmlGetAttr, "d")
# els

# top <- xmlInternalTreeParse(file=file_name, useInternalNodes=TRUE)
# els = xpathApply(doc=top, path="//svg:path[@d]", fun=xmlGetAttr, "d")
# els
# paste(els, collapse = " ")
# root <- xmlRoot(xmlTreeParse(file=file_name, useInternalNodes=TRUE))
# xmlName(root)
# xmlSApply(X=root, FUN=function(x) xmlGetAttr(x, "d"))
# els = getNodeSet(root, "//path")
# els

# doc = xmlParse(system.file("exampleData", "tagnames.xml", package = "XML"))

# els = getNodeSet(doc, "/doc//a[@status]")
# l <- sapply(1:length(c), function(x) xmlGetAttr(c[[x]], name="d"), simplify=FALSE, USE.NAMES = FALSE)
# class(l[[1]])
# names(l)

# class(l)
# lapply(l, extract_paths_)

paths <- extract_paths_from_svg(file_name)

plot_paths <- function(paths){
  if (class(paths) != "list"){
    stop("'plot_paths' takes a list as argument")
  }
  g <- qplot(x = paths[[1]][,1], y = paths[[1]][,2], geom = "blank") + geom_polygon(colour = "black", fill = "NA")
  if (length(paths) > 1){
    for (index in 2:length(paths)){
      g <- g + geom_polygon(aes_string(x = paste0("paths[[", index, "]][,1]"),
                                     y = paste0("paths[[", index, "]][,2]")), colour = "black", fill = "NA")
    }
  }
  g <- g + xlab("x") + ylab("y") + theme_minimal()
  return(g)
}
plot_paths(paths)
