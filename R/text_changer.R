# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

file <- "raw_data/przyklady_plikow_dla_temperatury_powierzchni_(LST)/p_mod11a2_a2000065_h18v01_005_2007176171013_lst_day_1km.RDC"
t1 <- "7500.0000000"
t2 <- "0.0000000"
t3 <- "Background"

text_changer <- function(file, t1, t2, t3){
    full_path <- normalizePath(file)
    text1 <- readLines(file)
    p_text <- text1[c(16, 18, 22, 23)]
    s_text <- strsplit(p_text, ":")
    s_text <- lapply(s_text, function(x) x[1])
    s2 <- c(t1, t1, t2, t3)
    text2 <- paste0(s1, ": ", s2)
    text1[c(16, 18, 22, 23)] <- text2
    name <- gsub("^[.]*|[.][^.]*$", "", basename(full_path), perl = TRUE)
    writeLines(text1, con = paste0(dirname(full_path), "/", name, "_new", ".RDC"), sep = "\n", useBytes = FALSE)
}

text_changer(file, t1, t2, t3)


mt_changer <- function(folder_path, ...){
    files <- normalizePath(list.files(folder_path, full.names = TRUE))
    lapply(files, text_changer, t1, t2, t3)
}

mt_changer("/home/jakub/Documents/change_doc/raw_data/przyklady_plikow_dla_temperatury_powierzchni_(LST)", t1, t2, t3)
