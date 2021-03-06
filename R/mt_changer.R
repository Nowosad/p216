#' Funkcja mt_changer
#'
#' Ta funkcja zamienia wartości parametrów w wierszach 16, 18, 22, 23
#' w folderze zawierającym pliki o rozszerzeniu RDC.
#'
#' @param folder_path ścieżka do folderu zawierającego pliki RDC
#' @param t1 tekst do zastąpienia wartości w wierszach 16 i 18
#' @param t2 tekst do zastąpienia wartości w wierszu 22
#' @param t3 tekst do zastąpienia wartości w wierszu 23
#'
#' @return nowe pliki ze zmienionymi wartościami.
#' Nowy plik będzie miał taką samą nazwę w folderze new
#'
#' @export
#'
#' @examples
#' \donttest{
#'    folder <- "C:/Users/jakub/pliki_RDC"
#'    mt_changer(folder, "7500.0000000", "0.0000000", "Background")
#' }

mt_changer <- function(folder_path, t1, t2, t3, ...){
    assert_that(is.dir(folder_path))
    assert_that(is.string(t1))
    assert_that(is.string(t2))
    assert_that(is.string(t3))
    files <- normalizePath(list.files(folder_path, pattern="*.RDC" , full.names = TRUE))
    x <- lapply(files, text_changer, t1=t1, t2=t2, t3=t3)
}
