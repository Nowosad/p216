#' Funkcja text_changer
#'
#' Ta funkcja zamienia wartości parametrów w wierszach 16,
#' 18, 22, 23 w plikach o rozszerzeniu RDC.
#'
#' @param file ścieżka do pliku RDC
#' @param t1 tekst do zastąpienia wartości w wierszach 16 i 18
#' @param t2 tekst do zastąpienia wartości w wierszu 22
#' @param t3 tekst do zastąpienia wartości w wierszu 22
#'
#' @return nowy plik ze zmienionymi wartościami.
#' Nowy plik będzie miał taką samą nazwę z dopiskiem _new
#' @export
#'
#' @examples
#' \donttest{
#'    file <- "raw_data/przyklady_plikow_dla_temperatury_powierzchni_(LST)/p_mod11a2_a2000065_h18v01_005_2007176171013_lst_day_1km.RDC"
#'    text_changer(file, "7500.0000000", "0.0000000", "Background")
#' }

text_changer <- function(file, t1, t2, t3){
    assert_that(is.string(file))
    assert_that(is.string(t1))
    assert_that(is.string(t2))
    assert_that(is.string(t3))
    full_path <- normalizePath(file)
    text1 <- readLines(file)
    p_text <- text1[c(16, 18, 22, 23)]
    s_text <- strsplit(p_text, ":")
    s_text <- lapply(s_text, function(x) x[1])
    s2 <- c(t1, t1, t2, t3)
    text2 <- paste0(s_text, ": ", s2)
    text1[c(16, 18, 22, 23)] <- text2
    name <- gsub("^[.]*|[.][^.]*$", "", basename(full_path), perl = TRUE)
    new_file <- paste0(dirname(full_path), "/", name, "_new", ".RDC")
    writeLines(text1, con = new_file, sep = "\n", useBytes = FALSE)
    print(paste0("Dodano nowy plik ", new_file, " z wybranymi wartościami."))
}

