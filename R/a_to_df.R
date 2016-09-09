#' Funkcja a_to_df
#'
#' Ta funkcja zamienia wielowymiarową macierz na obiekt klasy data.frame lub na plik .csv.
#'
#' @param x ścieżka do pliku .mat
#' @param save TRUE/FALSE określający czy obiekt wynikowy ma być zapisany do pliku. W przypadku opcji TRUE, obiekt zapisywany jest do tego samego miejsca na dysku, gdzie znajduje się plik .mat.
#'
#' @return obiekt klasy data.frame lub plik .csv
#' @importFrom R.matlab readMat
#' @importFrom utils write.csv
#'
#' @export
#'
#' @examples
#' \donttest{
#'    plik <- 'C:/Users/kot/dane/10symulacji.mat'
#'    a_to_df(plik, save=TRUE)
#' }

a_to_df <- function(x, save=TRUE){
    if(is.character(x)){
        file_name <- x
        x <- readMat(x)[[1]]
    }
    to_df <- function(nr, ar){
        df <- data.frame(Z=as.vector(ar[, , nr]))
        names(df) <- paste0('Z', nr)
        df
    }
    y <- lapply(seq_len(dim(x)[[3]]), to_df, x)
    y <- do.call(cbind, y)
    coord <- x[, , dim(x)[[3]]]
    coord_df <- data.frame(X=as.vector(col(coord)), Y=as.vector(row(coord)))
    df <- cbind(coord_df, y)
    if (save){
        write.csv(df, file=paste0(sub("(.*)(\\.([^.]{3}))$", "\\1", file_name), ".csv"), row.names=FALSE)
    } else{
        df
    }
}
