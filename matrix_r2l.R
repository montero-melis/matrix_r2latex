# Converts matrix from R to LaTeX format

matrix_2l <- function(X, fname = "matrix_2l.out"){

	if (!(is.matrix(X)))
		{
		return("error: X is not a matrix");
      	stop			
		}

	else {
		return("X is a matrix!!!");		
		}

	cat("\\begin{table}\n", file=fname);
	cat("\\begin{center}\n", file=fname, append=TRUE);
	cat("\\begin{tabular}{", file=fname, append=TRUE);

}
