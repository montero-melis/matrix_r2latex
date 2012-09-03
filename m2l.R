m2l <- function(X, fname = "m2l.out", rowlabel = 0, collabel = 0, caption = " ")
{
	# Prints a matrix as latex table into a file
	
	# Input:	
	#		X:		a matrix
	#		fname:		a filename as String
	#		rowlabel:	a string array of labels, dim = nrow
	#		collabel:	a string array of labels, dim = ncol
	#		caption:	a string for caption field
				

	if (!(is.matrix(X)))
	{
		error("X is not a matrix");
		stop;
	}

	row <- nrow(X);
	col <- ncol(X);

	if (length(rowlabel) < 2)
	{
		rowlabel <- c(1:nrow);
	}
	if (length(collabel) < 2)
	{
		collabel <- c(1:ncol);
	}

	if ((length(rowlabel) != row) || (length(collabel) != col))
	{	
		error("wrong label dimensions");
		stop;
	}

	cat("\\begin{table}\n", file=fname);
	cat("\\begin{center}\n", file=fname, append=TRUE);
	cat("\\begin{tabular}{", file=fname, append=TRUE);
	

	for (j in c(1:(col+1)))
	{
		cat("|c", file=fname, append=TRUE);
	}
	cat("|} \\hline \n", file=fname, append=TRUE);
	
	for (j in c(1:col))
	{
		cat("&", file=fname, append=TRUE);
		cat(collabel[j], file=fname, append=TRUE);
	}
	cat("\\\\ \\hline \n", file=fname, append=TRUE);

	for (i in c(1:row))
	{
		cat(rowlabel[i], file=fname, append=TRUE);
		for (j in c(1:col))
		{
			cat("&", file=fname, append=TRUE);
			cat(X[i,j], file=fname,append=TRUE);
		}
		if (i < row)
		{
			cat("\\\\ \n", file=fname, append=TRUE);
		} else {
			cat("\\\\ \\hline \n", file=fname, append=TRUE);
		}
	}
	
	cat("\\end{tabular} \n", file=fname, append=TRUE);
	cat("\\caption{", file=fname, append=TRUE);
	cat(caption, file=fname, append=TRUE);
	cat("} \n", file=fname, append=TRUE);
	cat("\\end{center} \n", file=fname, append=TRUE);
	cat("\\end{table} \n", file=fname, append=TRUE);
}
