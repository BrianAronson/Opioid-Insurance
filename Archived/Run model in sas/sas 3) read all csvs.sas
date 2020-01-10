*Read all CSVs in directory;

*1) run macro to read all csvs in directory;
	%drive(hidden/1_csv_PageRank, csv)

*2) append all files;
	*a) identify data files in work library;
		PROC SQL;
		SELECT MEMNAME INTO : MEMNAMES SEPARATED BY ' ' from dictionary.tables where libname='WORK';quit;
	*b) append libraries in work to a new file;
		DATA df;
		SET &MEMNAMES.;
		RUN;
