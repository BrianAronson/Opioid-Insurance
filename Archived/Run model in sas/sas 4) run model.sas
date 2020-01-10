
* 1) Make sure to output results to an xls file;
ods csv file="hidden/sas_mod1.csv";

* 2) Run regression;
proc logistic data = df;
model IO(event = '1') = 
	    hepc hiv chronic concomitant psych_disorder
	    any_rx MAT_user palliative_care cancer female
	    saw_doc is_isolate comp_large
	    log_conf_count log_doc_count log_year_quarter log_age log_TOTAL_MME log_t1birank_MME_BiRank log_t1degree_unweighted_none;
STRATA patid;
run;

*3) finalize output
ods csv close;
