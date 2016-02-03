* write model statistics summary
report_summary(%scenario%,%model_case%,'modStat')   = %model_type%.modelstat ;
report_summary(%scenario%,%model_case%,'solveStat') = %model_type%.solvestat ;
report_summary(%scenario%,%model_case%,'resUsd')    = %model_type%.resUsd ;
report_summary(%scenario%,%model_case%,'objEst')    = %model_type%.objEst ;
report_summary(%scenario%,%model_case%,'objVal')    = %model_type%.objVal ;
report_summary(%scenario%,%model_case%,'objGap')    =
	report_summary(%scenario%,%model_case%,'objVal') / report_summary(%scenario%,%model_case%,'objEst') - 1 ;
