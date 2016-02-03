$ONTEXT
Output report assignment specific to the binary equilibrium model

$OFFTEXT

report_dev(%case%,i,phi,'profit') =
	kappa_act_profit.l(i,phi) - kappa_act_loss.l(i,phi) ;
report_dev(%case%,i,phi,'profit cf') =
	kappa_inact_profit.l(i,phi) - kappa_inact_loss.l(i,phi) ;

report_wf(%case%,i,'compensation','all','total') = zeta.l(i) ;
report_wf(%case%,'total','compensation','all','total') = sum(i, zeta.l(i) ) ;

report_summary(%case%,i,'compensation') = zeta.l(i) ;
report_summary(%case%,'total','compensation') = sum(i, zeta.l(i) ) ;

