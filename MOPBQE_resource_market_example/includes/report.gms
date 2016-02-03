*** output report for MOPBQE - naturas gas market and investment game example ***

* revenue, costs, profits by firm
report_wf(%case%,i,'revenue',t,n) = price.l(t,n) * y_S.l(t,i,n) ;
report_wf(%case%,i,'inv costs','all',n) = p_X_inv(i,n) * x_P.l(i,n) + p_Z_inv(i,n) * z_P.l(i,n) ;
report_wf(%case%,i,'inv costs','all',l) = a_X_inv(i,l) * x_A.l(i,l) + a_Z_inv(i,l) * z_A.l(i,l) ;
report_wf(%case%,i,'inv costs','all','total') = sum(n, report_wf(%case%,i,'inv costs','all',n) )
	+ sum(l, report_wf(%case%,i,'inv costs','all',l) ) ;
report_wf(%case%,i,'prod costs','all',n) = sum(t, p_Y_mc(t,i,n) * y_P.l(t,i,n) ) ;
report_wf(%case%,i,'trspt costs','all',l) =  sum(t, a_Y_mc(t,i,l) * y_A.l(t,i,l) ) ;
report_wf(%case%,i,'oprtn costs','all','total') =  sum(n, report_wf(%case%,i,'prod costs','all',n) )
	+ sum(l, report_wf(%case%,i,'trspt costs','all',l) ) ;
report_wf(%case%,i,'total costs','all','total') = report_wf(%case%,i,'inv costs','all','total')
	+ report_wf(%case%,i,'oprtn costs','all','total') ;
report_wf(%case%,i,'profits','all','total') = sum((t,n), report_wf(%case%,i,'revenue',t,n) )
	- report_wf(%case%,i,'total costs','all','total') ;

report_summary(%case%,i,'profits') = report_wf(%case%,i,'profits','all','total') ;
report_summary(%case%,'total','profits') = sum(i, report_wf(%case%,i,'profits','all','total') ) ;

report_summary(%case%,i,'strategy')$( sum(n, x_P.l(i,n) ) eq 0 AND sum(l, x_A.l(i,l) ) eq 0 ) = OO ;
report_summary(%case%,i,'strategy')$( sum(n, x_P.l(i,n) ) eq 1 AND sum(l, x_A.l(i,l) ) eq 0 ) = pO ;
report_summary(%case%,i,'strategy')$( sum(n, x_P.l(i,n) ) eq 0 AND sum(l, x_A.l(i,l) ) eq 1 ) = Oa ;
report_summary(%case%,i,'strategy')$( sum(n, x_P.l(i,n) ) eq 1 AND sum(l, x_A.l(i,l) ) eq 1 ) = pa ;

* consumer surplues
report_wf(%case%,'cons','surplus',t,n) =
	( int_D(t,n) - 0.5 * slp_D(t,n) * d.l(t,n) ) * d.l(t,n)
	- price.l(t,n) * d.l(t,n) ;

report_summary(%case%,'cons','surplus') = sum((t,n), report_wf(%case%,'cons','surplus',t,n) ) ;

* total welfare
report_wf(%case%,'total','welfare','all','total') =
	report_summary(%case%,'total','profits')
	+ sum((t,n), report_wf(%case%,'cons','surplus',t,n) ) ;

report_summary(%case%,'total','welfare') = report_wf(%case%,'total','welfare','all','total') ;
