
report_flow(%scenario%,%model_case%,l,t) = sum(n, H_M(l,n) * delta.l(t,n) ) ;

report_iso(%scenario%,%model_case%,'price',n,t) = price.l(t,n) ;
report_iso(%scenario%,%model_case%,'generation',i,t) = sum(b, g.l(t,i,b) ) ;
report_iso(%scenario%,%model_case%,'generation',n,t) = sum(i$map_G(n,i), report_iso(%scenario%,%model_case%,'generation',i,t) ) ;
report_iso(%scenario%,%model_case%,'load',n,t) = d.l(t,n) ;
report_iso(%scenario%,%model_case%,'wind_av',n,t) = sum(w$( map_W(w,n) ), w_det(t,w) ) ;
report_iso(%scenario%,%model_case%,'wind_curt',n,t) = sum(w$( map_W(w,n) ), w_curt.l(t,w) ) ;
report_iso(%scenario%,%model_case%,'on-off',i,t) = x.l(t,i) ;

report_iso(%scenario%,%model_case%,'generation','all',t) = sum(n, report_iso(%scenario%,%model_case%,'generation',n,t) ) ;
report_iso(%scenario%,%model_case%,'load','all',t) = sum(n, report_iso(%scenario%,%model_case%,'load',n,t) ) ;
report_iso(%scenario%,%model_case%,'wind_av','all',t) = sum(n, report_iso(%scenario%,%model_case%,'wind_av',n,t) ) ;
report_iso(%scenario%,%model_case%,'wind_curt','all',t) = sum(n, report_iso(%scenario%,%model_case%,'wind_curt',n,t) ) ;

report_wf(%scenario%,%model_case%,i,'revenue','all')       = sum((t,n,b)$map_G(n,i), price.l(t,n) * g.l(t,i,b) ) ;
report_wf(%scenario%,%model_case%,i,'no-load cost','all')  = sum(t, c_NL(i) * x.l(t,i) ) ;
report_wf(%scenario%,%model_case%,i,'gen cost','all')      = sum((t,b), c_G(i,b) * g.l(t,i,b) ) ;
report_wf(%scenario%,%model_case%,i,'dispatch cost','all') = sum(t, c_on(i) * z_on.l(t,i) + c_off(i) * z_off.l(t,i) ) ;

report_wf(%scenario%,%model_case%,i,'profit','all') =
	report_wf(%scenario%,%model_case%,i,'revenue','all')
	- report_wf(%scenario%,%model_case%,i,'no-load cost','all')
	- report_wf(%scenario%,%model_case%,i,'gen cost','all')
	- report_wf(%scenario%,%model_case%,i,'dispatch cost','all') ;

report_wf(%scenario%,%model_case%,'all','profit','all') = sum(i, report_wf(%scenario%,%model_case%,i,'profit','all') ) ;

report_wf(%scenario%,%model_case%,'all','wind_rent','all') =
	sum((t,n), sum(w$( map_W(w,n) ), w_det(t,w) - w_curt.l(t,w) ) *  price.l(t,n) ) ;

report_wf(%scenario%,%model_case%,'all','cons rent','all') = sum((t,n), ( u_D(t,n) - price.l(t,n) ) * d.l(t,n) ) ;

report_wf(%scenario%,%model_case%,'all','cong rent','all') =
	sum((l,t), - report_flow(%scenario%,%model_case%,l,t) * sum(n, Incidence(l,n) * price.l(t,n) ) ) ;

report_wf(%scenario%,%model_case%,'all','welfare','all') =
	report_wf(%scenario%,%model_case%,'all','profit','all')
	+ report_wf(%scenario%,%model_case%,'all','wind_rent','all')
	+ report_wf(%scenario%,%model_case%,'all','cons rent','all')
	+ report_wf(%scenario%,%model_case%,'all','cong rent','all') ;

report_wf(%scenario%,%model_case%,'all','obj','all') = obj.l ;

report_summary(%scenario%,%model_case%,'profit')    = report_wf(%scenario%,%model_case%,'all','profit','all') / 1e6 ;
report_summary(%scenario%,%model_case%,'wind_rent') = report_wf(%scenario%,%model_case%,'all','wind_rent','all') / 1e6 ;
report_summary(%scenario%,%model_case%,'cons rent') = report_wf(%scenario%,%model_case%,'all','cons rent','all') / 1e6 ;
report_summary(%scenario%,%model_case%,'cong rent') = report_wf(%scenario%,%model_case%,'all','cong rent','all') / 1e6 ;
report_summary(%scenario%,%model_case%,'welfare')   = report_wf(%scenario%,%model_case%,'all','welfare','all') / 1e6 ;

* count the number of nodes/hours where the price limit is reached (because of numerical issues, we add a slack factor
report_summary(%scenario%,%model_case%,'price_limit') = sum((t,n)$( price.l(t,n) + 0.001 > u_D(t,n) ), 1 ) ;
