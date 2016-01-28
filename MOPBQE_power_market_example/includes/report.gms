
report_flow(%case%,l,'cap') = f_max(l) ;
report_flow(%case%,l,t) = sum(n, H_M(l,n) * delta.l(t,n) ) ;

report_iso(%case%,'price',n,t) = price.l(t,n) ;
report_iso(%case%,'generation',i,t) = sum(b, g.l(t,i,b) ) ;
report_iso(%case%,'on-off',i,t) = x.l(t,i) ;
report_iso(%case%,'load',j,t) = sum(k, d.l(t,j,k) ) ;

report_wf(%case%,i,'profit') =
	sum((t,n,b)$map_G(n,i), price.l(t,n) * g.l(t,i,b) )
	- sum((t,b), c_G(t,i,b) * g.l(t,i,b) )
	- sum(t, c_on(i) * z_on.l(t,i) + c_off(i) * z_off.l(t,i) ) ;
report_wf(%case%,'all','profit') = sum(i, report_wf(%case%,i,'profit') ) ;

report_wf(%case%,'all','cons rent') = sum((t,n,j,k)$( map_D(n,j) ), ( u_D(t,j,k) - price.l(t,n) ) * d.l(t,j,k) ) ;

report_wf(%case%,'all','cong rent') = sum((l,t), - report_flow(%case%,l,t) * sum(n, Incidence(l,n) * price.l(t,n) ) ) ;

report_wf(%case%,'all','welfare') = report_wf(%case%,'all','profit') + report_wf(%case%,'all','cons rent') + report_wf(%case%,'all','cong rent') ;

report_wf(%case%,'all','obj') = obj.l ;
