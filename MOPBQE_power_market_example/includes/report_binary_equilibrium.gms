$ONTEXT
Output report assignment specific to the binary equilibrium model

$OFFTEXT

report_wf(%case%,i,'kappa_1_p') =
	sum((t,b), kappa_1_plus.l(t,i) ) ;

report_wf(%case%,i,'kappa_1_m') =
	sum((t,b), kappa_1_minus.l(t,i) ) ;

report_wf(%case%,i,'kappa_0_p') =
	sum((t,b), kappa_0_plus.l(t,i) ) ;

report_wf(%case%,i,'kappa_0_m') =
	sum((t,b), kappa_0_minus.l(t,i) ) ;

report_wf(%case%,i,'start-up') =
	sum(t, c_on(i) * z_on.l(t,i) ) ;

report_wf(%case%,i,'ramp-down') =
	sum(t, c_off(i) * z_off.l(t,i) ) ;

report_wf(%case%,i,'compensation') =
	zeta.l(i) ;

report_wf(%case%,'all','compensation') = sum(i, report_wf(%case%,i,'compensation') ) ;

report_dev(%case%,i,'actual') = report_wf(%case%,i,'profit') ;

report_dev(%case%,i,phi) = 
	sum(t$( map_phi(phi,t) ), sum(b, beta_1.l(t,i,b) * g_max(t,i,b) ) - alpha_1.l(t,i) * g_min(t,i) )
	- sum(t$( ORD(t) > 1 AND map_phi(phi,t) > map_phi(phi,t-1) ), c_on(i) )
	- ( c_on(i) )$( x_init(i) = 0 AND map_phi(phi,'t1') = 1 )
	- sum(t$( ORD(t) > 1 AND map_phi(phi,t) < map_phi(phi,t-1) ), c_off(i) ) 
	- ( c_off(i) )$( x_init(i) = 1 AND map_phi(phi,'t1') = 0 ) ;

report_dev(%case%,i,'compensation') = zeta.l(i) ;

report_dev(%case%,i,'result') = 
	sum(phi$( sum(t$(x.l(t,i) = map_phi(phi,t) ), 1 ) = CARD(t) ), ORD(phi) ) ;