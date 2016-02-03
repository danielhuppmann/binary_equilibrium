$ONTEXT
Output report assignment specific to the binary equilibrium model

$OFFTEXT

$ONTEXT
report_wf(%model_case%,i,'kappa_1_p') =
	sum((t,b), kappa_1_plus.l(t,i) ) ;

report_wf(%model_case%,i,'kappa_1_m') =
	sum((t,b), kappa_1_minus.l(t,i) ) ;

report_wf(%model_case%,i,'kappa_0_p') =
	sum((t,b), kappa_0_plus.l(t,i) ) ;

report_wf(%model_case%,i,'kappa_0_m') =
	sum((t,b), kappa_0_minus.l(t,i) ) ;

report_wf(%model_case%,i,'start-up') =
	sum(t, c_on(i) * z_on.l(t,i) ) ;

report_wf(%model_case%,i,'ramp-down') =
	sum(t, c_off(i) * z_off.l(t,i) ) ;
$OFFTEXT

report_wf(%scenario%,%model_case%,i,'compensation','all') =
	zeta.l(i) ;

report_wf(%scenario%,%model_case%,'all','compensation','all') = sum(i, report_wf(%scenario%,%model_case%,i,'compensation','all') ) ;

report_dev(%scenario%,%model_case%,i,'profit (actual)') = report_wf(%scenario%,%model_case%,i,'profit','all') ;

report_dev(%scenario%,%model_case%,i,z)$( map_Z_I(i,z,'available') ) =
	sum(t$map_Z_I(i,z,t), sum(b, beta_1.l(t,i,b) * g_max(i,b) ) - alpha_1.l(t,i) * g_min(i) )
	- sum(t$map_Z_I(i,z,t), c_NL(i) )
	- sum(t$( ORD(t) > 1 AND map_Z(z,t) > map_Z(z,t-1) ), c_on(i) )
	- ( c_on(i) )$( x_init(i) = 0 AND map_Z(z,'t1') = 1 )
	- sum(t$( ORD(t) > 1 AND map_Z(z,t) < map_Z(z,t-1) ), c_off(i) )
	- ( c_off(i) )$( x_init(i) = 1 AND map_Z(z,'t1') = 0 ) ;

report_dev(%scenario%,%model_case%,i,'compensation') = zeta.l(i) ;

report_dev(%scenario%,%model_case%,i,'result') =
	sum(z$( sum(t$(x.l(t,i) = map_Z(z,t) ), 1 ) = CARD(t) ), ORD(z) ) ;

report_summary(%scenario%,%model_case%,'compensation') = report_wf(%scenario%,%model_case%,'all','compensation','all') / 1e6  ;

