* set initial values for binary variables for binary equilibrium formulation
r_G_stat.l(t,i,b)$( ORD(b) > 1 ) = 1 ;
r_G_stat.l(t,i,b)$( ORD(b) > 1 AND g.l(t,i,b) ) = 0 ;
r_G_stat.l(t,i,b)$( ORD(b) > 1 AND NOT x.l(t,i) ) = 1 ;

r_G_max.l(t,i,b) = 1 ;
r_G_max.l(t,i,b)$( x.l(t,i) AND ( CON_G_max.m(t,i,b) < 0 ) ) = 0 ;
r_G_min.l(t,i) = 1 ;
r_G_min.l(t,i)$( x.l(t,i) AND ( x.l(t,i) * g_min(i) eq g.l(t,i,'b1') OR CON_G_min.m(t,i) < 0 ) ) = 0 ;

r_G_max.l(t,i,b)$( CON_G_max.m(t,i,'b1') < 0 AND NOT x.l(t,i) ) = 0 ;
r_G_min.l(t,i)$( CON_G_max.m(t,i,'b1') eq 0 AND NOT x.l(t,i) ) = 0 ;

r_F_pos.l(t,l) = 1 ;
r_F_pos.l(t,l)$( CON_F_pos.m(t,l) < 0 ) = 0 ;
r_F_neg.l(t,l) = 1 ;
r_F_neg.l(t,l)$( CON_F_neg.m(t,l) < 0 ) = 0 ;

r_delta_up.l(t,n) = 1 ;
r_delta_up.l(t,n)$( CON_delta_up.m(t,n) < 0 ) = 0 ;
r_delta_lo.l(t,n) = 1 ;
r_delta_lo.l(t,n)$( CON_delta_lo.m(t,n) < 0 ) = 0 ;
