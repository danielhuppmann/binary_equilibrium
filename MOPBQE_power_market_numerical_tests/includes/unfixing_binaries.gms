* unfix disjunctive constraints binary variables
x.up(t,i) = 1 ;
x.lo(t,i) = 0 ;
r_G_stat.up(t,i,b) = 1 ;
r_G_stat.lo(t,i,b) = 0 ;
r_G_max.up(t,i,b) = 1 ;
r_G_max.lo(t,i,b) = 0 ;
r_G_min.up(t,i) = 1 ;
r_G_min.lo(t,i) = 0 ;
r_F_pos.up(t,l) = 1 ;
r_F_pos.lo(t,l) = 0 ;
r_F_neg.up(t,l) = 1 ;
r_F_neg.lo(t,l) = 0 ;
r_delta_up.up(t,n) = 1 ;
r_delta_up.lo(t,n) = 0 ;
r_delta_lo.up(t,n) = 1 ;
r_delta_lo.lo(t,n) = 0 ;
