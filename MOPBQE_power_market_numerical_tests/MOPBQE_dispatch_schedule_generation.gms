$TITLE Dispatch schedule generation script for equilibrium problems with binary decision variables

$ONTEXT
Daniel Huppmann, Sauleh Siddiqui
Johns Hopkins University, DIW Berlin, International Institute for Applied Systems Analysis (IIASA)
dhuppmann (at) jhu.edu, siddiqui (at) jhu.edu

Application of the exact reformulation of a binary Nash (quasi-) equilibrium model
to an example of a unit commitment problem in a DCLF power market

Numerical test instances, Appendix
Auxiliary routine to generate dispatch schedule option files for an arbitrary duration

Published as DIW Discussion Paper 1475, 2015 (http://diw.de/sixcms/detail.php?id=diw_01.c.502763.de)
and posted on arXive (http://arxiv.org/abs/1504.05894)

Please cite as:
Daniel Huppmann and Sauleh Siddiqui.
"An exact solution method for binary equilibrium problems with compensation
and the power market uplift problem", DIW Discussion Paper 1475, 2015.

This work is licensed under a Creative Commons Attribution 4.0 International License
-> http://creativecommons.org/licenses/by/4.0/

For more information and additional examples of binary equilibrium problems, please visit:
-> http://danielhuppmann.github.io/binary_equilibrium/

$OFFTEXT

$EOLCOM #

************************************************************************************************************************
*** Generate dispatch schedule option file (for arbitrarz duration                                               ***
************************************************************************************************************************

$SETGLOBAL duration '24'

file logfile / '' /;
put logfile ;

*** dispatch schedule set-up and mapping to time ***
* this method to automatically generate all dispatch schedule options and map them to the respective time steps (hours)
* is based on code provided by Ingmar Schlecht @ Universität Basel < ingmar dot schlecht at unibas dot ch >

$eval z_Card 2**%duration%
Set
	z dispatch schedules		/z1*z%z_Card%/    # dimension of set Z is 2^T to describe all dispatch schedule options
* note that z1 is the schedule of all zeroes (not generating in any period)
	z_subset(z)
	z_subset2(z)
	t time periods used in model	/ t1*t%duration%  /
;

Alias (z,zz) ;

Parameter
	map_Z(z,*)	mapping of on-off decision (dispatch schedule) to time
;

put_utility 'log' /'+++ Start creating dispatch schedules +++';

Parameter
	identity(z)
	z_counter(z)
	counter
	counter_up(z)
	counter_down(z)
	max_Z_up(z)
	max_Z_down(z)
	counter_init_up(z)
	counter_init_down(z)
	init_Z_up(z)
	init_Z_down(z)
;

counter = 0 ;

z_counter(z) = ORD(z) - 1 ;
identity(z) = 1 ;

*z_subset(z) = no ;
*z_subset(z)$( sum(t, map_Z(z,t) ) ) = yes ;
z_subset(z) = yes ;

map_Z(z_subset,t) = floor( mod( z_counter(z_subset) / ( power(2, ORD(t) - 1 ) ), 2 ) ) ;

execute_unload 'temp_data/dispatch_option_map_%duration%.gdx',  map_Z  ;
put_utility 'log' /'+++ Created and saved ' ( sum(z_subset, identity(z_subset) ) ):0:0 ' dispatch schedules! +++ ' ;

counter_init_up(z) = 1$( map_Z(z,'t1') = 1 ) ;
counter_init_down(z) = 1$( map_Z(z,'t1') = 0 ) ;
counter_up(z) = 0 ;
counter_down(z) = 0 ;
max_Z_up(z) = %duration% ;
max_Z_down(z) = %duration% ;

Parameter
	end_init(z)
	change_up(z)
	change_down(z)
;

end_init(z) = 0 ;
change_up(z) = 0 ;
change_down(z) = 0 ;

loop(t$( ORD(t) > 1 ),

	put_utility 'log' /'+++ Computed time parameters up to time step ' t.tl ' +++ ' ;
	$$INCLUDE includes/aux_computation_time.gms

* treat dispatch schedules that are still in the initial phase
	z_subset2(z) = no ;
	z_subset2(z)$( end_init(z) = 0 ) = yes ;

	counter_init_up(z_subset2)$( map_Z(z_subset2,t) = 1 AND map_Z(z_subset2,t-1) = 1 ) = counter_init_up(z_subset2) + 1 ;
 	counter_init_down(z_subset2)$(map_Z(z_subset2,t) = 0 AND map_Z(z_subset2,t-1) = 0 ) = counter_init_down(z_subset2) + 1 ;
	end_init(z_subset2)$( map_Z(z_subset2,t) ne map_Z(z_subset2,t-1) ) = 1 ;

* treat dispatch schedules that are already in the "normal" counting phase
	z_subset2(z) = no ;
	z_subset2(z)$( end_init(z) = 2 ) = yes ;

	counter_up(z_subset2)$( map_Z(z_subset2,t) = 1 AND map_Z(z_subset2,t-1) = 1 ) = counter_up(z_subset2) + 1 ;
	counter_down(z_subset2)$( map_Z(z_subset2,t) = 0 AND map_Z(z_subset2,t-1) = 0 ) = counter_down(z_subset2) + 1 ;

	change_up(z_subset2)$( map_Z(z_subset2,t) = 1 AND map_Z(z_subset2,t-1) = 0 ) = 1 ;
	change_down(z_subset2)$( map_Z(z_subset2,t) = 0 AND map_Z(z_subset2,t-1) = 1 ) = 1 ;

* treat dispatch schedules where the initial phase ends in time step t
	z_subset2(z) = no ;
	z_subset2(z)$( end_init(z) = 1 ) = yes ;

	init_Z_up(z_subset2) = counter_init_up(z_subset2) ;
	init_Z_down(z_subset2) = counter_init_down(z_subset2) ;
	counter_up(z_subset2)$( map_Z(z_subset2,t) = 1 ) = 1 ;
	counter_down(z_subset2)$( map_Z(z_subset2,t) = 0 ) = 1 ;
	end_init(z_subset2) = 2 ;

*treat dispatch schedules where the status changes in time step t
	z_subset2(z) = no ;
	z_subset2(z)$( change_up(z) ) = yes ;

	max_Z_down(z_subset2)$( max_Z_down(z_subset2) > counter_down(z_subset2) ) = counter_down(z_subset2) ;
	counter_up(z_subset2) = 1 ;

	z_subset2(z) = no ;
	z_subset2(z)$( change_down(z) ) = yes ;

	max_Z_up(z_subset2)$( max_Z_up(z_subset2) > counter_up(z_subset2) ) = counter_up(z_subset2) ;
	counter_down(z_subset2) = 1 ;

	change_up(z) = 0 ;
	change_down(z) = 0 ;
) ;

* assign and export all parameters
map_Z(z,'up') = max_Z_up(z) ;
map_Z(z,'down') = max_Z_down(z) ;
map_Z(z,'up_init') = init_Z_up(z) ;
map_Z(z,'down_init') = init_Z_down(z) ;

execute_unload 'temp_data/dispatch_option_map_%duration%.gdx',  map_Z, max_Z_up, max_Z_down,init_Z_up, init_Z_down  ;
put_utility 'log' /'+++ Computed time parameters for all ' ( CARD(z) ):0:0 ' dispatch schedules! +++' ;

************************************************************************************************************************
*** End of file                                                                                                      ***
************************************************************************************************************************
