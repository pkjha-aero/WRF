
MODULE module_gen_act_disk

 USE module_dm

  real, parameter :: pi=4*atan(1.0)

  !!!!!!!!NM sign conventions: angles are + counterclockwise on xy plane
  !or in a plane looking from upwind at the front of the turbine
  !dau is + on right of x' or axial axis and - on left when looking from upwind
  !and dru is negative in direction of U from rotor plane and positive in
  !opposite direction; Turbine is assumed to rotate in ccw direction for
  !formulation so that t direction is positive ccw; x' is the same as n and is
  !positive in same direction as wind U. y' is positive in same direction as y
  !and offset by angle phi.

  CONTAINS

  SUBROUTINE gen_act_disk ( ru_tendf, rv_tendf, rw_tendf, &
                            wp_acc_amp,                   &
                            n_turbines,                   &
                            n_timeseries,                 & !NM
                            wp_acc_u, wp_acc_v, wp_acc_w, &
                            trbloc_ugx, trbloc_mlx,       & !NM	 
                            trbloc_vgy, trbloc_mly,       & !NM	
                            trbloc_ulx, trbloc_vly,       & !NM
                            upwnd_ugx, upwnd_vgy,         & !NM
                            upwnd_mgx, upwnd_mgy,         & !NM
                            trblsp_xs, trblsp_xe,         & !NM
                            trblsp_ys, trblsp_ye,         & !NM
                            trblsp_zs, trblsp_ze,         & !NM
                            x_turbine, y_turbine,         & !NM
                            hub_height, rotor_diameter,   & !NM
                            blade_length, phi_turbine,    & !NM
                            cutinspd, cutoutspd,          & !NM -new
                            xlat, xlon,                   & !NM -new u lat
                            itimestep, dt,                & !NM
                            yaw,                          & !NM
                            rst_cust,                     & !NM
                            ywert, v0t,                   & !NM
                            u_x, vw_x, v_y, uw_y,         & !NM
                            dau, dav, daw, dru, drv, drw, & !NM
                            ywacc_trb,                    & !NM
                            yetmct,                       & !NM
                            x_s_turbA, x_e_turbA,         & !NM
                            y_s_turbA, y_e_turbA,         & !NM
                            thrust, torque,               &
                            rotrate, power,               &
                            ! out1, out2, out3,           & !NM comment out
                            u, v, w,                      & 
                            muu, muv, mut,                & 
                            rdzw,  dx, dy,                &  
                            ht, alt,                      & !NM       
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            ips, ipe, jps, jpe, kps, kpe, &
                            its, ite, jts, jte, kts, kte  )

!-----------------------------------------------------------------------------		!!!!!!This subroutine calculates forces using the generalized actuator disk model. Finds the u v w points that need to be assigned an acceleration (force).
!
! Purpose: To calculate forces using the generalized actuator disk model. 
! Find the computational  u, v (and w) points that are to be assigned an 
! acceleration according to the Blade Element Momentum theory.
!
!  October, 2010: Branko Kosovic, NCAR
!
!=============================================================================

!NM  USE module_domain      !NM so know what domain is

  IMPLICIT NONE										!!!!This inhibits an old feature in fortran that treats by default all variables that start with the letters i,j,k,l,m and n as integers and all other variables as real arguments. Prevents confusion and should always be used. Kinda stupid
											!!!!here we declare the different input variables in gen_act_disk and their dimensions (ims:ime) etc. for each direction, u, v, and w.

!NM  INCLUDE 'mpif.h'    !NM I believe this includes mpi functions

!NM  TYPE(domain) , TARGET          :: grid  

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT(INOUT) &			!!!!These are the tendencies for navier stoke's equations. These terms are for each direction or velocity tendency times mass for units of kg/s^4
  :: ru_tendf    & ! u tendency * mass           (kg s-4)				!!!!tendency basically means it changes a value xy a certain amount, in this case it is changing u,v, and w. Not sure what ru etc. is du/dt. This is due to the disk model. It is flux because in WRF the variable are flux form which means that they are multiplied by the column pressure{force over area-or mass per unit area of column which fluctuates withtime} [(kg*m*s-2)/(m2)], so dudt (m*s-2) times that is in units of (kg*s-4). What does the r stand for Tendency goes into navier stoke's equations as forcing. Yes
   , rv_tendf    & ! v									!!!!!!!!not sure what f at end means
   , rw_tendf      ! w
  
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT(INOUT) &			!!!!these input values are the accelerations for u, v, w for the whole domain just like the tendencies above. These are for the Gaussian distribution of the force in the normal direction. Range from 0 to 1. Unitless
  :: wp_acc_u    & ! acceleration for u          ()				!!!!  Should be dimensionless I believe
   , wp_acc_v    & !                  v          ()
   , wp_acc_w      !                  w          ()

!NM  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT(INOUT) &
!NM  :: out1        & !storage variable for diagnostics     
!NM   , out2        &
!NM   , out3         

  REAL,                                          INTENT( IN  ) &  			!!!!amplitude of acceleration is for gaussian smoothing of the force so not at one point. It is just one value not for entire domain. ranges from 0 to 1 and affects wp_acc_u etc. above
  :: wp_acc_amp    ! amplitude of acceletration  ()			!!!dimensionless   from namelist

  INTEGER,                                       INTENT( IN  ) &  			!!!!This is the number of turbines in the domain I assume (also input)
  :: n_turbines &    ! number of turbines from namelist
   , n_timeseries    ! number of time series points for time average	

  INTEGER, DIMENSION( n_turbines ),  INTENT( INOUT ) & !NM 
  :: trbloc_ugx   &     !u-staggered global index of turbine x coord
   , trbloc_mlx   &	!mass-center local index for turbine x coord
   , trbloc_vgy   &	!v-staggered global index for turbine y coord
   , trbloc_mly   &	!mass-center local index for turbine y coord
   , trbloc_ulx   &	!u-staggered local index for turbine x coord
   , trbloc_vly   &	!v-staggered local index for turbine y coord
   , trblsp_xs   &	!starting local index for turbine span on x-cord 
   , trblsp_xe   &	!end local index for turbine span on x-cord
   , trblsp_ys   &	!start local index for turbine span y coord
   , trblsp_ye   &	!end local index for turbine span y coord
   , trblsp_zs   &	!start local index for turbine span z coord
   , trblsp_ze	 & 	!end local index for turbine span z coord
   , upwnd_ugx   &	!u-stagg global index of upwind turb x cor
   , upwnd_vgy	 &	!v-stagg global index for upwnd turb y cor
   , upwnd_mgx	 &	!mass-center global index for upwnd turb x
   , upwnd_mgy	 	!mass-center global index for upwnd turb y

  INTEGER,                                     INTENT( IN ) & !NM 
  :: itimestep     !timestep     

  REAL, DIMENSION( n_turbines ),             INTENT( INOUT ) &    !NM global 
  :: x_turbine  &  !x turbine location in meters
   , y_turbine  &  !y turbine location in meters
   , hub_height &  !hub height of turbine in meters
   , rotor_diameter &  !rotor diameter of a turbine
   , blade_length   &  !blade length of turbine
   , phi_turbine  &  !angle between x axis and x' (normal to rotor) wind blows
   		     !in +x' direction (determines orientation of turbine. ccw
		     !from +x axis is positive. Can interchange x and y
   , ywacc_trb &    !yaw error accumulation
   , cutinspd &     !cut in speed
   , cutoutspd 	    !cut out speed

  REAL, DIMENSION( n_turbines ),		INTENT(INOUT) & !NM 
  :: x_s_turbA  &  !NM start for turbine are calc in x dir
   , x_e_turbA  &
   , y_s_turbA  &
   , y_e_turbA       

  INTEGER, DIMENSION( n_turbines ),		INTENT(INOUT)  & !NM
  :: yetmct	 !NM yaw error time counter 

  LOGICAL, DIMENSION( n_turbines ),		INTENT(INOUT)  &  !NM 
  :: yaw    !NM tells us if turbine yawing .true. or not .false.

  LOGICAL,					INTENT(INOUT)  & !NM
  :: rst_cust !NM keeps track of restart
 
  REAL, DIMENSION( n_timeseries, n_turbines),		INTENT(INOUT)  & !NM
  :: ywert  & !NM keeps track of yaw errors in time series
   , v0t      !NM wind speed time serries

  REAL, DIMENSION( ims:ime ),                  INTENT( INOUT ) & !NM so can get memory location of turbine!its:ite ) &  !what does t mean (means tiles) use for local variables
  :: u_x         & !x coordinate of u-nodes (m) x-coordinates of u nodes (staggered) or in meters (distance), only one to !probably have all points used since staggered rest will be ite-1 or its+1 !(missing data then????)
   , vw_x          !NM                v,w nodes (m)

  REAL, DIMENSION( jms:jme ),                  INTENT( INOUT ) & !NM so can get memory location of turbine !jts:jte ) &
  :: v_y         & !y coordinate of v-nodes (m)
!y-coord for u,v,w (distance)
   , uw_y          !NM             u,w nodes (m)

  REAL, DIMENSION( ims:ime, n_turbines, jms:jme ), INTENT( INOUT ) &  !NM if we only need to change once why use INOUT ??????????????
  :: dau       & !normal distance from u-nodes to the rotor plane (m) !This is perpendicular distance from the u-nodes to the axial or n plane
   , dav       & !                    v                           (m)
   , daw         !                    w                           (m)

  REAL, DIMENSION( ims:ime, n_turbines, jms:jme ), INTENT( INOUT ) & !perpendicular distance from the u-nodes to the rotor plane
  :: dru       & !normal distance from u-nodes to the rotor plane (m) !this is per column since it does not change with height
   , drv       & !                    v                           (m) 
   , drw         !                    w                           (m)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),           INTENT( OUT ) &			!!This is the thrust and torque produced at each location. 
  :: thrust, torque, rotrate, power     !in 3D units are:
 
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT( IN  ) &
  :: u           & ! u-velocity                  (m s-1)				!!!This is just the velocities for the entire domain !!are these unstaggered?????????????/
   , v           & ! v
   , w             ! w

  REAL, DIMENSION( ims:ime, jms:jme ),           INTENT( IN  ) &			!!!!This is what we multiply with to get the flux (like in tendency)
  :: muu         & ! column pressure at u nodes  (kg m-1 s-2)				!!!at u nodes (because staggered) this is per unit area	
   , muv         & !                    v						!!!at v nodes
   , mut           !                    t						!!!at cell center or temperature location

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT( IN  ) &			!!!this is 1/grid spacing in vertical at each point in the domain (memory)
  :: rdzw          !inverse vertical grid spacing at w-nodes (m-1)			!!!at w nodes so staggered. do ims etc.

  REAL,                                          INTENT( IN  ) &			!!!horizontal grid spacing which does not vary so fine
  :: dx          & !grid spacing in x-direction  (m) 
   , dy          & !                y            (m)
   , dt            !NM temporal resolution (seconds)     

  REAL, DIMENSION( ims:ime, jms:jme ),           INTENT( IN  ) & !NM 
  :: ht         !NM elevation/ grid height cell center (m)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT( IN  ) & !NM
  :: alt        !NM, inverse air density (m3/kg)

  REAL, DIMENSION( ims:ime, jms:jme ), INTENT( IN ) & !NM 
  :: xlat      	& !NM latitude of u value
   , xlon	  !NM longitude of u value

  INTEGER,                                       INTENT( IN  ) &			!!The difference is between these. Memory, domain etc.
  :: ids, ide, jds, jde, kds, kde   &							!d means for domain (one for each nest) bottom will all have the same, for example ids for parent is 1 (intermediate has different)?? but in nest domain itself, ids is 1, but will be given parent features too. to interpolate with. 2 sets of numbers for each of these: parent domain and itself. This input is itself...They overlap since 3:1 ratio:::all of these are staggered not cell centered
   , ims, ime, jms, jme, kms, kme   &							!m means memory. This goes outside domain to get stencil (-4 into bnds) used for global variables --> all of these start at 1 for the domain. memory is for the processor. splits up ids ide domains etc. This is a subsection of ids ide for each processor
   , ips, ipe, jps, jpe, kps, kpe   &							!p means patch. Not used in parallel
   , its, ite, jts, jte, kts, kte							!t means tile. Used more for local variables since no need stencil, but still has same numbers. so will be shorter than memory because don't need stencil. subsection of ids, ide for each processor, its on next processor is not equal to ite on previous processor. There is no overlap!!!!

! LOCAL VARIABLES									!These are now local variables not taken in from outside or going outside

  REAL, DIMENSION( its:ite, kts:kte-1, jts:jte ) &					!z-coordinate of u and v for all domain (since changes) (distance) ;;added -1 to kte
  :: u_z         & !z coordinate of u-nodes (m)						! only kte-1 points actually used since has one less points since staggered
   , v_z           !                v       (m)						!these terms are just off the ground but should be elevation so should add terrain to these values ???
   
  REAL, DIMENSION( its-1:ite, kts:kte, jts-1:jte) & !NM for turbine location in neighboring cell !its-1:ite, kts:kte, jts-1:jte ) &	!z-coordinate for z, since in cell center, we expand by 2 to be able to interpolate to the staggered u node possibly, took away +1 from end of jte and ite
  :: w_z           !z coordinate of w-nodes  (m)					! uses kte and all points staggered

  REAL, DIMENSION( its:ite, kts:kte, jts:jte ) &				!I think this is normal distance from point to horizontal hub height plane, took out -1 and +1 in dimensions i and j
  :: dzu       & !normal distance from u-nodes to the hub height plane (m)
   , dzv       & !                    v                           (m)			!!+1 and -1 not actually used in this code
   , dzw         !                    w                           (m)			!! also top is not used for any of these kte-1, but no matter	

!NM  REAL, DIMENSION( its:ite, kts:kte, jts:jte ) &				!why did we expand the horizontal, took out -1 and +1 in dimensions i j
!NM  :: ru       & !distance from u-nodes to the nacelle            (m)			!this is distance to axial line and not plane, so it varies with height
!NM   , rv       & !                    v                           (m)			!!also top is not used for any of these kte-1 even for w
!NM   , rw         !                    w                           (m)			!!+1 and -1 not actually used in this code


  INTEGER :: i, j, k, n, z_turb_ind, z_turb_ind_c !, i_start, j_start, i_end, j_end				!random integers to be used for indices etc.

  !NM local variables for reading file
  CHARACTER*256 fname
  CHARACTER*512 inline
  INTEGER istat, lat_ind, lat_ind2, lon_ind, lon_ind2
  REAL lat,lon,hubheight,bldlngth,phiturb,y_turbine_temp,x_turbine_temp
  
  !NM REAL :: aa, ba, ca
  REAL :: ar, br, cr
  REAL :: phi
 !NM REAL :: x0u, x0v, x0w
!NM  REAL :: y0u, y0v, y0w

  REAL :: sigma
  REAL :: adenom, bdenom

  INTEGER :: naninf

!--------------------------------------------------------

  REAL, DIMENSION(2)      :: fnft
  REAL                    :: v0, hght_turb, wnd_dir, wndspd !NM 
  REAL                    :: rho
  REAL                    :: rpm, blades, bladelen, diameter, pitchdeg
  REAL			  :: radmin, radmax !NM max and min radius
  REAL                    :: rotor_radius
  REAL                    :: dscale, cscale
  REAL                    :: radius
  REAL                    :: fn, ft
  REAL                    :: yd, zd, zeta
  REAL                    :: fx, fy, fz
  REAL                    :: cnst
  REAL                    :: sgn
  REAL                    :: dum1, dum2, dum3  !NM
  INTEGER, DIMENSION(2)   :: rmin
  INTEGER                 :: imin, iwnd, iupwnd, idex, jdex    !NM added ints 

  REAL			  :: sigmar, bdenom2, adenom2  !NM AL sigma values
  REAL			  :: acc_u, acc_v, acc_w !NM temporary just so can use
  				!Nm wp_ for something else

  !NM Yaw local variables:
  REAL			  :: turb_phi, wnd_dir_phi, turb_phi_temp, &
  			     yaw_error, acc_err, yaw_thres,    &
			     yawspd, cutin, cutout,  &
			     trbhrz_tol, trbvrt_tol
  INTEGER		  :: divby, plhldr

  !NM upwind local variables:
  REAL			  :: phi_tem, D, m, y_up, x_up

  !NM drag coeff for hub
  REAL 			  :: Cdh, Cdt, Rtwrt, Rtwrb, Rtwr, hoff, tilt, drut, drvt, drwt !NM drag coef hub tower, radius twr top, base, current level, hub offset, turb tilt

!---------------------------------------------------------

  pitchdeg=  0.							!;;;;pitch angle in degrees (angle between relative velocity to blade and tangential axis or rotor plane
  dscale  =  1.
  cscale  =  1.
  rpm     = 16.							!;;;;;revolutions per minute; you will see we don't actually use this value in the code
  blades  =  3							!;;;;number of blades
  rho     =  1.225						!;;;;air density, I thought this changes depending on where location (for analysis assume incompressible) We should use rho in WRF for each point or at least at turbine location ????

! Kosovic, December 13, 2012 - modified
  iupwnd  = 0							!!! number of grid points in i direction upwind to use for v0 (may need changing???), use value at turbine JDM

  Cdh = 1.0 !NM setting drag at hub to be 1.0
  Cdt = 1.2 !NM drag coefficient for tower
  Rtwrt = 1.5 !NM radius is say 1.5 meters
  Rtwrb = 2.0 !NM radius of tower at base
  hoff = 1.0 !NM offset of hub from tower in meters
  tilt = 20.0*pi/180. !NM tilt angle of rotor plane in degrees, then change to radians

!---------------------------------------------------------


  !NM a few variable turbine parameters
  yaw_thres = 10.0*60.0*10.0**2  !NM 10 min* 60 sec/min * 10 deg squared so that
                                 !NM 10 degree error take 10 minutes to yaw
                                 !(pos) so need abs for ywacc_trb(n) comp.
				 !units: deg^2*sec
  yawspd = 1.0 !NM yaw speed of 1 degree per second
  trbhrz_tol = 0.71 !NM tolerance * rotor diameter around hub to calculate
  trbvrt_tol = 1.3 !NM tolerance * blade length around hub to calculate
 !print *, "how about here?"

IF (v0t(1,1) .eq. 0.0) then        !NM if this is the first time, we turn
       				       !NM turbine on, all procs go in here	
				       !NM use wind speed not timestep (restart)

  !print *, "In first time step"

! Find x,z,y locations for u, v, and w nodes throughout the domain

  u_x(ims:ime) = (/ (I, I=ims-1, ime-1) /)*dx    !sets all u_x values, easier than above, NM changed from it to im
  vw_x(ims:ime) = dx/2.0 + u_x(ims:ime)                      !sets all v_x values

  v_y(jms:jme) = (/ (I, I=jms-1, jme-1) /)*dy !set all v_y values, easier than above !NM changed jt to jm
  uw_y(jms:jme) = dy/2.0 + v_y(jms:jme)                   !set all u_y values, no need for 2 vars

  CALL nl_get_windturb_spec(1, fname ) !NM calls value of namelist term
    					 !NM windturb_spec and puts it in fname

  IF (TRIM(fname) .NE. 'none') THEN !NM if there is a file
	
	OPEN(file=TRIM(fname),unit=19,FORM='FORMATTED',STATUS='OLD',IOSTAT=istat) !NM open file
	!NM First term file= is name of file or its specifier
	!NM second unit= is the specifier for the file
	!NM IOSTAT= is positive integer when error occurs else zero
	!NM Status is file status at open, old means already exists
	!NM Form is if it is formatted. TRIM removes trailing blank characters
	IF ( istat .EQ. 0 ) THEN  !NM means no error in opening file
          ! first time count things up
	  n = 0  !NM starting index
	  DO WHILE (.true.) !NM do infinite loop
	  READ(19,'(A256)',END=30)inline   !NM reads file 19, format A256, end
	  !NM of file is 30, go to line 30 when end of file, inline is a line
	  !specifier I think
	   IF ( index(inline,'!') .EQ. 0 ) n = n+1 !NM if reach end of line,
	   !NM index says if ! in inline is 0 I think means new line This
	   !basically just counts number of turbines in files or no. lines 
	  ENDDO
 30       CONTINUE
	  IF (n_turbines .NE. n) THEN
		PRINT*,'n_turbines in namelist does not match number of &
		turbines in file windturb_spec'
      		STOP
	  ENDIF
	  REWIND(19) !NM means go back to beginning of file 19
	  i = 1 !NM assign value to i
	  DO WHILE (.true.)  !NM infinite do loop
	    READ(19,'(A256)',END=120)inline !NM read file again and go to 120 if
	    !NM end of file
	    IF ( i .LE. n_turbines .AND. index(inline,'!') .EQ. 0 ) THEN !NM
	    !if i (line number) is less than number of turbines and beginning of
	    !new line
	    READ(inline,*,ERR=130)lat,lon,hubheight,diameter,bldlngth,phiturb,cutin,cutout !NM tells us to read in each part of the line seperated by spaces and go to line 130 if there is an error
	        y_turbine(i) = lat      !NM assigning line values to each thing
		x_turbine(i) = lon	   
		hub_height(i) = hubheight
		rotor_diameter(i) = diameter
		blade_length(i) = bldlngth
		phi_turbine(i) = phiturb
		cutinspd(i) = cutin
		cutoutspd(i) = cutout
		i = i + 1      !NM counting lines
	    END IF
    	  ENDDO
120	  CONTINUE   !NM reached end of file again
 	  CLOSE(19)  !NM close file id 19
	  GOTO 150   !NM go to number 150
130	  CONTINUE   !NM error so close file and istat error give for file
 	  CLOSE(19) ! in case of error, close the unit
	  istat = 150150
          GOTO 150
      ENDIF
 150  CONTINUE
      IF ( istat .NE. 0 ) THEN
	 PRINT*,'UNABLE TO OPEN OR READ TURBINE FILE NAME'
	 STOP
        !WRITE(message,*)'Unable to open or read ',TRIM(fname),'. Proceeding without wind-turbine parameterization.'
        !CALL wrf_message(message)
        !IF ( istat .EQ. 150150 ) THEN
	!  PRINT*,'Possibly bad syntax in file'
	!  STOP
          !WRITE(message,*)'Perhaps bad syntax at line ',lineno,' of ',TRIM(fname)
          !CALL wrf_message(message)
        !ENDIF
    ENDIF
   ENDIF !NM end if for reading in turbine data


  DO n = 1, n_turbines !JDM April 16, 2013 ; loop hardcodes each turb loc

        IF (TRIM(fname) .EQ. "none" ) THEN   !NM if no file

    	x_turbine(n)           = 500.0   ! all of these are in meters !x_turb
    	y_turbine(n)           = 500.0   ! to put u in center 1000.0 !y_turb
    	hub_height(n)           = 80.0 !90.0     !80.0 !hub_height
    	rotor_diameter(n)           = 101.0 !96.0     !80.0   !rotor_diam
    	blade_length(n)           = 49.0 !45.0     !38.5    !blade lngth
    	phi_turbine(n)           = 80.0 !0.0                    !phi, initial, zero
    				               !means that wind comes from West,
					       !angle is positive ccw from
					       !positive x axis to positive x'
					       !axis or +y to +y' (rotor)
	cutinspd(n) = 3.5		       !NM cut-in speed	
	cutoutspd(n) = 20.			!NM cut out speed	

       ENDIF !NM end if for reading in turbine data

      !print*, "x_turbine"
      !print*, x_turbine
      !print*, "y_turbine"
      !print*, y_turbine

    !NM now need to find closest grid point for latitude and longitude
    IF (sum(abs(xlat(its:ite,jts:jte))) .NE. 0.0) then !NM if real case and all values not 0
	    IF ((sum(xlat(its,jts-1:jts)*10000)/2 .LE. y_turbine(n)*10000) .and. (sum(xlat(ite,jte:jte+1)*10000)/2 .GT. & !NM because ite is not equal to its of next processor, can't have both terms equal to or 2 processors might do this
       y_turbine(n)*10000) .and. (xlon(its,jts)*10000 .LE. x_turbine(n)*10000) .and. &
       (xlon(ite+1,jte)*10000 .GT. x_turbine(n)*10000)) then  !making sure in here but no overlap with other processor

        !print*,"are we even in here in terms of latitude or longitude"

	lat_ind = minloc(abs(xlat(its,jts:jte)*10000-y_turbine(n)*10000),1)+jts-1 !NM latitude of index
	IF (xlat(its,lat_ind)*10000 .GT. y_turbine(n)*10000) then
		lat_ind2=lat_ind-1 !use one before for ref
	ELSE
		lat_ind2=lat_ind+1 !use one after for ref
	ENDIF
	!NM getting y_turbine in meters from edge of domain it is in in y dir
	y_turbine_temp = abs(y_turbine(n)*10000-xlat(its,min(lat_ind,lat_ind2))*10000)/abs(xlat(its,lat_ind)*10000-xlat(its,lat_ind2)*10000)*abs(uw_y(lat_ind)-uw_y(lat_ind2))+min(uw_y(lat_ind),uw_y(lat_ind2))

    lon_ind = minloc(abs(xlon(its:ite,jts)*10000-x_turbine(n)*10000),1)+its-1 !NM latitude of index
	IF (xlon(lon_ind,jts)*10000 .GT. x_turbine(n)*10000) then
		lon_ind2=lon_ind-1 !use one before for ref
	ELSE
		lon_ind2=lon_ind+1 !use one after for ref
	ENDIF
	!NM getting x_turbine in meters from edge of domain it is in in x dir
	x_turbine_temp = abs(x_turbine(n)*10000-xlon(min(lon_ind,lon_ind2),jts)*10000)/abs(xlon(lon_ind,jts)*10000-xlon(lon_ind2,jts)*10000)*abs(u_x(lon_ind)-u_x(lon_ind2))+min(u_x(lon_ind),u_x(lon_ind2))



       ELSE   !NM if not in this tile
	 x_turbine_temp = -999   !NM give very small number
	 y_turbine_temp = -999
       ENDIF

       CALL wrf_dm_maxval_real(x_turbine_temp, idex, jdex)  !calls processor communications to get best value for turb loc
      !NM others will all be negative so get only valid x and y turbine
       CALL wrf_dm_maxval_real(y_turbine_temp, idex, jdex) 
       x_turbine(n) = x_turbine_temp !NM giving turbine location
       y_turbine(n) = y_turbine_temp  

       !print*, "xturb"
       !print*, x_turbine_temp
       !print*, "yturb"
       !print*, y_turbine_temp

    END IF

    x_s_turbA(n) = (x_turbine(n) - trbhrz_tol*rotor_diameter(n))     !NM extent of turbine in -x direction !!NM Could put in registry!!!!!
    x_e_turbA(n) = (x_turbine(n) + trbhrz_tol*rotor_diameter(n))     !NM extent of turbine in +x direction
    y_s_turbA(n) = (y_turbine(n) - trbhrz_tol*rotor_diameter(n))     !NM extent in -y
    y_e_turbA(n) = (y_turbine(n) + trbhrz_tol*rotor_diameter(n))     !NM extent in +y

    trbloc_ugx(n) = NINT(x_turbine(n)/dx)+1 !NM so that actual location turbine and round to nearest integer !minloc(abs(u_x-hc(1,n)),1)+its-1  !NM x index of turbine center or closest one (will be different from other tiles)???, staggered global location
    trbloc_ulx(n) = minloc(abs(u_x(its:ite)-x_turbine(n)),1)+its-1  !NM, closest turbine location on processor, staggered local

    trbloc_vly(n) = minloc(abs(v_y(jts:jte)-y_turbine(n)),1)+jts-1  !NM y index of turb closest on proc, staggered local
    trbloc_vgy(n) = NINT(y_turbine(n)/dy)+1 !NM so that actual location turbine, round, staggered, global location

    if (((v_y(trbloc_vly(n))+0.5*dy) .eq. y_turbine(n)) .and. (trbloc_vly(n)+1 .le. jte)) then  !NM so round up
	trbloc_vly(n) = trbloc_vly(n) + 1  !NM so we round up when at 0.5x, not
	!down because minloc gives first element of minimum but we want second
    end if

    if (((u_x(trbloc_ulx(n))+0.5*dx) .eq. x_turbine(n)) .and. (trbloc_ulx(n)+1 .le. ite)) then  !NM so round up
	trbloc_ulx(n) = trbloc_ulx(n) + 1  !NM so we round up when at 0.5x, not
	!down because minloc gives first element of minimum but we want second
    end if


    if (u_x(trbloc_ulx(n)) .gt. x_turbine(n)) then     !NM make sure right choice for v,w
        trbloc_mlx(n) = trbloc_ulx(n)-1
        trbloc_mlx(n) = max(trbloc_mlx(n),its) !NM so not out of bounds
     else
        trbloc_mlx(n) = trbloc_ulx(n)
     end if

     if (v_y(trbloc_vly(n)) .gt. y_turbine(n)) then   !NM make sure right choice for u,w
        trbloc_mly(n) = trbloc_vly(n)-1
        trbloc_mly(n) = max(trbloc_mly(n),jts) !NM so not out of bounds
     else
        trbloc_mly(n) = trbloc_vly(n)
     end if

     !print *, "trbloc_ugx for turb ", n, " is: ", trbloc_ugx(n)
     !print *, "trbloc_ulx for turb ", n, " is: ", trbloc_ulx(n)
     !print *, "trbloc_vgy for turb ", n, " is: ", trbloc_vgy(n)
     !print *, "trbloc_vly for turb ", n, " is: ", trbloc_vly(n)

     IF ((trbloc_ugx(n) .EQ. trbloc_ulx(n)) .AND. (trbloc_vgy(n) .EQ. trbloc_vly(n))) THEN    !NM nothing will happen if not turb center   
        
        hght_turb = hub_height(n) + ht(trbloc_mlx(n), trbloc_mly(n)) !NM turb hght with elevation
        
     ELSE  !NM if not turbine center
        
        hght_turb = -999.9 !NM Very small number  

     END IF

     CALL wrf_dm_maxval_real(hght_turb, idex, jdex)  !calls processor communications to get best value for hght      

     hub_height(n) = hght_turb  !setting hc(3, n) to be same for all processors 

     !!print *, "turb_elev:"
     !!print *, hc(3,n)

     !NM the following is to initialize certain registry variables
     yaw(n) = .false.  !NM so no yawing, all turb initial
     v0t(1,n) = 0.0  !NM so no yaw error at first
     ywacc_trb(n) = 0.0 !NM no yaw error accum at first
     yetmct(n) = 1 !NM setting initial yaw error time counter to 1

  END DO
   
END IF    !!!Ending first time only

!print *, "got here?"

IF ((rst_cust) .and. (v0t(1,1) .ne. 0.0)) then !NM if restart is true, so can get new values	!NM turbine on, all procs go in here

     !print *, "what abouts now?" 

     u_x(ims:ime) = (/ (I, I=ims-1, ime-1) /)*dx    !sets all u_x values, easier than above, NM changed from it to im
     vw_x(ims:ime) = dx/2.0 + u_x(ims:ime)                      !sets all v_x values

     v_y(jms:jme) = (/ (I, I=jms-1, jme-1) /)*dy !set all v_y values, easier than above !NM changed jt to jm
     uw_y(jms:jme) = dy/2.0 + v_y(jms:jme)                   !set all u_y values, no need for 2 vars

     !print *, "or now?"

     DO n = 1, n_turbines !loop through each turb 

     		x_s_turbA(n) = (x_turbine(n) - trbhrz_tol*rotor_diameter(n))     !NM extent of turbine in -x direction !!NM Could put in registry!!!!!
     		x_e_turbA(n) = (x_turbine(n) + trbhrz_tol*rotor_diameter(n))     !NM extent of turbine in +x direction
     		y_s_turbA(n) = (y_turbine(n) - trbhrz_tol*rotor_diameter(n))     !NM extent in -y
     		y_e_turbA(n) = (y_turbine(n) + trbhrz_tol*rotor_diameter(n))     !NM extent in +y

		trbloc_ulx(n) = minloc(abs(u_x(its:ite)-x_turbine(n)),1)+its-1  !NM, closest turbine location on processor
		trbloc_vly(n) = minloc(abs(v_y(jts:jte)-y_turbine(n)),1)+jts-1  !NM y index of turb closest on proc

       if (((v_y(trbloc_vly(n))+0.5*dy) .eq. y_turbine(n)) .and. (trbloc_vly(n)+1 .le. jte)) then  !NM so round up
	trbloc_vly(n) = trbloc_vly(n) + 1  !NM so we round up when at 0.5x, not
	!down because minloc gives first element of minimum but we want second
       end if

       if (((u_x(trbloc_ulx(n))+0.5*dx) .eq. x_turbine(n)) .and. (trbloc_ulx(n)+1 .le. ite)) then  !NM so round up
	trbloc_ulx(n) = trbloc_ulx(n) + 1  !NM so we round up when at 0.5x, not
	!down because minloc gives first element of minimum but we want second
       end if


    	if (u_x(trbloc_ulx(n)) .gt. x_turbine(n)) then     !NM make sure right choice for v,w
        	trbloc_mlx(n) = trbloc_ulx(n)-1
        	trbloc_mlx(n) = max(trbloc_mlx(n),its) !NM so not out of bounds
     	else
        	trbloc_mlx(n) = trbloc_ulx(n)
	end if
	if (v_y(trbloc_vly(n)) .gt. y_turbine(n)) then   !NM make sure right choice for u,w
		trbloc_mly(n) = trbloc_vly(n)-1
		trbloc_mly(n) = max(trbloc_mly(n),jts) !NM so not out of bounds
	else
		trbloc_mly(n) = trbloc_vly(n)
	end if
     END DO
END IF			!Nm end restart if


  !print *, "after first time step"
  !!print *, hc
  !print *, u_x(its:ite)
  !print *, v_y(jts:jte)

!  goto 1000

  !out1 = -999.   !initialize missing, per timestep
  !out2 = -999.
  !out3 = -999.

  wp_acc_u = 0.0       !NM cleaner way to initalize each value to zero
  wp_acc_v = 0.0       !NM
  wp_acc_w = 0.0       !NM

  !!!height needs to change every time step
  w_z(:,kts,:) = ht(its-1:ite,jts-1:jte) !NM base is terrain 0.0     !NM, initialize ground height to zero better

  DO k = kts+1, kte     !do loop through k

     w_z(:,k,:) = w_z(:,k-1,:) + 1.0/rdzw(its-1:ite,k-1,jts-1:jte) !easier way to write above, makes sure w_z dims match rdzw, NM

  END DO

  u_z = 0.5*( 0.5*(w_z(its-1:ite-1,kts+1:kte,jts:jte) + w_z(its:ite,kts+1:kte,jts:jte) ) &   !doing above but shorter, average in x then z
             + 0.5*(w_z(its-1:ite-1,kts:kte-1,jts:jte) + w_z(its:ite,kts:kte-1,jts:jte) ) )

  v_z = 0.5*( 0.5*( w_z(its:ite,kts+1:kte,jts-1:jte-1) + w_z(its:ite,kts+1:kte,jts:jte) ) &  !doing above but shorter, average in y then z
             + 0.5*(w_z(its:ite,kts:kte-1,jts-1:jte-1) + w_z(its:ite,kts:kte-1,jts:jte) ) )


!
!----------Loop over all the turbines

  DO n = 1, n_turbines								!!!here we are looping over the turbines; Better to loop instead of doing them all at once because we only have one processor so it does not matter?????????? I don't even think it is possible for me to do calculations on multiple turbines at the same time on one processor????????? Need loops.
     
     bladelen = blade_length(n)							!!!blade length stored for this turbine
     diameter = rotor_diameter(n)						!!!diameter also stored for this turbine
     cnst     = 1.0/rho  !JDM
     radmax= 0.5 * diameter							!!!set maximum radius to that of the blade
     radmin= radmax - bladelen							!!!minimum radius from blade to that of start of blade from rotor	

! dau,dav,daw - distance, in a horizontal plane, from the vertical plane in		!!!This is normal distance from axial plane to point
!               going through the turbine (nacelle) axis
! dru,drv,drw - distance, in a horizontal plane, from the vertical rotor plane		!!!distance from direction nodes to rotor plane

     !NM The following will set the limits on what to calculate so resources are
     !not wasted

     
   IF ((yaw(n)) .or. (v0t(1,n) .EQ. 0.0)) then !NM if yawing, or first time on
	 				!NM will be different number; also don't
					!need for restart, because can't change
					!number of grid points even if change
					!number of processors
     !!NM following is to pick proper upwind location for v0 and wind direction
     !and in bounds of domain
     phi_tem = phi_turbine(n)*pi/180.  !NM temporarily using phi from turbine

     do while (phi_tem .lt. 0)  !NM while phi is negative
    	phi_tem = phi_tem+2*pi  !NM making this positive
     end do
     do while (phi_tem .ge. 2*pi) !NM so within our range
     	phi_tem = phi_tem-2*pi  !NM making less
     end do

     D = (rotor_diameter(n)+hoff)/dx  !NM distance based on rotor diameter and grid
     			       !spacing. dx and dy assumed same
    !NMprint *, "phi_tem: ", phi_tem
     !NMprint *, "D= ", D
     !NMprint *, "trbloc_ugx(n): ", trbloc_ugx(n)
     !NMprint *, "trbloc_vgy(n): ", trbloc_vgy(n)

     if (NINT(phi_tem*180/pi*100) .eq. 9000 ) then !NM if slope is 90 deg 
     	upwnd_ugx(n) = trbloc_ugx(n) !NM x would be the same
	upwnd_vgy(n) = max(NINT(trbloc_vgy(n)-D),jds) !NM y would decrease (round to #)
     elseif (NINT(phi_tem*180/pi*100) .eq. 27000 ) then !NM if 270 deg
     	upwnd_ugx(n) = trbloc_ugx(n) !NM x same
	upwnd_vgy(n) = min(NINT(trbloc_vgy(n)+D),jde) !NM y would increase (min)
     elseif ((phi_tem .lt. pi/2) .or. (phi_tem .gt. 3*pi/2)) then !NM so -x
        m = tan(phi_tem) !NM getting the slope of the line

	x_up = ids !NM first x_up
	y_up = m*(x_up-trbloc_ugx(n))+trbloc_vgy(n) !NM finding y upwind for ids

	if (y_up .lt. jds) then !NM see if y makes sense
		x_up = (jds-trbloc_vgy(n))/m+trbloc_ugx(n) !new x_up
		y_up = jds !NM new y_up
	end if
	if (y_up .gt. jde) then !NM if y too large instead
		x_up = (jde-trbloc_vgy(n))/m+trbloc_ugx(n) !new x_up
		y_up = jde !NM new y_up
	end if

	!NMprint *, "m= ", m
	!NMprint *, "x_up: ", x_up
	!NMprint *, "y_up: ", y_up

	D = min(D,sqrt((trbloc_ugx(n)-x_up)**2+(trbloc_vgy(n)-y_up)**2)) !NM get
								!NM smaller D

   	upwnd_ugx(n) = NINT((-sqrt(D*D+m*m*D*D)+trbloc_ugx(n)+m*m*trbloc_ugx(n))/(1+m*m)) !NM smaller
     	upwnd_vgy(n) = NINT((-m*sqrt((1+m*m)*D*D))/(1+m*m)-m*trbloc_ugx(n)+m*trbloc_ugx(n)/(1+m*m)+m**3*trbloc_ugx(n)/(1+m*m)+trbloc_vgy(n)) !NM corresponding y

	!NMprint *, "D= ", D
	!NMprint *, "upwnd_ugx: ", upwnd_ugx(n)
	!NMprint *, "upwnd_vgy: ", upwnd_vgy(n)

     else !NM for rest	
	m = tan(phi_tem) !NM getting the slope of the line
	
	x_up = ide !NM first x_up
	y_up = m*(x_up-trbloc_ugx(n))+trbloc_vgy(n) !NM finding y upwind for ids

	if (y_up .lt. jds) then !NM see if y makes sense
		x_up = (jds-trbloc_vgy(n))/m+trbloc_ugx(n) !new x_up
		y_up = jds !NM new y_up
	end if
	if (y_up .gt. jde) then !NM if y too large instead
		x_up = (jde-trbloc_vgy(n))/m+trbloc_ugx(n) !new x_up
		y_up = jde !NM new y_up
	end if

	!NMprint *, "m= ", m
	!NMprint *, "x_up: ", x_up
	!NMprint *, "y_up: ", y_up

	D = min(D,sqrt((trbloc_ugx(n)-x_up)**2+(trbloc_vgy(n)-y_up)**2)) !NM get
								!NM smaller D

   	upwnd_ugx(n) = NINT((sqrt(D*D+m*m*D*D)+trbloc_ugx(n)+m*m*trbloc_ugx(n))/(1+m*m)) !NM larger
     	upwnd_vgy(n) = NINT((m*sqrt((1+m*m)*D*D))/(1+m*m)-m*trbloc_ugx(n)+m*trbloc_ugx(n)/(1+m*m)+m**3*trbloc_ugx(n)/(1+m*m)+trbloc_vgy(n)) !NM corresponding y

	!NMprint *, "D= ", D
	!NMprint *, "upwnd_ugx: ", upwnd_ugx(n)
	!NMprint *, "upwnd_vgy: ", upwnd_vgy(n)

     end if

     !NM diff_trbloc_mgx = min(max(NINT((x_turbine(n)-dx/2)/dx),1),ide)-trbloc_ugx(n) !NM difference btwn u and m loc turbine
     !NM diff_trbloc_mgy = min(max(NINT((y_turbine(n)-dy/2)/dy),1),jde)-trbloc_vgy(n) !NM difference btwn v and m loc

     upwnd_mgx(n) = max(ids,upwnd_ugx(n)+max(NINT((x_turbine(n)-dx/2)/dx),1)-trbloc_ugx(n))        !diff_trbloc_mgx !NM upwnd_mgx
     upwnd_mgy(n) = max(jds,upwnd_vgy(n)+max(NINT((y_turbine(n)-dy/2)/dy),1)-trbloc_vgy(n))       !diff_trbloc_mgy !NM upwnd_mgy

  END IF  !NM ends if statement for yaw or first time
  

     !print *, "u_x(its): "
     !print *, u_x(its)
     !print *, "x_e_turbA: "
     !print *, x_e_turbA(n)
     !print *, "v0t(1,n)"
     !print *, v0t(1,n)


IF ((u_x(its) .lt. x_e_turbA(n)) .and. (vw_x(ite) .gt. x_s_turbA(n)) .and. (v_y(jts) .lt. y_e_turbA(n)) .and. (uw_y(jte) .gt. y_s_turbA(n))) then !NM, making sure the turbine area is in this tile

     !print *, "Went through"
     !print *, v0t(1,n)

   IF ((v0t(1,n) .EQ. 0.0) .or. (rst_cust)) THEN      !If this is the first time, so no
	   						  !no timestep, or if
							  !restarted run
     !print *, "in here now"
     trblsp_xs(n) = minloc(abs(vw_x(its:ite)-x_s_turbA(n)),1)+its-1 !NM start index for turbine in x dir i_end??? To make sure we are covering all necessary stuff
     trblsp_xe(n) = minloc(abs(u_x(its:ite)-x_e_turbA(n)),1)+its-1 !NM end index for turbine in x direction   
!NM don't think this is needed? who cares?     i_te = min(i_te,ide-1) !NM so not
!edge
     trblsp_ys(n) = minloc(abs(uw_y(jts:jte)-y_s_turbA(n)),1)+jts-1 !NM start index for turbine in y dir !uw more conservative so is vw
     trblsp_ye(n) = minloc(abs(v_y(jts:jte)-y_e_turbA(n)),1)+jts-1 !NM end index for turbine in y dir
!NM don't think this is needed? who cares?    j_te = min(j_te,jde-1) !NM so not
!edge

!NM-no need since more accurate     hc(3,n) = hc(3,n) + ht(turb_loc(2,1,n), turb_loc(2,2,n))  !NM gives hub height with elevation, height is only closest in processor to turbine, so may need change such as proc communication.???????? Maybe use IF statement below (for turb center) for this to happen, but then we have to do this on all???? proper stagger

     !trblsp_zs(n) = minloc(abs(w_z(trbloc_mlx(n),:,trbloc_mly(n))-(hub_height(n)- trbvrt_tol*bladelen)),1)+kts-1 !NM z start
     trblsp_ze(n) = minloc(abs(w_z(trbloc_mlx(n),:,trbloc_mly(n))-(hub_height(n)+ trbvrt_tol*bladelen)),1)+kts-1 !NM z end
     !trblsp_zs(n) = max(trblsp_zs(n),kts+1)  !NM so not first point
     trblsp_ze(n) = min(trblsp_ze(n),kte-1) !NM so not end point
     trblsp_zs = kts !NM first point for tower. maybe change later	

     !!!Setting the values to the final value, also declare these local
     !variables only for first time

 END IF          !NM ends the if statement for first time

 IF ((yaw(n)) .or. (v0t(1,n) .EQ. 0.0)) then !NM if yawing, or first time on
	 							 !NM will be
								 !different
     !print *, "calculating distance"
     CALL calc_distance( x_turbine(n), y_turbine(n), phi_turbine(n), n, n_turbines, & !!!I guessthis calculates the distance from the rotor plane and axial plane to point 
      			 ims, ime, jms, jme, & !NM needed
                         trblsp_xs(n), trblsp_xe(n), trblsp_ys(n), trblsp_ye(n),                &   !NM added for less calculations
                         u_x, vw_x, v_y, uw_y,                  &   !NM, add
                         dau, dav, daw, dru, drv, drw )
END IF


   !NM Because location hub could vary:
     IF ((trbloc_ugx(n) .EQ. trbloc_ulx(n)) .AND. (trbloc_vgy(n) .EQ. trbloc_vly(n))) THEN    !NM nothing will happen if not turb center 
        z_turb_ind = minloc(abs(w_z(trbloc_mlx(n),trblsp_zs(n):trblsp_ze(n),trbloc_mly(n))-hub_height(n)),1)+trblsp_zs(n)-1
!NM z index of turbine, assumes not much variation in height of turbine area,
!use v and u so cell centered for w
      if (w_z(trbloc_mlx(n),z_turb_ind,trbloc_mly(n)) .gt. hub_height(n)) then
         z_turb_ind_c = z_turb_ind-1 !NM so for u, v same level, since w is staggered
      else
         z_turb_ind_c = z_turb_ind   !Make zturbindc a variable tooo!!!!!
      end if


	!NM make empty v0, wndspd, wnd_dir etc.
	v0=-999
	wnd_dir=-999
	wndspd=-999


     ELSE                         !NM does not contain turbine center
        z_turb_ind   = -999       !NM Missing value
        z_turb_ind_c = -999       !NM Missing value 
        v0 = -999              !silly number for missing
        wnd_dir = -999          !NM silly number for wind direction
	wndspd = -999	
     END IF !NM end if center

  ELSE  !NM if turbine are is not in here area

	z_turb_ind = -999 !NM missing
      	z_turb_ind_c = -999 !NM missing level	
        v0 = -999  !NM this is a silly number
        wnd_dir = -999          !NM silly number of wind direction
	wndspd = -999

END IF !NM finishig this option up for turbines, end fo statement that turbines
	!Nm not in here

  CALL wrf_dm_maxval_integer(z_turb_ind_c, idex, jdex)  !NM gets z for turbine 	

  IF ((its .le. upwnd_ugx(n)) .and. (ite .ge. upwnd_ugx(n)) .and. (jts .le. upwnd_vgy(n)) .and. (jte .ge. upwnd_vgy(n))) then  !NM tile where upwind located

        !NMprint *, "Contains upwind location"
        
        phi =  phi_turbine(n)/180.*pi   !NM converting phi to radians
        !NM finds the actual v0 in direction of +x', because phi is transform
	!angle between coordinate systems x and x'; making u in direction of +x'
	!and v in direction of +x'; x' positive in direction of wind blowing or
	!downwind of rotor: +x*cos(phi)+y*sin(phi)=+x'

        v0 = u(upwnd_ugx(n),z_turb_ind_c,upwnd_mgy(n))*cos(phi)*cos(tilt)+v(upwnd_mgx(n),z_turb_ind_c,upwnd_vgy(n))*sin(phi)*cos(tilt)
        wndspd = sqrt((u(upwnd_ugx(n),z_turb_ind_c,upwnd_mgy(n)))**2+(v(upwnd_mgx(n),z_turb_ind_c,upwnd_vgy(n)))**2) !NM windspeed

        wnd_dir = (atan2(u(upwnd_ugx(n),z_turb_ind_c,upwnd_mgy(n)),v(upwnd_mgx(n),z_turb_ind_c,upwnd_vgy(n))) & !NM wind direction at turbine hub
                   *180.0/pi+180.0)  !NM so we get dir come frm in deg 

   END IF !NM ends location upwind 	  


  CALL wrf_dm_maxval_real(v0, idex, jdex)  !calls processor communications to
                                            !get best value for v0
  CALL wrf_dm_maxval_real(wnd_dir, idex, jdex) !NM calls all processors

  CALL wrf_dm_maxval_real(wndspd, idex, jdex) !NM call all procs for wndspd

   !NM adding to v0t term					   
   IF (itimestep .le. n_timeseries) then !NM if not full					   
   	v0t(itimestep,n) = v0            !NM storing v0 for the future or testing, all procs
   ELSE
	v0t(1:n_timeseries-1,n) = v0t(2:n_timeseries,n) !NM shifting times

	v0t(n_timeseries,n) = v0  !NM setting new wind speed
   END IF


   !NM getting average value for v0 
   IF (itimestep .lt. n_timeseries) then !NM if not full					   
   	v0 = SUM(v0t(1:itimestep,n))/itimestep    !NM getting average for v0
   ELSE
	v0 = SUM(v0t(:,n))/n_timeseries !NM getting average for v0
   END IF

   !NM setting up yaw code:, This is all processors!!!!!!
 IF (.not. yaw(n)) then !NM this makes sure turbine is not yawing   
  IF ((wndspd .ge. cutinspd(n)) .and. (wndspd .le. cutoutspd(n))) then !NM if above cutin speed for instantaneous 
   !we are setting reference to be from positive x axis to wind direction
   !wind is blowing from	  
   turb_phi = phi_turbine(n)  !NM turbine position as function of phi 
   wnd_dir_phi = 270.0 - wnd_dir   !NM wind direction as related to phi turbine postion so 90 degrees wind to rotor
   !NM we want both turb_phi and wnd_dir_phi to be >= 0  and < 360 than so
   !we do the following:
   IF (abs(turb_phi) .ge. 360.0) then    !NM making sure that not over +/-360
      divby = aint(abs(turb_phi)/360.0)  !NM rounds number down so that we know number 360's
      turb_phi_temp = abs(turb_phi)-divby*360.0  !this will give constrained value below 360
   ELSE
      turb_phi_temp = abs(turb_phi)  !NM just the same
   END IF
   IF (turb_phi .lt. 0) then  !NM see if negative
      turb_phi = 360.0-turb_phi_temp  !NM so that wind dir is btwn 0 and less than 360
   ELSE
      turb_phi = turb_phi_temp  !NM otherwise just regular
   END IF

   IF (abs(wnd_dir_phi) .ge. 360.0) then    !NM making sure that not over 360
      divby = aint(abs(wnd_dir_phi)/360.0)  !NM rounds number down so that we know number 360's
      turb_phi_temp = abs(wnd_dir_phi)-divby*360.0  !this will give constrained value
   ELSE
      turb_phi_temp = abs(wnd_dir_phi)   !NM just the same
   END IF
   IF (wnd_dir_phi .lt. 0) then  !NM see if negative
      wnd_dir_phi = 360.0-turb_phi_temp  !NM so that wind dir is btwn 0 and less than 360
   ELSE
      wnd_dir_phi = turb_phi_temp  !NM just regular like otherwise
   END IF

   yaw_error = turb_phi-wnd_dir_phi   !NM getting the yaw error, phi (turb loc) must change by - yaw error
   !NM we don't want to yaw unnecessarily so we want yaw error to be less than
   !or equal to 180 degrees
   IF (abs(yaw_error) .gt. 180.0) then !NM this is if yaw greater than 180.0
      IF ((yaw_error) .lt. 0.0) then !NM if negative
         yaw_error = (360.0+yaw_error) !NM if negative need to switch direction to positive
      ELSE
         yaw_error = -(360.0-yaw_error) !NM switch signs so go opposite way
      END IF
   END IF
  ELSE !NM if not above cutin speed
   yaw_error = 0.0    !NM do not record errors if below cut-in speed
   ywacc_trb(n) = 0.0 !NM no need to record accumulation (start from zero again)
  END IF

   IF (yaw_error .lt. 0.0) then !NM this means that yaw error negative
      acc_err = -(yaw_error)**2 * dt  !NM this is integrating the square error per timestep and preserving the sign
   ELSE
      acc_err = (yaw_error)**2 * dt !NM no need for negative here
   END IF

   IF (yetmct(n) .le. n_timeseries) then !NM if less than or equal to min step

	  plhldr = yetmct(n) !NM less than minimum step
   ELSE
	  plhldr = n_timeseries
   END IF 

   !NM conditions if wind is coming from behind
   IF (sign(1.,yaw_error) .ne. sign(1.,ywert(plhldr-1,n))) then !NM diff sign
!JMTN changed 1 to 1.
     !NM both are errors greater than 90
     IF ((abs(yaw_error) .gt. 90.) .and. (abs(ywert(plhldr-1,n)) .gt. 90.)) then
	     acc_err = -acc_err  !NM accumulation error opposite sign
	     yaw_error = yaw_error-sign(360.,yaw_error) !NM make same sign
     END IF
   END IF

      ywacc_trb(n) = ywacc_trb(n)+acc_err !NM add error, could be + or -
      					  !NM no need for if statement because
					  !ywacc_trb(n) should be zero at first
					  !anyway, This is for all processors

   IF (yetmct(n) .le. n_timeseries) then   !NM if less than or equal minimum step
         ywert(yetmct(n),n) = yaw_error   !NM recording yaw errors according to timestep at first
	 yetmct(n) = yetmct(n)+1   !Nm adding counter
   ELSE                                 !NM if have full load
         ywert(1:(n_timeseries-1),n) = ywert(2:n_timeseries,n) !NM making room for latest time
         ywert(n_timeseries,n) = yaw_error            !NM entering last value (most recent)
   END IF


   IF (abs(ywacc_trb(n)) .ge. yaw_thres) then !NM if accum error is greater than or

      IF (yetmct(n) .lt. n_timeseries) then  !NM not all the values are filled in for first 10 minutes keeping
         yaw_error = SUM(ywert(1:yetmct(n),n))/(yetmct(n)-1)   !NM average yaw error per time step          
      ELSE                             !NM array is full of values
         yaw_error = SUM(ywert(:,n))/n_timeseries  !NM average
      END IF

      IF (abs(yaw_error) .gt. 180.) then !NM if abs value of error more than 180
	 yaw_error = yaw_error-sign(360.,yaw_error) !NM switch direction
      END IF

        yaw(n)       = .true.                  !NM this is meant to indicate yawing
        phi_turbine(n) = phi_turbine(n)-sign((yawspd*dt),yaw_error)  !NM yawing at 1 degree per second
						  !NM sign takes sign second
						  !value and puts it in front of
						  !first
        ywacc_trb(n) = yaw_error-sign((yawspd*dt),yaw_error) !NM taking away from
                                                        !total yaw error

   END IF  !NM this ends accumulation threshold statement 
  ELSE     !NM if turbine is yawing
	 IF (abs(ywacc_trb(n)) .lt. (yawspd*dt)) then  !NM error is less than tol
		yaw(n) = .false. 	!NM finished yawing
	        ywacc_trb(n) = 0.0 !NM reset yaw accumulation
		ywert(:,n) = 0.0 !NM set yaw average to zero so old values
				 !NM don't influence, need counter
		yetmct(n) = 1      !NM set yaw error time counter to 1
	 ELSE
		phi_turbine(n) = phi_turbine(n)-sign((yawspd*dt),ywacc_trb(n))  !NM yawing at 1 degree per second
	 	ywacc_trb(n) = ywacc_trb(n)-sign((yawspd*dt),ywacc_trb(n)) !NM taking away yaw, 1*dt is yaw rate each timestep
	END IF
	
  END IF  !NM end stuff for turbine yawing, all turbines


!NM making sure we are withing cutin and cutout speed  
IF ((v0 .ge. cutinspd(n)) .and. (v0 .le. cutoutspd(n))) then

  IF ( v0 .lt. 0. ) THEN			! if for some reason v0 is less than zero we make it zero. v0 should just be magnitude coming into blades, always positive???
        v0 = 0.
     ENDIF
    !NM IF ( v0 .gt. 11.2 ) THEN			! if it is greater than 11.2 m/s we make it 11.2 ; We do this because of the power limit. Power curve should be limited (need to change eventually ??)
    !NM    v0 = 11.2
    !NM ENDIF

     rpm   = calc_rpm( v0 )			! here we calculate the revolutions per minute based on the upwind wind speed
     pitchdeg = calc_pitch( v0 )		! we also calculate the pitch degree based on upwind wind speed, this is how much we pitch or feather the blade 

     !NMprint *, "pitch: ", pitchdeg

     rotrate = 0. !2.*pi*rpm/60.    !NM simplifying above
     thrust = 0.
     torque = 0.
     power = 0.


  IF ((u_x(its) .lt. x_e_turbA(n)) .and. (vw_x(ite) .gt. x_s_turbA(n)) .and. (v_y(jts) .lt. y_e_turbA(n)) .and. (uw_y(jte) .gt. y_s_turbA(n))) then !NM, making sure the turbine area is in this tile

 dzu(trblsp_xs(n):trblsp_xe(n),trblsp_zs(n):trblsp_ze(n),trblsp_ys(n):trblsp_ye(n)) = u_z(trblsp_xs(n):trblsp_xe(n),trblsp_zs(n):trblsp_ze(n),trblsp_ys(n):trblsp_ye(n)) - hub_height(n) !NM shortcut from above loop
 dzv(trblsp_xs(n):trblsp_xe(n),trblsp_zs(n):trblsp_ze(n),trblsp_ys(n):trblsp_ye(n)) = v_z(trblsp_xs(n):trblsp_xe(n),trblsp_zs(n):trblsp_ze(n),trblsp_ys(n):trblsp_ye(n)) - hub_height(n) !NM shortcut from above loop
 dzw(trblsp_xs(n):trblsp_xe(n),trblsp_zs(n):trblsp_ze(n),trblsp_ys(n):trblsp_ye(n)) = w_z(trblsp_xs(n):trblsp_xe(n),trblsp_zs(n):trblsp_ze(n),trblsp_ys(n):trblsp_ye(n)) - hub_height(n) !NM shortcut from above loop 

     rotor_radius=0.5*rotor_diameter(n)							!!!Calculating the rotor radius for the specific turbine

     phi =  phi_turbine(n)/180.*pi   !NM converting phi to radians                  
     !NM old when we had theta!converting theta to radians ;;theta is positive angle from negative rotor plane (-y') to positive x axis in WRF. ;;;Since we don't know direction of wind, could be shortest angle and follow the sign convention to set arbitrary positive direction for x' with phi???? make sure to follow that positive is counterclockwise from -y' or y' since don't know direction of flow. of course we want to go to x
     !NM no more need since no theta.....     phi   =  0.5*pi-theta                      !getting phi because right angles from theta like in notes; Is positive from positive x axis to positive x' or axial axis of turbine

! Kosovic, December 14, 2012 - modified
     sigma   = (abs(dx*cos(phi))+abs(dy*sin(phi))) !NM so normal for rotor plane sqrt(dx*dy)	!!!This is the standard deviation, or if it is large gaussian is spread out more, if small not so spread out
     adenom  = sigma*sqrt(2.0*pi)							!!want integral to be 1 for Gaussian function so wp_acc_amp must also be 1 ;;accel will have units of 1/m
     bdenom  = 2.*sigma*sigma								!!determines spread of gaussian

      !NM sigmar normal to rotor plane; this is standard deviation; or if
      !it is large, the Gaussian is spread out more
      sigmar = abs(dx*cos(phi)*cos(tilt))+abs(dy*sin(phi)*cos(tilt)) !NM
      adenom2 = sigmar*sqrt(2.0*pi) !NM want integral to be 1 for gaussian

     sigma = (abs(dx*cos(phi))+abs(dy*sin(phi))) !NM normal to rotor plane This is standard deviation. If larger then spread out more
     adenom = sigma*sqrt(2.0*pi) !NM want integral to be 1 for gaussian
     bdenom = 2.*sigma**2 !NM spread of 1-D gaussian 

     DO j = trblsp_ys(n), trblsp_ye(n)          !NM j_start, j_end

        DO k = trblsp_zs(n), trblsp_ze(n)    !NM kts+1, kte-1 			!!! go through k's but start at first point above and end at one below top (not in radius so who cares (force will be zero

           DO i = trblsp_xs(n), trblsp_xe(n)  !NM i_start, i_end		!!! do x loop

	      !NM in direction of +x'
              v0=u(i,k,j)*cos(phi)*cos(tilt)+v(i,k,j)*sin(phi)*cos(tilt)	!!!transforms u and v into axial direction so we get the v0 but for whole domain (This might need to be changed so that we actually use an upwind velocity ???). So v0 does have direction associated with it. It is based on phi. n positive if going towards north East. Negative if going towards southwest. !!should be positive or just magnitude of incoming wind speed ;;could have direction due to phi if in positive x' direction or negative so may need to abs value in calc_fnft ??What we may need to do is make v0 positive and assume that stuff is coming in positive direction, but then switch the signs at the end to make it all opposit (even z dir since dependent on ft force, which would be opposite), would just need to make fn and ft opposite signs when in fnft function.

!------ X ----									!!!for x direction

              zd = dzu(i,k,j)							!!this is height from u node to hub height (+ if above rotor and - if below)
              yd = dau(i,n,j) !calc_yprime(aa,ba,ca,x0u,y0u)				!!this is normal distance from point to axial plane like above for whole domain !!? I think this calculates distance from point to axis turbine with + sign if point is to the right of the turbine axis and - sign if point is to left of turbine axis ; this is all looking at turbine from upwind. y' is in xy plane but goes through turbine rotor plane too (intersection). It is positive and offset by positive phi angle from y axis in WRF

              radius=sqrt( yd*yd + zd*zd )					!!this is the radius calculated using values above at all points (radius from axial plane at all points)

              !!print *, yd
              !!print *, zd

              rho = (1.0/alt(i,k,j)+1.0/alt(i-1,k,j))/2.0  !NM using u-node density
	      cnst = 1.0/rho  !NM use right constant

	      !NM for turbine hub:
	      IF (zd .le. -(radmin)) then !NM tower below hub

		 !NM finding Rtwer
	     	 Rtwr = abs(zd)*(Rtwrb-Rtwrt)/hub_height(n)+Rtwrt !NM linear	 

		  IF (abs(yd) .le. Rtwr) then !NM tower below hub; dau needs to be
		      			   !less than tower radius
	      	  fn = 0.5*rho*v0*v0*Cdt !NM force per unit area for tower
		  ft = 0.0 !NM no tangential force
		  acc_u = wp_acc_amp/adenom*exp(-(dru(i,n,j)**2)/bdenom) !NM 1-D
		  							!Gaussian
		  fx = fn*cos(phi) !NM to x direction
		  ru_tendf(i,k,j) = ru_tendf(i,k,j) - muu(i,j)*acc_u*cnst*fx !NM
	          END IF

	      END IF

	      IF ((radius .le. radmin) .and. (radius .gt. -0.001)) then !hub

		  fn = 0.5*rho*v0*v0*Cdh  !NM only need force per unit area
		  ft = 0.0 !NM no tangential force
		  !NM including off set of hub from tower hoff in + dru
		  !direction or opposite of wind direction U
		  acc_u =  wp_acc_amp/adenom*exp(-((dru(i,n,j)-hoff)**2)/bdenom) !NM 1-D gaussian

		  fx = fn*cos(phi) !+ft*sin(zeta)*sin(phi)				!!now transform into WRF x direction like in note;; looks like +x=+x'*cos(phi)-y'*sin(phi) ;;;now it gets pointed the right direction in fx, either positive or negative because of phi and zeta. This is force on turbine in x dir. ft now assumes rotation of turbine is clockwise if viewed from upwind and sign in front of ft would change if rotation is counter-clockwise. ft iteslf is assumed counterclockwise. !!!!NM If turbine were spinning counter-clockwise like assumed for the calculation of zeta there would need to be a negative sign in front of ft because in the +y' direction sin(phi) gives gives the opposite sign of the positive x direction. +y'sin(phi) gives answer in -x direction.

		  ru_tendf(i,k,j) =   ru_tendf(i,k,j) - muu(i,j)*acc_u*cnst*fx	!???we subtract because we want force on air so we take the opposite force on turbine; maybe consider multiplying by sigma and dividing by radius to get right units???maybe good enough, look up definition of ru_tendf

                 thrust(i,k,j) = thrust(i,k,j)-fn*cos(phi)*acc_u !wp_acc_u(i,k,j)*fn    		!!thrust everywhere on the map is equal to current thrust plus force distribution due to gaussian times normal force
                 torque(i,k,j) = torque(i,k,j)-ft*sin(zeta)*sin(phi)*acc_u 

	
	      ELSE !NM disk

		!NM calculating drut or dru for tilt, changes at each time due
		!to height:
		IF (tilt .eq. 0.) THEN !NM if no tilt 
        	  drut = dru(i,n,j)-hoff	!distance only depends on x value              			!!!if phi is zero the normal distance will be x0
                ELSE 
			drut = -calc_yprime(1./tan(tilt),-1.,0.,-(dru(i,n,j)-hoff),zd)   !NM new distance due to tilt, need proper sign convention so dru needs to be + downwind for calculation and drut needs to be - downwind for other calcs so double negative needed
		ENDIF

		!NM need to calculate new radius too for tilt:
		!NM first need zd new from new horizontal plane:
		IF (tilt .ne. 0.) THEN !NM has tilt
		  zd = -calc_yprime(-tan(tilt),-1.,0.,-(dru(i,n,j)-hoff),zd) !NM new zd, need make negative too to make sense
		  !NM new radius:
		  radius=sqrt( yd*yd + zd*zd )
		ENDIF

	      
	      acc_u = wp_acc_amp/adenom2*exp(-(drut**2)/bdenom2) !NM 1-D
		  							!Gaussian

              !!print *, "rho u:"
              !!print *, rho

              fnft=calc_fnft( v0, rho, blades, bladelen, diameter,   &		!!calculates normal force and tangential force from all points, but if use same v0 and rho will be same force throughout cylinder ;;does not take into account direction, just calculates magnitudes of forces ;;should just be all positive values or magnitudes, except for maybe pitch degree since fnft only calculates magnitudes and not anything else??? Maybe absolute value the v0 because if blowing the other direction like downwind
                              radius, pitchdeg, rpm, dscale, cscale, dum1, &
                              dum2, dum3 ) 

!NM              out1(i,k,j) = dum1        !NM output dummy variable into lpdm
!NM              out2(i,k,j) = dum2        !NM 
!NM              out3(i,k,j) = dum3        !NM  

              fn = fnft(1)							!!break it down into normal and tangential components
              ft = fnft(2)

	      zeta=atan2( zd, yd )						!!this finds angle to for distance from axis or radius (zeta in notes to convert from tangential direction into xy plane; more specifically it gets the y' component of ft. y' is positive (+) on left and negative on right direction if looking from upwind of turbine. This calculates zeta based on signs of zd and yd. zeta is positive from positive z axis to positive t axis which is the direction of rotation in counter-clockwise direction.	We are assuming here that t or the direction of rotation is counter clockwise when looking upwind. 

	      !NM fx, fy, fz calculated below are forces on the turbine. we take
	      !their negatives to get forces on atmosphere in tendency terms.

	      fx = fn*cos(phi)*cos(tilt)+ft*sin(zeta)*sin(phi)*cos(tilt)				!!now transform into WRF x direction like in note;; looks like +x=+x'*cos(phi)-y'*sin(phi) ;;;now it gets pointed the right direction in fx, either positive or negative because of phi and zeta. This is force on turbine in x dir. ft now assumes rotation of turbine is clockwise if viewed from upwind and sign in front of ft would change if rotation is counter-clockwise. ft iteslf is assumed counterclockwise. !!!!NM If turbine were spinning counter-clockwise like assumed for the calculation of zeta there would need to be a negative sign in front of ft because in the +y' direction sin(phi) gives gives the opposite sign of the positive x direction. +y'sin(phi) gives answer in -x direction.


              ru_tendf(i,k,j) =   ru_tendf(i,k,j)                        &	!!now dudt will be decreased by column stuff times gaussian times 1/(radius blade times density air times the force in the x direction      (I guess r does not mean inverse) Why do we have cnst equal 1/blade radius. for units to make sense but why use radius of blade and not point for example, I can accept rho, something to possibly look into???
                                - muu(i,j)*acc_u*cnst*fx	!???we subtract because we want force on air so we take the opposite force on turbine; maybe consider multiplying by sigma and dividing by radius to get right units???maybe good enough, look up definition of ru_tendf

! B. Kosovic, NCAR, July 5, 2012 - this is for now hard wired assuming
!         turbine rotors are perpendicualar to x axis

!NM changed to 3-D
!              IF ( radius .lt. rotor_radius ) THEN					!! Results here are still just in terms of force per unit area, I think we should multiply fn by 2*pi*radius*1/rdwz(i,k,j)  to get the total force through that cell since this is at u centers I assume and add it up in vertical like we are doing to get total force in horizontal and how it is spaced out.
                 thrust(i,k,j) = wp_acc_u(i,k,j)*fn    		!!thrust everywhere on the map is equal to current thrust plus force distribution due to gaussian times normal force
                 torque(i,k,j) = wp_acc_u(i,k,j)*radius*ft   		!!torque is gaussian times radius (from axis) times tangential force
                 power(i,k,j)  = wp_acc_u(i,k,j)*ft            !rotrate(i,j)*radius*ft  		!!power is gaussian times rotation rate times radius times tangential force or torque times rotation rate or omega
!              ENDIF
         ENDIF !NM for hub etc.

!------- Y ----										!!now same thing but for y direction

              zd = dzv(i,k,j)
              yd = dav(i,n,j) !calc_yprime(aa,ba,ca,x0v,y0v)					!!this is now for v nodes

              radius=sqrt( yd*yd + zd*zd )

              rho = (1.0/alt(i,k,j)+1.0/alt(i,k,j-1))/2.0  !NM using v-node density  
	      cnst = 1.0/rho  !NM getting right constant	

              !!print *, "rho v:"
              !!print *, rho  
        
	      IF (zd .le. -(radmin)) then !NM tower velow hub
	          
		  !NM finding Rtwer
	     	  Rtwr = abs(zd)*(Rtwrb-Rtwrt)/hub_height(n)+Rtwrt !NM linear

		  IF (abs(yd) .le. Rtwr) then !NM tower below hub; dau needs to be
		      			   !less than tower radius
	      	  fn = 0.5*rho*v0*v0*Cdt !NM force per unit area for tower
		  ft = 0.0 !NM no tangential force
		  acc_v = wp_acc_amp/adenom*exp(-(drv(i,n,j)**2)/bdenom) !NM 1-D
		  							!Gaussian
		  fy = fn*sin(phi) !NM to x direction
		  rv_tendf(i,k,j) = rv_tendf(i,k,j) - muv(i,j)*acc_v*cnst*fy !NM

	          END IF

	       END IF
   
	      IF ((radius .le. radmin) .and. (radius .gt. -0.001)) then !hub

		  fn = 0.5*rho*v0*v0*Cdh !NM drag force, only need force per area
		  ft = 0.0 !NM no tangential force
		  acc_v =  wp_acc_amp/adenom*exp(-((drv(i,n,j)-hoff)**2)/bdenom) !NM 1-D gaussian
		  fy = fn*sin(phi) !NM in y dir
		  rv_tendf(i,k,j) =   rv_tendf(i,k,j) - muv(i,j)*acc_v*cnst*fy
		  !NM rv tendency

		 power(i,k,j) = 	power(i,k,j)-fn*sin(phi)*acc_v
		 wp_acc_u(i,k,j) = wp_acc_u(i,k,j)+ft*sin(zeta)*cos(phi)*acc_v
	      
	      ELSE !NM for disk

		!NM calculating drut or dru for tilt, changes at each time due
		!to height:
		IF (tilt .eq. 0.) THEN !NM if no tilt 
        	  drvt = drv(i,n,j)-hoff	!distance only depends on x value              			!!!if phi is zero the normal distance will be x0
                ELSE 
			drvt = -calc_yprime(1./tan(tilt),-1.,0.,-(drv(i,n,j)-hoff),zd)   !NM new distance due to tilt, need proper sign convention so dru needs to be + downwind for calculation and drut needs to be - downwind for other calcs so double negative needed
		ENDIF

		!NM need to calculate new radius too for tilt:
		!NM first need zd new from new horizontal plane:
		IF (tilt .ne. 0.) THEN !NM has tilt
		  zd = -calc_yprime(-tan(tilt),-1.,0.,-(drv(i,n,j)-hoff),zd) !NM new zd, need make negative too to make sense
		  !NM new radius:
		  radius=sqrt( yd*yd + zd*zd )
		ENDIF
  
		acc_v = wp_acc_amp/adenom2*exp(-(drvt**2)/bdenom2) !NM 1-D
		  							!Gaussian

              fnft=calc_fnft( v0, rho, blades, bladelen, diameter,   &
                              radius, pitchdeg, rpm, dscale, cscale, dum1, &
                              dum2, dum3 ) 

              fn = fnft(1)
              ft = fnft(2)

              zeta=atan2( zd, yd )

              fy = fn*sin(phi)*cos(tilt)-ft*sin(zeta)*cos(phi)*cos(tilt)					!!ft is negative here because rotation of turbine is clockwise and of air counterclockwise. This is force turbine is making in y dir. sin zeta gives us in +y' direction. We assumed ft is in counterclockwise direction when calculating zeta etc. so need negative for turbine to spin clockwise. +y'cos(phi) gives answer in +y direction.;;;;fy--> +y=+x'*sin(phi)+y'*cos(phi)


              rv_tendf(i,k,j) =   rv_tendf(i,k,j)                        &		!! just for rv tendency, no need for thrust and torque since done already by u nodes
                                - muv(i,j)*acc_v*cnst*fy
	
	      power(i,k,j) = 	power(i,k,j)-fn*sin(phi)*acc_v
		wp_acc_u(i,k,j) = wp_acc_u(i,k,j)+ft*sin(zeta)*cos(phi)*acc_v
            END IF  !NM for hub drag		

!-------- Z ----									!! for w nodes

              zd = dzw(i,k,j)
              yd = daw(i,n,j) !calc_yprime(aa,ba,ca,x0w,y0w)

              radius=sqrt( yd*yd + zd*zd )

              rho = (1.0/alt(i,k,j)+1.0/alt(i,k-1,j))/2.0  !NM using w-node density  
	      cnst = 1.0/rho !NM getting right constant

              !!print *, "rho w:"
              !!print *, rho  

	      !NM calculating drut or dru for tilt, changes at each time due
		!to height:
		IF (tilt .eq. 0.) THEN !NM if no tilt 
        	  drwt = drw(i,n,j)-hoff	!distance only depends on x value              			!!!if phi is zero the normal distance will be x0
                ELSE 
			drwt = -calc_yprime(1./tan(tilt),-1.,0.,-(drw(i,n,j)-hoff),zd)   !NM new distance due to tilt, need proper sign convention so dru needs to be + downwind for calculation and drut needs to be - downwind for other calcs so double negative needed
		ENDIF

		!NM need to calculate new radius too for tilt:
		!NM first need zd new from new horizontal plane:
		IF (tilt .ne. 0.) THEN !NM has tilt
		  zd = -calc_yprime(-tan(tilt),-1.,0.,-(drw(i,n,j)-hoff),zd) !NM new zd, need make negative too to make sense
		  !NM new radius:
		  radius=sqrt( yd*yd + zd*zd )
		ENDIF

	       acc_w = 	wp_acc_amp/adenom2*exp(-(drwt**2)/bdenom2) !NM 1-D
		  							!Gaussian

              fnft=calc_fnft( v0, rho, blades, bladelen, diameter,   &
                              radius, pitchdeg, rpm, dscale, cscale, dum1, &
                              dum2, dum3 ) 

              fn = fnft(1)
              ft = fnft(2)

              zeta=atan2( zd, yd )

              fz =fn*sin(tilt)-ft*cos(zeta)*cos(tilt) 							!!ft is negative here because turbine is spinning clockwise if looked at from upwind. -ft ensures it is spinning clockwise because when calculating ft etc. we assumed it is spinning counterclockwise. t (counterclockwise) in t*cos(zeta) gives +z direction. or +z=+t*cos(zeta)

              rw_tendf(i,k,j) =   rw_tendf(i,k,j)                        &
                                - mut(i,j)*acc_w*cnst*fz

           END DO         ! j loop
        END DO         ! k loop
     END DO       ! i loop

END IF  ! ends if statement to exclude tiles or areas too far from turbine

END IF  !NM ends if statement to not do anything if not in cutin or cutout range

!NMtest  CALL wrf_dm_bcast_real( grid%v0_cntr , 1 )

  END DO  ! n loop 

 IF (rst_cust) then !NM if restart 
	 rst_cust = .false.  !NM set to false for next time
 END IF


!
!----------End of the loop over all the turbines

! 1000 continue

END SUBROUTINE gen_act_disk

!==============================================================================

SUBROUTINE calc_distance( x_turbinen, y_turbinen, phi_turbinen, n, n_turbines, &			!!!this calculates distances from axis and rotor plane in x,y
			  ims, ime, jms, jme, 			 &   !NM need
                          i_ts, i_te, j_ts, j_te,                &   !NM substituted i_start etc with i_ts
                          u_x, vw_x, v_y, uw_y,                  &   !NM easier, and added memory values
                          dau, dav, daw, dru, drv, drw )         

!-----------------------------------------------------------------------------
!
! Purpose: To calculate distances from the axis and rotor plane in (x,y)
! plane. 
!
!  October, 2010: Branko Kosovic, NCAR
!
!=============================================================================

  REAL, INTENT( IN )          :: x_turbinen, y_turbinen, phi_turbinen  !!!getting variables from master sub-routine
  INTEGER, INTENT( IN )                              :: n, n_turbines			!!!n indicates the turbine in the loop it is on
  INTEGER, INTENT( IN )                              :: ims, ime, jms, jme		!!!locations
  INTEGER, INTENT( IN )                              :: i_ts, i_te, j_ts, j_te !NM added these

  REAL, DIMENSION( ims:ime ), INTENT( IN )           :: u_x, vw_x !v_x, w_x !NM		!!!x and y locations of velocity nodes !NM took out t and put m
  REAL, DIMENSION( jms:jme ), INTENT( IN )           :: v_y, uw_y !u_y, v_y, w_y !NM, took out t and put m

  REAL, DIMENSION( ims:ime, n_turbines, jms:jme ), INTENT( INOUT ) :: dau, dav, daw			!!!this is what we want to find
  REAL, DIMENSION( ims:ime, n_turbines, jms:jme ), INTENT( INOUT ) :: dru, drv, drw    !!NM SHOULD THIS BE INOUT???????????

! LOCAL VARIABLES

  REAL    :: phi									!!!local variables as angles
  REAL    :: aa, ba, ca
  REAL    :: ar, br, cr
  REAL    :: x0u, x0v, x0w
  REAL    :: y0u, y0v, y0w
  INTEGER :: i, j


     phi = phi_turbinen*pi/180.0	!NM angle from x to x' cartesian to rotor coordinates, x' is normal to rotor and y' is parallel, converting to radians							
     !NM OLD!!!angle between rotor plane and x axis converted to radians
     !phi   = 0.5*pi-theta								!!!this gives us the angle between x axis and normal (notes), however the sign is important because theta must be between 0 at x and positive counter clockwise, and negative clockwise. Also limit between -pi/2 and pi/2 ??

!NM     IF ( abs(phi) .ne. 0.5*pi ) THEN							!!!if angle not 90 degrees between normal and x axis	
!NM        aa= tan(phi)									!!!tangent of phi slope of line 
!NM        ba=-1.										!!!this fits shortest distance from a point to a line equation/pdf
!NM     ELSE
!NM        aa=1.										!!!answer is just x distance or point, negative because 
!NM	ba=0.										!!!needs to be zero since slope is undefined
!NM     ENDIF
!NM     ca= 0.                    !!no slope intercept, turbine is origin

!NM     IF ( abs(phi) .ne. 0. ) THEN							!!!if pi not zero so not same x dir and normal, for rotor disk now
!NM        ar=-1./tan(phi)									!!!Since perpendicular, take negative reciprocal for slope
!NM        br=-1.                            !!!Must correspond to shortest line
!NM     ELSE 
!NM        ar=1.										!!!if phi is zero the normal distance will be x0
!NM        br=0.           !!! must be 0 since slope of line undefined
!NM     ENDIF
!NM     cr= 0.


	 do while (abs(phi) .ge. 2*pi)	!NM loop while above this value
	    phi = phi - sign(2*pi, phi)  !NM gets reduced, sign of phi
	 end do

     IF (phi .lt. 0.0) THEN !NM don't want negative or ifs below won't work
	 phi = phi + 2*pi  !NM so positive version
     END IF 

     !NM here we get coefficients for normal distance to axial plane dau (normal to
     !rotor). If looking from upwind of the turbine and facing it, positive (+)
     !distance is to the right and negative (-) is to the left. Switching this
     !sign convention along with the zd sign convention would be needed to get
     !assumtion that ft rotates clockwise and not counterclockwise 	
     IF (( phi .lt. 0.5*pi ) .or. (phi .gt. 1.5*pi)) THEN			!!!if angle not 90 degrees between normal and x axis	
        aa= tan(phi)									!!!tangent of phi slope of line 
        ba=-1.										!!!this fits shortest distance from a point to a line equation/pdf
     ELSEIF (phi .eq. 0.5*pi) THEN	
        aa=1.										!!!answer is just x distance or point, negative because 
	ba=0.										!!!needs to be zero since slope is undefined
     ELSEIF (phi .eq. 1.5*pi) THEN
    	aa=-1.
        ba=0.
     ELSE						!NM if in other	quadrants
     	aa=-tan(phi)
	ba=1.	
     ENDIF
     ca= 0.                    !!no slope intercept, turbine is origin

     !!NM here we get coefficients for normal distance to rotor plane dru; negative
     !(-) in direction of U or downwind from the plane and positive (+) upwind
     !from the rotor
     IF (phi .eq. 0.5*pi) THEN
        ar=0.
	br=-1.     !NM distance only depends on y value
     ELSEIF (phi .eq. 0.) THEN 
        ar=-1.		!distance only depends on x value              			!!!if phi is zero the normal distance will be x0
        br=0.           !!! must be 0 since slope of line undefined
     ELSEIF ( phi .lt. pi ) THEN							!!!if pi less than pi, for rotor disk now
        ar=-1./tan(phi)									!!!Since perpendicular, take negative reciprocal for slope
        br=-1.                                                                          !!!Must correspond to shortest line
     ELSEIF (phi .eq. 1.5*pi) THEN
     	ar=0.
	br=1.	!!distance only depends on y value
     ELSEIF (phi .eq. pi) THEN  !NM if pi
     	ar=1.			!NM distance only depends on x value
	br=0.
     ELSE 			!NM if phi is between pi and 2 pi
	ar=1./tan(phi)
	br=1.
     ENDIF
     cr= 0.


     DO j = j_ts, j_te								!!!goes through j's, replaced with nocal NM

        y0u= uw_y(j)-y_turbinen								!!!y location of u node minus y location of turbine !NM
        y0v= v_y(j)-y_turbinen								!!!same for rest but diff direction
        y0w= uw_y(j)-y_turbinen            !!NM new

        DO i = i_ts, i_te								!!!goes through i's, NM replaced with local

           x0u= u_x(i)-x_turbinen								!!!x location of u node minus x location of turbine
           x0v= vw_x(i)-x_turbinen !NM changed
           x0w= vw_x(i)-x_turbinen !NM changed

! calculate distance to the turbine axis in (x,y) plane					!!!calculates distance to turbine axis (n) (perp to turb plane)
           dau(i,n,j)=calc_yprime(aa,ba,ca,x0u,y0u)					
           dav(i,n,j)=calc_yprime(aa,ba,ca,x0v,y0v)
           daw(i,n,j)=calc_yprime(aa,ba,ca,x0w,y0w)

! calculate distance to the rotor plane of the turbine in (x,y) plane			!!!calculates normal distance to turbine rotor plane
           dru(i,n,j)=calc_yprime(ar,br,cr,x0u,y0u)					!!!see diagram in hard data for explanation of equation derivation
           drv(i,n,j)=calc_yprime(ar,br,cr,x0v,y0v)
           drw(i,n,j)=calc_yprime(ar,br,cr,x0w,y0w)


        END DO
     END DO

    !Avoid loop above: maybe code up later if need since two functions change

END SUBROUTINE calc_distance

!==============================================================================
!
!      Functions to check a variable is NaN or Inf
!
!----------------------------------------------------------------------
FUNCTION check_naninf( rnaninf, snaninf ) result( naninf )
!
    real, intent( in )             :: rnaninf
    character(len=8), intent( in ) :: snaninf
    integer                        :: naninf
!
    naninf=0
!
    IF ( rnaninf*0 .NE. 0 ) THEN
      !print *, ' Variable ',snaninf,' = ',rnaninf,' is Inf'
      naninf=1
    ENDIF
!   
    IF ( rnan .NE. rnan ) THEN
      !print *, ' Variable ',srnaninf,' = ',rnaninf,' is NaN'
      naninf=2
    ENDIF
!
END FUNCTION check_naninf

!==============================================================================
!
!      Functions to compute distance from a line in 2d
!
!----------------------------------------------------------------------
FUNCTION calc_distline( a, b, c, x0, y0 ) result( distline ) 		!!!returns a result, calculates distance line from node to plane with turb 
!
    real, intent( in )   :: a, b, c
    real, intent( in )   :: x0, y0
    real                 :: distline
!   
    distline=abs(a*x0+b*y0+c)/sqrt(a*a+b*b)				!!!look at derivation in hard paper. (Found derivation as shortest distance from point to line)	!! takes form by assuming the turbine is at (0,0) and the point is at (x0,y0). Now the line through the turbine is ax+by+c=0. Then you can plug everything in.
!
END FUNCTION calc_distline

!==============================================================================
!
!      Functions to compute y' coordinate with respect to the axis of
!
!----------------------------------------------------------------------
FUNCTION calc_yprime( a, b, c, x0, y0 ) result( yprime ) 		!!!very similar to calculate distance line with same input 
!
    real, intent( in )   :: a, b, c
    real, intent( in )   :: x0, y0
    real                 :: yprime
!   
    yprime=(a*x0+b*y0+c)/sqrt(a*a+b*b)					!!!but instead of absolute value we take negative answer ;;Now I think there should not be a negative value in front of this equation because y'=y for calculation of forces and not y'=-y as Branko officially noted
!
END FUNCTION calc_yprime

!-------------------------------------------------------------
!------------ Specific normal and tangential force 
!
! BK FUNCTION calc_fnft( v0, rho, blades, bladelen, diameter, dscale, cscale, &
! BK                     radius, rpm, pitchdeg ) &
FUNCTION calc_fnft( v0, rho, blades, bladelen, diameter,      &
                    radius, pitchdeg, rpm, dscale, cscale, dum1, &
                    dum2, dum3 )   &
                   result( fnft )
!
!  --v0       - "free stream" velocity normal to the turbine rotor plane, [m/s]
!  --rho      - air density, [kg/m^3]
!  --blades   - number of turbine blades, [non-dimensional]
!  --diameter - turbine rotor diameter, [m]
!  --xcoord   - streamwise coordinate of the grid point with respect to
!               local coordinate system with the origin at the turbine hub, [m]
!  --ycoord   - cross-stream coordinate of the grid point with respect to
!               local coordinate system with the origin at the turbine hub, [m]
!  --zcoord   - vertical coordinate of the grid point with respect to
!               local coordinate system with the origin at the turbine hub, [m]
!
!  --fnft     - one-dimensional array (two members) with FUNCTION output
!               specific normal and tangential forces per unit area, [N/m^2]
!
     real, intent( in ) :: v0, rho, blades, bladelen, diameter
     real, intent( in ) :: dscale, cscale 
     real, intent( in ) :: radius
     real, intent( in ) :: rpm
     real, intent( in ) :: pitchdeg
     real, dimension( 2 )  :: fnft
     real               :: rps, omega 
     real               :: radmax, radmin
     real               :: an, at
     real               :: anold, atold
     real               :: cord
     real               :: twist
     real               :: xi, xideg, beta, betadeg, psi, psideg
     real               :: vn, vt
     real               :: cl, cd
     real               :: cn, ct
     real               :: solid
     real               :: fac1, fac2, tiploss, hubloss, lossfac
     real               :: vr
     real               :: lift, drag
     real               :: fn, ft
     real               :: epsi, epscrit
     integer            :: iter, itmax
!
     rps   = rpm/60.0								!!!changes revolutions per minute to per second
     omega = 2.*pi*rps								!!!like omega in notes is radians per second, rotation velocity
!
     radmax= 0.5*diameter							!!!set maximum radius to that of the blade
     radmin= radmax - bladelen						!!!minimum radius from blade to that of start of blade from rotor
!

  fnft(1) = 0.0
  fnft(2) = 0.0

  IF (( radius .LT. radmax ) .AND. ( radius .GE. radmin) ) THEN 		!!!if radius is less than max and more than min for point away from turbine axis
!
        an = 0.0
        at = 0.0
!
        iter = 0
        epsi = 1.0        
        epscrit = 1.e-5
        itmax   = 20
!
!    --Twist angle
        xideg = calc_twist( (radius - radmin) / dscale ) + pitchdeg		!!!xi is really just the local blade twist. calc_twist calculates the global blade twist which is built into the blade and varies from root to tip of blade. pitchdeg is how much we pitch the blade during operation (feathering) to control the power output and must be subtracted to get the local angle of the twist with respect to the coordinate system. twist is not the same as advance angle. 
        xi    = xideg*pi/180.0
!
!    --Cord length 
        cord  = cscale * calc_chord( (radius - radmin) / dscale )		!!calculating the chord length dependent on the length of blade from bottom rotor. cscale and dscale are optional scales i think
!
!    --Compute induction factors an and at iteratively

        do while ( ( epsi .gt. epscrit ) .and. ( iter .lt. itmax ) ) 
!
           iter = iter + 1
!
!       --Normal and tangential velocity components
                
           an = 0.02 !JDM This is just the tip loss component of an. This was computed using the
                     !    IDL code as the difference between an computed with and without tip loss
                     !    across the power curve. The value of 0.02 was nearly constant.

           vn = v0*( 1.0 - an )
           vt = omega*radius*( 1.0 + at ) 
 
!       --Advance angle 
  
           psi    = atan2(vn,vt)
           psideg = psi*180.0/pi

!       --Incidence angle  
 
           beta    = psi - xi
           betadeg = beta*180.0/pi

!       --Lift and drag coefficients       
           cl = calc_cl( (radius-radmin), betadeg )				!!we now calculate the lift and drag coefficients using the beta angles and how far away the radius is from rotor hub
           cd = calc_cd( (radius-radmin), betadeg )				!!same thing done for drag coefficients why limit set on incidence angle?

!       --Normal and tangential force coefficients     
  
           cn = cl*cos( psi ) + cd*sin( psi )
           ct = cl*sin( psi ) - cd*cos( psi )

!       --Solidity factor

           solid = cord*blades / ( 2.0*pi*radius )

!       --Tip loss and hub loss								!!tip loss
           if ( sin( psi ) .ne. 0. ) then						!so that v1 is not zero in notes or psi is not zero
              fac1 = blades / 2.0*( radmax - radius ) / ( radius*sin( psi ) )
              fac2 = blades / 2.0*( radius - radmin ) / ( radmin*sin( psi ) )
           else
               fac1 = 1.e5								!this is the factor if psi is 0 and we can't divide by 0
               fac2 = 1.e5      !for hub loss so don't divide by zero 
           endif
              tiploss = (2.0/pi)*acos( exp( -fac1 ) )
              hubloss = (2.0/pi)*acos( exp( -fac2 ) )

              lossfac = tiploss*hubloss

!             --Induction factors

              anold = an
              atold = at

!JDM             an = ( 1. + 4.0*lossfac*sin( psi )*sin( psi )/ ( solid*cn ) )**(-1.0)

              at = ( 4.0*lossfac*sin( psi )*cos( psi )/ ( solid*ct ) - 1.0 )**(-1.0)


!       --Convergence criteria

           epsi = sqrt( ( anold - an )**2.0 + ( atold - at )**2.0 )
  
        end do
!
!    --Normal and tangential velocity components

       vn = v0*( 1.0 - an )

       vt = omega*radius*( 1.0 + at ) 

!    --Relative velocity

       vr = sqrt( vn*vn + vt*vt ) 
! 
!    --Advance angle   

       psi    = atan2(vn,vt)
       psideg = psi*180.0/pi
!
!    --Incidence angle   

       beta    = psi - xi
       betadeg = beta*180.0/pi

!    --Lift and drag coefficients       
        cl = calc_cl( (radius-radmin), betadeg )					!we calculate the lift and drag coefficient final using the new incidence angle 
        cd = calc_cd( (radius-radmin), betadeg )			
!
!    --Lift and drag forces
       lift = 0.5*rho*vr*vr*cord*cl 
       drag = 0.5*rho*vr*vr*cord*cd 

!    --Specific normal and tangential forces

       fn = lift*cos( psi ) + drag*sin( psi )
       ft = lift*sin( psi ) - drag*cos( psi )

       fn = blades*fn/(2.0*pi*radius)
       ft = blades*ft/(2.0*pi*radius)

       if( fn .lt. 0.0 ) fn = 0.0
       if( ft .lt. 0.0 ) ft = 0.0

       fnft(1) = fn
       fnft(2) = ft

  ENDIF   !radius .GE. radmin .AND. radius .LT. radmax

END FUNCTION calc_fnft

!
!-------------------------------------------------------------
!------- RPM as a FUNCTION of the wind speed -----
FUNCTION calc_rpm( x ) result( rpm )
!
    real, intent( in )   :: x								!! this is the upwind wind speed	
    real, dimension( 7 ) :: rpmcoeff = [                        &			!! what is this revolutions per minute coefficient based on turbine
                                         1.652080754842292e+01, &
                                        -9.743876254821833e+00, &
                                         2.878871203990343e+00, &
                                        -3.328240583243683e-01, &
                                         1.882413136012323e-02, &
                                        -5.217537521771266e-04, &
                                         5.679811459143678e-06  &
                                         ]
    real                 :: rpm
    integer              :: i
!
    rpm=0.										!!assume zero revolutions per minute
    do i=1,7
        rpm=rpm+rpmcoeff(i)*(x**(i-1))							!!goes through each ramping coefficient and takes to wind speed power  where does this equation come from???
    enddo
!
END FUNCTION calc_rpm
!
!
!-------------------------------------------------------------
!------- Pitch as a FUNCTION of the wind speed -----
FUNCTION calc_pitch( x ) result( pitch )					!!calculates pitch angle based on upwind wind speed angle between relative velocity and tangential axis.
!
    real, intent( in )   :: x
    real, dimension( 9 ) :: pitchcoeff = [                        &
                                          -1.061372854320387e-01, &
                                          -2.474859944242993e-01, &
                                          -3.270782224024930e-01, &
                                           1.728787966831591e-01, &
                                          -3.262956452017539e-02, &
                                           2.976572163838095e-03, &
                                          -1.404959872966906e-04, &
                                           3.306853803299496e-06, &
                                          -3.073103898705851e-08  &
                                         ]
    real                 :: pitch
    integer              :: i
!
    pitch=0.
    do i=1,9
        pitch=pitch+pitchcoeff(i)*(x**(i-1))       !!!similar to calculation of rpm
    enddo
!
END FUNCTION calc_pitch


!----------------------------------------------------------------------
!
!      Functions to compute normal and tangenetial forces
!      based on Blade Element Momentum theory for the generalized
!      actuator model 
!
!  This subroutine computes specific normal and tangenetial forces 
!  due to the presence of operating wind turbine using generalized
!  actuator model described by Mikkelsen (2003, Ph.D. Dissertation)
!
!
!
!------- Lift Coefficients -----
FUNCTION calc_cl1( x ) result( cl1 ) 
!
    real, intent( in )   :: x
    real, dimension( 3 ) :: clcoeff=[                        &
                                     -8.945700347807454e-19, &
                                      3.030747828070469e-05, &
                                     -2.424404231691676e-06  &
                                    ]
    real                 :: cl1
    real                 :: y
    integer              :: i
!
    cl1=0.
    y = x
    if( y .lt. -15. ) y=-15.
    if( y .gt.  25. ) y= 25.
    do i=1,3
       cl1=cl1+clcoeff(i)*(y**(i-1))
    enddo
!
END FUNCTION calc_cl1
!
!------- Lift Coefficients -----
FUNCTION calc_cl2( x ) result( cl2 )
!
    real, intent( in )   :: x
    real, dimension( 8 ) :: clcoeff=[                        &
                                      1.339831701399220e-01, &
                                      1.273605806854426e-01, &
                                      2.670858364495304e-03, &
                                     -5.171416992233193e-04, &
                                      5.154103761751980e-06, &
                                      9.103319521156643e-07, &
                                     -4.247207288197822e-08, &
                                      6.149517559210604e-10  &
                                    ]
    real                 :: cl2
    real                 :: y
    integer              :: i
!
    cl2=0.
    y = x
    if( y .lt. -15. ) y=-15.		! setting limits on the twist angle
    if( y .gt.  25. ) y= 25.
    do i=1,8
       cl2=cl2+clcoeff(i)*(y**(i-1))	!coefficients depending on twist angle
    enddo
!
END FUNCTION calc_cl2
!
!------- Lift Coefficients -----
FUNCTION calc_cl3( x ) result( cl3 )
!
    real, intent( in )   :: x
    real, dimension( 8 ) :: clcoeff=[                        &
                                      1.944743510817245e-01, &
                                      1.000517903122225e-01, &
                                      3.197826235204042e-03, &
                                     -5.421365110425472e-05, &
                                     -2.548603053618932e-05, &
                                      5.596832426548656e-08, &
                                      5.892141669694199e-08, &
                                     -1.254579741428682e-09  &
                                    ]
    real                 :: cl3
    real                 :: y
    integer              :: i
!
    cl3=0.
    y = x
    if( y .lt. -15. ) y=-15.
    if( y .gt.  25. ) y= 25.
    do i=1,8
       cl3=cl3+clcoeff(i)*(y**(i-1))
    enddo
!
END FUNCTION calc_cl3
!
!------- Lift Coefficients -----
FUNCTION calc_cl4( x ) result( cl4 )
!
    real, intent( in )   :: x
    real, dimension( 8 ) :: clcoeff=[                        &
                                      3.523499144894372e-01, &
                                      1.123841767761360e-01, &
                                      8.341579984000093e-04, &
                                     -1.335324149006132e-04, &
                                     -1.981383032916323e-05, &
                                      2.224183323767474e-07, &
                                      5.437327977095365e-08, &
                                     -1.333750087524698e-09  &
                                    ]
    real                 :: cl4
    real                 :: y
    integer              :: i
!
    cl4=0.
    y = x
    if( y .lt. -15. ) y=-15.
    if( y .gt.  25. ) y= 25.
    do i=1,8
       cl4=cl4+clcoeff(i)*(y**(i-1))
    enddo
!
END FUNCTION calc_cl4
!
!------- Lift Coefficients -----
FUNCTION calc_cl5( x ) result( cl5 )
!
    real, intent( in )   :: x
    real, dimension( 8 ) :: clcoeff=[                        &
                                      4.723171711361847e-01, &
                                      1.218771566691490e-01, &
                                     -6.086116733736481e-04, &
                                     -2.358233923100615e-04, &
                                     -2.124530372224732e-06, &
                                      3.660517591113997e-07, &
                                     -2.644237896940440e-09, &
                                     -6.489522579060555e-11  &
                                    ]
    real                 :: cl5
    real                 :: y
    integer              :: i
!
    cl5=0.
    y = x
    if( y .lt. -15. ) y=-15.
    if( y .gt.  25. ) y= 25.
    do i=1,8
       cl5=cl5+clcoeff(i)*(y**(i-1))
    enddo
!
END FUNCTION calc_cl5
!
!-------------------------------------------------------------
!------- Drag Coefficients -----
FUNCTION calc_cd1( x ) result ( cd1 )
!
    real, intent( in )   :: x
    real                 :: cdcoeff= 6.e-01
    real                 :: cd1
!
    cd1=cdcoeff
!
END FUNCTION calc_cd1
!
!------- Drag Coefficients -----
FUNCTION calc_cd2( x ) result ( cd2 )
!
    real, intent( in )   :: x
    real, dimension( 6 ) :: cdcoeff=[                        &
                                      4.161002804282589e-02, &
                                      1.018291205148969e-03, &
                                     -4.090577683441135e-04, &
                                      1.922413700861447e-05, &
                                      3.176432914120688e-06, &
                                     -9.098582298900235e-08  &
                                    ]
    real                 :: cd2
    real                 :: y
    integer              :: i
!
    cd2=0.
    y = x
    if( y .lt. -15. ) y=-15.
    if( y .gt.  25. ) y= 25.
    do i=1,6
       cd2=cd2+cdcoeff(i)*(y**(i-1))
    enddo
!
END FUNCTION calc_cd2
!
!------- Drag Coefficients -----
FUNCTION calc_cd3( x ) result ( cd3 )
!
    real, intent( in )   :: x
    real, dimension( 5 ) :: cdcoeff=[                        &
                                      2.312243515932134e-02, &
                                     -1.199873248978045e-03, &
                                      1.424381323810507e-04, &
                                     -4.036983875639889e-06, &
                                      1.079332906610164e-06  &
                                    ]
    real                 :: cd3
    real                 :: y
    integer              :: i
!
    cd3=0.
    y = x
    if( y .lt. -15. ) y=-15.
    if( y .gt.  25. ) y= 25.
    do i=1,5
       cd3=cd3+cdcoeff(i)*(y**(i-1))
    enddo
!
END FUNCTION calc_cd3
!
!------- Drag Coefficients -----
FUNCTION calc_cd4( x ) result ( cd4 )
!
    real, intent( in )   :: x
    real, dimension( 5 ) :: cdcoeff=[                        &
                                      8.519601384462769e-03, &
                                     -2.473390398892131e-03, &
                                      3.538573315602367e-04, &
                                      2.229011109819266e-06, &
                                      4.120416277636828e-07  &
                                    ]
    real                 :: cd4
    real                 :: y
    integer              :: i
!
    cd4=0.
    y = x
    if( y .lt. -15. ) y=-15.
    if( y .gt.  25. ) y= 25.
    do i=1,5
       cd4=cd4+cdcoeff(i)*(y**(i-1))
    enddo
!
END FUNCTION calc_cd4
!
!------- Drag Coefficients -----
FUNCTION calc_cd5( x ) result ( cd5 )
!
    real, intent( in )   :: x
    real, dimension( 5 ) :: cdcoeff=[                        &
                                      7.632822361787280e-03, &
                                      9.253284402338159e-04, &
                                     -7.286247299868759e-05, &
                                     -1.030789389347442e-05, &
                                      1.688496706584846e-06  &
                                    ]
    real                 :: cd5
    real                 :: y
    integer              :: i
!
    cd5=0.
    y = x
    if( y .lt. -15. ) y=-15.
    if( y .gt.  25. ) y= 25.
    do i=1,5
       cd5=cd5+cdcoeff(i)*(y**(i-1))
    enddo
!
END FUNCTION calc_cd5
!
! ------- Lift Coefficient -----
FUNCTION calc_cl( r,x ) result ( cl )				!calculating the lift coefficient based on the radius of blade and blade twist calculated
!
    real, intent( in )  :: r, x
    real                :: cl
!
    if ((r .ge. 0.) .and. (r .lt. 2.5)) then			!different conditions depending on what part of blade and this gives us different cd coeff
       cl = calc_cl1(x)						!the coefficients then only depend on blade twist angle calculated
    endif
    if ((r .ge. 2.5) .and. (r .lt. 10.0)) then
       cl = calc_cl2(x)
    endif
    if ((r .ge. 10.0) .and. (r .lt. 20.5)) then
       cl = calc_cl3(x)
    endif
    if ((r .ge. 20.5) .and. (r .lt. 24.5)) then
       cl = calc_cl4(x)
    endif
    if ((r .ge. 24.5) .and. (r .lt. 50.0)) then      !NM Changed 46.3 to 50
       cl = calc_cl5(x)
    endif
!
END FUNCTION calc_cl
!
! ------- Drag Coefficient -----
FUNCTION calc_cd( r,x ) result ( cd )
!
    real, intent( in )  :: r, x
    real                :: cd
!
    if ((r .ge. 0.) .and. (r .lt. 2.5)) then
       cd = calc_cd1(x)
    endif
    if ((r .ge. 2.5) .and. (r .lt. 10.0)) then
       cd = calc_cd2(x)
    endif
    if ((r .ge. 10.0) .and. (r .lt. 20.5)) then
       cd = calc_cd3(x)
    endif
    if ((r .ge. 20.5) .and. (r .lt. 24.5)) then
       cd = calc_cd4(x)
    endif
    if ((r .ge. 24.5) .and. (r .lt. 50.0)) then      !NM Changed to 50   
       cd = calc_cd5(x)
    endif
!
END FUNCTION calc_cd
!
!-------------------------------------------------------------
!------- Cord Length -----
FUNCTION calc_chord( x ) result( cord )						!!calculates the chord length of the blade using length of blade from rotor base
!
    real, intent( in )   :: x							!!length of blade from rotor base in terms of r
    real, dimension( 7 ) :: cordcoeff = [                        &		!!some sort of blade property
                                          2.059543624867598e+00, &
                                         -2.048575308833385e-01, &
                                          7.266836250702186e-02, &
                                         -6.190764896134115e-03, &
                                          2.236223789319999e-04, &
                                         -3.726338491673692e-06, &
                                          2.352490639201739e-08  &
                                        ]
    real                 :: cord
    integer              :: i
!
    cord=0.
    do i=1,7 
       cord=cord+cordcoeff(i)*(x**(i-1))
    enddo
!
END FUNCTION calc_chord
!
!-------------------------------------------------------------
!------- Twist Angle -----
FUNCTION calc_twist( x ) result( twist )					!!calculates twist angle which is dependent on blade
!
    real, intent( in )   :: x							!!this is input which is the length of blade at this point
    real, dimension( 7 ) :: twistcoeff = [                        &		!!once again uses weird coefficients
                                           1.446618326894617e+01, &
                                          -5.146551570182272e-01, &
                                           1.244386610948123e-01, &
                                          -1.444453252931746e-02, &
                                           6.378463069261417e-04, &
                                          -1.236423083232531e-05, &
                                           8.906081086715364e-08  &
                                         ]
    real                 :: twist
    integer              :: i
!
    twist=0.
    do i=1,7
        twist=twist+twistcoeff(i)*(x**(i-1))
    enddo
!
END FUNCTION calc_twist

END MODULE module_gen_act_disk
