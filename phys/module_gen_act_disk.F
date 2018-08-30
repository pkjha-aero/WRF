
MODULE module_gen_act_disk

! USE module_wrf_error !RSA couldn't get to compile with this

#ifdef DM_PARALLEL
  USE MODULE_DM
#endif
  
  real, parameter :: pi=3.14159265359
  
  CONTAINS
    
  SUBROUTINE gen_act_disk ( ru_tendf, rv_tendf, rw_tendf, &
                            wp_opt,                       & 
                            n_turbines,                   &
                            n_timeseries,                 &
                            x_turbine,                    &
                            y_turbine,                    &
                            hub_height,                   &
                            rotor_diameter,               &
                            blade_length,                 &
                            theta_turbine,                &
                            cutinspd, cutoutspd,          & !RSA
                            acc_yaw_err,                  & 
                            wp_acc_u, wp_acc_v, wp_acc_w, & ! these variables likely not needed. 
                            wp_ts,                        &
                            thrust, torque,               &
                            rotrate, power,               &
                            u, v, w,                      &
                            muu, muv, mut,                &
                            rdzw,  dx, dy, dt,            &
                            v0t, d0t, itimestep,          &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            ips, ipe, jps, jpe, kps, kpe, &
                            its, ite, jts, jte, kts, kte  )

!-----------------------------------------------------------------------------
!
! Purpose: To calculate forces using the generalized actuator disk model. 
!
! Version using MPI for turbine v0 and yaw, with validated yaw orientation
! using calc_yprime. 
!
! Lookup tables use generic Penn State 1.5 MW GE SLE turbine (approximate).
!
!  1. Here, unlike the siemens code which uses xi = twist - pitch , we use
!     xi = twist + pitch; Except near the blade root, the angles of attack
!     stay within the lookup tables (they did not for xi = twist - pitch). 
!
!  2. Rather than polynomial curve fits we now use linear interpolation
!     for Cd and Cl.
!
!  3. Corrected equations for tilt term, 12 01 2014
!
!     Last modified Dec 01, 2014. Cleaned up on April 09, 2015
!
!  If the code dies, check to see if the flow is coming from behind the turbine.
!  (e.g. rotate the yaw). Also see if imin is beyond the patch. Check with
!  grep -i 'imin' rsl.out.* If so, decrease the offset. This needs to be fixed 
!  and will be someday...
!
!=============================================================================

  IMPLICIT NONE

#ifdef DM_PARALLEL
  INCLUDE 'mpif.h'
#endif

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT(INOUT) &
  :: ru_tendf    & ! u tendency * mass                   (kg s-4)
   , rv_tendf    & ! v
   , rw_tendf      ! w
  
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT(INOUT) &
  :: wp_acc_u    & ! acceleration for u                  (m s-2)
   , wp_acc_v    & !                  v                  (m s-2)
   , wp_acc_w      !                  w                  (m s-2)
 
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT(INOUT) & 
  :: wp_ts         ! diagnostic for debuging

  INTEGER,                                       INTENT( IN  ) &  
  :: n_turbines    ! number of turbines

  INTEGER,                                       INTENT( IN  ) &  
  :: n_timeseries  ! length of timeseries for averages

  INTEGER,                                       INTENT( IN  ) &  
  :: wp_opt  ! 

  REAL, DIMENSION( n_turbines ),                 INTENT(INOUT) &  !should be in if reading from aux file
  :: x_turbine      & ! x location of a turbine              (m)
   , y_turbine      & ! y location of a turbine              (m)
   , hub_height     & ! hub height                           (m)
   , rotor_diameter & ! hub height                           (m)
   , blade_length   & ! blade_length                         (m)
   , theta_turbine  & ! angle of rotor plane w.r.t. x axis (deg)
   , cutinspd       & ! turbine cutin speed                (m/s)
   , cutoutspd        ! turbine cutout speed               (m/s)

  REAL, DIMENSION( n_turbines ),                 INTENT(INOUT) & 
  ::  acc_yaw_err

  REAL, DIMENSION( ims:ime, jms:jme ),            INTENT( OUT ) &
  :: thrust, torque, rotrate, power

   REAL, DIMENSION( n_timeseries, n_turbines ),   INTENT(INOUT) &
  :: v0t          & ! Wind speed timeseries               (m s-1)
   , d0t            ! Wind direction timeseries.            (deg)
                    ! 0 is north, positive rotating clockwise  

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT( IN  ) &
  :: u           & ! u-velocity                  (m s-1)
   , v           & ! v
   , w             ! w

  REAL, DIMENSION( ims:ime, jms:jme ),           INTENT( IN  ) &
  :: muu         & ! column pressure at u nodes  (kg m-1 s-2)
   , muv         & !                    v
   , mut           !                    t

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT( IN  ) &
  :: rdzw          !inverse vertical grid spacing at w-nodes (m-1)

  REAL,                                          INTENT( IN  ) &
  :: dx          & !grid spacing in x-direction  (m) 
   , dy            !                y            (m)

  REAL,                                          INTENT( IN  ) & !JDM May 2, 2013
  :: dt

  INTEGER,                                       INTENT( IN  ) & !JDM May 2, 2013
  :: itimestep

  INTEGER,                                       INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde   &
   , ims, ime, jms, jme, kms, kme   &
   , ips, ipe, jps, jpe, kps, kpe   &
   , its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  REAL, DIMENSION( its:ite+1 ) &
  :: u_x         & !x coordinate of u-nodes (m)
   , v_x         & !                v       (m)
   , w_x           !                w       (m)
  
  REAL, DIMENSION( jts:jte+1 ) &
  :: u_y         & !y coordinate of u-nodes (m)
   , v_y         & !                v       (m)
   , w_y           !                w       (m)

  REAL, DIMENSION( its:ite+1, kts:kte, jts:jte+1 ) &
  :: u_z         & !z coordinate of u-nodes (m)
   , v_z           !                v       (m)
   
  REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ) &
  :: w_z           !z coordinate of w-nodes  (m)

  REAL, DIMENSION( its:ite, jts:jte ) &
  :: dru       & !normal distance from u-nodes to the rotor plane (m)
   , drv       & !                     v                          (m)
   , drw         !                     w                          (m)

  REAL, DIMENSION( its:ite, jts:jte ) &
  :: dau       & !normal distance from u-nodes to the rotor axis line (m)
   , dav       & !                     v                              (m)
   , daw         !                     w                              (m)

  REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ) &
  :: dzu       & !normal distance from u-nodes to the rotor plane (m)
   , dzv       & !                    v                           (m)
   , dzw         !                    w                           (m)

  REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ) &
  :: ru       & !distance from u-nodes to the nacelle            (m)
   , rv       & !                    v                           (m)
   , rw         !                    w                           (m)

  REAL, DIMENSION( kts:kte ) &
  :: turb_z     ! z coordinate at a turbine                      (m)  

  REAL :: xmin_tile, xmax_tile, ymin_tile, ymax_tile
  REAL :: xmin_turb, xmax_turb, ymin_turb, ymax_turb
  REAL :: zmax_turb
  REAL :: start_fac

  INTEGER :: i, j, k, n, i_start, j_start, i_end, j_end, k_end, k_top, imin, jmin, kmin

  REAL :: dxo2, dyo2, zmin_k, zmin_kp1, zmax_k, zmax_kp1, dz, eps

  REAL :: theta

  INTEGER :: naninf

#ifdef DM_PARALLEL
  INTEGER         :: ierr
  INTEGER         :: tag
  INTEGER         :: master
  INTEGER         :: status(MPI_STATUS_SIZE)
#endif

  REAL, DIMENSION(n_turbines) :: v0n
  REAL, DIMENSION(n_turbines) :: d0n
  INTEGER, DIMENSION(n_turbines) :: yawing


  REAL, DIMENSION(2) :: v0d0inst

!--------------------------------------------------------

  REAL, DIMENSION(2)    :: fnft
  REAL                  :: fn, ft
  REAL                  :: V0
  REAL                  :: rho
  REAL                  :: rpm, blades, pitchdeg
  REAL                  :: rotor_radius
  REAL                  :: dscale, cscale
  REAL                  :: zeta
  REAL                  :: fx, fy, fz
  REAL                  :: cnst
  REAL                  :: v0inst
  INTEGER               :: nv0t
  INTEGER               :: turbine_on_patch

  REAL                  :: time_fac
  REAL                  :: xtime
  REAL                  :: u_geo
  REAL                  :: v_geo
  REAL                  :: th_geo
  REAL                  :: ws_geo
  REAL                  :: nt

  REAL                  :: acc_u, acc_v, acc_w
  REAL                  :: radmin, radmax

! variables for yawing

  REAL                  :: d0inst
  REAL                  :: yaw_thresh
  REAL                  :: yaw_err
  REAL                  :: yaw_inc
  REAL                  :: yaw_new
  REAL                  :: yaw_rate
  REAL                  :: delta
  REAL                  :: sgn
  REAL                  :: dum

! variables for hub drag

  REAL                  :: cdh

! variables for tower drag

  REAL                  :: cdt, d_tower, Rtwrt, Rtwrb, Rtwr

! variables for tilt

  REAL                  :: tilt_deg 
  REAL                  :: tilt   
  REAL                  :: sigma
  REAL                  :: sigmar
  REAL                  :: adenom, bdenom
  REAL                  :: adenom2, bdenom2
  REAL                  :: hoff
  REAL                  :: drut, drvt, drwt
  REAL                  :: radius

! RSA for reading in turbine info from file
  CHARACTER*256        :: fname
  CHARACTER*512        :: inline
  INTEGER              :: istat
  REAL                 :: lat,lon,hubheight,diameter,bldlngth,thetaturb,cutin,cutout

! test
  REAL :: zd, zetazd

!---------------------------------------------------------

  CHARACTER(LEN=12), PARAMETER :: FMT1 = "(I5,1F18.12)"
  CHARACTER(LEN=12), PARAMETER :: FMT2 = "(I5,2F18.12)"
  CHARACTER(LEN=12), PARAMETER :: FMT3 = "(I5,3F18.12)"
  CHARACTER(LEN=12), PARAMETER :: FMT4 = "(3I5,2F18.12)"

  REAL, DIMENSION(10) :: turb_xy_locs

#ifdef DM_PARALLEL
  master = 0
  tag    = 0
#endif

!____________________________________________________________________________________________________________________________
!
! 0. Set turbine parameters (to be assigned via auxiliary input file in future version)
!____________________________________________________________________________________________________________________________

  hoff = 5.0 ! hub offset
  yaw_rate = 2.0 !degrees per second
  pitchdeg=  0.0
  dscale  =  1.0
  cscale  =  1.0
  rpm     = 16.0
  blades  =  3
  rho     =  1.225
  start_fac = 1.0
  yaw_thresh = 10000.0 !10.0*60.0*10.0**2
  yaw_new = 0.0
  tilt_deg = 4.0 ! in degrees
  tilt = tilt_deg*pi/180.0
  cdh = 0.0! 1.0
  cdt = 0.0!  1.2
  !JDM-04-2015  d_tower = 2.0 !m
  Rtwrt = 1.5 !NM radius is say 1.5 meters
  Rtwrb = 2.0 !NM radius of tower at base
  !print*,'n_timeseries',n_timeseries

  !RSA if this is the first timestep, then read in turbine info 
  !from file or hard coded values
  dum = 0.0
  DO k = 1,n_timeseries
      dum = dum + v0t(k,1)
  ENDDO
  ! print*,'dum',dum
  
  IF ( dum .EQ. 0.0 ) THEN
     !RSA for turbine info read from file (adapted from NM code)
     CALL nl_get_windturb_spec(1, fname )

     IF (TRIM(fname) .NE. 'none') THEN
        OPEN(file=TRIM(fname),unit=19,FORM='FORMATTED',STATUS='OLD',IOSTAT=istat)
        IF ( istat .EQ. 0 ) THEN !no error in file open
           n = 0
           !count number of lines in file
           DO WHILE (.true.)
              READ(19,'(A256)',END=30) inline !RSA read file and goto 30 if end of file
              IF ( index(inline,'!') .EQ. 0 ) n = n + 1
           ENDDO

30         CONTINUE !RSA reached end of file
           IF (n_turbines .NE. n) THEN
              print*,'RSA n_turbines in namelist does not match number of turbines in file windturb_spec'
              STOP
              ! CALL wrf_error_fatal('RSA n_turbines in namelist does not match number of turbines in file windturb_spec')
           ENDIF

           REWIND(19) !RSA go back to beginning of file
           print*,'RSA reading input file windturb_spec for ',n,' turbines'
           i = 1
           DO WHILE (.true.)
              READ(19,'(A256)',END=120) inline !RSA read file and goto 120 if end of file
              IF ( i .LE. n_turbines .AND. index(inline,'!') .EQ. 0 ) THEN
                 !RSA read in each part of the line seperated by spaces and go to line 130 if there is an error
                 READ(inline,*,ERR=130) lat,lon,hubheight,diameter,bldlngth,thetaturb,cutin,cutout 
                 x_turbine(i) = lon   
                 y_turbine(i) = lat
                 hub_height(i) = hubheight
                 rotor_diameter(i) = diameter
                 blade_length(i) = bldlngth
                 theta_turbine(i) = thetaturb
                 cutinspd(i) = cutin
                 cutoutspd(i) = cutout
                 i = i + 1 !RSA counting lines
              ENDIF
           ENDDO

120	     CONTINUE !RSA reached end of file
           CLOSE(19)
           GOTO 150

130	     CONTINUE !RSA error so close file and set istat to nonzero
           CLOSE(19)
           istat = 150150
           GOTO 150

150        CONTINUE
        ELSE
           print*,'RSA UNABLE TO OPEN OR READ TURBINE FILE'
           ! CALL wrf_error_fatal('RSA UNABLE TO OPEN OR READ TURBINE FILE')
        ENDIF
     ELSE 
        !RSA for hard coded turbine info
        PRINT*,"RSA NO TURBINE FILE SPECIFIED, USING HARD CODED TURBINE LOCATION."
        x_turbine(1)      = 500.0 
        y_turbine(1)      = 500.0
        hub_height(1)     = 80.0  
        rotor_diameter(1) = 77.0 
        blade_length(1)   = 38.5*(1.0-0.0444) !PSU1.5 turbine
        theta_turbine(1)  = 0.0

        !JDM for hard coded turbine info
        !turb_xy_locs( 1) = 200.1
        !turb_xy_locs( 2) = 400.1
        !turb_xy_locs( 3) = 600.1
        !turb_xy_locs( 4) = 800.1
        !turb_xy_locs( 5) = 1000.1
        !turb_xy_locs( 6) = 1200.1
        !turb_xy_locs( 7) = 1400.1
        !turb_xy_locs( 8) = 1600.1
        !turb_xy_locs( 9) = 1800.1
        !turb_xy_locs(10) = 2000.1
        ! sgn = 1
        ! DO i = 1,10
        ! DO j = 1,10
        !   x_turbine(sgn)      = turb_xy_locs(i)
        !   y_turbine(sgn)      = turb_xy_locs(j)
        !   hub_height(sgn)     = 80.0  
        !   rotor_diameter(sgn) = 101.0 
        !   blade_length(sgn)   = 49.0  
        !   theta_turbine(sgn)  = (360.0/100.0)*(sgn-1)
        !   sgn = sgn + 1
        ! END DO
        ! END DO
        ! DO i = 1,100
        !   print*,'i angle ',i,theta_turbine(i)
        ! END DO
     ENDIF
  ENDIF

!____________________________________________________________________________________________________________________________
!
! 1. Find x, y locations for u, v, and w nodes within the patch
!____________________________________________________________________________________________________________________________

  i_start = its
  i_end   = MIN(ite,ide-1)
  j_start = jts
  j_end   = MIN(jte,jde-1)

!print*,'i_start, i_end',i_start,i_end
!print*,'j_start, j_end',j_start,j_end

  dxo2 = dx/2.0
  dyo2 = dy/2.0

  DO i = i_start, i_end !only for visualizing instantaneous position of multiple turbines
  DO j = j_start, j_end
  DO k = 1, 20
    wp_ts(i,k,j) = 0.0
  END DO
  END DO
  END DO

!print*,'i,u_x(i)'
  DO i = i_start, i_end+1
 
    u_x(i) = (i-1)*dx
    v_x(i) = dxo2 + u_x(i)
    w_x(i) = v_x(i)

!print*,i,u_x(i)

  END DO

!print*,'j,v_y(j)'

  DO j = j_start, j_end+1
 
    v_y(j) = (j-1)*dy
    u_y(j) = dyo2 + v_y(j)
    w_y(j) = u_y(j)

!print*,j,v_y(j)

  END DO

  turbine_on_patch = 0


  DO n = 1, n_turbines
!____________________________________________________________________________________________________________________________
!
! 2. If some part of the turbine is on this patch, compute the height dependent gridpoint position variables
!____________________________________________________________________________________________________________________________

!--------- Define if turbine falls within tile neighborhood
  
    xmin_tile=u_x(i_start)
    xmax_tile=u_x(i_end+1)
    ymin_tile=v_y(j_start)
    ymax_tile=v_y(j_end+1)

    xmin_turb = x_turbine(n) - rotor_diameter(n)
    xmax_turb = x_turbine(n) + rotor_diameter(n)
    ymin_turb = y_turbine(n) - rotor_diameter(n)
    ymax_turb = y_turbine(n) + rotor_diameter(n)

!print*,'xmin_turb,xmax_turb,ymin_turb,ymax_turb ',xmin_turb,xmax_turb,ymin_turb,ymax_turb

!-- If turbine rectangle overlaps with tile do the following
!   the if statement actually checks if there is no overlap

    IF ( .NOT. ( (xmax_tile .LT. xmin_turb) .OR.              &  
                 (xmin_tile .GE. xmax_turb) .OR.              &   !changed from GT
                 (ymax_tile .LT. ymin_turb) .OR.              &  
                 (ymin_tile .GE. ymax_turb) )         ) THEN      !changed from GT

 
!   print*,'------------------itimestep------------------',itimestep

      turbine_on_patch = 1

      DO j = j_start-1, j_end+1   
        DO i = i_start-1, i_end+1

           w_z(i,kts,j)=0.0

        END DO
      END DO
  
      zmax_turb = hub_height(n) + rotor_diameter(n)*0.5                                      

      DO k = kts+1,kte             ! compute 1D column of z-values to determine k_end for the tile

        w_z(i_start,k,j_start) = w_z(i_start,k-1,j_start) + 1.0/rdzw(i_start,k-1,j_start)

      END DO
        
      DO k=kts+1,kte-1             ! k_end = 1 index greater to allow for nonuniform heights within the tile

        IF ( (w_z(i_start,k-1,j_start) .LT. zmax_turb) .AND.    &
             (w_z(i_start,k,j_start) .GE. zmax_turb)         ) THEN

          k_end=k+1

        ENDIF

      ENDDO 

      DO j = j_start, j_end        ! compute the rest of w_z up to k_end
        DO k = kts+1, k_end+1
          DO i = i_start, i_end

            w_z(i,k,j) = w_z(i,k-1,j) + 1.0/rdzw(i,k-1,j)

          END DO
        END DO
      END DO

      DO j = j_start, j_end         ! compute the i_start and i_end ghost nodes
        DO k = kts+1, k_end+1

          w_z(i_start-1,k,j)=w_z(i_start,k,j)
          w_z(i_end+1,k,j)=w_z(i_end,k,j)

        END DO
      END DO

      DO k = kts+1, k_end+1           ! compute the j_start and j_end ghost nodes
        DO i = i_start, i_end  

          w_z(i,k,j_start-1) = w_z(i,k,j_start)
          w_z(i,k,j_end+1) = w_z(i,k,j_end)

        END DO
      END DO

      DO k = kts+1, k_end+1           ! compute get the corner ghost nodes

        w_z(i_start-1,k,j_start-1)=0.5*(w_z(i_start-1,k,j_start) + &
                                       w_z(i_start,k,j_start-1) )
        w_z(i_start-1,k,j_end+1)=0.5*(w_z(i_start-1,k,j_end) +     &
                                        w_z(i_start,k,j_start+1) )
        w_z(i_end+1,k,j_start-1)=0.5*(w_z(i_end+1,k,j_start) +     &
                                    w_z(i_end,k,j_start-1) ) 
        w_z(i_end+1,k,j_end+1)=0.5*(w_z(i_end+1,k,j_end) +     &
                                  w_z(i_end,k,j_end+1) ) 

      END DO

      DO j = j_start, j_end           ! compute u_z and v_z up to k_end
        DO k = kts, k_end
          DO i = i_start, i_end+1

! 0.5 for averaging in horizontal, 0.5 for averaging in vertical

            u_z(i,k,j) = 0.5*(   0.5*( w_z(i-1,k+1,j) + w_z(i,k+1,j) )   &
                               + 0.5*( w_z(i-1,k,j)   + w_z(i,k,j)   ) )

          END DO
        END DO
      END DO

      DO j = j_start, j_end+1           ! compute u_z and v_z up to k_end
        DO k = kts, k_end
          DO i = i_start, i_end

! 0.5 for averaging in horizontal, 0.5 for averaging in vertical
     
            v_z(i,k,j) = 0.5*(   0.5*( w_z(i,k+1,j-1) + w_z(i,k+1,j) )   &
                               + 0.5*( w_z(i,k,j-1)   + w_z(i,k,j)   ) )

          END DO
        END DO
      END DO

    END IF ! turbine neighborhood is on patch

  END DO ! n_turbines

!____________________________________________________________________________________________________________________________
!
! 3. If turbine neighborhood is on patch, check if the hub is too. 
! If so, find the wind speed at each hub location and exchange across all processors
! Each processor must send and receive, so we cannot put this in a turbine_on_patch loop. 
!____________________________________________________________________________________________________________________________

! shift wind speed for each turbine (column of v0t) back by one unit to make room
! for the current v0 value 

  IF ( itimestep .le. 1) THEN

    v0t = 0.0
    d0t = 0.0
    acc_yaw_err = 0.0

!    start_time = 1
!    n_mins_startup = 1.0
!    nt = ceiling( n_mins_startup*60.0/dt )
!    start_fac = MIN(float(itimestep - start_time)/float(nt),1.0) !Apply turbine forces gradually at startup

  ENDIF

  DO n = 1, n_turbines
    DO k = n_timeseries-1,1,-1
      v0t(k+1,n)=v0t(k,n)
    ENDDO
  ENDDO

  IF (wp_opt .EQ. 3 ) THEN

    DO n = 1, n_turbines
      DO k = n_timeseries-1,1,-1
        d0t(k+1,n)=d0t(k,n)
      ENDDO
    ENDDO

  ENDIF

  DO n = 1, n_turbines

    IF (turbine_on_patch .EQ. 1 ) THEN ! 

      xmin_tile = u_x(i_start)
      xmax_tile = u_x(i_end+1)
      ymin_tile = v_y(j_start)
      ymax_tile = v_y(j_end+1)

      theta =  theta_turbine(n)*pi/180.0

!-- If turbine hub is within the tile do the following

      IF ( (x_turbine(n) .GE. xmin_tile) .AND. (x_turbine(n) .LT. xmax_tile) .AND. &
           (y_turbine(n) .GE. ymin_tile) .AND. (y_turbine(n) .LT. ymax_tile) ) THEN
  
!      print*,'turbine hub n on patch mytask',n,mytask
!      print*,'xmin_tile,xmax_tile',xmin_tile,xmax_tile
!      print*,'ymin_tile,ymax_tile',ymin_tile,ymax_tile

        imin = minloc(abs(u_x(i_start:i_end)-x_turbine(n)),1)+i_start-1 !Using u-locations for no reason other than convenience
        jmin = minloc(abs(u_y(j_start:j_end)-y_turbine(n)),1)+j_start-1
     
        imin = imin-6 ! 1D Upstream

        IF (imin .LT. its ) THEN

          print*,'imin < its in phys/module_gen_act_disk.F, stopping'

          STOP

        ENDIF

        DO k = kts+1, k_end

          turb_z(k) = u_z(imin,k,jmin) - hub_height(n)

        END DO

        kmin=minloc(abs(turb_z(kts+1:k_end)),1)+kts

        v0inst = u(imin,kmin,jmin)*cos(theta)*cos(tilt)+v(imin,kmin,jmin)*sin(theta)*cos(tilt) !Instantaneous velocity component normal to the rotor.
                                                                       !Negative indicates flow coming from behind the rotor.

        IF (wp_opt .EQ. 3 ) THEN

          IF ( ( u(imin,kmin,jmin) .GE. 0.0 ) .AND. ( v(imin,kmin,jmin) .LT. 0.0 ) ) THEN    !Wind direction, independent of the
             d0inst = 360.0 - (atan2(u(imin,kmin,jmin),v(imin,kmin,jmin) )*180.0/pi - 90.0)  !rotor angle. 0 and 360 are Westerly. 
          ELSE                                                                               !Positive values are counterclockwise
             d0inst = 90.0 - atan2(u(imin,kmin,jmin),v(imin,kmin,jmin) )*180.0/pi            !up to 360. No negative values.
          ENDIF

!        print*,'imin,jmin,u,v ',imin,jmin,u(imin,kmin,jmin),v(imin,kmin,jmin)
!        print*,'v0inst, d0inst',v0inst,d0inst
!        print*,'n u ,v',n, u(imin,kmin,jmin),v(imin,kmin,jmin) 
!        print*,'n v0inst,d0inst',n,v0inst,d0inst 

        ENDIF    

#ifdef DM_PARALLEL

        tag=n

        CALL MPI_SEND(v0inst,1,MPI_REAL,master,tag,            &
                             MPI_COMM_WORLD,ierr )

        IF (wp_opt .EQ. 3 ) THEN
          CALL MPI_SEND(d0inst,1,MPI_REAL,master,tag,            &
                                 MPI_COMM_WORLD,ierr )
        ENDIF
#else
        v0n(n) = v0inst
        IF (wp_opt .EQ. 3 ) THEN
          d0n(n) = d0inst
        ENDIF
#endif

      ENDIF ! hub is on the tile

    ENDIF ! Part of turbine is on this patch,

  END DO ! n_turbines

#ifdef DM_PARALLEL

  IF ( mytask .eq. master ) THEN

    DO n=1,n_turbines

      tag=n
      CALL MPI_RECV(v0inst,1,MPI_REAL,MPI_ANY_SOURCE,tag,                 &
                             MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierr )
      v0n(n)=v0inst

    ENDDO

    IF (wp_opt .EQ. 3 ) THEN

      DO n=1,n_turbines

        tag=n
        CALL MPI_RECV(d0inst,1,MPI_REAL,MPI_ANY_SOURCE,tag,                 &
                               MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierr )
        d0n(n)=d0inst

      ENDDO

    ENDIF

  ENDIF ! turbine on patch

  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

  CALL MPI_BCAST(v0n,n_turbines,MPI_REAL,master,MPI_COMM_WORLD,ierr)

  IF (wp_opt .EQ. 3 ) THEN
    CALL MPI_BCAST(d0n,n_turbines,MPI_REAL,master,MPI_COMM_WORLD,ierr)
  ENDIF

#endif 

! Check to make sure namelist values of ntme_turb and n_turbines are consistent
!  nv0t=size(v0t,1)
!  IF ( nv0t .NE. (n_timeseries/n_turbines) ) THEN 
!    PRINT*,'nvot not equal to ntme_turb/n_turbines'
!    STOP
!  ENDIF

  IF (turbine_on_patch .EQ. 1 ) THEN !only do this if part of the turbine is on the patch

!____________________________________________________________________________________________________________________________
!
! 4. Add current v0 to timeseries in v0t, compute current average, and calculate pitch and rpm
!____________________________________________________________________________________________________________________________
!
    DO n = 1, n_turbines

! cnsider skipping the following lines for a gradual startup

!     IF ( sum(v0t(:,n)) .eq. 0. ) THEN !This is the first time the turbine has run

      dum = 0.0                         ! Looking for problems...
      DO k = 1,n_timeseries
        dum = dum + v0t(k,n)
      ENDDO

      IF ( dum .eq. 0. ) THEN !

!      print*,'n, here 1, sum(v0t(:,n)',n,dum

!!!        v0t(:,n)=v0n(n)             ! Assign all rows (times) of each column (turbine number) the current v0 

        DO k = 1,n_timeseries          ! Looking for problems...
          v0t(k,n) = v0n(n)
          d0t(k,n) = d0n(n)
        ENDDO

!!        IF (wp_opt .EQ. 3 ) THEN
!          d0t(:,n)=d0n(n)   
!!        ENDIF

      ENDIF 

      v0t(1,n)=v0n(n)                  ! Assign the first element the current v0 

!!     v0 = sum(v0t(:,n))/real(n_timeseries)

      dum = 0.0                        ! Looking for problems...
      DO k = 1,n_timeseries
         dum = dum + v0t(k,n)
      ENDDO
      v0 = dum/real(n_timeseries)

      IF (wp_opt .EQ. 3) THEN

        IF ( abs(acc_yaw_err(n)) .GE. yaw_thresh ) THEN ! If acc_yaw_err(n) exceeds threshold, turbine n begins yawing.
                                                        ! During yawing, no new entries are stored in d0t(n) and 
                                                        ! acc_yaw_err(n) remains fixed.Turbine n rotates toward
                                                        ! the time average direction before yawing began,yaw_new. 
                                                        ! The turbine rotates at yaw_rate until it is less than 
                                                        ! yaw_rate*dt from yaw_new. At this point turbine_theta is
                                                        ! set to yaw_new, yawing is set to 0, and everything starts over.

          yawing(n) = 1
       
        ELSE
   
          yawing(n) = 0 

        ENDIF

!        print*,'n,acc_yaw_err,yawing ',n,acc_yaw_err(n),yawing(n) 
!        print*,'v0n(n),d0n(n)',v0n(n),d0n(n)

        IF (yawing(n) .EQ. 0 ) THEN
 
          d0t(1,n)=d0n(n)

          delta = theta_turbine(n) - d0t(1,n)

          IF ( delta .LT. -180.0 ) THEN 

            delta = theta_turbine(n) + ( 360.0 - d0t(1,n) )
  
          ELSE IF (delta .GE. 180.0 ) THEN 

            delta = theta_turbine(n) - ( 360.0 + d0t(1,n) )

          ENDIF

          sgn = 1.0

          IF ( delta .LT. 0.0 ) sgn = -1.0
 
          yaw_err = sgn*(delta)*(delta)*dt

          acc_yaw_err(n) = acc_yaw_err(n) + yaw_err
  
!          print*,'delta,theta_turbine(n) ',delta,theta_turbine(n)

!          print*,'------------------'

        ELSE ! turbine is yawing or ready to start

!        yaw_new = sum(d0t(:,n))/real(n_timeseries)

         dum = 0.0
         DO k = 1,n_timeseries
           dum = dum + d0t(k,n)
         ENDDO

         yaw_new = dum/real(n_timeseries)

         yaw_inc = yaw_rate*dt

         delta = theta_turbine(n) - yaw_new  

         IF ( delta .LT. -180.0 ) THEN 

           delta = theta_turbine(n) + ( 360.0 - d0t(1,n) )
  
         ELSE IF (delta .GE. 180.0 ) THEN 

           delta = theta_turbine(n) - ( 360.0 + d0t(1,n) )

         ENDIF

         sgn = 1.0

         IF ( delta .GT. 0.0 ) sgn = -1.0

         IF ( abs(yaw_inc) .GE. abs(delta) ) THEN 

           theta_turbine(n) = yaw_new

           yawing(n) = 0

           acc_yaw_err(n) = 0.0

         ELSE

           theta_turbine(n) = theta_turbine(n) + sgn*yaw_inc

         ENDIF

        ENDIF !yawing or not

        ! Keep turbine-theta between 0 and 360 degrees.

        IF ( theta_turbine(n) .GE. 360.0 ) theta_turbine(n) = theta_turbine(n) - 360.0
        IF ( theta_turbine(n) .LT. 0.0 ) theta_turbine(n) = 360.0 + theta_turbine(n)

      ENDIF !wp_opt .EQ. 3

! Kosovic, December 14, 2012 - end modification
!---------------------------------------------

      theta =  theta_turbine(n)*pi/180.0

      IF ( v0 .lt. 0.0 ) THEN
        v0 = 0.0
      ENDIF

      rpm   = calc_rpm_psu15( v0 ) ! Use time average velocity at hub location for rpm and pitch
      pitchdeg = calc_pitch_psu15( v0 )
   
      DO j = j_start, j_end
        DO i = i_start, i_end

           rotrate(i,j) = 2.0*pi*rpm/60.0
           thrust(i,j)  = 0.0
           torque(i,j)  = 0.0
           power(i,j)   = 0.0

        ENDDO
      ENDDO

!--------end----added by B. Kosovic, NCAR, July 5, 2012
!------------------------------------------------------

!____________________________________________________________________________________________________________________________
!
! 5.  Compute distances from computational nodes to rotor place in order to 
!     ii) Smear forces as a function of distance in the axial direction
!____________________________________________________________________________________________________________________________
!
! Compute distances from computational nodes to rotor place in order to 
!  ii) Smear forces as a function of distance in the axial direction

! dau,dav,daw - Signed distance, in a horizontal plane, from the gridpoint to a line
!               going through the turbine (nacelle) axis. 
! dru,drv,drw - signed distance, in a horizontal plane, from the vertical rotor plane

      CALL calc_distance( x_turbine(n), y_turbine(n), theta_turbine(n), &
                          its, ite, jts, jte, &
                          i_start, i_end, j_start, j_end,        &
                          u_x, u_y, v_x, v_y, w_x, w_y,          &
                          dau, dav, daw, dru, drv, drw )

      DO j = j_start, j_end
        DO k = kts, k_end
          DO i = i_start, i_end
 
            dzu(i,k,j) = u_z(i,k,j) - hub_height(n)
            dzv(i,k,j) = v_z(i,k,j) - hub_height(n)
            dzw(i,k,j) = w_z(i,k,j) - hub_height(n)

          END DO
        END DO
      END DO

      DO j = j_start, j_end
        DO k = kts, k_end
          DO i = i_start, i_end

            ru(i,k,j) = sqrt(dau(i,j)**2+dzu(i,k,j)**2)
            rv(i,k,j) = sqrt(dav(i,j)**2+dzv(i,k,j)**2)
            rw(i,k,j) = sqrt(daw(i,j)**2+dzw(i,k,j)**2)
              
            wp_acc_u(i,k,j) = 0.0
            wp_acc_v(i,k,j) = 0.0
            wp_acc_w(i,k,j) = 0.0

          END DO
        END DO
      END DO

      rotor_radius = 0.5*rotor_diameter(n)

      cnst = 1.0/rho !JDM

!      sigma = sqrt(dx*dy)
!      adenom = sigma*sqrt(2.0*pi)
!      bdenom  = 2.0*sigma*sigma

      sigma   = (abs(dx*cos(theta))+abs(dy*sin(theta))) !NM so normal for rotor plane sqrt(dx*dy)	!!!This is the standard deviation
      adenom  = sigma*sqrt(2.0*pi)                  !want integral=1 for Gaussian ;accel will have units of 1/m
      bdenom  = 2.*sigma*sigma                      !!determines spread of gaussian

      !NM sigmar normal to rotor plane
      sigmar = abs(dx*cos(theta)*cos(tilt))+abs(dy*sin(theta)*cos(tilt)) !NM
      adenom2 = sigmar*sqrt(2.0*pi) !NM want integral to be 1 for gaussian
      bdenom2 = 2.*sigmar**2 !NM spread of 1-D gaussian 

      radmin = rotor_diameter(n)/2.0 - blade_length(n)

!____________________________________________________________________________________________________________________________
!
! 6. Compute local lift and drag accelerations using instantaneous local velocity, and apply to u,v, and w tendencies
!____________________________________________________________________________________________________________________________


      DO j = j_start, j_end

        DO k = kts, k_end

          DO i = i_start, i_end

             v0 = u(i,k,j)*cos(theta)*cos(tilt) + v(i,k,j)*sin(theta)*cos(tilt) - w(i,k,j)*sin(tilt)  

             IF (cdt .GT. 0.0 ) THEN !Tower------------------------------------------------------	

                    IF (dzu(i,k,j) .le. -(radmin)) then !NM tower below hub

                        Rtwr = abs(dzu(i,k,j))*(Rtwrb-Rtwrt)/hub_height(n)+Rtwrt !NM linear	 

                        IF ( abs( dau(i,j) ) .le. Rtwr) then !NM tower below hub; dau needs to be less than tower radius

!JDM-04-201 Note to self. This should be fixed so that the tower is not grid location dependent. Same goes for the hub!

                            fn = 0.5*rho*v0*v0*Cdt !NM force per unit area for tower
                            ft = 0.0 !NM no tangential force
                            acc_u = (1.0/adenom)*exp(-(dru(i,j)**2)/bdenom) !NM 1-D !Gaussian
                            fx = fn*cos(theta) !NM to x direction

                            ru_tendf(i,k,j) = ru_tendf(i,k,j) - muu(i,j)*acc_u*cnst*fx !NM

                        END IF

                     END IF

                    IF (dzv(i,k,j) .le. -(radmin)) then !NM tower below hub

                        Rtwr = abs(dzv(i,k,j))*(Rtwrb-Rtwrt)/hub_height(n)+Rtwrt !NM linear	 

                        IF ( abs( dav(i,j) ) .le. Rtwr) then !NM tower below hub; dau needs to be less than tower radius

!JDM-04-201 Note to self. This should be fixed so that the tower is not grid location dependent. Same goes for the hub!

                            fn = 0.5*rho*v0*v0*Cdt !NM force per unit area for tower
                            ft = 0.0 !NM no tangential force
                            acc_v = (1.0/adenom)*exp(-(drv(i,j)**2)/bdenom) !NM 1-D !Gaussian
                            fy = fn*sin(theta) !NM to x direction
                            rv_tendf(i,k,j) = rv_tendf(i,k,j) - muv(i,j)*acc_v*cnst*fy !NM

                        END IF

                     END IF

                ENDIF !cdt > 0

!------ X ---- 

	     IF ( (ru(i,k,j) .le. radmin) .and. (ru(i,k,j) .gt. -0.001) ) then !HUB-----------------------------------	
          
	        fn = 0.5*rho*v0*v0*Cdh  !NM only need force per unit area

	        ft = 0.0 !NM no tangential force

	        !NM including off set of hub from tower hoff in + dru
	        !direction or opposite of wind direction U

	        acc_u = (1.0/adenom)*exp(-((dru(i,j)-hoff)**2)/bdenom) !NM 1-D gaussian, hub has no tilt.
 
                fx = fn*cos(theta) 	 
 
                ru_tendf(i,k,j) = ru_tendf(i,k,j) &
                                  - muu(i,j)*acc_u*cnst*fx

!JDM	        thrust(i,k,j) = thrust(i,k,j)-fn*cos(theta)*acc_u     		
                !!thrust everywhere is equal to current thrust plus force distribution due to gaussian times normal force

	     ELSE !GAD-------------------------------------------------------------------------------------------------

	        IF (tilt .eq. 0.0) THEN !No Tilt-----------------------------------------------------------------------

                   drut = dru(i,j)-hoff	!distance only depends on x value!!!if theta is zero the normal distance will be x0

                   radius = sqrt( dau(i,j)**2 + dzu(i,k,j)**2 )

                   zeta = atan2( dzu(i,k,j), dau(i,j) )

                ELSE !Tilt ------------------------------------------------------------------------------------------

! drut-----------------

                   drut = calc_yprime( 1.0/tan(tilt), 1.0, 0.0, (dru(i,j)-hoff), dzu(i,k,j) ) 

! zd-----------------
 
                   zd = calc_yprime( -tan(tilt), 1.0, 0.0, (dru(i,j)-hoff), dzu(i,k,j) ) 

		   radius = sqrt( dau(i,j)**2 + zd**2 ) !NM new radial distance to tilted turbine:

                   zeta = atan2( zd, dau(i,j) )

!                   WRITE(*,201),'i,j,k,dru,drut',i,j,k,drut,zd

                ENDIF !Tilt or No Tilt-------------------------------------------------------------------------------

                acc_u = (1.0/adenom2)*exp(-(drut**2)/bdenom2) !NM 1-D

                !RSA adding logic to prevent fx from being NaN. See explanation in calc_fnft below.
                if ( v0 .gt. 0.0 ) then
                   fnft = calc_fnft_psu15( v0, rho, blades, blade_length(n), rotor_diameter(n),   &
                                           radius, pitchdeg, rpm, dscale, cscale) 

                   fn = fnft(1)
                   ft = fnft(2)
     
                   fx = fn*cos(theta)*cos(tilt) + ft*sin(zeta)*sin(theta) - ft*cos(zeta)*sin(tilt)*cos(theta)
                else
                   !RSA set fx to 0 for now, but turbine should eventually yaw into place.
                   ! write(*,*) "RSA to prevent NaNs, skipping call to calc_fnft for fx: i,j,k=",i,j,k 
                   fx = 0.0
                endif
                
!JDM TEST SIGN 12/2014     fx = fn*cos(theta)*cos(tilt) 
!JDM TEST SIGN 12/2014     fx = ft*sin(zeta)*sin(theta) 
!JDM TEST SIGN 12/2014     fx = -ft*cos(zeta)*sin(tilt)*cos(theta)

!print*,'i,j,k',k,j,i
!print*,'v0, pitchdeg, rpm',v0, pitchdeg, rpm
!print*,'fn,ft,fx,accu',fn,ft,fx,acc_u

                ru_tendf(i,k,j) = ru_tendf(i,k,j) &
                                 - muu(i,j)*acc_u*cnst*fx

                wp_ts(i,k,j) = -1.0*muu(i,j)*acc_u*cnst*fx

              ENDIF !Hub or GAD-----------------------------------------------------------------------------------------

!------- Y ----

	      IF ( (rv(i,k,j) .le. radmin) .and. (rv(i,k,j) .gt. -0.001) ) then !HUB------------------------------------	

		  fn = 0.5*rho*v0*v0*Cdh 

		  ft = 0.0

		  acc_v = (1.0/adenom)*exp(-((drv(i,j)-hoff)**2)/bdenom) 

		  fy = fn*sin(theta) 

		  rv_tendf(i,k,j) = rv_tendf(i,k,j) &
                                   - muv(i,j)*acc_v*cnst*fy

	      ELSE !GAD-------------------------------------------------------------------------------------------------	

                 IF (tilt .eq. 0.0) THEN !No Tilt-----------------------------------------------------------------------

        	    drvt = drv(i,j)-hoff	!distance only depends on x value !!!if theta is zero the normal distance will be x0

                    radius = sqrt( dav(i,j)**2 + dzv(i,k,j)**2 )

                    zeta = atan2( dzv(i,k,j), dav(i,j) )

                 ELSE !Tilt---------------------------------------------------------------------------------------------

                    drvt = calc_yprime( 1.0/tan(tilt), 1.0, 0.0, (drv(i,j)-hoff), dzv(i,k,j) )  

                    zd = calc_yprime( -tan(tilt), 1.0, 0.0, (drv(i,j)-hoff), dzv(i,k,j) )

                    radius = sqrt( dav(i,j)**2 + zd**2 )

                    zeta = atan2( zd, dav(i,j) )

		 ENDIF !Tilt or No Tilt-------------------------------------------------------------------------------

                 acc_v = (1.0/adenom2)*exp(-(drvt**2)/bdenom2) !NM 1-D

                !RSA adding logic to prevent fy from being NaN. See explanation in calc_fnft below.
                if ( v0 .gt. 0.0 ) then
                   fnft = calc_fnft_psu15( v0, rho, blades, blade_length(n), rotor_diameter(n),   &
                                           radius, pitchdeg, rpm, dscale, cscale)  

                   fn = fnft(1)
                   ft = fnft(2)

                   fy = fn*sin(theta)*cos(tilt) - ft*sin(zeta)*cos(theta) - ft*cos(zeta)*sin(tilt)*sin(theta)	!NM 
                else
                   !RSA set fy to 0 for now, but turbine should eventually yaw into place.
                   ! write(*,*) "RSA to prevent NaNs, skipping call to calc_fnft for fy: i,j,k=",i,j,k 
                   fy = 0.0
                endif

!JDM TEST SIGN 12/2014      fy = fn*sin(theta)*cos(tilt)
!JDM TEST SIGN 12/2014      fy = -ft*sin(zeta)*cos(theta)
!JDM TEST SIGN 12/2014      fy = -ft*cos(zeta)*sin(tilt)*sin(theta)

                 rv_tendf(i,k,j) = rv_tendf(i,k,j) &
                                  - muv(i,j)*acc_v*cnst*fy

!JDM                     wp_ts(i,k,j) = -1.0*muv(i,j)*acc_v*cnst*fy

              ENDIF !Hub or GAD------------------------------------------------------------------------------------

!-------- Z ----

              IF (tilt .eq. 0.0) THEN !No Tilt --------------------------------------------------------------------

        	 drwt = drw(i,j)-hoff	!distance only depends on x value              			!!!if theta is zero the normal distance will be x0

                 radius = sqrt( daw(i,j)**2 + dzw(i,k,j)**2 )

                 zeta = atan2( dzw(i,k,j), daw(i,j) ) 

              ELSE !Tilt ------------------------------------------------------------------------------------------

		 drwt = calc_yprime( 1.0/tan(tilt), 1.0, 0.0, (drw(i,j)-hoff), dzw(i,k,j) ) 
				 
                 zd = calc_yprime( -tan(tilt), 1.0, 0.0, (drw(i,j)-hoff), dzw(i,k,j) ) 
		
		 radius = sqrt( daw(i,j)**2 + zd**2 )

                 zeta = atan2( zd, daw(i,j) ) 

	      ENDIF !Tilt or No Tilt-------------------------------------------------------------------------------
	
	      acc_w = 	(1.0/adenom2)*exp(-(drwt**2)/bdenom2) !NM 1-D

              !RSA adding logic to prevent fz from being NaN. See explanation in calc_fnft below.
              if ( v0 .gt. 0.0 ) then
                 fnft = calc_fnft_psu15( v0, rho, blades, blade_length(n), rotor_diameter(n), &
                                         radius, pitchdeg, rpm, dscale, cscale ) 

                 fn = fnft(1)
                 ft = fnft(2) 

                 fz = -fn*sin(tilt) - ft*cos(zeta)*cos(tilt)
              else
                 !RSA set fz to 0 for now, but turbine should eventually yaw into place.
                 ! write(*,*) "RSA to prevent NaNs, skipping call to calc_fnft for fz: i,j,k=",i,j,k 
                 fz = 0.0
              endif

!JDM TEST SIGN      fz = -fn*sin(tilt) 
!JDM TEST SIGN      fz = -ft*cos(zeta)*cos(tilt) 

              rw_tendf(i,k,j) = rw_tendf(i,k,j)  &
                              - mut(i,j)*acc_w*cnst*fz

!JDM                 wp_ts(i,k,j) = -1.0*mut(i,j)*acc_w*cnst*fz
 


!!         Assuming turbine rotors are perpendicualar to x axis. Generalize to arbitrary orientation.  
!
!!            IF ( ru(i,k,j) .lt. rotor_radius ) THEN
!               thrust(i,j) = thrust(i,j) +                             &
!                             wp_acc_u(i,k,j)*fn
!               torque(i,j) = torque(i,j) +                             &
!                             wp_acc_u(i,k,j)*ru(i,k,j)*ft
!                power(i,j)  = power(i,j)  +                             &
!                             wp_acc_u(i,k,j)*rotrate(i,j)*ru(i,k,j)*ft
!!            ENDIF
!!

          END DO         ! j loop
        END DO         ! k loop
      END DO       ! i loop

    END DO  ! n loop over all turbines

  ENDIF !turbine_on_patch = 1

201 format (a18,3I6,2F12.6)

END SUBROUTINE gen_act_disk

!==============================================================================

SUBROUTINE calc_distance( x_turb, y_turb, theta_turb,     &
                          its, ite, jts, jte,             &
                          i_start, i_end, j_start, j_end, &
                          u_x, u_y, v_x, v_y, w_x, w_y,   &
                          dau, dav, daw, dru, drv, drw    )

!-----------------------------------------------------------------------------
!
! Purpose: To calculate distances from the axis and rotor plane in (x,y)
! plane. 
!
!  October, 2010: Branko Kosovic, NCAR
!  Summer, 2013: JDM. Modified for consistent sign relative to quadrant.
!
!=============================================================================

  REAL, INTENT( IN )                                 :: x_turb
  REAL, INTENT( IN )                                 :: y_turb
  REAL, INTENT( IN )                                 :: theta_turb
  INTEGER, INTENT( IN )                              :: its, ite, jts, jte
  INTEGER, INTENT( IN )                              :: i_start, i_end
  INTEGER, INTENT( IN )                              :: j_start, j_end

  REAL, DIMENSION( its:ite ), INTENT( IN )           :: u_x, v_x, w_x
  REAL, DIMENSION( jts:jte ), INTENT( IN )           :: u_y, v_y, w_y

  REAL, DIMENSION( its:ite, jts:jte ), INTENT( OUT ) :: dau, dav, daw !signed distance from each gridpoint to axis centerline
  REAL, DIMENSION( its:ite, jts:jte ), INTENT( OUT ) :: dru, drv, drw !signed distance from each gridpoint to rotor plane

! LOCAL VARIABLES

  REAL    :: aa, ba, ca
  REAL    :: ar, br, cr
  REAL    :: x0u, x0v, x0w
  REAL    :: y0u, y0v, y0w
  INTEGER :: i, j

  theta =  theta_turb*pi/180.0

! rp indicates rotor plane 

!  z                                               y    
!  |             rp                          rp    |      tan(theta) = y/x  
!                |                            \    |               .    
!                |                             \   |           . ^      
!                |                              \  |       .     |       
!                |___                            \ |   .   theta | 
!    .  .  .  .  |___|  .  .  .       ____________\|_____________|__ x
!        |       | ||                          .   |\
!    dau |       | ||                      .       | \
!        |       | ||                  .  \        |  \
!        p _ _ _ | ||                      \       | . \
!           dru    ||                   dau \    . |dru \
!                  ||                        p     |
!__________________||___ surface                            


!
! 1. Obtain coefficients for normal distance, dau, to axial plane, the plane 
! containing the dotted lines above, normal to the center of the rotor disk. 
! tan(theta) = y/x is the slope of this line in the x-y plane.
!
! If looking from a point p upwind of the turbine and facing it, positive
! distance is to the right and negative is to the left. Switching this
! sign convention along with the zd sign convention would be needed to get
! assumtion that ft rotates clockwise and not counterclockwise 	

  IF ( ( theta .LT. 0.5*pi ) .OR. (theta .GT. 1.5*pi) ) THEN   ! Quadrants 1 or 4 	
        aa = tan(theta)                                          
        ba = -1.0                      !neg because for ax + by + c = 0, slope m = -a/b                                        
     ELSEIF ( theta .EQ. 0.5*pi ) THEN	                   ! INFINITE SLOPE
        aa = 1.0                       !x coordinate of point p 
	ba = 0.0                      
     ELSEIF ( theta .EQ. 1.5*pi ) THEN	                   ! INFINITE SLOPE
    	aa = -1.0                      !negative of x coordinate of point p 
        ba = 0.0
     ELSE                                                  ! Quadrants 2 or 3 	
     	aa = -tan(theta)
	ba = 1.0	
     ENDIF
     ca = 0.0                    !no slope intercept, center of rotor plane is the origin

! 2. Obtain coefficients for normal distance to rp, dru.
!    Negative downwind and positive (+) upwind

     IF ( theta .EQ. 0.5*pi ) THEN
        ar = 0.0
	br = -1.0                  !distance only depends on y value
     ELSEIF ( theta .EQ. 0.) THEN 
        ar = -1.0                  !distance only depends on x value, if theta is zero the normal distance will be x0
        br = 0.0  
     ELSEIF ( theta .LT. pi ) THEN 
        ar = -1.0/tan(theta)         !Since perpendicular, take negative reciprocal for slope
        br = -1.0                  !Must correspond to shortest line
     ELSEIF ( theta .EQ. 1.5*pi ) THEN
     	ar = 0.0
	br = 1.0                   !distance only depends on y value
     ELSEIF ( theta .EQ. pi ) THEN 
     	ar = 1.0                   !distance only depends on x value
	br = 0.0
     ELSE                          !theta between pi and 2pi
	ar = 1.0/tan(theta)
	br = 1.0
     ENDIF
     cr = 0.0

    DO j = j_start, j_end

      y0u= u_y(j) - y_turb
      y0v= v_y(j) - y_turb
      y0w= w_y(j) - y_turb

      DO i = i_start, i_end

         x0u= u_x(i) - x_turb
         x0v= v_x(i) - x_turb
         x0w= w_x(i) - x_turb

!JDM calc_distline returns unsigned distance.
!JDM ! calculate distance to the turbine axis in (x,y) plane
!JDM           dau(i,j)=calc_distline(aa,ba,ca,x0u,y0u)
!JDM           dav(i,j)=calc_distline(aa,ba,ca,x0v,y0v)
!JDM           daw(i,j)=calc_distline(aa,ba,ca,x0w,y0w)
!JDM 
!JDM ! calculate distance to the rotor plane of the turbine in (x,y) plane
!JDM            dru(i,j)=calc_distline(ar,br,cr,x0u,y0u)
!JDM            drv(i,j)=calc_distline(ar,br,cr,x0v,y0v)
!JDM            drw(i,j)=calc_distline(ar,br,cr,x0w,y0w)

!JDM calc_yprime returns signed distance. 

! calculate distance to the turbine axis in (x,y) plane

         dau(i,j)=calc_yprime(aa,ba,ca,x0u,y0u)
         dav(i,j)=calc_yprime(aa,ba,ca,x0v,y0v)
         daw(i,j)=calc_yprime(aa,ba,ca,x0w,y0w)
 
! calculate distance to the rotor plane of the turbine in (x,y) plane

         dru(i,j)=calc_yprime(ar,br,cr,x0u,y0u)
         drv(i,j)=calc_yprime(ar,br,cr,x0v,y0v)
         drw(i,j)=calc_yprime(ar,br,cr,x0w,y0w)

       END DO
    END DO

END SUBROUTINE calc_distance

!==============================================================================
!
!      Functions to compute distance from a line in 2d
!
!----------------------------------------------------------------------
FUNCTION calc_distline( a, b, c, x0, y0 ) result( distline ) 
!
    real, intent( in )   :: a, b, c
    real, intent( in )   :: x0, y0
    real                 :: distline
!   
    distline=abs(a*x0+b*y0+c)/sqrt(a*a+b*b)
!
END FUNCTION calc_distline

!==============================================================================
!
!      Function to compute signed distance from a line in 2D
!
!----------------------------------------------------------------------
FUNCTION calc_yprime( a, b, c, x0, y0 ) result( yprime ) 
!
    real, intent( in )   :: a, b, c
    real, intent( in )   :: x0, y0
    real                 :: yprime
!   
    yprime = (a*x0+b*y0+c)/sqrt(a*a+b*b)
!
END FUNCTION calc_yprime

!-------------------------------------------------------------

!=========================================================================

FUNCTION calc_rpm_psu15( x ) result( rpm )

!_________________________________________________________________________
!
! Data from GE-PSU/GE.inp
! ws = VWIND = wind speeds, cf = RPMPRE = rpm at wind speed ws
!_________________________________________________________________________


  REAL, INTENT( in ) :: x

  REAL, PARAMETER :: nws = 26

  REAL, DIMENSION( 0:nws-1 ) :: ws
  REAL, DIMENSION( 0:nws-1 ) :: rp

  REAL :: rpm
  INTEGER :: i

  rpm = 0.0

  rp(0:4) = 6.875
  rp(5) = 8.19
  rp(6) = 9.83
  rp(7) = 11.47
  rp(8) = 13.11
  rp(9:nws-1) = 13.75

  ws(0) = 0.0

  DO i = 1,nws-1
     
     ws(i) = float(i)

     IF ( ( x .GE. ws(i-1) ) .AND. ( x .LT. ws(i) ) ) THEN
        rpm = rp(i-1) + (x - ws(i-1))*( ( rp(i) - rp(i-1) )/( ws(i) - ws(i-1) ) )
        GOTO 1
     ENDIF

  END DO
  
  IF ( x .LT. ws(0) ) rpm = rp(0)
  IF ( x .EQ. ws(nws-1) ) rpm = rp(nws-1)

1 CONTINUE
 
!PRINT*,x,rpm
!_________________________________________________________________________

END FUNCTION calc_rpm_psu15
!_________________________________________________________________________
 
!=========================================================================

FUNCTION calc_pitch_psu15( x ) RESULT( pitch )

!_________________________________________________________________________
!
! Data from GE-PSU/GE.inp
! ws = VWIND = wind speeds, cf = PITCHPRE = blade pitch at wind speed ws
!_________________________________________________________________________
 
  REAL, INTENT( IN ) :: x

  REAL, PARAMETER :: nws = 26

  REAL, DIMENSION( 0:nws-1 ) :: ws 
  REAL, DIMENSION( 0:nws-1 ) :: p

  REAL :: pitch
  INTEGER :: i

  pitch = 0.0

  p(0:11) = 0.0
  p(12) = 6.4184
  p(13) = 9.8238
  p(14) = 12.5685
  p(15) = 14.9713
  p(16) = 17.1529
  p(17) = 19.1704
  p(18) = 21.0612
  p(19) = 22.8434
  p(20) = 24.5408
  p(21) = 26.1633
  p(22) = 27.7116
  p(23) = 29.1939
  p(24) = 30.6287
  p(25) = 32.0000
  
  ws(0) = 0.0

  DO i = 1,nws-1
     
      ws(i) = float(i)

     IF ( ( x .GT. ws(i-1) ) .AND. ( x .LE. ws(i) ) ) THEN 
        pitch = p(i-1) + (x - ws(i-1))*( ( p(i) - p(i-1) )/( ws(i) - ws(i-1) ) )
        GOTO 2
     ENDIF
     
  ENDDO

  IF ( x .LE. ws(0) ) pitch = p(0)
  IF ( x .GT. ws(nws-1) ) pitch = p(nws-1)

2 CONTINUE
  
!PRINT*,x,pitch
!_________________________________________________________________________

END FUNCTION calc_pitch_psu15
!_________________________________________________________________________


FUNCTION calc_fnft_psu15( v0, rho, blades, bladelen, diameter,           &
                          radius, pitchdeg, rpm, dscale, cscale)         &
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
     real               :: cd_nac
     integer            :: iter, itmax
!
     rps   = rpm/60.0
     omega = 2.0*pi*rps
!
     radmax = 0.5*diameter
     radmin = radmax - bladelen
!

  fnft(1) = 0.0
  fnft(2) = 0.0

  IF (radius .LT. radmax) THEN !JDM
     
     IF (radius .GE. radmin) THEN !JDM

        an = 0.0
        at = 0.0
!
        iter = 0
        epsi = 1.0        
        epscrit = 1.e-5
        itmax   = 20
!
!    --Incidence angle

        xideg = calc_twist_psu15( radius/dscale ) + pitchdeg
        xi    = xideg*pi/180.0
!
!    --Cord length 

        cord  = cscale * calc_chord_psu15( radius/dscale )
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

! print*,'iter',iter
! print*,'v0,  omega, radius',v0,omega,radius
! print*,'vn, at, vt',vn,at,vt
! print*,'psi',psi
! print*,'------'

!       --Incidence angle  
 
           beta    = psi - xi
           betadeg = beta*180.0/pi

!       --Lift and drag coefficients  
     
           cl = calc_cl_psu15( radius/dscale, betadeg )
           cd = calc_cd_psu15( radius/dscale, betadeg )
  
!       --Normal and tangential force coefficients     
  
           cn = cl*cos( psi ) + cd*sin( psi )
           ct = cl*sin( psi ) - cd*cos( psi )

!       --Solidity factor

           solid = cord*blades / ( 2.0*pi*radius )

!       --Tip and hub loss

!          When psi < 0, fac1 and fac 2 can give bizarre answers, and resulting in tip and hub loss terms - NaN.
!          Some logic should be added to keep things from going crazy
           
!          RSA update: the argument of acos should have an abs. value <= 1. When psi
!          is negative, fac1 and/or fac2 are negative, causing exp(-fac) to be a
!          large positive number, causing acos to return NaN. When psi is positive, fac1 and fac2 are
!          positive, such that exp(-fac) is small and positive, which is ok.
!          Thus, we have added logic around the calls to calc_fnft above such
!          that when v0 < 0 (ie psi < 0), this function is not called and the
!          force is set to 0. The turbine should still yaw into place eventually.

           if ( abs( psi ) .gt. 0.001 ) then

              fac1 = blades / 2.0*( radmax - radius ) / ( radius*sin( psi ) )
              fac2 = blades / 2.0*( radius - radmin ) / ( radmin*sin( psi ) )

              tiploss = (2.0/pi)*acos( exp( -fac1 ) )
              hubloss = (2.0/pi)*acos( exp( -fac2 ) )

              lossfac = tiploss*hubloss

!             --Induction factors

              anold = an
              atold = at

!JDM             an = ( 1. + 4.0*lossfac*sin( psi )*sin( psi )/ ( solid*cn ) )**(-1.0)

              at = ( 4.0*lossfac*sin( psi )*cos( psi )/ ( solid*ct ) - 1.0 )**(-1.0)

! print*,'inside of iteration'
! print*,'iter',iter
! print*,'omega, radius, radmin, radmax',omega,radius,radmin,radmax
! print*,'v0, vn, vt, an, at',v0,vn,vt,an,at
! print*,'psi, fac1, fac2',psi,fac1,fac2
! print*,'tiploss, hubloss, lossfac',tiploss,hubloss,lossfac
! print*,'------'

          else

             anold = an
             atold = at

             an = 0.0
             at = 0.0

print*,'what am I doing in here???'

          endif

!       --Convergence criteria

           epsi = sqrt( ( anold - an )**2.0 + ( atold - at )**2.0 )
  
        end do

!print*,'radius,radmax,radmin',radius,radmax,radmin
!print*,'radius - radmin',radius - radmin
!print*,'fac1,fac2',fac1,fac2
!print*,'psi,sin(psi)',psi,sin(psi)
!print*,'tiploss,hubloss',tiploss,hubloss
!print*,'cl,cd',cl,cd
!print*,'cn,ct',cn,ct

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

! print*,'outside of iteration'
! print*,'v0, vn, at, vt',v0,vn,at,vt
! print*,'psideg, betadeg',psideg,betadeg
! print*,'------'

!    --Lift and drag coefficients     
  
       cl = calc_cl_psu15( radius/dscale, betadeg )
       cd = calc_cd_psu15( radius/dscale, betadeg )

!    --Lift and drag forces

       lift = 0.5*rho*vr*vr*cord*cl 
       drag = 0.5*rho*vr*vr*cord*cd 

!    --Specific normal and tangential forces

       fn = lift*cos( psi ) + drag*sin( psi )
       ft = lift*sin( psi ) - drag*cos( psi )

!       print*,'psi',psideg

       fn = blades*fn/(2.0*pi*radius)
       ft = blades*ft/(2.0*pi*radius)

       if( fn .lt. 0.0 ) fn = 0.0
       if( ft .lt. 0.0 ) ft = 0.0

       fnft(1) = fn
       fnft(2) = ft              

!print*,'radius,radmax,radmin',radius,radmax,radmin
!print*,'radius - radmin',radius - radmin
!print*,'fac1,fac2',fac1,fac2
!print*,'psi,sin(psi)',psi,sin(psi)
!print*,'tiploss,hubloss',tiploss,hubloss
!print*,'cl,cd',cl,cd
!print*,'cn,ct',cn,ct
!print*,'fn,ft',fn,ft


     ENDIF !radius .GE. radmin

  ENDIF   !radius .LT. radmax

END FUNCTION calc_fnft_psu15


!=========================================================================

FUNCTION calc_twist_psu15( x ) RESULT( twist )
!_________________________________________________________________________
!
! Computes blade twist as a function of radial distance from hub center
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x

  REAL, PARAMETER :: ns = 5

  REAL, DIMENSION( 0:ns-1 ) :: rf
  REAL, DIMENSION( 0:ns-1 ) :: r
  REAL, DIMENSION( 0:ns-1 ) :: t

  REAL :: twist 
  REAL :: rad 
  INTEGER :: i

  rad = 38.5
  twist = 0.0
  
  rf(0) = 0.0444
  rf(1) = 0.15
  rf(2) = 0.8
  rf(3) = 0.95
  rf(4) = 1.0
  
  DO i = 1,ns-1
     r(i) = rad*rf(i)
  ENDDO
  
  t(0) = 10.0
  t(1) = 10.0
  t(2) = 2.75
  t(3) = -1.5
  t(4) = 1.0
  
  DO i = 1,ns-1
 
     IF ( ( x .GT. r(i-1) ) .AND. ( x .LE. r(i) ) ) THEN
        twist = t(i-1) + (x - r(i-1))*( ( t(i) - t(i-1) )/( r(i) - r(i-1) ) )
        goto 1
     ENDIF

1 CONTINUE

     IF ( x .LE. r(0) ) THEN
        PRINT*,'—————————————'
        PRINT*,'radius < blade root in calc_twist_psu15.pro'
        PRINT*,'—————————————'
        STOP
     ENDIF
     
     IF ( x .GT. r(ns-1) ) THEN
        PRINT*,'—————————————'
        PRINT*,'radius >  blade length in calc_twist_psu15.pro'
        PRINT*,'—————————————'
        STOP
     ENDIF 
     
  ENDDO
  
!PRINT*,x,twist
  
!_________________________________________________________________________
  
END FUNCTION calc_twist_psu15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_chord_psu15( x ) RESULT( chord )
!_________________________________________________________________________
!
! Data from GE-PSU/GE.inp
! rf = RTAPER = radial sections, cf = CTAPER = chord length per section
!_________________________________________________________________________
 

  REAL, INTENT( IN ) :: x

  REAL, PARAMETER :: ns = 10

  REAL, DIMENSION( 0:ns-1 ) :: rf
  REAL, DIMENSION( 0:ns-1 ) :: r
  REAL, DIMENSION( 0:ns-1 ) :: cf
  REAL, DIMENSION( 0:ns-1 ) :: c

  REAL :: chord 
  REAL :: rad 
  INTEGER :: i

  chord = 0.0
  
  rad = 38.5 
  
  rf(0) = 0.0444
  rf(1) = 0.06
  rf(2) = 0.1
  rf(3) = 0.15
  rf(4) = 0.2
  rf(5) = 0.25
  rf(6) = 0.94
  rf(7) = 0.97
  rf(8) = 0.99
  rf(9) = 1.0
  
  cf(0) = 0.05333
  cf(1) = 0.05333
  cf(2) = 0.05889
  cf(3) = 0.06667
  cf(4) = 0.07111
  cf(5) = 0.07333
  cf(6) = 0.02889
  cf(7) = 0.02556
  cf(8) = 0.02000
  cf(9) = 0.01111
  
  DO i = 1,ns-1
     r(i) = rad*rf(i)
     c(i) = rad*cf(i)
  ENDDO

  DO i = 1,ns-1
     IF ( ( x .GE. r(i-1) ) .AND. ( x .LT. r(i) ) ) THEN
        chord = c(i-1) + (x - r(i-1))*( ( c(i) - c(i-1) )/( r(i) - r(i-1) ) )
        GOTO 1
     ENDIF
  ENDDO

  IF ( x .LT. r(0) ) chord = 0.0 !No airfoil here, just a cylinder
  IF ( x .EQ. r(ns-1) ) chord = cf(ns-1)
  
  IF ( x .GT. r(ns-1) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius > blade length in calc_chord_psu15.pro'
     PRINT*,'—————————————'
     STOP
  ENDIF
  
1 CONTINUE
  
!PRINT*,x,chord
   
!_________________________________________________________________________
  
END FUNCTION calc_chord_psu15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cl_psu15( x,y ) RESULT( cl )
!_________________________________________________________________________
!
! Calls appropriate airfoil section table depending upon radial distance
! from blade root. Data from GE-PSU/GE.inp
!
! rf = RAIRF = Section boundaries for different blade airfoil lookup tables
! ns = RAIRF + 1, endpoint values was added for error checking
!
! x = radial distance from hub center
! y = angle of attack
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x,y

  REAL, PARAMETER :: ns = 11

  REAL, DIMENSION( 0:ns-1 ) :: r,rf

  REAL :: cl
  REAL :: rad 
  INTEGER :: i

  cl = 0.0
  
  rad = 38.5
  
  rf( 0) = 0.0444
  rf( 1) = 0.09
  rf( 2) = 0.11
  rf( 3) = 0.13
  rf( 4) = 0.15
  rf( 5) = 0.18
  rf( 6) = 0.4
  rf( 7) = 0.5
  rf( 8) = 0.6
  rf( 9) = 0.825
  rf(10) = 1.0 
  
  DO i = 0,ns-1
     r(i) = rad*rf(i)
  ENDDO
  
  IF ( ( x .GT. r( 0) ) .AND. ( x .LE. r( 1) ) ) THEN
       cl = calc_cl1_psu15(y)
     ELSEIF ( ( x .GT. r( 1) ) .AND. ( x .LE. r( 2) ) ) THEN
       cl = calc_cl2_psu15(y)
     ELSEIF ( ( x .GT. r( 2) ) .AND. ( x .LE. r( 3) ) ) THEN
       cl = calc_cl3_psu15(y)
     ELSEIF ( ( x .GT. r( 3) ) .AND. ( x .LE. r( 4) ) ) THEN
       cl = calc_cl4_psu15(y)
     ELSEIF ( ( x .GT. r( 4) ) .AND. ( x .LE. r( 5) ) ) THEN
       cl = calc_cl5_psu15(y)
     ELSEIF ( ( x .GT. r( 5) ) .AND. ( x .LE. r( 6) ) ) THEN
       cl = calc_cl6_psu15(y)
     ELSEIF ( ( x .GT. r( 6) ) .AND. ( x .LE. r( 7) ) ) THEN
       cl = calc_cl7_psu15(y)
     ELSEIF ( ( x .GT. r( 7) ) .AND. ( x .LE. r( 8) ) ) THEN
       cl = calc_cl8_psu15(y)
     ELSEIF ( ( x .GT. r( 8) ) .AND. ( x .LE. r( 9) ) ) THEN
       cl = calc_cl9_psu15(y)
     ELSEIF ( ( x .GT. r( 9) ) .AND. ( x .LE. r(10) ) ) THEN
       cl = calc_cl10_psu15(y)
  ENDIF
  
  IF ( x .LT. r(0) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius less than blade root in calc_cl_psu15.pro'
     PRINT*,'—————————————'
     STOP
  ENDIF
  IF ( x .GT. r(ns-1) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius greater than blade length in calc_cl_psu15.pro'
     PRINT*,'—————————————'
     STOP
  ENDIF
  
!_________________________________________________________________________
  
END FUNCTION calc_cl_psu15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd_psu15( x,y ) RESULT( cd )
!_________________________________________________________________________
!
! Calls appropriate airfoil section table depending upon radial distance
! from blade root. Data from GE-PSU/GE.inp
!
! rf = RAIRF = Section boundaries for different blade airfoil lookup tables
! ns = RAIRF + 1, endpoint values was added for error checking
!
! x = radial distance from hub center
! y = angle of attack
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x,y

  REAL, PARAMETER :: ns = 11

  REAL, DIMENSION( 0:ns-1 ) :: r,rf

  REAL :: cd
  REAL :: rad 
  INTEGER :: i

  cd = 0.0
  
  rad = 38.5
  
  rf( 0) = 0.0444
  rf( 1) = 0.09
  rf( 2) = 0.11
  rf( 3) = 0.13
  rf( 4) = 0.15
  rf( 5) = 0.18
  rf( 6) = 0.4
  rf( 7) = 0.5
  rf( 8) = 0.6
  rf( 9) = 0.825
  rf(10) = 1.0 
  
  DO i = 0,ns-1
     r(i) = rad*rf(i)
  ENDDO

  IF ( ( x .GT. r( 0) ) .AND. (x .LE. r( 1) ) ) THEN
       cd = calc_cd1_psu15(y)
     ELSEIF ( ( x .GT. r( 1) ) .AND. (x .LE. r( 2) ) ) THEN
       cd = calc_cd2_psu15(y)
     ELSEIF ( ( x .GT. r( 2) ) .AND. (x .LE. r( 3) ) ) THEN
       cd = calc_cd3_psu15(y)
     ELSEIF ( ( x .GT. r( 3) ) .AND. (x .LE. r( 4) ) ) THEN
       cd = calc_cd4_psu15(y)
     ELSEIF ( ( x .GT. r( 4) ) .AND. (x .LE. r( 5) ) ) THEN
       cd = calc_cd5_psu15(y)
     ELSEIF ( ( x .GT. r( 5) ) .AND. (x .LE. r( 6) ) ) THEN
       cd = calc_cd6_psu15(y)
     ELSEIF ( ( x .GT. r( 6) ) .AND. (x .LE. r( 7) ) ) THEN
       cd = calc_cd7_psu15(y)
     ELSEIF ( ( x .GT. r( 7) ) .AND. (x .LE. r( 8) ) ) THEN
       cd = calc_cd8_psu15(y)
     ELSEIF ( ( x .GT. r( 8) ) .AND. (x .LE. r( 9) ) ) THEN
       cd = calc_cd9_psu15(y)
     ELSEIF ( ( x .GT. r( 9) ) .AND. (x .LE. r(10) ) ) THEN
       cd = calc_cd10_psu15(y)
  ENDIF
  
  IF ( x .LT. r(0) ) THEN
     PRINT*,'—————————————'
     PRINT*,'radius less than blade root in calc_cd_psu15.pro'
     PRINT*,'—————————————'
     STOP
  ENDIF
  
  IF ( x .GT. r(ns-1) ) THEN
     PRINT*,'—————————————'
     PRINT*,'radius greater than blade length in calc_cd_psu15.pro'
     PRINT*,'—————————————'
     STOP
  ENDIF
!_________________________________________________________________________
  
END FUNCTION calc_cd_psu15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl1_psu15( y ) RESULT( cl1 )
!_________________________________________________________________________
!
! Data from Cylinder.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL :: cl1

  cl1 = 0.0
!_________________________________________________________________________
  
END FUNCTION calc_cl1_psu15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl2_psu15( y ) RESULT( cl2 )
!_________________________________________________________________________
!
! Data from GE-PSU/Cylinder1.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 41

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cl

  REAL :: cl2
  INTEGER :: i

  cl2 = 0.0

!                alpha     CL    
  cl(0:1, 0) = [-7.000, -0.0049]   
  cl(0:1, 1) = [-6.000, -0.0044]  
  cl(0:1, 2) = [-5.000, -0.0040]  
  cl(0:1, 3) = [-4.000, -0.0031]  
  cl(0:1, 4) = [-3.000, -0.0019]  
  cl(0:1, 5) = [-2.000, -0.0006]  
  cl(0:1, 6) = [-1.000,  0.0008]   
  cl(0:1, 7) = [ 0.000,  0.0021]   
  cl(0:1, 8) = [ 1.000,  0.0034]   
  cl(0:1, 9) = [ 2.000,  0.0046]   
  cl(0:1,10) = [ 3.000,  0.0057]   
  cl(0:1,11) = [ 4.000,  0.0068]   
  cl(0:1,12) = [ 5.000,  0.0077]   
  cl(0:1,13) = [ 6.000,  0.0085]   
  cl(0:1,14) = [ 7.000,  0.0094]   
  cl(0:1,15) = [ 8.000,  0.0101]   
  cl(0:1,16) = [ 9.000,  0.0106]   
  cl(0:1,17) = [10.000,  0.0111]  
  cl(0:1,18) = [11.000,  0.0113]  
  cl(0:1,19) = [12.000,  0.0116]  
  cl(0:1,20) = [13.000,  0.0119]  
  cl(0:1,21) = [14.000,  0.0119]  
  cl(0:1,22) = [14.500,  0.0120] 
  cl(0:1,23) = [15.000,  0.0121]  
  cl(0:1,24) = [15.500,  0.0121]  
  cl(0:1,25) = [16.000,  0.0121]  
  cl(0:1,26) = [16.500,  0.0120]  
  cl(0:1,27) = [17.000,  0.0119]  
  cl(0:1,28) = [18.000,  0.0119]  
  cl(0:1,29) = [19.000,  0.0115]  
  cl(0:1,30) = [20.000,  0.0110]  
  cl(0:1,31) = [21.000,  0.0106]  
  cl(0:1,32) = [22.000,  0.0103]  
  cl(0:1,33) = [23.000,  0.0102]  
  cl(0:1,34) = [24.000,  0.0100]  
  cl(0:1,35) = [25.000,  0.0099]  
  cl(0:1,36) = [26.000,  0.0099]  
  cl(0:1,37) = [27.000,  0.0098]  
  cl(0:1,38) = [28.000,  0.0096]  
  cl(0:1,39) = [29.000,  0.0097]  
  cl(0:1,40) = [30.000,  0.0096]   
  
  IF ( y .EQ. cl(0,0) ) cl2 = cl(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl2 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN 
     cl2 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl2_psu15.pro'
  ENDIF
  IF ( y .GT. cl(0,ns-1) ) THEN
     cl2 = cl(1,ns-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl2_psu15.pro'
  ENDIF
  
!print,y,cl2
  
!_________________________________________________________________________
  
END FUNCTION calc_cl2_psu15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cl3_psu15( y ) RESULT( cl3 )
!_________________________________________________________________________
!
! Data from GE-PSU/Cylinder7.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 41

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cl

  REAL :: cl3
  INTEGER :: i

  cl3 = 0.0
  
!                alpha     CL    
  cl(0:1, 0) = [-7.000, -0.2437]
  cl(0:1, 1) = [-6.000, -0.2201]
  cl(0:1, 2) = [-5.000, -0.2010]
  cl(0:1, 3) = [-4.000, -0.1545]
  cl(0:1, 4) = [-3.000, -0.0941]
  cl(0:1, 5) = [-2.000, -0.0304]
  cl(0:1, 6) = [-1.000,  0.0390]
  cl(0:1, 7) = [ 0.000,  0.1043]
  cl(0:1, 8) = [ 1.000,  0.1692]
  cl(0:1, 9) = [ 2.000,  0.2303]
  cl(0:1,10) = [ 3.000,  0.2880]
  cl(0:1,11) = [ 4.000,  0.3436]
  cl(0:1,12) = [ 5.000,  0.3864]
  cl(0:1,13) = [ 6.000,  0.4291]
  cl(0:1,14) = [ 7.000,  0.4717]
  cl(0:1,15) = [ 8.000,  0.5081]
  cl(0:1,16) = [ 9.000,  0.5330]
  cl(0:1,17) = [10.000,  0.5556]
  cl(0:1,18) = [11.000,  0.5679]
  cl(0:1,19) = [12.000,  0.5808]
  cl(0:1,20) = [13.000,  0.5955]
  cl(0:1,21) = [14.000,  0.5972]
  cl(0:1,22) = [14.500,  0.6035]
  cl(0:1,23) = [15.000,  0.6097]
  cl(0:1,24) = [15.500,  0.6082]
  cl(0:1,25) = [16.000,  0.6065]
  cl(0:1,26) = [16.500,  0.6018]
  cl(0:1,27) = [17.000,  0.5971]
  cl(0:1,28) = [18.000,  0.5965]
  cl(0:1,29) = [19.000,  0.5779]
  cl(0:1,30) = [20.000,  0.5535]
  cl(0:1,31) = [21.000,  0.5341]
  cl(0:1,32) = [22.000,  0.5188]
  cl(0:1,33) = [23.000,  0.5116]
  cl(0:1,34) = [24.000,  0.5017]
  cl(0:1,35) = [25.000,  0.4982]
  cl(0:1,36) = [26.000,  0.4973]
  cl(0:1,37) = [27.000,  0.4898]
  cl(0:1,38) = [28.000,  0.4822]
  cl(0:1,39) = [29.000,  0.4883]
  cl(0:1,40) = [30.000,  0.4835]
  
  IF ( y .EQ. cl(0,0) ) cl3 = cl(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. ( y .LE. cl(0,i) ) ) THEN
        cl3 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO

1 CONTINUE
 
  IF ( y .LT. cl(0,0) ) THEN
     cl3 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cdl_psu15.pro'
  ENDIF
  IF ( y .GT. cl(0,ns-1) ) THEN
     cl3 = cl(1,ns-1)
!     PRINT*,'alpha > tabulated range in calc_cl3_psu15.pro'
  ENDIF
  
!PRINT*,y,cl3
  
!_________________________________________________________________________
  
END FUNCTION calc_cl3_psu15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cl4_psu15( y ) RESULT( cl4 )
!_________________________________________________________________________
!
! Data from GE-PSU/Cylinder9.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 41

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cl

  REAL :: cl4
  INTEGER :: i

  cl4 = 0.0
  
!                alpha     CL    
  cl(0:1, 0) = [-7.000, -0.3967]
  cl(0:1, 1) = [-6.000, -0.3581]
  cl(0:1, 2) = [-5.000, -0.3270]
  cl(0:1, 3) = [-4.000, -0.2514]
  cl(0:1, 4) = [-3.000, -0.1531] 
  cl(0:1, 5) = [-2.000, -0.0495]  
  cl(0:1, 6) = [-1.000,  0.0635]
  cl(0:1, 7) = [ 0.000,  0.1697]
  cl(0:1, 8) = [ 1.000,  0.2753]
  cl(0:1, 9) = [ 2.000,  0.3748]
  cl(0:1,10) = [ 3.000,  0.4686]
  cl(0:1,11) = [ 4.000,  0.5592]
  cl(0:1,12) = [ 5.000,  0.6288]
  cl(0:1,13) = [ 6.000,  0.6983]
  cl(0:1,14) = [ 7.000,  0.7676]
  cl(0:1,15) = [ 8.000,  0.8268]
  cl(0:1,16) = [ 9.000,  0.8674]
  cl(0:1,17) = [10.000,  0.9042]
  cl(0:1,18) = [11.000,  0.9241]
  cl(0:1,19) = [12.000,  0.9452]
  cl(0:1,20) = [13.000,  0.9690]
  cl(0:1,21) = [14.000,  0.9719]
  cl(0:1,22) = [14.500,  0.9821]
  cl(0:1,23) = [15.000,  0.9923]
  cl(0:1,24) = [15.500,  0.9897]
  cl(0:1,25) = [16.000,  0.9870]
  cl(0:1,26) = [16.500,  0.9794]
  cl(0:1,27) = [17.000,  0.9717]
  cl(0:1,28) = [18.000,  0.9707]
  cl(0:1,29) = [19.000,  0.9404]
  cl(0:1,30) = [20.000,  0.9006]
  cl(0:1,31) = [21.000,  0.8691]
  cl(0:1,32) = [22.000,  0.8443]
  cl(0:1,33) = [23.000,  0.8326]
  cl(0:1,34) = [24.000,  0.8165]
  cl(0:1,35) = [25.000,  0.8107]
  cl(0:1,36) = [26.000,  0.8093]
  cl(0:1,37) = [27.000,  0.7970]
  cl(0:1,38) = [28.000,  0.7846]
  cl(0:1,39) = [29.000,  0.7946]
  cl(0:1,40) = [30.000,  0.7868]
  
  IF ( y .EQ. cl(0,0) ) cl4 = cl(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl4 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO

1 CONTINUE

   IF ( y .LT. cl(0,0) ) THEN 
     cl4 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl4_psu15.pro'
  ENDIF
  IF ( y .GT. cl(0,ns-1) ) THEN 
     cl4 = cl(1,ns-1) 
!     PRINT*,'alpha > tabulated range in calc_cl4_psu15.pro'
  ENDIF
  
!PRINT*,y,cl4
  
 
!————————————————————————————————————
END FUNCTION calc_cl4_psu15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl5_psu15( y ) RESULT( cl5 )
!_________________________________________________________________________
!
! Data from GE-PSU/00W240103.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 41

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cl

  REAL :: cl5
  INTEGER :: i

  cl5 = 0.0
  
!                alpha     CL    
  cl(0:1, 0) = [-7.000, -0.5395]
  cl(0:1, 1) = [-6.000, -0.4871]
  cl(0:1, 2) = [-5.000, -0.4448]
  cl(0:1, 3) = [-4.000, -0.3419]
  cl(0:1, 4) = [-3.000, -0.2083]
  cl(0:1, 5) = [-2.000, -0.0673]
  cl(0:1, 6) = [-1.000,  0.0864]
  cl(0:1, 7) = [ 0.000,  0.2308]
  cl(0:1, 8) = [ 1.000,  0.3744]
  cl(0:1, 9) = [ 2.000,  0.5098]
  cl(0:1,10) = [ 3.000,  0.6374]
  cl(0:1,11) = [ 4.000,  0.7606]
  cl(0:1,12) = [ 5.000,  0.8552]
  cl(0:1,13) = [ 6.000,  0.9498]
  cl(0:1,14) = [ 7.000,  1.0440]
  cl(0:1,15) = [ 8.000,  1.1246]
  cl(0:1,16) = [ 9.000,  1.1798]
  cl(0:1,17) = [10.000,  1.2298]
  cl(0:1,18) = [11.000,  1.2569]
  cl(0:1,19) = [12.000,  1.2856]
  cl(0:1,20) = [13.000,  1.3180]
  cl(0:1,21) = [14.000,  1.3219]
  cl(0:1,22) = [14.500,  1.3358]
  cl(0:1,23) = [15.000,  1.3496]
  cl(0:1,24) = [15.500,  1.3461]
  cl(0:1,25) = [16.000,  1.3425]
  cl(0:1,26) = [16.500,  1.3321]
  cl(0:1,27) = [17.000,  1.3217]
  cl(0:1,28) = [18.000,  1.3203]
  cl(0:1,29) = [19.000,  1.2791]
  cl(0:1,30) = [20.000,  1.2250]
  cl(0:1,31) = [21.000,  1.1821]
  cl(0:1,32) = [22.000,  1.1484]
  cl(0:1,33) = [23.000,  1.1324]
  cl(0:1,34) = [24.000,  1.1105]
  cl(0:1,35) = [25.000,  1.1027]
  cl(0:1,36) = [26.000,  1.1007]
  cl(0:1,37) = [27.000,  1.0840]
  cl(0:1,38) = [28.000,  1.0672]
  cl(0:1,39) = [29.000,  1.0807]
  cl(0:1,40) = [30.000,  1.0702]
  
  IF ( y .EQ. cl(0,0) ) cl5 = cl(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. ( y .LE. cl(0,i) ) ) THEN
        cl5 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 

1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN
     cl5 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl5_psu15.pro'
  ENDIF
  IF ( y .GT. cl(0,ns-1) ) THEN
     cl5 = cl(1,ns-1) 
!     PRINT*,'alpha > tabulated range in calc_cl5_psu15.pro'
  ENDIF
  
!PRINT*,y,cl5
  
!_________________________________________________________________________
  
END FUNCTION calc_cl5_psu15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cl6_psu15( y ) RESULT( cl6 )
!_________________________________________________________________________
!
! Data from GE-PSU/00W235003.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 50

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cl

  REAL :: cl6
  INTEGER :: i

  cl6 = 0.0
  
!                alpha     CL    
  cl(0:1, 0) = [-10.000, -0.9792]
  cl(0:1, 1) = [-9.000,  -0.9152]
  cl(0:1, 2) = [-8.000,  -0.8411]
  cl(0:1, 3) = [-7.000,  -0.7238]
  cl(0:1, 4) = [-6.000,  -0.5905]
  cl(0:1, 5) = [-5.000,  -0.4492]
  cl(0:1, 6) = [-4.000,  -0.2993]
  cl(0:1, 7) = [-3.000,  -0.1540]
  cl(0:1, 8) = [-2.000,  -0.0014]
  cl(0:1, 9) = [-1.000,   0.1442]
  cl(0:1,10) = [ 0.000,   0.2836]
  cl(0:1,11) = [ 1.000,   0.4263]
  cl(0:1,12) = [ 2.000,   0.5630]
  cl(0:1,13) = [ 3.000,   0.6971]
  cl(0:1,14) = [ 4.000,   0.8308]
  cl(0:1,15) = [ 5.000,   0.9584]
  cl(0:1,16) = [ 6.000,   1.0839]
  cl(0:1,17) = [ 7.000,   1.2033]
  cl(0:1,18) = [ 8.000,   1.3140]
  cl(0:1,19) = [ 9.000,   1.4013]
  cl(0:1,20) = [10.000,   1.4874]
  cl(0:1,21) = [11.000,   1.5668]
  cl(0:1,22) = [12.000,   1.6366]
  cl(0:1,23) = [13.000,   1.6915]
  cl(0:1,24) = [14.000,   1.7345]
  cl(0:1,25) = [15.000,   1.7584]
  cl(0:1,26) = [15.500,   1.7641]
  cl(0:1,27) = [16.000,   1.7697]
  cl(0:1,28) = [17.000,   1.7647]
  cl(0:1,29) = [18.000,   1.7552]
  cl(0:1,30) = [19.000,   1.7424]
  cl(0:1,31) = [20.000,   1.7254]
  cl(0:1,32) = [21.000,   1.7048]
  cl(0:1,33) = [22.000,   1.6858]
  cl(0:1,34) = [22.500,   1.6880]
  cl(0:1,35) = [23.000,   1.6901]
  cl(0:1,36) = [23.500,   1.6859]
  cl(0:1,37) = [24.000,   1.6817]
  cl(0:1,38) = [24.500,   1.6804]
  cl(0:1,39) = [25.000,   1.6790]
  cl(0:1,40) = [25.500,   1.6854]
  cl(0:1,41) = [26.000,   1.6918]
  cl(0:1,42) = [26.500,   1.6932]
  cl(0:1,43) = [27.000,   1.6946]
  cl(0:1,44) = [27.500,   1.6907]
  cl(0:1,45) = [28.000,   1.6868]
  cl(0:1,46) = [28.500,   1.6943]
  cl(0:1,47) = [29.000,   1.7018]
  cl(0:1,48) = [29.500,   1.7037]
  cl(0:1,49) = [30.000,   1.7056]
  
  IF ( y .EQ. cl(0,0) ) cl6 = cl(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl6 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO

1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN
     cl6 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl6_psu15.pro'
  ENDIF
  IF ( y .GT. cl(0,ns-1) ) THEN
     cl6 = cl(1,ns-1) 
!     PRINT*,'alpha > tabulated range in calc_cl6_psu15.pro'
  ENDIF
  
!PRINT*,y,cl6
  
!_________________________________________________________________________
  
END FUNCTION calc_cl6_psu15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cl7_psu15( y ) RESULT( cl7 )
!_________________________________________________________________________
!
! Data from GE-PSU/97W30003.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 44

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cl

  REAL :: cl7
  INTEGER :: i

  cl7 = 0.0
  
!                alpha     CL    
  cl(0:1, 0) = [-10.000, -1.0412]
  cl(0:1, 1) = [ -9.000, -0.8980]
  cl(0:1, 2) = [ -8.000, -0.7553]
  cl(0:1, 3) = [ -7.000, -0.6131]
  cl(0:1, 4) = [ -6.000, -0.4726]
  cl(0:1, 5) = [ -5.000, -0.3349]
  cl(0:1, 6) = [ -4.000, -0.1986]
  cl(0:1, 7) = [ -3.000, -0.0596]
  cl(0:1, 8) = [ -2.000,  0.0762]
  cl(0:1, 9) = [ -1.000,  0.2106]
  cl(0:1,10) = [  0.000,  0.3466]
  cl(0:1,11) = [  1.000,  0.4799]
  cl(0:1,12) = [  2.000,  0.6130]
  cl(0:1,13) = [  3.000,  0.7449]
  cl(0:1,14) = [  4.000,  0.8755]
  cl(0:1,15) = [  5.000,  1.0050]
  cl(0:1,16) = [  6.000,  1.1326]
  cl(0:1,17) = [  7.000,  1.2584]
  cl(0:1,18) = [  8.000,  1.3822]
  cl(0:1,19) = [  9.000,  1.5001]
  cl(0:1,20) = [ 10.000,  1.6098]
  cl(0:1,21) = [ 11.000,  1.6929]
  cl(0:1,22) = [ 12.000,  1.7342]
  cl(0:1,23) = [ 13.000,  1.7622]
  cl(0:1,24) = [ 14.000,  1.7843]
  cl(0:1,25) = [ 14.500,  1.7874]
  cl(0:1,26) = [ 15.000,  1.7905]
  cl(0:1,27) = [ 16.000,  1.7788]
  cl(0:1,28) = [ 17.000,  1.7504]
  cl(0:1,29) = [ 18.000,  1.7108]
  cl(0:1,30) = [ 19.000,  1.6728]
  cl(0:1,31) = [ 20.000,  1.6449]
  cl(0:1,32) = [ 21.000,  1.6316]
  cl(0:1,33) = [ 21.500,  1.6312]
  cl(0:1,34) = [ 22.000,  1.6308]
  cl(0:1,35) = [ 22.500,  1.6324]
  cl(0:1,36) = [ 23.000,  1.6339]
  cl(0:1,37) = [ 23.500,  1.6380]
  cl(0:1,38) = [ 24.000,  1.6420]
  cl(0:1,39) = [ 25.000,  1.6484]
  cl(0:1,40) = [ 26.000,  1.6630]
  cl(0:1,41) = [ 27.000,  1.6682]
  cl(0:1,42) = [ 28.000,  1.6729]
  cl(0:1,43) = [ 30.000,  1.6873]
  
  IF ( y .EQ. cl(0,0) ) cl7 = cl(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl7 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO

1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN
     cl7 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl7_psu15.pro'
  ENDIF
  IF ( y .GT. cl(0,ns-1) ) THEN
     cl7 = cl(1,ns-1) 
!     PRINT*,'alpha > tabulated range in calc_cl7_psu15.pro'
  ENDIF

!PRINT*,y,cl7
  
!_________________________________________________________________________
  
END FUNCTION calc_cl7_psu15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cl8_psu15( y ) RESULT( cl8 )
!_________________________________________________________________________
!
! Data from GE-PSU/91W225003.polar
!_________________________________________________________________________
 
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 45

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cl

  REAL :: cl8
  INTEGER :: i
 
  cl8 = 0.0
  
!                alpha     CL    
  cl(0:1, 0) = [ -10.000, -0.9152]
  cl(0:1, 1) = [  -9.000, -0.7714]
  cl(0:1, 2) = [  -8.000, -0.6229]
  cl(0:1, 3) = [  -7.000, -0.4764]
  cl(0:1, 4) = [  -6.000, -0.3400]
  cl(0:1, 5) = [  -5.000, -0.2055]
  cl(0:1, 6) = [  -4.000, -0.0730]
  cl(0:1, 7) = [  -3.000,  0.0587]
  cl(0:1, 8) = [  -2.000,  0.1895]
  cl(0:1, 9) = [  -1.000,  0.3198]
  cl(0:1,10) = [   0.000,  0.4485]
  cl(0:1,11) = [   1.000,  0.5772]
  cl(0:1,12) = [   2.000,  0.7031]
  cl(0:1,13) = [   3.000,  0.8291]
  cl(0:1,14) = [   4.000,  0.9538]
  cl(0:1,15) = [   5.000,  1.0769]
  cl(0:1,16) = [   6.000,  1.1983]
  cl(0:1,17) = [   7.000,  1.3121]
  cl(0:1,18) = [   8.000,  1.4004]
  cl(0:1,19) = [   9.000,  1.4749]
  cl(0:1,20) = [  10.000,  1.5198]
  cl(0:1,21) = [  11.000,  1.5637]
  cl(0:1,22) = [  12.000,  1.6047]
  cl(0:1,23) = [  13.000,  1.6393]
  cl(0:1,24) = [  14.000,  1.6582]
  cl(0:1,25) = [  15.000,  1.6655]
  cl(0:1,26) = [  16.000,  1.6656]
  cl(0:1,27) = [  16.500,  1.6689]
  cl(0:1,28) = [  17.000,  1.6721]
  cl(0:1,29) = [  18.000,  1.6688]
  cl(0:1,20) = [  18.500,  1.6744]
  cl(0:1,31) = [  19.000,  1.6799]
  cl(0:1,32) = [  19.500,  1.6819]
  cl(0:1,33) = [  20.000,  1.6839]
  cl(0:1,34) = [  21.000,  1.6859]
  cl(0:1,35) = [  22.000,  1.6924]
  cl(0:1,36) = [  23.000,  1.7013]
  cl(0:1,37) = [  24.000,  1.7064]
  cl(0:1,38) = [  25.000,  1.7076]
  cl(0:1,39) = [  26.000,  1.7100]
  cl(0:1,40) = [  27.000,  1.7072]
  cl(0:1,41) = [  27.500,  1.7037]
  cl(0:1,42) = [  28.000,  1.7001]
  cl(0:1,43) = [  29.000,  1.6839]
  cl(0:1,44) = [  30.000,  1.6649]
  
  IF ( y .EQ. cl(0,0) ) cl8 = cl(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl8 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO

1 CONTINUE
 
  IF ( y .LT. cl(0,0) ) THEN 
     cl8 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl8_psu15.pro'
  ENDIF
  IF ( y .GT. cl(0,ns-1) ) THEN 
     cl8 = cl(1,ns-1) 
!     PRINT*,'alpha > tabulated range in calc_cl8_psu15.pro'
  ENDIF
  
!PRINT*,y,cl8
   
!_________________________________________________________________________
  
END FUNCTION calc_cl8_psu15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cl9_psu15( y ) RESULT( cl9 )
!_________________________________________________________________________
!
! Data from GE-PSU/93W21003.polar
!_________________________________________________________________________

  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 40

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cl

  REAL :: cl9
  INTEGER :: i
  
  cl9 = 0.0
  
!                alpha     CL    
  cl(0:1, 0) = [-10.000, -0.7484]
  cl(0:1, 1) = [ -9.000, -0.6190]
  cl(0:1, 2) = [ -8.000, -0.4905]
  cl(0:1, 3) = [ -7.000, -0.3574]
  cl(0:1, 4) = [ -6.000, -0.2261]
  cl(0:1, 5) = [ -5.000, -0.0986]
  cl(0:1, 6) = [ -4.000,  0.0285]
  cl(0:1, 7) = [ -3.000,  0.1544]
  cl(0:1, 8) = [ -2.000,  0.2796]
  cl(0:1, 9) = [ -1.000,  0.4042]
  cl(0:1,10) = [  0.000,  0.5283]
  cl(0:1,11) = [  1.000,  0.6519]
  cl(0:1,12) = [  2.000,  0.7747]
  cl(0:1,13) = [  3.000,  0.8966]
  cl(0:1,14) = [  4.000,  1.0169]
  cl(0:1,15) = [  5.000,  1.1333]
  cl(0:1,16) = [  6.000,  1.2458]
  cl(0:1,17) = [  7.000,  1.3369]
  cl(0:1,18) = [  8.000,  1.4118]
  cl(0:1,19) = [  9.000,  1.4709]
  cl(0:1,20) = [ 10.000,  1.5261]
  cl(0:1,21) = [ 11.000,  1.5830]
  cl(0:1,22) = [ 12.000,  1.6326]
  cl(0:1,23) = [ 13.000,  1.6752]
  cl(0:1,24) = [ 14.000,  1.7063]
  cl(0:1,25) = [ 15.000,  1.7301]
  cl(0:1,26) = [ 16.000,  1.7449]
  cl(0:1,27) = [ 17.000,  1.7596]
  cl(0:1,28) = [ 19.000,  1.7698]
  cl(0:1,29) = [ 19.500,  1.7710]
  cl(0:1,30) = [ 20.000,  1.7721]
  cl(0:1,31) = [ 20.500,  1.7686]
  cl(0:1,32) = [ 21.000,  1.7651]
  cl(0:1,33) = [ 21.500,  1.7640]
  cl(0:1,34) = [ 22.000,  1.7628]
  cl(0:1,35) = [ 23.000,  1.7605]
  cl(0:1,36) = [ 24.000,  1.7581]
  cl(0:1,37) = [ 25.000,  1.7502]
  cl(0:1,38) = [ 26.000,  1.7363]
  cl(0:1,39) = [ 27.000,  1.7118]
  
  IF ( y .EQ. cl(0,0) ) cl9 = cl(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl9 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO

1 CONTINUE

   IF ( y .LT. cl(0,0) ) THEN
     cl9 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl9_psu15.pro'
  ENDIF
  IF ( y .GT. cl(0,ns-1) ) THEN
     cl9 = cl(1,ns-1) 
!     PRINT*,'alpha > tabulated range in calc_cl9_psu15.pro'
  ENDIF 

!PRINT*,y,cl9

!_________________________________________________________________________

END FUNCTION calc_cl9_psu15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cl10_psu15( y ) RESULT( cl10 )
!_________________________________________________________________________
!
! Data from GE-PSU/95W18003.polar
!_________________________________________________________________________

  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 38

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cl

  REAL :: cl10
  INTEGER :: i
  
  cl10 = 0.0
  
!                alpha     CL    
  cl(0:1, 0) = [-9.000, -0.7037]
  cl(0:1, 1) = [-8.000, -0.6099]
  cl(0:1, 2) = [-7.000, -0.5124]
  cl(0:1, 3) = [-5.000, -0.3115]
  cl(0:1, 4) = [-4.000, -0.2101]
  cl(0:1, 5) = [-1.000,  0.1175]
  cl(0:1, 6) = [ 0.000,  0.2324]
  cl(0:1, 7) = [ 1.000,  0.3471]
  cl(0:1, 8) = [ 2.000,  0.4619]
  cl(0:1, 9) = [ 3.000,  0.5764]
  cl(0:1,10) = [ 4.000,  0.6902]
  cl(0:1,11) = [ 5.000,  0.8023]
  cl(0:1,12) = [ 6.000,  0.9125]
  cl(0:1,13) = [ 7.000,  1.0159]
  cl(0:1,14) = [ 9.000,  1.1311]
  cl(0:1,15) = [10.000,  1.1496]
  cl(0:1,16) = [11.000,  1.1982]
  cl(0:1,17) = [12.000,  1.2776]
  cl(0:1,18) = [13.000,  1.3222]
  cl(0:1,19) = [14.000,  1.3529]
  cl(0:1,20) = [15.000,  1.3766]
  cl(0:1,21) = [16.000,  1.3972]
  cl(0:1,22) = [17.000,  1.4070]
  cl(0:1,23) = [18.000,  1.4194]
  cl(0:1,24) = [18.500,  1.4210]
  cl(0:1,25) = [19.000,  1.4225]
  cl(0:1,26) = [20.000,  1.4198]
  cl(0:1,27) = [20.500,  1.4164]
  cl(0:1,28) = [21.000,  1.4129]
  cl(0:1,29) = [22.000,  1.3839]
  cl(0:1,30) = [23.000,  1.3731]
  cl(0:1,31) = [24.000,  1.3518]
  cl(0:1,32) = [25.000,  1.3237]
  cl(0:1,33) = [26.000,  1.3010]
  cl(0:1,34) = [27.000,  1.2707]
  cl(0:1,35) = [28.000,  1.2393]
  cl(0:1,36) = [29.000,  1.2137]
  cl(0:1,37) = [30.000,  1.1938]
  
  IF ( y .EQ. cl(0,0) ) cl10 = cl(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl10 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO

1 CONTINUE
  
   IF ( y .LT. cl(0,0) ) THEN
     cl10 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl9_psu15.pro'
  ENDIF
  IF ( y .GT. cl(0,ns-1) ) THEN
     cl10 = cl(1,ns-1) 
!     PRINT*,'alpha > tabulated range in calc_cl9_psu15.pro'
  ENDIF 
  
!PRINT*,y,cl10
  
!_________________________________________________________________________
  
END FUNCTION calc_cl10_psu15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cd1_psu15( y ) RESULT( cd1 )
!_________________________________________________________________________
!
! Data from Cylinder.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL :: cd1

  cl1 = 0.3
  
!_________________________________________________________________________
  
END FUNCTION calc_cd1_psu15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd2_psu15( y ) RESULT( cd2 )
!_________________________________________________________________________
!
!  Data from GE-PSU/Cylinder1.polar
!_________________________________________________________________________

  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 41

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cd

  REAL :: cd2
  INTEGER :: i

  cd2 = 0.0
  
!                alpha    CD   
  cd(0:1, 0) = [-7.000, 0.2974]
  cd(0:1, 1) = [-6.000, 0.2974]
  cd(0:1, 2) = [-5.000, 0.2974]
  cd(0:1, 3) = [-4.000, 0.2973]
  cd(0:1, 4) = [-3.000, 0.2973]
  cd(0:1, 5) = [-2.000, 0.2973]
  cd(0:1, 6) = [-1.000, 0.2973]
  cd(0:1, 7) = [ 0.000, 0.2973]
  cd(0:1, 8) = [ 1.000, 0.2973]
  cd(0:1, 9) = [ 2.000, 0.2973]
  cd(0:1,10) = [ 3.000, 0.2973]
  cd(0:1,11) = [ 4.000, 0.2973]
  cd(0:1,12) = [ 5.000, 0.2973]
  cd(0:1,13) = [ 6.000, 0.2974]
  cd(0:1,14) = [ 7.000, 0.2974]
  cd(0:1,15) = [ 8.000, 0.2974]
  cd(0:1,16) = [ 9.000, 0.2974]
  cd(0:1,17) = [10.000, 0.2974]
  cd(0:1,18) = [11.000, 0.2975]
  cd(0:1,19) = [12.000, 0.2976]
  cd(0:1,20) = [13.000, 0.2976]
  cd(0:1,21) = [14.000, 0.2977]
  cd(0:1,22) = [14.500, 0.2978]
  cd(0:1,23) = [15.000, 0.2978]
  cd(0:1,24) = [15.500, 0.2979]
  cd(0:1,25) = [16.000, 0.2979]
  cd(0:1,26) = [16.500, 0.2980]
  cd(0:1,27) = [17.000, 0.2981]
  cd(0:1,28) = [18.000, 0.2982]
  cd(0:1,29) = [19.000, 0.2983]
  cd(0:1,30) = [20.000, 0.2984]
  cd(0:1,31) = [21.000, 0.2984]
  cd(0:1,32) = [22.000, 0.2985]
  cd(0:1,33) = [23.000, 0.2985]
  cd(0:1,34) = [24.000, 0.2986]
  cd(0:1,35) = [25.000, 0.2986]
  cd(0:1,36) = [26.000, 0.2987]
  cd(0:1,37) = [27.000, 0.2988]
  cd(0:1,38) = [28.000, 0.2988]
  cd(0:1,39) = [29.000, 0.2989]
  cd(0:1,40) = [30.000, 0.2989]
  
  IF ( y .EQ. cd(0,0) ) cd2 = cd(1,0)
  DO i = 1,ns-1 
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd2 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN 
     cd2 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd2_psu15.pro'
  ENDIF
  IF ( y .GT. cd(0,ns-1) ) THEN 
     cd2 = cd(1,ns-1)
!     PRINT*,'alpha > tabulated range in calc_cd2_psu15.pro'
  ENDIF
!_________________________________________________________________________
  
END FUNCTION calc_cd2_psu15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cd3_psu15( y ) RESULT( cd3 )
!_________________________________________________________________________
!
! Data from GE-PSU/Cylinder7.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 41

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cd

  REAL :: cd3
  INTEGER :: i

  cd3 = 0.0
  
!                alpha    CD   
  cd(0:1, 0) = [-7.000, 0.1691]
  cd(0:1, 1) = [-6.000, 0.1679]
  cd(0:1, 2) = [-5.000, 0.1671]
  cd(0:1, 3) = [-4.000, 0.1667]
  cd(0:1, 4) = [-3.000, 0.1664]
  cd(0:1, 5) = [-2.000, 0.1664]
  cd(0:1, 6) = [-1.000, 0.1663]
  cd(0:1, 7) = [ 0.000, 0.1663]
  cd(0:1, 8) = [ 1.000, 0.1663]
  cd(0:1, 9) = [ 2.000, 0.1664]
  cd(0:1,10) = [ 3.000, 0.1664]
  cd(0:1,11) = [ 4.000, 0.1665]
  cd(0:1,12) = [ 5.000, 0.1667]
  cd(0:1,13) = [ 6.000, 0.1669]
  cd(0:1,14) = [ 7.000, 0.1673]
  cd(0:1,15) = [ 8.000, 0.1680]
  cd(0:1,16) = [ 9.000, 0.1694]
  cd(0:1,17) = [10.000, 0.1713]
  cd(0:1,18) = [11.000, 0.1746]
  cd(0:1,19) = [12.000, 0.1782]
  cd(0:1,20) = [13.000, 0.1819]
  cd(0:1,21) = [14.000, 0.1869]
  cd(0:1,22) = [14.500, 0.1890]
  cd(0:1,23) = [15.000, 0.1910]
  cd(0:1,24) = [15.500, 0.1939]
  cd(0:1,25) = [16.000, 0.1967]
  cd(0:1,26) = [16.500, 0.1997]
  cd(0:1,27) = [17.000, 0.2026]
  cd(0:1,28) = [18.000, 0.2073]
  cd(0:1,29) = [19.000, 0.2128]
  cd(0:1,30) = [20.000, 0.2176]
  cd(0:1,31) = [21.000, 0.2201]
  cd(0:1,32) = [22.000, 0.2231]
  cd(0:1,33) = [23.000, 0.2260]
  cd(0:1,34) = [24.000, 0.2292]
  cd(0:1,35) = [25.000, 0.2320]
  cd(0:1,36) = [26.000, 0.2347]
  cd(0:1,37) = [27.000, 0.2380]
  cd(0:1,38) = [28.000, 0.2414]
  cd(0:1,39) = [29.000, 0.2433]
  cd(0:1,40) = [30.000, 0.2465]
  
  IF ( y .EQ. cd(0,0) ) cd3 = cd(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd3 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN
     cd3 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd3_psu15.pro'
  ENDIF
  IF ( y .GT. cd(0,ns-1) ) THEN
     cd3 = cd(1,ns-1)
!     PRINT*,'alpha > tabulated range in calc_cd3_psu15.pro'
  ENDIF
!_________________________________________________________________________
  
END FUNCTION calc_cd3_psu15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cd4_psu15( y ) RESULT( cd4 )
!_________________________________________________________________________
! 
! Data from GE-PSU/Cylinder9.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 41

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cd

  REAL :: cd4
  INTEGER :: i

  cd4 = 0.0
!               alpha    CD   
  cd(0:1, 0) = [-7.000, 0.0869]
  cd(0:1, 1) = [-6.000, 0.0850]
  cd(0:1, 2) = [-5.000, 0.0838]
  cd(0:1, 3) = [-4.000, 0.0831]
  cd(0:1, 4) = [-3.000, 0.0827]
  cd(0:1, 5) = [-2.000, 0.0825]
  cd(0:1, 6) = [-1.000, 0.0824]
  cd(0:1, 7) = [ 0.000, 0.0824]
  cd(0:1, 8) = [ 1.000, 0.0824]
  cd(0:1, 9) = [ 2.000, 0.0825]
  cd(0:1,10) = [ 3.000, 0.0827]
  cd(0:1,11) = [ 4.000, 0.0828]
  cd(0:1,12) = [ 5.000, 0.0830]
  cd(0:1,13) = [ 6.000, 0.0835]
  cd(0:1,14) = [ 7.000, 0.0841]
  cd(0:1,15) = [ 8.000, 0.0852]
  cd(0:1,16) = [ 9.000, 0.0874]
  cd(0:1,17) = [10.000, 0.0906]
  cd(0:1,18) = [11.000, 0.0959]
  cd(0:1,19) = [12.000, 0.1018]
  cd(0:1,20) = [13.000, 0.1077]
  cd(0:1,21) = [14.000, 0.1160]
  cd(0:1,22) = [14.500, 0.1194]
  cd(0:1,23) = [15.000, 0.1227]
  cd(0:1,24) = [15.500, 0.1273]
  cd(0:1,25) = [16.000, 0.1319]
  cd(0:1,26) = [16.500, 0.1367]
  cd(0:1,27) = [17.000, 0.1416]
  cd(0:1,28) = [18.000, 0.1491]
  cd(0:1,29) = [19.000, 0.1582]
  cd(0:1,30) = [20.000, 0.1660]
  cd(0:1,31) = [21.000, 0.1699]
  cd(0:1,32) = [22.000, 0.1749]
  cd(0:1,33) = [23.000, 0.1796]
  cd(0:1,34) = [24.000, 0.1848]
  cd(0:1,35) = [25.000, 0.1894]
  cd(0:1,36) = [26.000, 0.1938]
  cd(0:1,37) = [27.000, 0.1991]
  cd(0:1,38) = [28.000, 0.2046]
  cd(0:1,39) = [29.000, 0.2077]
  cd(0:1,40) = [30.000, 0.2129]
  
  IF ( y .EQ. cd(0,0) ) cd4 = cd(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd4 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN
     cd4 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd4_psu15.pro'
  ENDIF
  IF ( y .GT. cd(0,ns-1) ) THEN
     cd4 = cd(1,ns-1)
!     PRINT*,'alpha > tabulated range in calc_cd4_psu15.pro'
  ENDIF
!_________________________________________________________________________
  
END FUNCTION calc_cd4_psu15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cd5_psu15( y ) RESULT( cd5 )
!_________________________________________________________________________
! 
! Data from GE-PSU/00W240103.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 41

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cd

  REAL :: cd5
  INTEGER :: i

  cd5 = 0.0

!                alpha    CD   
  cd(0:1, 0) = [-7.000, 0.01017]
  cd(0:1, 1) = [-6.000, 0.00764]
  cd(0:1, 2) = [-5.000, 0.00587]
  cd(0:1, 3) = [-4.000, 0.00500]
  cd(0:1, 4) = [-3.000, 0.00437]
  cd(0:1, 5) = [-2.000, 0.00421]
  cd(0:1, 6) = [-1.000, 0.00399]
  cd(0:1, 7) = [ 0.000, 0.00402]
  cd(0:1, 8) = [ 1.000, 0.00405]
  cd(0:1, 9) = [ 2.000, 0.00419]
  cd(0:1,10) = [ 3.000, 0.00439]
  cd(0:1,11) = [ 4.000, 0.00461]
  cd(0:1,12) = [ 5.000, 0.00487]
  cd(0:1,13) = [ 6.000, 0.00550]
  cd(0:1,14) = [ 7.000, 0.00632]
  cd(0:1,15) = [ 8.000, 0.00784]
  cd(0:1,16) = [ 9.000, 0.01092]
  cd(0:1,17) = [10.000, 0.01519]
  cd(0:1,18) = [11.000, 0.02238]
  cd(0:1,19) = [12.000, 0.03039]
  cd(0:1,20) = [13.000, 0.03848]
  cd(0:1,21) = [14.000, 0.04967]
  cd(0:1,22) = [14.500, 0.05426]
  cd(0:1,23) = [15.000, 0.05885]
  cd(0:1,24) = [15.500, 0.06506]
  cd(0:1,25) = [16.000, 0.07127]
  cd(0:1,26) = [16.500, 0.07788]
  cd(0:1,27) = [17.000, 0.08448]
  cd(0:1,28) = [18.000, 0.09481]
  cd(0:1,29) = [19.000, 0.10714]
  cd(0:1,30) = [20.000, 0.11769]
  cd(0:1,31) = [21.000, 0.12306]
  cd(0:1,32) = [22.000, 0.12991]
  cd(0:1,33) = [23.000, 0.13622]
  cd(0:1,34) = [24.000, 0.14327]
  cd(0:1,35) = [25.000, 0.14959]
  cd(0:1,36) = [26.000, 0.15547]
  cd(0:1,37) = [27.000, 0.16284]
  cd(0:1,38) = [28.000, 0.17020]
  cd(0:1,39) = [29.000, 0.17451]
  cd(0:1,40) = [30.000, 0.18150]

  IF ( y .EQ. cd(0,0) ) cd5 = cd(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd5 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN
     cd5 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd5_psu15.pro'
  ENDIF
  IF ( y .GT. cd(0,ns-1) ) THEN
     cd5 = cd(1,ns-1)
!     PRINT*,'alpha > tabulated range in calc_cd5_psu15.pro'
  ENDIF
!_________________________________________________________________________
  
END FUNCTION calc_cd5_psu15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd6_psu15( y ) RESULT( cd6 )
!_________________________________________________________________________
! 
! Data from GE-PSU/00W240103.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 50

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cd

  REAL :: cd6
  INTEGER :: i

  cd6 = 0.0

!                alpha    CD   
  cd(0:1, 0) = [-10.000, 0.01996]
  cd(0:1, 1) = [ -9.000, 0.01634]
  cd(0:1, 2) = [ -8.000, 0.01361]
  cd(0:1, 3) = [ -7.000, 0.01205]
  cd(0:1, 4) = [ -6.000, 0.01072]
  cd(0:1, 5) = [ -5.000, 0.00994]
  cd(0:1, 6) = [ -4.000, 0.00918]
  cd(0:1, 7) = [ -3.000, 0.00890]
  cd(0:1, 8) = [ -2.000, 0.00852]
  cd(0:1, 9) = [ -1.000, 0.00843]
  cd(0:1,10) = [  0.000, 0.00854]
  cd(0:1,11) = [  1.000, 0.00858]
  cd(0:1,12) = [  2.000, 0.00879]
  cd(0:1,13) = [  3.000, 0.00908]
  cd(0:1,14) = [  4.000, 0.00939]
  cd(0:1,15) = [  5.000, 0.00984]
  cd(0:1,16) = [  6.000, 0.01034]
  cd(0:1,17) = [  7.000, 0.01093]
  cd(0:1,18) = [  8.000, 0.01164]
  cd(0:1,19) = [  9.000, 0.01258]
  cd(0:1,20) = [ 10.000, 0.01416]
  cd(0:1,21) = [ 11.000, 0.01641]
  cd(0:1,22) = [ 12.000, 0.01959]
  cd(0:1,23) = [ 13.000, 0.02407]
  cd(0:1,24) = [ 14.000, 0.02982]
  cd(0:1,25) = [ 15.000, 0.03745]
  cd(0:1,26) = [ 15.500, 0.04201]
  cd(0:1,27) = [ 16.000, 0.04656]
  cd(0:1,28) = [ 17.000, 0.05767]
  cd(0:1,29) = [ 18.000, 0.06964]
  cd(0:1,30) = [ 19.000, 0.08228]
  cd(0:1,31) = [ 20.000, 0.09564]
  cd(0:1,32) = [ 21.000, 0.10982]
  cd(0:1,33) = [ 22.000, 0.12410]
  cd(0:1,34) = [ 22.500, 0.12990]
  cd(0:1,35) = [ 23.000, 0.13569]
  cd(0:1,36) = [ 23.500, 0.14229]
  cd(0:1,37) = [ 24.000, 0.14889]
  cd(0:1,38) = [ 24.500, 0.15525]
  cd(0:1,39) = [ 25.000, 0.16161]
  cd(0:1,40) = [ 25.500, 0.16690]
  cd(0:1,41) = [ 26.000, 0.17218]
  cd(0:1,42) = [ 26.500, 0.17802]
  cd(0:1,43) = [ 27.000, 0.18385]
  cd(0:1,44) = [ 27.500, 0.19051]
  cd(0:1,45) = [ 28.000, 0.19717]
  cd(0:1,46) = [ 28.500, 0.20210]
  cd(0:1,47) = [ 29.000, 0.20702]
  cd(0:1,48) = [ 29.500, 0.21266]
  cd(0:1,49) = [ 30.000, 0.21830]
 
  IF ( y .EQ. cd(0,0) ) cd6 = cd(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd6 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN
     cd6 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd6_psu15.pro'
  ENDIF
  IF ( y .GT. cd(0,ns-1) ) THEN
     cd6 = cd(1,ns-1)
!     PRINT*,'alpha > tabulated range in calc_cd6_psu15.pro'
  ENDIF
!_________________________________________________________________________
  
END FUNCTION calc_cd6_psu15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd7_psu15( y ) RESULT( cd7 )
!_________________________________________________________________________
! 
! Data from GE-PSU/97W30003.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 44

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cd

  REAL :: cd7
  INTEGER :: i

  cd7 = 0.0

!                alpha    CD   
  cd(0:1, 0) = [-10.000, 0.01146]
  cd(0:1, 1) = [ -9.000, 0.01054]
  cd(0:1, 2) = [ -8.000, 0.00998]
  cd(0:1, 3) = [ -7.000, 0.00953]
  cd(0:1, 4) = [ -6.000, 0.00921]
  cd(0:1, 5) = [ -5.000, 0.00909]
  cd(0:1, 6) = [ -4.000, 0.00904]
  cd(0:1, 7) = [ -3.000, 0.00890]
  cd(0:1, 8) = [ -2.000, 0.00891]
  cd(0:1, 9) = [ -1.000, 0.00900]
  cd(0:1,10) = [  0.000, 0.00902]
  cd(0:1,11) = [  1.000, 0.00915]
  cd(0:1,12) = [  2.000, 0.00929]
  cd(0:1,13) = [  3.000, 0.00947]
  cd(0:1,14) = [  4.000, 0.00970]
  cd(0:1,15) = [  5.000, 0.00996]
  cd(0:1,16) = [  6.000, 0.01029]
  cd(0:1,17) = [  7.000, 0.01067]
  cd(0:1,18) = [  8.000, 0.01111]
  cd(0:1,19) = [  9.000, 0.01180]
  cd(0:1,20) = [ 10.000, 0.01287]
  cd(0:1,21) = [ 11.000, 0.01499]
  cd(0:1,22) = [ 12.000, 0.01787]
  cd(0:1,23) = [ 13.000, 0.02253]
  cd(0:1,24) = [ 14.000, 0.02915]
  cd(0:1,25) = [ 14.500, 0.03383]
  cd(0:1,26) = [ 15.000, 0.03851]
  cd(0:1,27) = [ 16.000, 0.05097]
  cd(0:1,28) = [ 17.000, 0.06630]
  cd(0:1,29) = [ 18.000, 0.08373]
  cd(0:1,30) = [ 19.000, 0.10157]
  cd(0:1,31) = [ 20.000, 0.11856]
  cd(0:1,32) = [ 21.000, 0.13391]
  cd(0:1,33) = [ 21.500, 0.14077]
  cd(0:1,34) = [ 22.000, 0.14763]
  cd(0:1,35) = [ 22.500, 0.15425]
  cd(0:1,36) = [ 23.000, 0.16086]
  cd(0:1,37) = [ 23.500, 0.16706]
  cd(0:1,38) = [ 24.000, 0.17326]
  cd(0:1,39) = [ 25.000, 0.18594]
  cd(0:1,40) = [ 26.000, 0.19716]
  cd(0:1,41) = [ 27.000, 0.20972]
  cd(0:1,42) = [ 28.000, 0.22234]
  cd(0:1,43) = [ 30.000, 0.24615]
  
  IF ( y .EQ. cd(0,0) ) c7 = cd(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd7 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN
     cd7 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd7_psu15.pro'
  ENDIF
  IF ( y .GT. cd(0,ns-1) ) THEN
     cd7 = cd(1,ns-1)
!     PRINT*,'alpha > tabulated range in calc_cd7_psu15.pro'
  ENDIF
!_________________________________________________________________________
  
END FUNCTION calc_cd7_psu15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cd8_psu15( y ) RESULT( cd8 )
!_________________________________________________________________________
! 
! Data from GE-PSU/91W225003.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 45

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cd

  REAL :: cd8
  INTEGER :: i

  cd8 = 0.0

!                alpha    CD   
  cd(0:1, 0) = [-10.000, 0.01321]
  cd(0:1, 1) = [ -9.000, 0.01043]
  cd(0:1, 2) = [ -8.000, 0.00881]
  cd(0:1, 3) = [ -7.000, 0.00776]
  cd(0:1, 4) = [ -6.000, 0.00736]
  cd(0:1, 5) = [ -5.000, 0.00708]
  cd(0:1, 6) = [ -4.000, 0.00693]
  cd(0:1, 7) = [ -3.000, 0.00684]
  cd(0:1, 8) = [ -2.000, 0.00679]
  cd(0:1, 9) = [ -1.000, 0.00675]
  cd(0:1,10) = [  0.000, 0.00681]
  cd(0:1,11) = [  1.000, 0.00686]
  cd(0:1,12) = [  2.000, 0.00707]
  cd(0:1,13) = [  3.000, 0.00725]
  cd(0:1,14) = [  4.000, 0.00748]
  cd(0:1,15) = [  5.000, 0.00775]
  cd(0:1,16) = [  6.000, 0.00814]
  cd(0:1,17) = [  7.000, 0.00902]
  cd(0:1,18) = [  8.000, 0.01151]
  cd(0:1,19) = [  9.000, 0.01410]
  cd(0:1,20) = [ 10.000, 0.01673]
  cd(0:1,21) = [ 11.000, 0.02035]
  cd(0:1,22) = [ 12.000, 0.02538]
  cd(0:1,23) = [ 13.000, 0.03190]
  cd(0:1,24) = [ 14.000, 0.04040]
  cd(0:1,25) = [ 15.000, 0.05070]
  cd(0:1,26) = [ 16.000, 0.06240]
  cd(0:1,27) = [ 16.500, 0.06802]
  cd(0:1,28) = [ 17.000, 0.07363]
  cd(0:1,29) = [ 18.000, 0.08628]
  cd(0:1,30) = [ 18.500, 0.09178]
  cd(0:1,31) = [ 19.000, 0.09728]
  cd(0:1,32) = [ 19.500, 0.10333]
  cd(0:1,33) = [ 20.000, 0.10938]
  cd(0:1,34) = [ 21.000, 0.12183]
  cd(0:1,35) = [ 22.000, 0.13364]
  cd(0:1,36) = [ 23.000, 0.14516]
  cd(0:1,37) = [ 24.000, 0.15726]
  cd(0:1,38) = [ 25.000, 0.16986]
  cd(0:1,39) = [ 26.000, 0.18226]
  cd(0:1,40) = [ 27.000, 0.19550]
  cd(0:1,41) = [ 27.500, 0.20250]
  cd(0:1,42) = [ 28.000, 0.20949]
  cd(0:1,43) = [ 29.000, 0.22518]
  cd(0:1,44) = [ 30.000, 0.24152]

  IF ( y .EQ. cd(0,0) ) cd8 = cd(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd8 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN
     cd8 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd8_psu15.pro'
  ENDIF
  IF ( y .GT. cd(0,ns-1) ) THEN
     cd8 = cd(1,ns-1)
!     PRINT*,'alpha > tabulated range in calc_cd8_psu15.pro'
  ENDIF
!_________________________________________________________________________
  
END FUNCTION calc_cd8_psu15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd9_psu15( y ) RESULT( cd9 )
!_________________________________________________________________________
! 
! Data from GE-PSU/93W21003.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 40

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cd

  REAL :: cd9
  INTEGER :: i

  cd9 = 0.0

!                alpha    CD   
  cd(0:1, 0) = [-10.000, 0.01281]
  cd(0:1, 1) = [ -9.000, 0.01086]
  cd(0:1, 2) = [ -8.000, 0.00942]
  cd(0:1, 3) = [ -7.000, 0.00770]
  cd(0:1, 4) = [ -6.000, 0.00667]
  cd(0:1, 5) = [ -5.000, 0.00632]
  cd(0:1, 6) = [ -4.000, 0.00605]
  cd(0:1, 7) = [ -3.000, 0.00596]
  cd(0:1, 8) = [ -2.000, 0.00594]
  cd(0:1, 9) = [ -1.000, 0.00598]
  cd(0:1,10) = [  0.000, 0.00604]
  cd(0:1,11) = [  1.000, 0.00612]
  cd(0:1,12) = [  2.000, 0.00625]
  cd(0:1,13) = [  3.000, 0.00643]
  cd(0:1,14) = [  4.000, 0.00669]
  cd(0:1,15) = [  5.000, 0.00723]
  cd(0:1,16) = [  6.000, 0.00800]
  cd(0:1,17) = [  7.000, 0.01020]
  cd(0:1,18) = [  8.000, 0.01287]
  cd(0:1,19) = [  9.000, 0.01517]
  cd(0:1,20) = [ 10.000, 0.01765]
  cd(0:1,21) = [ 11.000, 0.02079]
  cd(0:1,22) = [ 12.000, 0.02520]
  cd(0:1,23) = [ 13.000, 0.03077]
  cd(0:1,24) = [ 14.000, 0.03771]
  cd(0:1,25) = [ 15.000, 0.04570]
  cd(0:1,26) = [ 16.000, 0.05503]
  cd(0:1,27) = [ 17.000, 0.06468]
  cd(0:1,28) = [ 19.000, 0.08687]
  cd(0:1,29) = [ 19.500, 0.09274]
  cd(0:1,30) = [ 20.000, 0.09860]
  cd(0:1,31) = [ 20.500, 0.10519]
  cd(0:1,32) = [ 21.000, 0.11178]
  cd(0:1,33) = [ 21.500, 0.11819]
  cd(0:1,34) = [ 22.000, 0.12460]
  cd(0:1,35) = [ 23.000, 0.13741]
  cd(0:1,36) = [ 24.000, 0.15051]
  cd(0:1,37) = [ 25.000, 0.16460]
  cd(0:1,38) = [ 26.000, 0.17998]
  cd(0:1,39) = [ 27.000, 0.19758]

  IF ( y .EQ. cd(0,0) ) cd9 = cd(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd9 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN
     cd9 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd9_psu15.pro'
  ENDIF
  IF ( y .GT. cd(0,ns-1) ) THEN
     cd9 = cd(1,ns-1)
!     PRINT*,'alpha > tabulated range in calc_cd9_psu15.pro'
  ENDIF
!_________________________________________________________________________
  
END FUNCTION calc_cd9_psu15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cd10_psu15( y ) RESULT( cd10 )
!_________________________________________________________________________
! 
! Data from GE-PSU/95W18003.polar
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: ns = 38

  REAL, DIMENSION( 0:1,0:ns-1 ) :: cd

  REAL :: cd10
  INTEGER :: i

  cd10 = 0.0

!                alpha    CD   
  cd(0:1, 0) = [-9.000, 0.01120]
  cd(0:1, 1) = [-8.000, 0.01033]
  cd(0:1, 2) = [-7.000, 0.00947]
  cd(0:1, 3) = [-5.000, 0.00770]
  cd(0:1, 4) = [-4.000, 0.00670]
  cd(0:1, 5) = [-1.000, 0.00524]
  cd(0:1, 6) = [ 0.000, 0.00520]
  cd(0:1, 7) = [ 1.000, 0.00520]
  cd(0:1, 8) = [ 2.000, 0.00527]
  cd(0:1, 9) = [ 3.000, 0.00538]
  cd(0:1,10) = [ 4.000, 0.00553]
  cd(0:1,11) = [ 5.000, 0.00575]
  cd(0:1,12) = [ 6.000, 0.00604]
  cd(0:1,13) = [ 7.000, 0.00663]
  cd(0:1,14) = [ 9.000, 0.01063]
  cd(0:1,15) = [10.000, 0.01297]
  cd(0:1,16) = [11.000, 0.01601]
  cd(0:1,17) = [12.000, 0.02061]
  cd(0:1,18) = [13.000, 0.02603]
  cd(0:1,19) = [14.000, 0.03275]
  cd(0:1,20) = [15.000, 0.04033]
  cd(0:1,21) = [16.000, 0.04856]
  cd(0:1,22) = [17.000, 0.05826]
  cd(0:1,23) = [18.000, 0.06806]
  cd(0:1,24) = [18.500, 0.07362]
  cd(0:1,25) = [19.000, 0.07918]
  cd(0:1,26) = [20.000, 0.09127]
  cd(0:1,27) = [20.500, 0.09769]
  cd(0:1,28) = [21.000, 0.10411]
  cd(0:1,29) = [22.000, 0.12020]
  cd(0:1,30) = [23.000, 0.13414]
  cd(0:1,31) = [24.000, 0.15026]
  cd(0:1,32) = [25.000, 0.16852]
  cd(0:1,33) = [26.000, 0.18692]
  cd(0:1,34) = [27.000, 0.20807]
  cd(0:1,35) = [28.000, 0.23155]
  cd(0:1,36) = [29.000, 0.25553]
  cd(0:1,37) = [30.000, 0.27962]

  IF ( y .EQ. cd(0,0) ) cd10 = cd(1,0)
  DO i = 1,ns-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd10 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN
     cd10 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd10_psu15.pro'
  ENDIF
  IF ( y .GT. cd(0,ns-1) ) THEN
     cd10 = cd(1,ns-1)
!     PRINT*,'alpha > tabulated range in calc_cd10_psu15.pro'
  ENDIF
!_________________________________________________________________________
  
END FUNCTION calc_cd10_psu15
!_________________________________________________________________________




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
      print *, ' Variable ',snaninf,' = ',rnaninf,' is Inf'
      naninf=1
    ENDIF
!   
    IF ( rnan .NE. rnan ) THEN
      print *, ' Variable ',srnaninf,' = ',rnaninf,' is NaN'
      naninf=2
    ENDIF
!_________________________________________________________________________
  
END FUNCTION check_naninf
!_________________________________________________________________________
  
END MODULE module_gen_act_disk

