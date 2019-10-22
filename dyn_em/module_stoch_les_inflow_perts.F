! WRF:MODEL_LAYER:PHYSICS
 
    MODULE module_stoch_les_inflow_perts

    USE module_model_constants    

#ifdef DM_PARALLEL
    USE MODULE_DM
#endif

    CONTAINS

!!=======================================================================

    SUBROUTINE force_down_meso_pblh( m_pblh, pblh,                        &
                                     ids, ide, jds, jde, kds, kde,        &
                                     ims, ime, jms, jme, kms, kme,        &
                                     its, ite, jts, jte, kts, kte         )

!-----------------------------------------------------------------------
!
! Assign pbl height to m_pblh array to be forced down for nested LES
!
!
    IMPLICIT NONE

    INTEGER, INTENT( IN )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    REAL , DIMENSION( ims:ime, jms:jme ), INTENT( IN  ) :: pblh
    REAL , DIMENSION( ims:ime, jms:jme ), INTENT( OUT ) :: m_pblh
    INTEGER :: itimestep
    INTEGER :: i,j
         

    DO j = jts, jte
       DO i = its, ite

          m_pblh(i,j) = pblh(i,j)

       END DO
    END DO
 
!    print*,'in m_pblh, m_pblh(its,jts)',m_pblh(its,jts)   

    END SUBROUTINE force_down_meso_pblh

!=======================================================================
 
    SUBROUTINE calc_pert_t( les_pert_opt,                      &
                            m_pblh_opt,                        &
                            prttms, prtdt, prtnk,              &
                            prtz, prtseed, pert_t,             &
                            m_pblh,                            &
                            t, u, v, rdz,                      &
                            dx, dt,                            &
                            ids, ide, jds, jde, kds, kde,      &
                            ims, ime, jms, jme, kms, kme,      &
                            its, ite, jts, jte, kts, kte       )

!-----------------------------------------------------------------------

    IMPLICIT NONE

#ifdef DM_PARALLEL
    INCLUDE 'mpif.h'
#endif

    INTEGER, INTENT( IN    )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) :: t      ! potential temperature               [k]
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) :: pert_t ! potential temperature perturbation  [k]
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN    ) :: u      ! zonal wind component                [m/s]
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN    ) :: v      ! meridional wind component           [m/s]
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN    ) :: rdz    ! inverse vertical grid spacing       [1/m]
    REAL , DIMENSION( ims:ime, jms:jme ),          INTENT( IN    ) :: m_pblh ! PBL height from messoscale PBL scheme [m]

    INTEGER , DIMENSION( kms:kme ), INTENT( INOUT ) :: prtseed         ! 1D variable to store variable-length seed array

    INTEGER,         INTENT( IN    ) :: les_pert_opt                   ! which perturbation option to use
    INTEGER,         INTENT( IN    ) :: m_pblh_opt                     ! whether to use (1) mesoscale PBL height or not (0)
    REAL,            INTENT( INOUT ) :: prttms                         ! time since last perturbation refresh
    REAL,            INTENT( INOUT ) :: prtdt                          ! time scale to refresh perturbations
    REAL,            INTENT( INOUT ) :: prtz                           ! height over swhich to apply perturbations
    INTEGER,         INTENT( INOUT ) :: prtnk                          ! number of vertical grid cells over which to apply perturbations
    REAL,            INTENT( IN    ) :: dt                             ! model time step
    REAL,            INTENT( IN    ) :: dx                             ! horizontal grid spacing

    REAL, DIMENSION( its:ite, kts:kte, jts:jte ) :: z  ! height above surface at midpoint levels   
    REAL, DIMENSION( kts:kte ) :: sf                   ! vertical scaling factor for perturbations  

    INTEGER :: i, j, k, l, m, n               ! loop variables
    INTEGER :: i_start, i_end, j_start, j_end ! patch endpoints
    INTEGER :: itilemid, jtilemid             ! approximate patch midpoints
    INTEGER :: i_seed                         ! size of seed
    INTEGER :: sum                            ! variable for summing the seed
    INTEGER :: ngc_h, ngc_v                   ! number of gridcells in i and j directions for each perturbation cell
    INTEGER :: ni, nj                         ! number of perturbation cells in i and j directions (for random number query)
    INTEGER :: ncells_h, ncells_v             ! number of perturbations cells in horizontal and vertical directions
    INTEGER :: k_pert_start, k_pert_end       ! vertical indices over which to apply perturbations
    INTEGER :: north, south, east, west, nsew !logic for identification of LBs to perturb
    INTEGER :: k_wg                           ! k index at which h_wg is computed
    INTEGER :: k_ws                           ! k index at which h_ws is computed
    REAL    :: h_wg                           ! height at which to estimate ug
    REAL    :: h_ws                           ! height at which to calculate slowest wind speed for pert timescale
    REAL    :: mpblhpsum, mpblhpavg, mpblhdsum, mpblhdavg                         ! pblh patch sum and domain averages
    REAL    :: uwsgpsum, vwsgpsum, uwsgpsumdsum, vwsgpsumdsum                     ! Sums for inflow/outflow boundary designation
    REAL    :: wsgpsum, wsgpsumdsum, wsgpavgdavg                                  ! Variables for perturbation magnitude
    REAL    :: wsspsum, wsspsumdsum, wsspavgdavg                                  ! Variables for perturbation timescale
    REAL    :: num_pts_in_sum, counter                                            ! Variables for computing averages
    REAL    :: tpertmag                       ! Magnitude of thermal perturbation
    REAL    :: angles, anglespsum, anglespsumdsum, anglespavgdavg 
    REAL    :: dz                             ! Depth
    REAL    :: pblh                           ! PBL depth if not using m_pblh
    REAL    :: sf2                            ! scaling factor for perturbation profile
    REAL    :: ek_opt = 0.20                  ! optimal value of the perturbation Ekman #
    REAL    :: pio2 = piconst/2.0
    REAL    :: pio4 = piconst/4.0

    INTEGER, dimension( : ), allocatable :: seed                 ! random number seed
    REAL, dimension( :, :, : ), allocatable :: pxs,pxe,pys,pye   ! random number arrays for each lateral boundary
  
#ifdef DM_PARALLEL
    INTEGER :: ierr
    INTEGER :: tag
    INTEGER :: master
    INTEGER :: status(MPI_STATUS_SIZE)
#endif

#ifdef DM_PARALLEL
    master = 0
    tag    = 0
#endif

! END DECLARATIONS

    prttms = prttms + dt

!    print*,'prttms',prttms

    IF (  prttms .GE. prtdt ) THEN !prdt should be zero at startup

       print*,'Computing new perturbations'

! Time to compute perturbations. Need to compute:
!  1. Heights of various control parameters (k-levels)
!  2. Timescale over which to hold perturbations constant 
!  3. Magnitude of perturbations
!  4. Which boundaries are inflow to apply perturbations to

       prttms = dt

       i_start = its
       i_end   = MIN(ite,ide-1)
       j_start = jts
       j_end   = MIN(jte,jde-1)

       ngc_h = 8      !width of perturbation in horizontal directions
       ngc_v = 2      !depth of perturbation in vertical
       ncells_h = 3

       DO j=j_start, j_end
          DO i=i_start, i_end
             z(i,kts,j)= 1.0/rdz(i,kts,j)
          END DO
       END DO
  
       DO j=j_start, j_end
          DO i=i_start, i_end
             DO k=kts+1,kte-1  
                z(i,k,j) = z(i,k-1,j) + 1.0/rdz(i,k,j)
             END DO
          END DO
       END DO  

!       pblh = 1200.0 !50 W/m^2
       pblh = 1000.0 !NEUTRAL

! 1) Compute average pblh on each tile. 2) Find the domain max and min of all patch averages.
! 3) Compute the average of the global max and min values and use on all patches
! 4) Cap upper and lower bounds.
! Something smarter will be needed for complex terrain.

       IF (m_pblh_opt .EQ. 1 ) THEN 

          mpblhpsum = 0.0
          mpblhpavg = 0.0
          counter = 0.0
          DO j=j_start, j_end
             DO i=i_start, i_end
                mpblhpsum = mpblhpsum + m_pblh(i,j)
                counter = counter + 1.0
             END DO
          END DO

#ifdef DM_PARALLEL

mpblhdsum  = wrf_dm_sum_real(mpblhpsum)

#endif
              
          counter = float(ide-ids)*float(jde-jds) ! One fewer since m_pblh is at mass points

          mpblhdavg = mpblhdsum/counter
          
          pblh = MAX(100.0,MIN(mpblhdavg,3000.0))
                   
!          print*,'its,mpblhdsum,counter,mpblhdavg ',its,mpblhdsum,counter,mpblhdavg

       ENDIF 

       print*,'pblh',pblh

!       prtz = 1.0*pblh
       prtz = 0.9*pblh !jdm TEST 09142017
       h_wg = 1.10*pblh 
       h_ws = 0.05*pblh
       sf2 = 4.0*prtz/27.0 !maximum value of sf(z) = z*(1-z/h)^2 
   
! Find the middle of the tile.

       itilemid = (ite+its)/2
       jtilemid = (jte+jts)/2

! Find the k-values corresponding to height values at the middle of the tile. 
! This will require modification for complex terrain.

       prtnk = 1

       DO k=kts,kte - 1  
!          sf(k) = (z(itilemid,k,jtilemid)*(1.0-z(itilemid,k,jtilemid)/prtz)**2)/sf2
          sf(k) = 1.0 !jdm TEST 09142017
          IF ( z(itilemid,k,jtilemid) .LE. prtz ) prtnk = k
          IF ( z(itilemid,k,jtilemid) .LE. h_wg ) k_wg = k
          IF ( z(itilemid,k,jtilemid) .LE. h_ws ) k_ws = k
          IF ( z(itilemid,k,jtilemid) .GT. prtz ) sf(k) = 0.0
!          print*,'k,z(itilemid,k,jtilemid),sf(k)',k,z(itilemid,k,jtilemid),sf(k)
         END DO

        k_ws = 4

        sf(1) = 0.0
        ! sf(2) = 0.33
        ! sf(3) = 0.67

!       print*,'prtnk,k_wg,k_ws',prtnk,k_wg,k_ws

! CALCULATE WIND SPEEDS FOR  PERTURBATION MAGNITUDE =============================================

! 1) Compute sum of u and v corresponding to k_wg along all lateral boundaries. This will determine which are inflow  
! Something smarter will be needed for complex terrain.   

       uwsgpsum = 0.0
       vwsgpsum = 0.0

       IF ( its .EQ. ids ) THEN ! include points along ids inflow boundary

          DO j=j_start, j_end

             uwsgpsum = uwsgpsum + u(ids,k_wg,j)
             vwsgpsum = vwsgpsum + v(ids,k_wg,j)

          END DO

       ENDIF

       IF ( ite .EQ. ide ) THEN ! include points along ide inflow boundary

          DO j=j_start, j_end
      
             uwsgpsum = uwsgpsum + u(ide,k_wg,j)
             vwsgpsum = vwsgpsum + v(ide,k_wg,j)

          END DO

       ENDIF

       IF ( jts .EQ. jds ) THEN ! include points along jds inflow boundary

          DO i=i_start, i_end
      
             uwsgpsum = uwsgpsum + u(i,k_wg,jds)
             vwsgpsum = vwsgpsum + v(i,k_wg,jds)

          END DO

       ENDIF

       IF ( jte .EQ. jde ) THEN ! include points along jde inflow boundary

          DO i=i_start, i_end

             uwsgpsum = uwsgpsum + u(i,k_wg,jde)
             vwsgpsum = vwsgpsum + v(i,k_wg,jde)

          END DO

       ENDIF

! Obtain global sum of uwsspsum and uwgspsum 

#ifdef DM_PARALLEL

uwsgpsumdsum  = wrf_dm_sum_real(uwsgpsum)

vwsgpsumdsum  = wrf_dm_sum_real(vwsgpsum)

#endif
   
       north = 0
       east = 0
       south = 0
       west = 0

       IF ( uwsgpsumdsum .GT. 0.0 ) west = 1
       IF ( uwsgpsumdsum .LT. 0.0 ) east = 1
       IF ( vwsgpsumdsum .GT. 0.0 ) south = 1
       IF ( vwsgpsumdsum .LT. 0.0 ) north = 1
           
!       print*,'uwsgpsumdsum,vwsgpsumdsum',uwsgpsumdsum,vwsgpsumdsum
!       print*,'n, s, e, w',north,south,east,west     

!
! Uncomment to select your own inflow/outflow boundaries
!
!       north = 0
!       east = 0
!       south = 0
!       west = 1

       print*,'Applying perturbations to the following boundary(ies):'
       IF (north .EQ. 1) print*,'north'
       IF (south .EQ. 1) print*,'south'
       IF (east .EQ. 1) print*,'east'
       IF (west .EQ. 1) print*,'west'

       nsew = 0
       IF  ( (west + east) .EQ. 2) nsew = 1
       IF  ( (north + south) .EQ. 2) nsew = 1
       IF  ( (north + south + east + west) .EQ. 1) nsew = 1
       IF  ( (north + south + east + west) .EQ. 4) nsew = 1
       IF  ( (north + south + east + west) .EQ. 3) nsew = 0

! CALCULATE WIND SPEEDS AT INFLOW BOUNDARIES AT k=k_wg AND k=k_ws FOR PERTURBATION MAGNITUDE AND TIMESCALE =============================================

       wsspsum = 0.0
       wsgpsum = 0.0
       anglespsum = 0.0
       counter = 0.0
       num_pts_in_sum = 0.0

       IF ( west .EQ. 1 ) THEN

          IF ( its .EQ. ids ) THEN ! include points along ids inflow boundary

             DO j=j_start, j_end

                angles = atan( abs( u(ids,k_ws,j) )/abs( v(ids,k_ws,j) ) )
                
                IF (angles .GT. pio4 ) angles = pio2 - angles 

                anglespsum = anglespsum  + angles  

                wsspsum = wsspsum + sqrt( u(ids,k_ws,j)*u(ids,k_ws,j) + v(ids,k_ws,j)*v(ids,k_ws,j) )
                wsgpsum = wsgpsum + sqrt( u(ids,k_wg,j)*u(ids,k_wg,j) + v(ids,k_wg,j)*v(ids,k_wg,j) )

                num_pts_in_sum = num_pts_in_sum + 1.0

             END DO

          ENDIF

       ENDIF

       IF ( east .EQ. 1 ) THEN

          IF ( ite .EQ. ide ) THEN ! include points along ide inflow boundary

             DO j=j_start, j_end
      
                angles = atan( abs( u(ide,k_ws,j) )/abs( v(ide,k_ws,j) ) )
                
                IF (angles .GT. pio4 ) angles = pio2 - angles 

                anglespsum = anglespsum  + angles  

                wsspsum = wsspsum + sqrt( u(ide,k_ws,j)*u(ide,k_ws,j) + v(ide,k_ws,j)*v(ide,k_ws,j) )
                wsgpsum = wsgpsum + sqrt( u(ide,k_wg,j)*u(ide,k_wg,j) + v(ide,k_wg,j)*v(ide,k_wg,j) )

                num_pts_in_sum = num_pts_in_sum + 1.0

             END DO

          ENDIF

       ENDIF

       IF ( south .EQ. 1 ) THEN

          IF ( jts .EQ. jds ) THEN ! include points along jds inflow boundary

             DO i=i_start, i_end
      
                angles = atan( abs( u(i,k_ws,jds) )/abs( v(i,k_ws,jds) ) )

                IF (angles .GT. pio4 ) angles = pio2 - angles 

                anglespsum = anglespsum  + angles  

                wsspsum = wsspsum + sqrt( u(i,k_ws,jds)*u(i,k_ws,jds) + v(i,k_ws,jds)*v(i,k_ws,jds) )
                wsgpsum = wsgpsum + sqrt( u(i,k_wg,jds)*u(i,k_wg,jds) + v(i,k_wg,jds)*v(i,k_wg,jds) )

                num_pts_in_sum = num_pts_in_sum + 1.0

             END DO

          ENDIF

       ENDIF

       IF ( north .EQ. 1 ) THEN

          IF ( jte .EQ. jde ) THEN ! include points along jde inflow boundary

             DO i=i_start, i_end

                angles = atan( abs( u(i,k_ws,jde) )/abs( v(i,k_ws,jde) ) )
                
                IF (angles .GT. pio4 ) angles = pio2 - angles 

                anglespsum = anglespsum  + angles  

                wsspsum = wsspsum + sqrt( u(i,k_ws,jde)*u(i,k_ws,jde) + v(i,k_ws,jde)*v(i,k_ws,jde) )
                wsgpsum = wsgpsum + sqrt( u(i,k_wg,jde)*u(i,k_wg,jde) + v(i,k_wg,jde)*v(i,k_wg,jde) )

                num_pts_in_sum = num_pts_in_sum + 1.0

             END DO

          ENDIF

       ENDIF

! Obtain global sum of uwsspsum and total number of gridpoints contributing to the sum

#ifdef DM_PARALLEL

wsspsumdsum  = wrf_dm_sum_real(wsspsum)

wsgpsumdsum  = wrf_dm_sum_real(wsgpsum)

anglespsumdsum  = wrf_dm_sum_real(anglespsum)

counter = wrf_dm_sum_real(num_pts_in_sum)

#endif

       wsgpavgdavg = wsgpsumdsum/counter

       wsspavgdavg = wsspsumdsum/counter

       anglespavgdavg = anglespsumdsum/counter

!       print*,'its,wsspsum,wsspsumdsum,wsspavgdavg,counter ',its,wsspsum,wsspsumdsum,wsspavgdavg,counter
!       print*,'its,wsgpsum,wsgpsumdsum,wsgpavgdavg ',its,wsgpsum,wsgpsumdsum,wsgpavgdavg
     
       IF (wsspavgdavg .EQ. 0.0) THEN !Prevent division by zero

          print*,'something wrong in stoch_les_inflow_perts'
          STOP

       ENDIF

       tpertmag = (wsgpavgdavg*wsgpavgdavg)/(ek_opt*cp)
    
       print*,'tpertmag',tpertmag

       prtdt = 0.75*(1.0/cos(anglespavgdavg))*ngc_h*ncells_h*dx/wsspavgdavg  !angle corrects for mean wind direction
                                                                              !0.875 corrrects for ramping at inflow, 21/24

!       print*,'anglespavgdavg',anglespavgdavg*180.0/piconst

       print*,'prtdt',prtdt

  
! CALCULATE (NEW) PERTURBATIONS ===============================================================================

       ni = (ide-1)/ngc_h 
       nj = (jde-1)/ngc_h  

!       print*,'ni,nj',ni,nj
 
       ncells_v = kte-1 !prtnk/ngc_v + 1 --if using prtnk must ensure same value on each patch

! Allocate perturbation array for each lateral boundary 

        ALLOCATE( pxs(1:nj,1:ncells_v,1:ncells_h) )
        ALLOCATE( pxe(1:nj,1:ncells_v,1:ncells_h) )
        ALLOCATE( pys(1:ni,1:ncells_v,1:ncells_h) )
        ALLOCATE( pye(1:ni,1:ncells_v,1:ncells_h) )

       CALL RANDOM_SEED(size=i_seed)  ! 1. Get the size of the seed, each processor.

       ALLOCATE( seed(1:i_seed) )     ! 2. Allocate an array to hold the seed, each processor.

#ifdef DM_PARALLEL

       IF ( mytask .EQ. master ) THEN

          sum = 0                     ! 3a. Sum the stored seed (up to i_seed) to see if it has been assigned.
          DO k = 1,i_seed             !     If so, now master has the current prtseed value in seed.
             seed(k) = prtseed(k)
             sum = sum + seed(k)
          END DO

          IF (sum .EQ. 0 ) THEN       !3b. If the seed is empty, get the first seed, which is now only on master.

             print*,'calling random seed for first time'   
             CALL RANDOM_SEED(get=seed)

          ENDIF
 
       ENDIF 

       CALL MPI_BCAST(seed,i_seed,MPI_REAL,master,MPI_COMM_WORLD,ierr) ! Broadcast seed from master to all

!       print*,'seed',seed

       CALL RANDOM_SEED(put=seed) !Use same seed value so all processors receive idential random numbers

       CALL RANDOM_NUMBER(pxs)
       CALL RANDOM_NUMBER(pxe)
       CALL RANDOM_NUMBER(pys)
       CALL RANDOM_NUMBER(pye)

!       CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

       IF ( mytask .EQ. master ) THEN! Must get a new seed and store into prtseed so the sequence at the next perturbation update begins with new seed. 

          CALL RANDOM_SEED(get=seed)

             DO k = 1,i_seed
                prtseed(k) = seed(k)
             ENDDO!
  
       ENDIF

#endif

!       k_pert_start = 1
       k_pert_start = 2 ! jdm TEST 09142017
       k_pert_end = prtnk

       DO k = kts,kte  ! Zero out. Probably not necessary but good religion. 
          DO j=j_start, j_end
             DO i=i_start, i_end
                pert_t(i,k,j) = 0.0
             END DO
          END DO
       END DO

       IF ( west .EQ. 1 ) THEN

          IF (its .LE. ids + ngc_h*ncells_h) THEN 

             DO k = k_pert_start, k_pert_end
  
                n = (k-1)/ngc_v+1

                DO j = MAX(jts,jds + ngc_h*ncells_h*(west - south - nsew)), MIN(jte,jde - 1 - ngc_h*ncells_h*(west - north - nsew))

!                   print*,'j,((j-1)/ngc_h+1)',j,((j-1)/ngc_h+1)

                   DO i = its, MIN(ite, ide-1)

!                      print*,'i,((i-1)/ngc_h+1)',i,((i-1)/ngc_h+1)

                      m = (i-1)/ngc_h+1

                      IF ( m .LE. ncells_h ) THEN

                         pert_t(i,k,j) =  (pxs(((j-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag !Output variable only, for debugging.

                         t(i,k,j) = t(i,k,j) + (pxs(((j-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

                      ENDIF

                   END DO ! i

                END DO ! j

             END DO ! k

          ENDIF

       ENDIF

       IF ( east .EQ. 1 ) THEN

          IF (ite .GE. ide - 1 - ngc_h*ncells_h) THEN 
  
             DO k = k_pert_start, k_pert_end

                n = (k-1)/ngc_v+1

                DO j = MAX(jts,jds + ngc_h*ncells_h*(east - south - nsew)), MIN(jte,jde - 1 - ngc_h*ncells_h*(east - north - nsew))

                   DO i = MIN(ite, ide-1), its, -1

                      m = (ide-i-1)/ngc_h+1
  
                      IF ( m .LE. ncells_h ) THEN

                         pert_t(i,k,j) = (pxe(((j-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

                         t(i,k,j) = t(i,k,j) + (pxe(((j-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

                      ENDIF

                   END DO ! i

                END DO ! j

             END DO ! k

          ENDIF 

       ENDIF

       IF ( south .EQ. 1 ) THEN

          IF (jts .LE. jds + ngc_h*ncells_h) THEN 

             DO k = k_pert_start, k_pert_end

                n = (k-1)/ngc_v+1

                DO i = MAX(its,ids + ngc_h*ncells_h*(south - west - nsew)), MIN(ite,ide - 1 - ngc_h*ncells_h*(south - east - nsew))

                   DO j = jts, MIN(jte, jde-1)

                      m = (j-1)/ngc_h+1

                      IF ( m .LE. ncells_h ) THEN    

!    print*,'i,(i-1)/ngc_h+1,j,m,pys',i,(i-1)/ngc_h+1,j,m
!    print*,'k,n,pys',k,n,pys(((i-1)/ngc_h+1),n,m)

                        pert_t(i,k,j) = (pys(((i-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

                         t(i,k,j) = t(i,k,j) + (pys(((i-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

                      ENDIF

                   END DO ! i

                END DO ! j

             END DO ! k

          ENDIF

       ENDIF

       IF ( north .EQ. 1 ) THEN

          IF (jte .GE. jde - 1 - ngc_h*ncells_h) THEN 

             DO k = k_pert_start, k_pert_end

                n = (k-1)/ngc_v+1

                DO i = MAX(its,ids + ngc_h*ncells_h*(north - west - nsew)), MIN(ite,ide - 1 - ngc_h*ncells_h*(north - east - nsew))

                   DO j = MIN(jte, jde-1),jts,-1

                      m = (jde-j-1)/ngc_h+1

                      IF ( m .LE. ncells_h ) THEN
  
                         pert_t(i,k,j) = (pye(((i-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

                         t(i,k,j) = t(i,k,j) + (pye(((i-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

                      ENDIF

                   END DO ! i

                END DO ! j

             END DO ! k

          ENDIF

       ENDIF

       DEALLOCATE(seed)

       DEALLOCATE( pxs )
       DEALLOCATE( pxe )
       DEALLOCATE( pys )
       DEALLOCATE( pye )

    ENDIF ! (prttms .GT. prdt)

!=================================================================================

END SUBROUTINE calc_pert_t

!=======================================================================

!=======================================================================
 
    SUBROUTINE calc_pert_uv( les_pert_opt,                      &
                             m_pblh_opt,                        &
                             prttms, prtdt, prtnk,              &
                             prtz, prtseed, pert_t,             &
                             m_pblh,                            &
                             ru_tendf, rv_tendf,                &
                             muu, muv,                          &
                             u, v, rdz,                         &
                             dx, dt,                            &
                             ids, ide, jds, jde, kds, kde,      &
                             ims, ime, jms, jme, kms, kme,      &
                             its, ite, jts, jte, kts, kte       )


! Similar to temperature perturbation, but applied to u and v. Still uses pert_t to store.
! Still cell centered. Computes new pert_t according to prttms, applies to tendency every timestep. 
! JDM March, 2018
!-----------------------------------------------------------------------

    IMPLICIT NONE

#ifdef DM_PARALLEL
    INCLUDE 'mpif.h'
#endif

    INTEGER, INTENT( IN    )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) :: pert_t ! potential temperature perturbation  [k]
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN    ) :: u      ! zonal wind component                [m/s]
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN    ) :: v      ! meridional wind component           [m/s]
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) :: ru_tendf  ! zonal wind component                []
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) :: rv_tendf  ! meridional wind component           []
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN    ) :: rdz    ! inverse vertical grid spacing       [1/m]
    REAL , DIMENSION( ims:ime, jms:jme ),          INTENT( IN    ) :: m_pblh ! PBL height from messoscale PBL scheme [m]

    REAL , DIMENSION( ims:ime, jms:jme ),          INTENT( IN    ) :: muu
    REAL , DIMENSION( ims:ime, jms:jme ),          INTENT( IN    ) :: muv



    INTEGER , DIMENSION( kms:kme ), INTENT( INOUT ) :: prtseed         ! 1D variable to store variable-length seed array

    INTEGER,         INTENT( IN    ) :: les_pert_opt                   ! which perturbation option to use
    INTEGER,         INTENT( IN    ) :: m_pblh_opt                     ! whether to use (1) mesoscale PBL height or not (0)
    REAL,            INTENT( INOUT ) :: prttms                         ! time since last perturbation refresh
    REAL,            INTENT( INOUT ) :: prtdt                          ! time scale to refresh perturbations
    REAL,            INTENT( INOUT ) :: prtz                           ! height over swhich to apply perturbations
    INTEGER,         INTENT( INOUT ) :: prtnk                          ! number of vertical grid cells over which to apply perturbations
    REAL,            INTENT( IN    ) :: dt                             ! model time step
    REAL,            INTENT( IN    ) :: dx                             ! horizontal grid spacing

    REAL, DIMENSION( its:ite, kts:kte, jts:jte ) :: z  ! height above surface at midpoint levels   
    REAL, DIMENSION( kts:kte ) :: sf                   ! vertical scaling factor for perturbations  

    INTEGER :: i, j, k, l, m, n               ! loop variables
    INTEGER :: i_start, i_end, j_start, j_end ! patch endpoints
    INTEGER :: itilemid, jtilemid             ! approximate patch midpoints
    INTEGER :: i_seed                         ! size of seed
    INTEGER :: sum                            ! variable for summing the seed
    INTEGER :: ngc_h, ngc_v                   ! number of gridcells in i and j directions for each perturbation cell
    INTEGER :: ni, nj                         ! number of perturbation cells in i and j directions (for random number query)
    INTEGER :: ncells_h, ncells_v             ! number of perturbations cells in horizontal and vertical directions
    INTEGER :: k_pert_start, k_pert_end       ! vertical indices over which to apply perturbations
    INTEGER :: north, south, east, west, nsew !logic for identification of LBs to perturb
    INTEGER :: k_wg                           ! k index at which h_wg is computed
    INTEGER :: k_ws                           ! k index at which h_ws is computed
    REAL    :: h_wg                           ! height at which to estimate ug
    REAL    :: h_ws                           ! height at which to calculate slowest wind speed for pert timescale
    REAL    :: mpblhpsum, mpblhpavg, mpblhdsum, mpblhdavg                         ! pblh patch sum and domain averages
    REAL    :: uwsgpsum, vwsgpsum, uwsgpsumdsum, vwsgpsumdsum                     ! Sums for inflow/outflow boundary designation
    REAL    :: wsgpsum, wsgpsumdsum, wsgpavgdavg                                  ! Variables for perturbation magnitude
    REAL    :: wsspsum, wsspsumdsum, wsspavgdavg                                  ! Variables for perturbation timescale
    REAL    :: num_pts_in_sum, counter                                            ! Variables for computing averages
    REAL    :: tpertmag                       ! Magnitude of thermal perturbation
    REAL    :: angles, anglespsum, anglespsumdsum, anglespavgdavg 
    REAL    :: dz                             ! Depth
    REAL    :: pblh                           ! PBL depth if not using m_pblh
    REAL    :: sf2                            ! scaling factor for perturbation profile
    REAL    :: ek_opt = 0.20                  ! optimal value of the perturbation Ekman #
    REAL    :: pio2 = piconst/2.0
    REAL    :: pio4 = piconst/4.0

    INTEGER, dimension( : ), allocatable :: seed                 ! random number seed
    REAL, dimension( :, :, : ), allocatable :: pxs,pxe,pys,pye   ! random number arrays for each lateral boundary
  
#ifdef DM_PARALLEL
    INTEGER :: ierr
    INTEGER :: tag
    INTEGER :: master
    INTEGER :: status(MPI_STATUS_SIZE)
#endif

#ifdef DM_PARALLEL
    master = 0
    tag    = 0
#endif

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)

! END DECLARATIONS

    prttms = prttms + dt

!    print*,'prttms',prttms

    IF (  prttms .GE. prtdt ) THEN !prdt should be zero at startup

       print*,'Computing new perturbations'

! Time to compute perturbations. Need to compute:
!  1. Heights of various control parameters (k-levels)
!  2. Timescale over which to hold perturbations constant 
!  3. Magnitude of perturbations
!  4. Which boundaries are inflow to apply perturbations to

       prttms = dt

       ngc_h = 8      !width of perturbation in horizontal directions
       ngc_v = 4      !depth of perturbation in vertical
       ncells_h = 3

       DO j=j_start, j_end
          DO i=i_start, i_end
             z(i,kts,j)= 1.0/rdz(i,kts,j)
          END DO
       END DO
  
       DO j=j_start, j_end
          DO i=i_start, i_end
             DO k=kts+1,kte-1  
                z(i,k,j) = z(i,k-1,j) + 1.0/rdz(i,k,j)
             END DO
          END DO
       END DO  

!       pblh = 1200.0 !50 W/m^2
       pblh = 1000.0 !NEUTRAL

! 1) Compute average pblh on each tile. 2) Find the domain max and min of all patch averages.
! 3) Compute the average of the global max and min values and use on all patches
! 4) Cap upper and lower bounds.
! Something smarter will be needed for complex terrain.

       IF (m_pblh_opt .EQ. 1 ) THEN 

          mpblhpsum = 0.0
          mpblhpavg = 0.0
          counter = 0.0
          DO j=j_start, j_end
             DO i=i_start, i_end
                mpblhpsum = mpblhpsum + m_pblh(i,j)
                counter = counter + 1.0
             END DO
          END DO

#ifdef DM_PARALLEL

mpblhdsum  = wrf_dm_sum_real(mpblhpsum)

#endif
              
          counter = float(ide-ids)*float(jde-jds) ! One fewer since m_pblh is at mass points

          mpblhdavg = mpblhdsum/counter
          
          pblh = MAX(100.0,MIN(mpblhdavg,3000.0))
                   
!          print*,'its,mpblhdsum,counter,mpblhdavg ',its,mpblhdsum,counter,mpblhdavg

       ENDIF 

       print*,'pblh',pblh

!       prtz = 1.0*pblh
       prtz = 0.9*pblh !jdm TEST 09142017
       h_wg = 1.10*pblh 
       h_ws = 0.05*pblh
       sf2 = 4.0*prtz/27.0 !maximum value of sf(z) = z*(1-z/h)^2 
   
! Find the middle of the tile.

       itilemid = (ite+its)/2
       jtilemid = (jte+jts)/2

! Find the k-values corresponding to height values at the middle of the tile. 
! This will require modification for complex terrain.

       prtnk = 1

       DO k=kts,kte - 1  
!          sf(k) = (z(itilemid,k,jtilemid)*(1.0-z(itilemid,k,jtilemid)/prtz)**2)/sf2
          sf(k) = 1.0 !jdm TEST 09142017
          IF ( z(itilemid,k,jtilemid) .LE. prtz ) prtnk = k
          IF ( z(itilemid,k,jtilemid) .LE. h_wg ) k_wg = k
          IF ( z(itilemid,k,jtilemid) .LE. h_ws ) k_ws = k
          IF ( z(itilemid,k,jtilemid) .GT. prtz ) sf(k) = 0.0
!          print*,'k,z(itilemid,k,jtilemid),sf(k)',k,z(itilemid,k,jtilemid),sf(k)
         END DO

        k_ws = 4

        sf(1) = 0.0
        sf(2) = 0.33
        sf(3) = 0.67

!       print*,'prtnk,k_wg,k_ws',prtnk,k_wg,k_ws

! CALCULATE WIND SPEEDS FOR  PERTURBATION MAGNITUDE =============================================

! 1) Compute sum of u and v corresponding to k_wg along all lateral boundaries. This will determine which are inflow  
! Something smarter will be needed for complex terrain.   

       uwsgpsum = 0.0
       vwsgpsum = 0.0

       IF ( its .EQ. ids ) THEN ! include points along ids inflow boundary

          DO j=j_start, j_end

             uwsgpsum = uwsgpsum + u(ids,k_wg,j)
             vwsgpsum = vwsgpsum + v(ids,k_wg,j)

          END DO

       ENDIF

       IF ( ite .EQ. ide ) THEN ! include points along ide inflow boundary

          DO j=j_start, j_end
      
             uwsgpsum = uwsgpsum + u(ide,k_wg,j)
             vwsgpsum = vwsgpsum + v(ide,k_wg,j)

          END DO

       ENDIF

       IF ( jts .EQ. jds ) THEN ! include points along jds inflow boundary

          DO i=i_start, i_end
      
             uwsgpsum = uwsgpsum + u(i,k_wg,jds)
             vwsgpsum = vwsgpsum + v(i,k_wg,jds)

          END DO

       ENDIF

       IF ( jte .EQ. jde ) THEN ! include points along jde inflow boundary

          DO i=i_start, i_end

             uwsgpsum = uwsgpsum + u(i,k_wg,jde)
             vwsgpsum = vwsgpsum + v(i,k_wg,jde)

          END DO

       ENDIF

! Obtain global sum of uwsspsum and uwgspsum 

#ifdef DM_PARALLEL

uwsgpsumdsum  = wrf_dm_sum_real(uwsgpsum)

vwsgpsumdsum  = wrf_dm_sum_real(vwsgpsum)

#endif
   
       north = 0
       east = 0
       south = 0
       west = 0

       IF ( uwsgpsumdsum .GT. 0.0 ) west = 1
       IF ( uwsgpsumdsum .LT. 0.0 ) east = 1
       IF ( vwsgpsumdsum .GT. 0.0 ) south = 1
       IF ( vwsgpsumdsum .LT. 0.0 ) north = 1
           
!       print*,'uwsgpsumdsum,vwsgpsumdsum',uwsgpsumdsum,vwsgpsumdsum
!       print*,'n, s, e, w',north,south,east,west     

!
! Uncomment to select your own inflow/outflow boundaries
!
!       north = 0
!       east = 0
!       south = 0
!       west = 1

       print*,'Applying perturbations to the following boundary(ies):'
       IF (north .EQ. 1) print*,'north'
       IF (south .EQ. 1) print*,'south'
       IF (east .EQ. 1) print*,'east'
       IF (west .EQ. 1) print*,'west'

       nsew = 0
       IF  ( (west + east) .EQ. 2) nsew = 1
       IF  ( (north + south) .EQ. 2) nsew = 1
       IF  ( (north + south + east + west) .EQ. 1) nsew = 1
       IF  ( (north + south + east + west) .EQ. 4) nsew = 1
       IF  ( (north + south + east + west) .EQ. 3) nsew = 0

! CALCULATE WIND SPEEDS AT INFLOW BOUNDARIES AT k=k_wg AND k=k_ws FOR PERTURBATION MAGNITUDE AND TIMESCALE =============================================

       wsspsum = 0.0
       wsgpsum = 0.0
       anglespsum = 0.0
       counter = 0.0
       num_pts_in_sum = 0.0

       IF ( west .EQ. 1 ) THEN

          IF ( its .EQ. ids ) THEN ! include points along ids inflow boundary

             DO j=j_start, j_end

                angles = atan( abs( u(ids,k_ws,j) )/abs( v(ids,k_ws,j) ) )
                
                IF (angles .GT. pio4 ) angles = pio2 - angles 

                anglespsum = anglespsum  + angles  

                wsspsum = wsspsum + sqrt( u(ids,k_ws,j)*u(ids,k_ws,j) + v(ids,k_ws,j)*v(ids,k_ws,j) )
                wsgpsum = wsgpsum + sqrt( u(ids,k_wg,j)*u(ids,k_wg,j) + v(ids,k_wg,j)*v(ids,k_wg,j) )

                num_pts_in_sum = num_pts_in_sum + 1.0

             END DO

          ENDIF

       ENDIF

       IF ( east .EQ. 1 ) THEN

          IF ( ite .EQ. ide ) THEN ! include points along ide inflow boundary

             DO j=j_start, j_end
      
                angles = atan( abs( u(ide,k_ws,j) )/abs( v(ide,k_ws,j) ) )
                
                IF (angles .GT. pio4 ) angles = pio2 - angles 

                anglespsum = anglespsum  + angles  

                wsspsum = wsspsum + sqrt( u(ide,k_ws,j)*u(ide,k_ws,j) + v(ide,k_ws,j)*v(ide,k_ws,j) )
                wsgpsum = wsgpsum + sqrt( u(ide,k_wg,j)*u(ide,k_wg,j) + v(ide,k_wg,j)*v(ide,k_wg,j) )

                num_pts_in_sum = num_pts_in_sum + 1.0

             END DO

          ENDIF

       ENDIF

       IF ( south .EQ. 1 ) THEN

          IF ( jts .EQ. jds ) THEN ! include points along jds inflow boundary

             DO i=i_start, i_end
      
                angles = atan( abs( u(i,k_ws,jds) )/abs( v(i,k_ws,jds) ) )

                IF (angles .GT. pio4 ) angles = pio2 - angles 

                anglespsum = anglespsum  + angles  

                wsspsum = wsspsum + sqrt( u(i,k_ws,jds)*u(i,k_ws,jds) + v(i,k_ws,jds)*v(i,k_ws,jds) )
                wsgpsum = wsgpsum + sqrt( u(i,k_wg,jds)*u(i,k_wg,jds) + v(i,k_wg,jds)*v(i,k_wg,jds) )

                num_pts_in_sum = num_pts_in_sum + 1.0

             END DO

          ENDIF

       ENDIF

       IF ( north .EQ. 1 ) THEN

          IF ( jte .EQ. jde ) THEN ! include points along jde inflow boundary

             DO i=i_start, i_end

                angles = atan( abs( u(i,k_ws,jde) )/abs( v(i,k_ws,jde) ) )
                
                IF (angles .GT. pio4 ) angles = pio2 - angles 

                anglespsum = anglespsum  + angles  

                wsspsum = wsspsum + sqrt( u(i,k_ws,jde)*u(i,k_ws,jde) + v(i,k_ws,jde)*v(i,k_ws,jde) )
                wsgpsum = wsgpsum + sqrt( u(i,k_wg,jde)*u(i,k_wg,jde) + v(i,k_wg,jde)*v(i,k_wg,jde) )

                num_pts_in_sum = num_pts_in_sum + 1.0

             END DO

          ENDIF

       ENDIF

! Obtain global sum of uwsspsum and total number of gridpoints contributing to the sum

#ifdef DM_PARALLEL

wsspsumdsum  = wrf_dm_sum_real(wsspsum)

wsgpsumdsum  = wrf_dm_sum_real(wsgpsum)

anglespsumdsum  = wrf_dm_sum_real(anglespsum)

counter = wrf_dm_sum_real(num_pts_in_sum)

#endif

       wsgpavgdavg = wsgpsumdsum/counter

       wsspavgdavg = wsspsumdsum/counter

       anglespavgdavg = anglespsumdsum/counter

!       print*,'its,wsspsum,wsspsumdsum,wsspavgdavg,counter ',its,wsspsum,wsspsumdsum,wsspavgdavg,counter
!       print*,'its,wsgpsum,wsgpsumdsum,wsgpavgdavg ',its,wsgpsum,wsgpsumdsum,wsgpavgdavg
     
       IF (wsspavgdavg .EQ. 0.0) THEN !Prevent division by zero

          print*,'something wrong in stoch_les_inflow_perts'
          STOP

       ENDIF

       tpertmag = (wsgpavgdavg*wsgpavgdavg)/(ek_opt*cp)
    
       print*,'tpertmag',tpertmag

       prtdt = 0.875*(1.0/cos(anglespavgdavg))*ngc_h*ncells_h*dx/wsspavgdavg  !angle corrects for mean wind direction
                                                                              !0.875 corrrects for ramping at inflow, 21/24

!       print*,'anglespavgdavg',anglespavgdavg*180.0/piconst

       print*,'prtdt',prtdt

  
! CALCULATE (NEW) PERTURBATIONS ===============================================================================

       ni = (ide-1)/ngc_h 
       nj = (jde-1)/ngc_h  

!       print*,'ni,nj',ni,nj
 
       ncells_v = kte-1 !prtnk/ngc_v + 1 --if using prtnk must ensure same value on each patch

! Allocate perturbation array for each lateral boundary 

        ALLOCATE( pxs(1:nj,1:ncells_v,1:ncells_h) )
        ALLOCATE( pxe(1:nj,1:ncells_v,1:ncells_h) )
        ALLOCATE( pys(1:ni,1:ncells_v,1:ncells_h) )
        ALLOCATE( pye(1:ni,1:ncells_v,1:ncells_h) )

       CALL RANDOM_SEED(size=i_seed)  ! 1. Get the size of the seed, each processor.

       ALLOCATE( seed(1:i_seed) )     ! 2. Allocate an array to hold the seed, each processor.

#ifdef DM_PARALLEL

       IF ( mytask .EQ. master ) THEN

          sum = 0                     ! 3a. Sum the stored seed (up to i_seed) to see if it has been assigned.
          DO k = 1,i_seed             !     If so, now master has the current prtseed value in seed.
             seed(k) = prtseed(k)
             sum = sum + seed(k)
          END DO

          IF (sum .EQ. 0 ) THEN       !3b. If the seed is empty, get the first seed, which is now only on master.

             print*,'calling random seed for first time'   
             CALL RANDOM_SEED(get=seed)

          ENDIF
 
       ENDIF 

       CALL MPI_BCAST(seed,i_seed,MPI_REAL,master,MPI_COMM_WORLD,ierr) ! Broadcast seed from master to all

!       print*,'seed',seed

       CALL RANDOM_SEED(put=seed) !Use same seed value so all processors receive idential random numbers

       CALL RANDOM_NUMBER(pxs)
       CALL RANDOM_NUMBER(pxe)
       CALL RANDOM_NUMBER(pys)
       CALL RANDOM_NUMBER(pye)

!       CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

       IF ( mytask .EQ. master ) THEN! Must get a new seed and store into prtseed so the sequence at the next perturbation update begins with new seed. 

          CALL RANDOM_SEED(get=seed)

             DO k = 1,i_seed
                prtseed(k) = seed(k)
             ENDDO!
  
       ENDIF

#endif

!       k_pert_start = 1
       k_pert_start = 2 ! jdm TEST 09142017
       k_pert_end = prtnk

       DO k = kts,kte  ! Zero out. Probably not necessary but good religion. 
          DO j=j_start, j_end
             DO i=i_start, i_end
                pert_t(i,k,j) = 0.0
             END DO
          END DO
       END DO

       IF ( west .EQ. 1 ) THEN

          IF (its .LE. ids + ngc_h*ncells_h) THEN 

             DO k = k_pert_start, k_pert_end
  
                n = (k-1)/ngc_v+1

                DO j = MAX(jts,jds + ngc_h*ncells_h*(west - south - nsew)), MIN(jte,jde - 1 - ngc_h*ncells_h*(west - north - nsew))

!                   print*,'j,((j-1)/ngc_h+1)',j,((j-1)/ngc_h+1)

                   DO i = its, MIN(ite, ide-1)

!                      print*,'i,((i-1)/ngc_h+1)',i,((i-1)/ngc_h+1)

                      m = (i-1)/ngc_h+1

                      IF ( m .LE. ncells_h ) THEN

                         pert_t(i,k,j) =  (pxs(((j-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag !Output variable only, for debugging.

!                         t(i,k,j) = t(i,k,j) + (pxs(((j-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

                      ENDIF

                   END DO ! i

                END DO ! j

             END DO ! k

          ENDIF

       ENDIF

       IF ( east .EQ. 1 ) THEN

          IF (ite .GE. ide - 1 - ngc_h*ncells_h) THEN 
  
             DO k = k_pert_start, k_pert_end

                n = (k-1)/ngc_v+1

                DO j = MAX(jts,jds + ngc_h*ncells_h*(east - south - nsew)), MIN(jte,jde - 1 - ngc_h*ncells_h*(east - north - nsew))

                   DO i = MIN(ite, ide-1), its, -1

                      m = (ide-i-1)/ngc_h+1
  
                      IF ( m .LE. ncells_h ) THEN

                         pert_t(i,k,j) = (pxe(((j-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

!                         t(i,k,j) = t(i,k,j) + (pxe(((j-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

                      ENDIF

                   END DO ! i

                END DO ! j

             END DO ! k

          ENDIF 

       ENDIF

       IF ( south .EQ. 1 ) THEN

          IF (jts .LE. jds + ngc_h*ncells_h) THEN 

             DO k = k_pert_start, k_pert_end

                n = (k-1)/ngc_v+1

                DO i = MAX(its,ids + ngc_h*ncells_h*(south - west - nsew)), MIN(ite,ide - 1 - ngc_h*ncells_h*(south - east - nsew))

                   DO j = jts, MIN(jte, jde-1)

                      m = (j-1)/ngc_h+1

                      IF ( m .LE. ncells_h ) THEN    

!    print*,'i,(i-1)/ngc_h+1,j,m,pys',i,(i-1)/ngc_h+1,j,m
!    print*,'k,n,pys',k,n,pys(((i-1)/ngc_h+1),n,m)

                        pert_t(i,k,j) = (pys(((i-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

!                         t(i,k,j) = t(i,k,j) + (pys(((i-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

                      ENDIF

                   END DO ! i

                END DO ! j

             END DO ! k

          ENDIF

       ENDIF

       IF ( north .EQ. 1 ) THEN

          IF (jte .GE. jde - 1 - ngc_h*ncells_h) THEN 

             DO k = k_pert_start, k_pert_end

                n = (k-1)/ngc_v+1

                DO i = MAX(its,ids + ngc_h*ncells_h*(north - west - nsew)), MIN(ite,ide - 1 - ngc_h*ncells_h*(north - east - nsew))

                   DO j = MIN(jte, jde-1),jts,-1

                      m = (jde-j-1)/ngc_h+1

                      IF ( m .LE. ncells_h ) THEN
  
                         pert_t(i,k,j) = (pye(((i-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

!                         t(i,k,j) = t(i,k,j) + (pye(((i-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

                      ENDIF

                   END DO ! i

                END DO ! j

             END DO ! k

          ENDIF

       ENDIF

       DEALLOCATE(seed)

       DEALLOCATE( pxs )
       DEALLOCATE( pxe )
       DEALLOCATE( pys )
       DEALLOCATE( pye )

    ENDIF ! (prttms .GT. prdt)


    DO j=j_start, j_end

       DO i=i_start, i_end

          DO k=kts+1,kte-1  

             ru_tendf(i,k,j) = ru_tendf(i,k,j) + 0.01*pert_t(i,k,j)*muu(i,j)
             rv_tendf(i,k,j) = rv_tendf(i,k,j) + 0.01*pert_t(i,k,j)*muv(i,j)

          END DO ! k

       END DO ! j

    END DO ! i



!=================================================================================

END SUBROUTINE calc_pert_uv

!=======================================================================
!=======================================================================
!=======================================================================

    END MODULE module_stoch_les_inflow_perts

!=======================================================================
!=======================================================================
