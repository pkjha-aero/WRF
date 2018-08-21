

MODULE module_sfs_drm

  IMPLICIT NONE

  CONTAINS

!=======================================================================

SUBROUTINE drm_filter( varfilt, var, vars, variable, ifilt, &
                        ids, ide, jds, jde, kds, kde,        &
                        ims, ime, jms, jme, kms, kme,        &
                        ips, ipe, jps, jpe, kps, kpe,        &
                        its, ite, jts, jte, kts, kte         )

!-----------------------------------------------------------------------
! PURPOSE: apply 3-D tophat filters of width 2*<dx,dy,dz> to var.
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: varfilt       !Filtered variable

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var           !input variable

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( IN  ) &
  :: vars          !surface value

  CHARACTER(LEN=2),                                      INTENT( IN  ) &
  :: variable

  INTEGER,                                               INTENT( IN  ) &
  :: ifilt

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  REAL, DIMENSION( its-1:ite+1, kts-1:kte+1, jts-1:jte+1 ) &
  :: fx         &  !variable filtered in x direction
   , fxy        &  !variable filtered in x and y directions
   , vart          !Temporary variable

  REAL :: a, b, c, du

  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend

!-----------------------------------------------------------------------

!Filter function coefficients

  IF ( ifilt .EQ. 1 ) THEN      !tophat with trapezoidal rule
    a = 0.25
    b = 0.5
    c = 0.25
  ELSEIF ( ifilt .EQ. 2 ) THEN  !tophat with Simpson's rule
    a = 1.0/6.0
    b = 4.0/6.0
    c = 1.0/6.0
  ELSEIF ( ifilt .EQ. 3 ) THEN !tophat test filter (trapezoidal rule)
    a = 0.5                    !applied after an explicit filter 
    b = 0.0                    !(ifilt=1,2), gives tophat of width 4*dx
    c = 0.5
  ELSE
    WRITE(*,*) 'Invalid filter type, ifilt=', ifilt
    STOP  
  ENDIF

  SELECT CASE (variable)
!_______________________________________________________________________

    CASE ('uu') !u or ufilt, centered in z..............................

      kbgn = kts-1
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = vart(i,kend-1,j)                   ! TOP: NVG
!         vart(i,kbgn,j) = - vart(i,kbgn+1,j)                 ! SFC: P0
!         vart(i,kbgn,j) =   vart(i,kbgn+1,j)                 ! SFC: NVG
        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)   ! SFC: KBC
      ENDDO
      ENDDO
!_______________________________________________________________________

    CASE ('vv') !v or vfilt, centered in z. ............................

      kbgn = kts-1
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) =  vart(i,kend-1,j)                  ! TOP: NVG
!         vart(i,kbgn,j) = - vart(i,kbgn+1,j)                 ! SFC: P0
!         vart(i,kbgn,j) =   vart(i,kbgn+1,j)                 ! SFC: NVG
        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)   ! SFC: KBC
      ENDDO
      ENDDO
!_______________________________________________________________________

    CASE ('ww') !w or wfilt at w-nodes, centered in xy, staggered in z..

      kbgn = kts
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = 0.0                                ! TOP: 0
!         vart(i,kbgn,j) = - vart(i,kbgn+1,j)                 ! SFC: P0
!         vart(i,kbgn,j) =   vart(i,kbgn+1,j)                 ! SFC: NVG
          vart(i,kbgn,j) = vars(i,j)                          ! SFC: KBC
!        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+2,j)   ! SFC: KBC
      END DO
      END DO

!_______________________________________________________________________

    CASE ('cw') ! w at cell center, centered in z.................

      kbgn = kts-1
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = - vart(i,kend-1,j)                 ! TOP: P -> 0
!         vart(i,kbgn,j) = - vart(i,kbgn+1,j)                 ! SFC: P 0
!         vart(i,kbgn,j) =   vart(i,kbgn+1,j)                 ! SFC: NVG
        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)   ! SFC: KBC
      ENDDO
      ENDDO
!_______________________________________________________________________

    CASE ('cc') ! variable at cell center, centered in z................

      kbgn = kts-1
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = vart(i,kend-1,j)                   ! TOP: NVG
!         vart(i,kbgn,j) = - vart(i,kbgn+1,j)                 ! SFC: P 0
!         vart(i,kbgn,j) =   vart(i,kbgn+1,j)                 ! SFC: NVG
        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)   ! SFC: KBC
      ENDDO
      ENDDO
!_______________________________________________________________________

    CASE ('dd') ! -12- corner point, centered in z......................

      kbgn = kts-1
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = vart(i,kend-1,j)                   ! TOP: NVG
!          vart(i,kbgn,j) = - vart(i,kbgn+1,j)                ! SFC: P0
!          vart(i,kbgn,j) =   vart(i,kbgn+1,j)                ! SFC: NVG
        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)   ! SFC: KBC
      END DO
      END DO
!_______________________________________________________________________

    CASE ('ee') ! -13- corner point, staggered in z....................

      kbgn = kts
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = vart(i,kend-1,j)                   ! TOP: NVG
!         vart(i,kbgn,j) = - vart(i,kbgn+1,j)                 ! SFC: P0
!         vart(i,kbgn,j) =   vart(i,kbgn+1,j)                 ! SFC: NVG
        vart(i,kbgn,j) = vars(i,j)                          ! SFC: KBC
      END DO
      END DO

!_______________________________________________________________________

    CASE ('ew') ! -13- corner point, staggered in z....................

      kbgn = kts
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = 0.0                                ! TOP: 0
!         vart(i,kbgn,j) = - vart(i,kbgn+1,j)                 ! SFC: P0
!         vart(i,kbgn,j) =   vart(i,kbgn+1,j)                 ! SFC: NVG
        vart(i,kbgn,j) = vars(i,j)                          ! SFC: KBC
      END DO
      END DO

!_______________________________________________________________________

    CASE ('ff') ! -23- corner point, staggered in z.....................

      kbgn = kts
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = vart(i,kend-1,j)                   ! TOP: NVG
!         vart(i,kbgn,j) = - vart(i,kbgn+1,j)                 ! SFC: P0
!         vart(i,kbgn,j) =   vart(i,kbgn+1,j)                 ! SFC: NVG
        vart(i,kbgn,j) = vars(i,j)                          ! SFC: KBC
      END DO
      END DO

!_______________________________________________________________________

    CASE ('fw') ! -23- corner point, staggered in z.....................

      kbgn = kts
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = 0.0                                ! TOP: 0
!         vart(i,kbgn,j) = - vart(i,kbgn+1,j)                 ! SFC: P0
!         vart(i,kbgn,j) =   vart(i,kbgn+1,j)                 ! SFC: NVG
        vart(i,kbgn,j) = vars(i,j)                          ! SFC: KBC
      END DO
      END DO
!_______________________________________________________________________


    CASE ('sh') ! s11,s22,s33,s12 at cell center, centered in z.........

      kbgn = kts-1    
      kend = MIN( kte, kde ) 

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
!        vart(i,kend,j) = - vart(i,kend-1,j)                ! TOP: P0
        vart(i,kend,j) =  vart(i,kend-1,j)                    ! TOP: NVG
!        vart(i,kbgn,j) = - vart(i,kbgn+1,j)                  ! SFC: P0
        vart(i,kbgn,j) = vart(i,kbgn+1,j)                  ! SFC: NVG
      ENDDO
      ENDDO
!_______________________________________________________________________

    CASE ('sv') ! s13,s23, at cell center, centered in z.........

      kbgn = kts-1    
      kend = MIN( kte, kde ) 

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = - vart(i,kend-1,j)                  ! TOP: P0
!        vart(i,kend,j) = - vart(i,kend-1,j)                  ! TOP: NVG
!        vart(i,kbgn,j) = - vart(i,kbgn+1,j)                  ! SFC: P0
        vart(i,kbgn,j) = vart(i,kbgn+1,j)                  ! SFC: NVG
      ENDDO
      ENDDO
!_______________________________________________________________________

    CASE ('tc') ! tau at cell center, centered in z.....................

      kbgn = kts-1    
      kend = MIN( kte, kde ) 

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = - vart(i,kend-1,j)                  ! TOP: P0
!        vart(i,kend,j) =   vart(i,kend-1,j)                  ! TOP: NVG
!        vart(i,kbgn,j) = - vart(i,kbgn+1,j)                  ! SFC: P0
        vart(i,kbgn,j) = vart(i,kbgn+1,j)                  ! SFC: NVG
      ENDDO
      ENDDO
!_______________________________________________________________________

    CASE ('td') ! tau at -12- corner, centered in z..................... 

      kbgn = kts-1
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = - vart(i,kend-1,j)                  ! TOP: P0
!        vart(i,kend,j) =   vart(i,kend-1,j)                  ! TOP: NVG
!        vart(i,kbgn,j) = - vart(i,kbgn+1,j)                  ! SFC: P0
        vart(i,kbgn,j) = vart(i,kbgn+1,j)                    ! SFC: NVG
      ENDDO
      ENDDO
!_______________________________________________________________________

    CASE ('te') ! tau at -13- corner, staggered in z.................... 

      kbgn = kts
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = 0.0                                 ! TOP
!        vart(i,kbgn,j) = 0.0                                ! SFC
        vart(i,kbgn,j) = vart(i,kbgn+1,j) 
      ENDDO
      ENDDO
!_______________________________________________________________________

    CASE ('tf') ! tau at -23- corner, staggered in z....................

      kbgn = kts
      kend = MIN( kte, kde )  

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO   
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = 0.0                                 ! TOP 
!        vart(i,kbgn,j) = 0.0                                 ! SFC
        vart(i,kbgn,j) = vart(i,kbgn+1,j) 
      ENDDO
      ENDDO
!_______________________________________________________________________

    CASE ('kc') ! tau at cell center, centered in z.....................

      kbgn = kts-1    
      kend = MIN( kte, kde ) 

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
!        vart(i,kend,j) = - vart(i,kend-1,j)                  ! TOP: P0
        vart(i,kend,j) =   vart(i,kend-1,j)                  ! TOP: NVG
        vart(i,kbgn,j) =  vart(i,kbgn+1,j)                 
      ENDDO
      ENDDO
!_______________________________________________________________________


    CASE DEFAULT
 
      WRITE(*,*)'Error-improperly specified variable in'
      WRITE(*,*)'dyn_em/module_reconstruct.F/subroutine drm_filter'
      STOP

  END SELECT

!-----------------------------------------------------------------------

! Filter in x-direction
  DO j = jts-1, jte+1
  DO k = kbgn, kend
  DO i = its, ite
    fx(i,k,j) = a*vart(i+1,k,j) + b*vart(i,k,j) + c*vart(i-1,k,j)
  ENDDO
  ENDDO
  ENDDO

! Filter in y-direction
  DO j = jts, jte
  DO k = kbgn, kend
  DO i = its, ite
    fxy(i,k,j) = a*fx(i,k,j+1) + b*fx(i,k,j) + c*fx(i,k,j-1)
  END DO
  END DO
  ENDDO

! Filter in z-direction
  DO j = jts, jte
  DO k = kbgn+1, kend-1
  DO i = its, ite
    varfilt(i,k,j) = a*fxy(i,k+1,j) + b*fxy(i,k,j) + c*fxy(i,k-1,j)
  ENDDO
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE drm_filter

!=======================================================================

SUBROUTINE uvw_s_c( usc, vsc, ws,                 &
                    uusc, vvsc, wws,              &
                    usd, vsd, uvsd,               &
                    wse, uwse,                    &
                    wsf, vwsf,                    &
                    u,                            &
                    v,                            &
                    w,                            &
                    uu,                           &
                    vv,                           &
                    ww,                           &
                    ud,                           &
                    vd,                           &
                    uvd,                          &
                    we,                           &
                    uwe,                          &
                    wf,                           &
                    vwf,                          &
                    ht, rdx, rdy,                 &
                    cf1, cf2, cf3,                &
                    rsfsvsbc,                     &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    ips, ipe, jps, jpe, kps, kpe, &
                    its, ite, jts, jte, kts, kte  )

!-----------------------------------------------------------------------
! PURPOSE: This subroutine calculates the value of the velocities
!          at the surface that are required prior to test filtering
!          the velocities that have already been projected to stress nodes.
!          ws = us*dh/dx + vs*dh/dy. us and vs are u and v at the 
!          surface. Here we use u_s = u(i,1,j) and v_s = v(i,1,j)
!          rather than the polynomial extrapolation function
!          that is used elsewhere. We found that the polynomial 
!          extrapolation can lead to goofy surface values of u and 
!          v when near-surface vertical gradients are large, such as 
!          at high resolution or in the presence of steep terrain.
!          This doesn't appear to be a problem elsewhere, however here  
!          it can lead to anomalously large near-surface RSFS stresses.
!          Using u_s = u(i,1,j) and v_s = v(i,1,j) fixes the problem.
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( OUT ) &
  :: usc         & ! surface u                           (m s^-1)
   , vsc         & ! surface v                           (m s^-1)
   , ws          & ! surface w                           (m s^-1)
   , uusc        & !
   , vvsc        & !
   , wws         & !                
   , usd         & !
   , vsd         & !
   , uvsd        & !
   , wse         & !
   , uwse        & !
   , wsf         & !
   , vwsf          !

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: u           &
   , v           &
   , w           &
   , uu          &
   , vv          &
   , ww          &
   , ud          &
   , vd          &
   , uvd         &
   , we          &
   , uwe         &
   , wf          &
   , vwf

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( IN  ) &
  :: ht            !terrain height                       (m)

  INTEGER,                                               INTENT( IN  ) &
  :: rsfsvsbc

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

  REAL,                                                  INTENT( IN  ) &
  :: rdx         & ! inverse grid spacing in x           (m^-1)
    ,rdy         & ! inverse grid spacing in y           (m^-1)
    ,cf1         & !
    ,cf2         & !
    ,cf3           ! 
 
! LOCAL VARIABLES

  INTEGER:: i, j

!-----------------------------------------------------------------------

  IF ( rsfsvsbc .EQ. 0 ) THEN !STANDARD WRF EXTRAPOLATION 

    DO j=jts, jte ! project u and v to surface
    DO i=its, ite

      usc(i,j) = cf1*u(i,1,j)   + cf2*u(i,2,j)   + cf3*u(i,3,j) 
      vsc(i,j) = cf1*v(i,1,j)   + cf2*v(i,2,j)   + cf3*v(i,3,j)
      ws(i,j)  = cf1*w(i,1,j)   + cf2*w(i,2,j)   + cf3*w(i,3,j)
      uusc(i,j)= cf1*uu(i,1,j)  + cf2*uu(i,2,j)  + cf3*uu(i,3,j)
      vvsc(i,j)= cf1*vv(i,1,j)  + cf2*vv(i,2,j)  + cf3*vv(i,3,j)
      wws(i,j) = cf1*ww(i,1,j)  + cf2*ww(i,2,j)  + cf3*ww(i,3,j)
      usd(i,j) = cf1*ud(i,1,j)  + cf2*ud(i,2,j)  + cf3*ud(i,3,j) 
      vsd(i,j) = cf1*vd(i,1,j)  + cf2*vd(i,2,j)  + cf3*vd(i,3,j) 
      uvsd(i,j)= cf1*uvd(i,1,j) + cf2*uvd(i,2,j) + cf3*uvd(i,3,j) 
      wse(i,j) = cf1*we(i,1,j)  + cf2*we(i,2,j)  + cf3*we(i,3,j)
      uwse(i,j)= cf1*uwe(i,1,j) + cf2*uwe(i,2,j) + cf3*uwe(i,3,j)
      wsf(i,j) = cf1*wf(i,1,j)  + cf2*wf(i,2,j)  + cf3*wf(i,3,j)
      vwsf(i,j)= cf1*vwf(i,1,j) + cf2*vwf(i,2,j) + cf3*vwf(i,3,j)

    ENDDO
    ENDDO


  ELSEIF ( rsfsvsbc .EQ. 1 ) THEN !ZERO VERTICAL GRADIENT

  DO j=jts, jte
  DO i=its, ite

    usc(i,j) = u(i,1,j)
    vsc(i,j) = v(i,1,j)
    ws(i,j)  = w(i,1,j)
    uusc(i,j)= uu(i,1,j)
    vvsc(i,j)= vv(i,1,j)
    wws(i,j) = ww(i,1,j)
    usd(i,j) = ud(i,1,j)
    vsd(i,j) = vd(i,1,j)
    uvsd(i,j)= uvd(i,1,j)
    wse(i,j) = we(i,1,j)
    uwse(i,j)= uwe(i,1,j)
    wsf(i,j) = wf(i,1,j)
    vwsf(i,j)= vwf(i,1,j)

  ENDDO
  ENDDO


  ELSEIF ( rsfsvsbc .EQ. 2 ) THEN !NO SLIP

    DO j=jts, jte
    DO i=its, ite

      usc(i,j) = 0.0
      vsc(i,j) = 0.0   
      ws(i,j)  = 0.0  
      uusc(i,j)= 0.0
      vvsc(i,j)= 0.0
      wws(i,j) = 0.0  
      usd(i,j) = 0.0
      vsd(i,j) = 0.0
      uvsd(i,j)= 0.0
      wse(i,j) = 0.0
      uwse(i,j)= 0.0
      wsf(i,j) = 0.0
      vwsf(i,j)= 0.0

    ENDDO
    ENDDO

  ENDIF

  RETURN

END SUBROUTINE uvw_s_c


!=======================================================================

SUBROUTINE hij( h11, h22, h33, h12, h13, h23, &
                tgrtau11, tgrtau22, tgrtau33,    &
                tgrtau12, tgrtau13, tgrtau23,    &
                tfrtau11, tfrtau22, tfrtau33, &
                tfrtau12, tfrtau13, tfrtau23, &
                ids, ide, jds, jde, kds, kde, & 
                ims, ime, jms, jme, kms, kme, & 
                ips, ipe, jps, jpe, kps, kpe, & 
                its, ite, jts, jte, kts, kte  )  

!-----------------------------------------------------------------------
! PURPOSE: Calculate Hij 
!-----------------------------------------------------------------------

  IMPLICIT NONE                                               

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &   
  :: h11         & !
   , h22         & !
   , h33         & !
   , h12         & !
   , h13         & !
   , h23           !   

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &   
  :: tgrtau11    & !
   , tgrtau22    & !
   , tgrtau33    & !
   , tgrtau12    & !
   , tgrtau13    & !
   , tgrtau23    & !
   , tfrtau11    & !
   , tfrtau22    & !
   , tfrtau33    & !
   , tfrtau12    & !
   , tfrtau13    & !
   , tfrtau23      !

  INTEGER,                                               INTENT( IN  ) & 
  :: ids, ide, jds, jde, kds, kde, &                              
     ims, ime, jms, jme, kms, kme, &                               
     ips, ipe, jps, jpe, kps, kpe, &                              
     its, ite, jts, jte, kts, kte       

! LOCAL VARIABLES

  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend

!-----------------------------------------------------------------------

  kbgn = kts
  kend = MIN( kte, kde )-1 

  DO j = jts, jte
  DO k = kbgn, kend 
  DO i = its, ite
    h11(i,k,j) = tgrtau11(i,k,j) - tfrtau11(i,k,j)
    h22(i,k,j) = tgrtau22(i,k,j) - tfrtau22(i,k,j)
    h33(i,k,j) = tgrtau33(i,k,j) - tfrtau33(i,k,j)
    h12(i,k,j) = tgrtau12(i,k,j) - tfrtau12(i,k,j)
    h13(i,k,j) = tgrtau13(i,k,j) - tfrtau13(i,k,j)
    h23(i,k,j) = tgrtau23(i,k,j) - tfrtau23(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  DO j = jts, jte
  DO i = its, ite
    h13(i,kts,j) = h13(i,kts+1,j)
    h23(i,kts,j) = h23(i,kts+1,j)
!    h13(i,kts,j) = 0.0
!    h23(i,kts,j) = 0.0
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE hij

!=======================================================================

SUBROUTINE product( prod, var1, var2,             & 
                    ids, ide, jds, jde, kds, kde, & 
                    ims, ime, jms, jme, kms, kme, & 
                    ips, ipe, jps, jpe, kps, kpe, & 
                    its, ite, jts, jte, kts, kte  ) 

!-----------------------------------------------------------------------
!  PURPOSE: Compute the product of var1 and var2
!-----------------------------------------------------------------------

  IMPLICIT NONE                      

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &   
  :: prod  
   
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &    
  :: var1        &         
   , var2
 
  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &                              
     ims, ime, jms, jme, kms, kme, &                               
     ips, ipe, jps, jpe, kps, kpe, &                              
     its, ite, jts, jte, kts, kte                      
       
! LOCAL VARIABLES
    
  INTEGER :: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend

!-----------------------------------------------------------------------
   
  kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts, kend
  DO i = its, ite
    prod(i,k,j) = var1(i,k,j)*var2(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE product

!======================================================================

SUBROUTINE drmnd( drmn, drmd,                   &
                  h11, h22, h33,                &
                  h12, h13, h23,                &
                  tfs11, tfs22, tfs33,          &
                  tfs12, tfs13, tfs23,          &
                  tfu, tfv, tfw,                &
                  tfuu, tfvv, tfww,             &
                  tfuv, tfuw, tfvw,             &
                  drm_opt,                      &
                  ids, ide, jds, jde, kds, kde, & 
                  ims, ime, jms, jme, kms, kme, & 
                  ips, ipe, jps, jpe, kps, kpe, & 
                  its, ite, jts, jte, kts, kte  )  

!-----------------------------------------------------------------------
! PURPOSE: Calculte numerator and denominator of dynamic wong-lilly
!          eddy diffusivity coefficients
!-----------------------------------------------------------------------

  IMPLICIT NONE  
                                             
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &   
  :: drmn        & !
   , drmd      
 
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &   
  :: h11         & !
   , h22         & !
   , h33         & !  
   , h12         & !
   , h13         & !
   , h23         & ! 
   , tfs11       & !
   , tfs22       & !
   , tfs33       & !
   , tfs12       & !
   , tfs13       & !
   , tfs23       & !     
   , tfu         & !
   , tfv         & !
   , tfw         & !                                
   , tfuu        & !
   , tfvv        & !
   , tfww        & !                              
   , tfuv        & !
   , tfuw        & !
   , tfvw          !  

  INTEGER,                                               INTENT( IN  ) & 
  :: drm_opt

  INTEGER,                                               INTENT( IN  ) & 
  :: ids, ide, jds, jde, kds, kde, &                              
     ims, ime, jms, jme, kms, kme, &                               
     ips, ipe, jps, jpe, kps, kpe, &                              
     its, ite, jts, jte, kts, kte       

! LOCAL VARIABLES

  REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ) &   
  :: l11         & !
   , l22         & !
   , l33         & !
   , l12         & !
   , l13         & !
   , l23         & !   
   , st11        & !
   , st22        & !
   , st33        & !
   , st12        & !
   , st13        & !
   , st23          !      

  REAL :: ddiff, tem, temx        

  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend

!-----------------------------------------------------------------------

  kbgn = kts
  kend = MIN( kte, kde )-1  

! Choose the coefficient used later which involves the grid scale chosen
  ddiff = 1.0 - 2.0**(4.0/3.0)  ! this is (1-(Delta_filter/Delta_grid)^(4/3))
			        ! assuming Delta_filter=2*Delta_grid

IF (drm_opt .EQ. 1) THEN

  DO j = jts, jte 
  DO k = kbgn, kend 
  DO i = its, ite

    l11(i,k,j) = tfuu(i,k,j) - tfu(i,k,j)*tfu(i,k,j) - h11(i,k,j)
    l22(i,k,j) = tfvv(i,k,j) - tfv(i,k,j)*tfv(i,k,j) - h22(i,k,j)
    l33(i,k,j) = tfww(i,k,j) - tfw(i,k,j)*tfw(i,k,j) - h33(i,k,j)
    l12(i,k,j) = tfuv(i,k,j) - tfu(i,k,j)*tfv(i,k,j) - h12(i,k,j)
    l13(i,k,j) = tfuw(i,k,j) - tfu(i,k,j)*tfw(i,k,j) - h13(i,k,j)
    l23(i,k,j) = tfvw(i,k,j) - tfv(i,k,j)*tfw(i,k,j) - h23(i,k,j)

    st11(i,k,j) = ddiff*tfs11(i,k,j)
    st22(i,k,j) = ddiff*tfs22(i,k,j)
    st33(i,k,j) = ddiff*tfs33(i,k,j)
    st12(i,k,j) = ddiff*tfs12(i,k,j)
    st13(i,k,j) = ddiff*tfs13(i,k,j)
    st23(i,k,j) = ddiff*tfs23(i,k,j)

  ENDDO
  ENDDO
  ENDDO

ELSE

  DO j = jts, jte 
  DO k = kbgn, kend 
  DO i = its, ite

    l11(i,k,j) = tfuu(i,k,j) - tfu(i,k,j)*tfu(i,k,j) 
    l22(i,k,j) = tfvv(i,k,j) - tfv(i,k,j)*tfv(i,k,j) 
    l33(i,k,j) = tfww(i,k,j) - tfw(i,k,j)*tfw(i,k,j) 
    l12(i,k,j) = tfuv(i,k,j) - tfu(i,k,j)*tfv(i,k,j) 
    l13(i,k,j) = tfuw(i,k,j) - tfu(i,k,j)*tfw(i,k,j) 
    l23(i,k,j) = tfvw(i,k,j) - tfv(i,k,j)*tfw(i,k,j) 

    st11(i,k,j) = ddiff*tfs11(i,k,j)
    st22(i,k,j) = ddiff*tfs22(i,k,j)
    st33(i,k,j) = ddiff*tfs33(i,k,j)
    st12(i,k,j) = ddiff*tfs12(i,k,j)
    st13(i,k,j) = ddiff*tfs13(i,k,j)
    st23(i,k,j) = ddiff*tfs23(i,k,j)

  ENDDO
  ENDDO
  ENDDO

ENDIF

  DO j = jts, jte 
  DO k = kbgn, kend 
  DO i = its, ite

    ! tem = 1/3*Trace(lij), temx = 1/3*Trace(stij)
    tem  = ( l11(i,k,j) + l22(i,k,j) + l33(i,k,j) )/3.0
    temx = ( st11(i,k,j) + st22(i,k,j) + st33(i,k,j) )/3.0

    ! numerator = Sij*Mij
    drmn(i,k,j) = ( l11(i,k,j) - tem )*( tfs11(i,k,j) - temx ) +  &
                  ( l22(i,k,j) - tem )*( tfs22(i,k,j) - temx ) +  &
                  ( l33(i,k,j) - tem )*( tfs33(i,k,j) - temx ) +  &
                  2.0*( l12(i,k,j)*st12(i,k,j) +                  &
                  l13(i,k,j)*st13(i,k,j) + l23(i,k,j)*st23(i,k,j) )

    ! denominator = Mij*Mij
    drmd(i,k,j) = ( st11(i,k,j) - temx )**2 +      &
                  ( st22(i,k,j) - temx )**2 +      &
                  ( st33(i,k,j) - temx )**2 +      &
                  2.0*( st12(i,k,j)**2 +         &
                  st13(i,k,j)**2 + st23(i,k,j)**2 )

!  if ((i.eq.its+5).and.(j.eq.jts+5)) print *, 'k=', k, 'drmn=', drmn(i,k,j), 'drmd=', drmd(i,k,j)

  ENDDO
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE drmnd


!=======================================================================

SUBROUTINE drmevc( xkmv, xkhv, xkmh, xkhh,       &
                   drmn, drmd,                   &      
                   ids, ide, jds, jde, kds, kde, & 
                   ims, ime, jms, jme, kms, kme, & 
                   ips, ipe, jps, jpe, kps, kpe, & 
                   its, ite, jts, jte, kts, kte  )  

!-----------------------------------------------------------------------
! PURPOSE: Calculate dynamic wong-lilly eddy diffusivity coefficients
!-----------------------------------------------------------------------

  IMPLICIT NONE                                               
      
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT(INOUT) &   
  :: xkmv        & ! vertical momentum v.c.
   , xkhv        & ! vertical scalar v.c.
   , xkmh        & ! horizontal momentum v.c.
   , xkhh          ! horizontal scalar v.c.
  
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &   
  :: drmn        & ! numerator of drm v.c.
   , drmd          ! denominator of drm v.c.
  
  INTEGER,                                               INTENT( IN  ) & 
  :: ids, ide, jds, jde, kds, kde, &                              
     ims, ime, jms, jme, kms, kme, &                               
     ips, ipe, jps, jpe, kps, kpe, &                              
     its, ite, jts, jte, kts, kte       

! LOCAL VARIABLES

  REAL :: eps, tem, pr

  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend

!-----------------------------------------------------------------------

!  DO j = jts-1,jte+1  
!  DO k = kts, kte  
!  DO i = its-1,ite+1   
!    xkmv(i,k,j)=0.0 !old xkmv values on (ide,jde)+1 if using periodic B.C.
!    xkmhd(i,k,j)=0.0 
!    xkhv(i,k,j)=0.0 
!    xkhh(i,k,j)=0.0  
!  END DO
!  END DO
!  END DO

  eps=1.e-10
  pr=1.0/3.0

  kbgn = kts
  kend = MIN( kte, kde )-1  

  DO j = jts, jte 
  DO k = kbgn, kend 
  DO i = its, ite
    tem = SIGN(eps,drmd(i,k,j))
    xkmv(i,k,j)= drmn(i,k,j)/(2.0*drmd(i,k,j)+tem)
    xkmv(i,k,j)=MAX( xkmv(i,k,j), -1.5E-5)
!    xkmv(i,k,j)=MAX( xkmv(i,k,j), 0.0)
    xkmh(i,k,j)=xkmv(i,k,j)
    xkhv(i,k,j)=xkmv(i,k,j)/pr
    xkhh(i,k,j)=xkhv(i,k,j)

!      if ((i.eq.its+5).and.(j.eq.jts+5)) print *, 'k=', k, 'drmn=', drmn(i,k,j), 'drmd=', drmd(i,k,j) 
!      if ((i.eq.its+5).and.(j.eq.jts+5)) print *, 'k=', k, 'B=', xkmh(i,k,j)
  
  ENDDO
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE drmevc


!=======================================================================

SUBROUTINE drm_nw_canopy( nwtau13, nwtau23,             &
                          u, v,                         &
                          w, rdz,                       &
                          rdzw, dx,                     &
                          fnm, fnp,                     &
                          canfact,                      &
                          ust,                          &
                          ids, ide, jds, jde, kds, kde, & 
                          ims, ime, jms, jme, kms, kme, & 
                          ips, ipe, jps, jpe, kps, kpe, & 
                          its, ite, jts, jte, kts, kte  )  

!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Calculate the canopy model to supplement the series expansion 
!  model near the lower wall. Used if tmixopt=8.
!
!-----------------------------------------------------------------------
!
!  AUTHOR: Tina Katopodes
!  10/10/2001
!
!  MODIFICATION HISTORY:
!  11/01/2001 change height at which canopy is first applied
!
!  JDM 11/2007-Implementation into WRF.
!  The surface momentum fluxes, usflx and vsflx, are computed exactly
!  as in dyn_em/module_diffusion.F subroutine vertical_diffusion_2.
!  The canopy stress profile is computed from usflx and vsflx and then 
!  added to the rsfs stress terms rtau13 and rtau23 from kts+1 to kend.
!  Also I made sure that all quantities are now projected to the correct 
!  nodes within the gridcell.
!-----------------------------------------------------------------------
  
  IMPLICIT NONE                                         

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT(OUT) &    
  :: nwtau13    & ! rsfs uw stress                        (m^2/s^2)
   , nwtau23      ! rsfs vw stress                        (m^2/s^2)

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT(IN) &    
  :: ust      ! ustar                                     (m/s)


  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &    
  :: u         & ! u-velocity                            (m/s)
   , v         & ! v-velocity                            (m/s)
   , w         & ! w-velocity                            (m/s)        
   , rdz       & ! inverse delta-z between midpoints     (m)
   , rdzw        ! inverse delta-z between interfaces    (m)    

  REAL, DIMENSION( kms:kme ),                            INTENT( IN  ) &
  :: fnm         &  
   , fnp

  REAL,                                                  INTENT( IN  ) &   
  :: dx          & ! Grid spacing in the x-direction     (m)
   , canfact       !
      
  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &                             
     ims, ime, jms, jme, kms, kme, &                               
     ips, ipe, jps, jpe, kps, kpe, &                              
     its, ite, jts, jte, kts, kte       

! LOCAL VARIABLES

  REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ) &
  :: u_canforce & ! Canopy force in u-direction
   , v_canforce & ! Canopy force in v-direction
   , a          & ! Canopy profile shape function
   , zw         & ! height at interface levels          
   , z            ! height at midpoint levels   

  REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ) &
  :: u_can       & ! Canopy force in u-direction
   , v_can       &
   , u_canopy    &
   , v_canopy 

  REAL, DIMENSION( its-1:ite+1, jts-1:jte+1 ) &   
  :: usflx     & ! Surface stress in u-direction 
   , vsflx     & ! Surface stress in v-direction 
   , C0        & ! Canopy model integration constant - uw
   , D0        & ! Canopy model integration constant - vw
   , kcan      & ! Canopy model height index
   , a0        & ! Temporary work array 
   , b0          ! Temporary work array

  REAL, DIMENSION( its-1:ite+1, jts-1:jte+1) &  
  :: Cd_u      & ! Coefficient of drag in u-direction
   , Cd_v        ! Coefficient of drag in v-direction

  REAL         &  
  :: PI        & ! pi
   , g0        & ! gravity
   , Hc        & ! Canopy height
   , V0_u      & ! Scalar wind speed at u-points
   , V0_v      & ! Scalar wind speed at v-points
   , tao_xz    & ! Surface stress in x-direction
   , tao_yz    & ! Surface stress in y-direction
   , epsilon   & !
   , ustar     & !
   , vk        & !
   , zu1       & ! 
   , zv1         ! 

  INTEGER      &  
  :: ibgn      & !
   , iend      & ! 
   , jbgn      & !
   , jend      & !
   , kbgn      & !
   , kend      & !   
   , i         & !
   , j         & !
   , k         & !
   , kint      & !
   , kmax      & !
   , ternopt   & !
   , canshape  & !
   , canheight & !
   , k_ref       ! KG

!-----------------------------------------------------------------------

  PI=3.14159265359
  ternopt = 1 
  g0=9.81     
  epsilon = 0.0000001
  vk = 0.4

  canshape = 2
  canheight = 1

!  canfact = 0.9 !!! JEFF OPTIMUM alphe = 12
!  canfact = 0.76 !!! JEFF OPTIMUM -- NEUTRAL; DRM=1, REC=0 !!
!  canfact = 0.72 !!! KG OPTIMUM -- NEUTRAL !!!
!!  canfact = 0.72
!!  canfact = 0.5  !!! KG OPTIMUM -- UNSTABLE CASE; DRM=1, REC=2 !!!
!!  canfact = 0.72  !!! KG OPTIMUM -- STABLE CASE; DRM=1, REC=2 !!!

!!  canfact = 0.9   !!! KG OPTIMUM -- OUTER NEST; DRM=1, REC=2 !!!

!  canfact = 0.86   !!! -- OUTER NEST; DRM=1, REC=0 !!!

!  IF ( dx .EQ. 99.0 ) THEN  !These work for dz=8.25; a=12 for 99 m domain, 4 for 33 m domain.
!
!    canfact = 0.90
!
!  ENDIF
!
!  IF ( dx .EQ. 33.0 ) THEN
!
!    canfact = 0.76
!
!  ENDIF
!
!  IF ( dx .EQ. 11.0 ) THEN
!
!    canfact = 0.76
!
!  ENDIF
!
!   canfact = 0.76  !JDM 12-2015 a = 4
!!   canfact = 0.58  !JDM 12-2015 a = 1.26

  ibgn = its
  jbgn = jts
  kbgn = kts
  iend = MIN(ite,ide-1)
  jend = MIN(jte,jde-1)
  kend = MIN( kte, kde )

  u_canforce = 0.0
  v_canforce = 0.0

  DO j = jts-1, jte+1  
  DO k = kts, kte
  DO i = its-1, ite+1 
    u_canopy(i,k,j) = 0.0 
    v_canopy(i,k,j) = 0.0
  ENDDO
  ENDDO
  ENDDO

  u_can = 0.0
  v_can = 0.0
  u_canforce = 0.0
  v_canforce = 0.0
  Cd_u = 0.0
  Cd_v = 0.0
  a = 0.0
  a0 = 0.0
  b0 = 0.0
  C0 = 0.0
  D0 = 0.0
  kcan = 0.0

  ! Define canopy height 
  ! should be approximately Delta_x = 2*dx for horizontal eddy size

!  IF (canheight .EQ. 1) THEN
!    Hc = dx  
!  ELSEIF (canheight .EQ. 2) THEN
!    Hc = 2*dx  
!  ELSEIF (canheight .EQ. 3) THEN 
!    Hc = 3*dx  
!  ELSEIF (canheight .EQ. 4) THEN
!    Hc = 4*dx  
!  ELSEIF (canheight .EQ. 8) THEN
!    Hc =  8.0 
!  ELSEIF (canheight .EQ. 12) THEN
!    Hc =  dx/2.0 
!  ELSE
!    print *,'ERROR: Invalid canopy height option chosen.'
!    stop
!  ENDIF


!  Hc =  8.0 
!  Hc = 3.0*dx !!! KG OPTIMUM -- NEUTRAL CASE !!!
!  Hc = 3.0*dx  !!! KG OPTIMUM -- UNSTABLE & STABLE CASES; DRM=1, REC=2 !!!
  Hc = 4.0*dx
!    Hc = 81.0 !  Hc = 3.0*dx
!  Hc = 108.0 !Hc = 4.0*dx
!  Hc = 120.0

  DO j=jbgn,jend
  DO i=ibgn,iend
    zw(i,1,j)=0.0
  END DO
  END DO

  DO j=jbgn,jend
  DO i=ibgn,iend
  DO k=kbgn+1,kend  
    zw(i,k,j) = zw(i,k-1,j) + 1.0/rdzw(i,k-1,j)
    z(i,k-1,j) = 0.5*( zw(i,k-1,j) + zw(i,k,j) )
  END DO
  END DO
  END DO

!  write(*,*) 'k,zw(9,k,9),z(9,k,9)'
!  DO k=1,kend  
!    write(*,*),k,zw(9,k,9),z(9,k,9)
!  END DO


!-----------------------------------------------------------------------
!loop through to find canopy height index for each i,j 
!-----------------------------------------------------------------------
  IF (ternopt .NE. 0) THEN

    DO j=jbgn,jend
    DO i=ibgn,iend
    DO k=kbgn,kend  

      !If z is greater than the canopy height, set kcan to be
      !that index and exit the k-loop

      IF( zw(i,k,j) .GT. Hc ) THEN
        kcan(i,j) = k-1
        EXIT
      END IF

    END DO
    END DO
    END DO

  ELSE !if no terrain, don't loop through all i,j

    DO k=kbgn,kend  

      IF( zw(1,k,1) .GT. Hc ) THEN
        kcan = k-1 !set all elements in kcan array to this value
        EXIT
      END IF

    END DO

  ENDIF

!-----------------------------------------------------------------------
!Define canopy as in Brown, Hobson and Wood 2001 
!-----------------------------------------------------------------------
         
!-----------------------------------------------------------------------
!     Define functional form of damping coefficient a(z) 
!     multiply by a0 later to scale it
!-----------------------------------------------------------------------

  DO j=jbgn,jend
  DO i=ibgn,iend
    kmax = kcan(i,j)
    DO k=1,kmax
!      a(i,k,j) = cos(PI*z(i,k,j)/(2*Hc))
!      a(i,k,j) = cos(PI*z(i,k,j)/(2*Hc))**2
!      a(i,k,j) = exp(-z(i,k,j)/(0.2*Hc))*cos(PI*z(i,k,j)/(2.0*Hc))**2
!      a(i,k,j) = cos(PI*z(i,k,j)/(2*Hc))**3
       a(i,k,j) = exp(-z(i,k,j)/(0.18*Hc)) !DRM
!      a(i,k,j) = (exp(-z(i,k,j)/(0.2*Hc)))**2 !!! KG FINAL !!!
    END DO
    DO k=kmax+1,kend
      a(i,k,j) = 0.0
    END DO
  END DO
  END DO


!-----------------------------------------------------------------------
! Define the canopy force dtau_can/dz and calculate the surface fluxes
!-----------------------------------------------------------------------

!!KG  k_ref = 1  ! reference k level at which the wall stress is calculated 
!!KG
!!KG  DO j = jbgn, jend+1
!!KG  DO i = ibgn, iend+1

!OLD    Cd_u(i,j)=( KARMAN /                                     &
!OLD                LOG( (2./( rdz(i-1,kts,j) + rdz(i  ,kts,j) ) &   
!OLD                              +z_rough )/z_rough ) )**2
!OLD
!OLD    Cd_v(i,j)=( KARMAN /                                     &
!OLD                LOG( (2./( rdz(i,kts,j-1) + rdz(i  ,kts,j) ) &   
!OLD                              +z_rough )/z_rough ) )**2

!!KG    Cd_u(i,j)=( KARMAN /                                     &
!!KG                LOG( (         z(i,k_ref,j)                      &   
!!KG                              +z_rough )/z_rough ) )**2

!!KG    Cd_v(i,j)=( KARMAN /                                     &
!!KG                LOG( (         z(i,k_ref,j)                      &   
!!KG                              +z_rough )/z_rough ) )**2

!!KG  ENDDO
!!KG  ENDDO


!!KG  DO j = jbgn,jend
!!KG  DO i = ibgn,iend
 
!OLD    V0_u=sqrt((u(i,kts,j)**2) +       &
!OLD             (((v(i  ,kts,j  )+       &
!OLD             v(i  ,kts,j+1)+          &
!OLD             v(i-1,kts,j  )+          &
!OLD             v(i-1,kts,j+1))/4.0)**2))+epsilon
!OLD
!OLD    tao_xz=Cd_u(i,j)*V0_u*u(i,kts,j)
!OLD    usflx(i,j) = -tao_xz
!OLD
!OLD
!OLD    V0_v=sqrt((v(i,kts,j)**2) +       &
!OLD             (((u(i  ,kts,j  )+       &
!OLD             u(i  ,kts,j-1)+          &
!OLD             u(i+1,kts,j  )+          &
!OLD             u(i+1,kts,j-1))/4.0)**2))+epsilon
!OLD
!OLD    tao_yz=Cd_v(i,j)*V0_v*v(i,kts,j)
!OLD    vsflx(i,j) = -tao_yz

!!KG    V0_u=sqrt((u(i,k_ref,j)**2) +       &
!!KG             (((v(i  ,k_ref,j  )+       &
!!KG             v(i  ,k_ref,j+1)+          &
!!KG             v(i-1,k_ref,j  )+          &
!!KG             v(i-1,k_ref,j+1))/4.0)**2))+epsilon
!!KG
!!KG    tao_xz=Cd_u(i,j)*V0_u*u(i,k_ref,j)
!!KG    usflx(i,j) = -tao_xz
!!KG
!!KG
!!KG    V0_v=sqrt((v(i,k_ref,j)**2) +       &
!!KG             (((u(i  ,k_ref,j  )+       &
!!KG             u(i  ,k_ref,j-1)+          &
!!KG             u(i+1,k_ref,j  )+          &
!!KG             u(i+1,k_ref,j-1))/4.0)**2))+epsilon
!!KG
!!KG    tao_yz=Cd_v(i,j)*V0_v*v(i,k_ref,j)
!!KG    vsflx(i,j) = -tao_yz
!!KG
!!KG    
!!KG  END DO
!!KG  END DO

!=====================================


    DO j = jbgn,jend
    DO i = ibgn,iend

       V0_u=0.
       tao_xz=0.
       V0_u=    sqrt((u(i,kts,j)**2) +         &
                        (((v(i  ,kts,j  )+          &
                           v(i  ,kts,j+1)+          &
                           v(i-1,kts,j  )+          &
                           v(i-1,kts,j+1))/4)**2))+epsilon
       ustar=0.5*(ust(i,j)+ust(i-1,j))
       tao_xz=ustar*ustar*u(i,kts,j)/V0_u

       usflx(i,j) = -tao_xz

    ENDDO
    ENDDO

    DO j = jbgn,jend
    DO i = ibgn,iend 

       V0_v=0.
       tao_yz=0.
       V0_v=    sqrt((v(i,kts,j)**2) +         &
                        (((u(i  ,kts,j  )+          &
                           u(i  ,kts,j-1)+          &
                           u(i+1,kts,j  )+          &
                           u(i+1,kts,j-1))/4)**2))+epsilon
       ustar=0.5*(ust(i,j)+ust(i,j-1))
       tao_yz=ustar*ustar*v(i,kts,j)/V0_v

       vsflx(i,j) = -tao_yz

    ENDDO
    ENDDO

!=====================================

  DO j = jbgn,jend
  DO i = ibgn,iend
  DO k = kbgn,kend
    V0_u=sqrt((u(i,k,j)**2) +       &
             (((v(i  ,k,j  )+       &
             v(i  ,k,j+1)+          &
             v(i-1,k,j  )+          &
             v(i-1,k,j+1))/4.0)**2))+epsilon

    u_canforce(i,k,j) = -canfact*a(i,k,j)*abs( u(i,k,j) )*V0_u

    V0_v=sqrt((v(i,k,j)**2) +       &
             (((u(i  ,k,j  )+       &
             u(i  ,k,j-1)+          &
             u(i+1,k,j  )+          &
             u(i+1,k,j-1))/4.0)**2))+epsilon

    v_canforce(i,k,j) = -canfact*a(i,k,j)*abs( v(i,k,j) )*V0_v
       
  END DO
  END DO
  END DO

!-----------------------------------------------------------------------
! calculate the integration constants C0, D0 
!
! Integrate using the trapezoidal rule (apply to each interval 
! since spacing is not generally uniform)
!-----------------------------------------------------------------------

  DO j = jbgn,jend
  DO i = ibgn,iend

    kmax = kcan(i,j)
    DO k = 1, kmax
      C0(i,j) = C0(i,j) + (u_canforce(i,k,j))*(zw(i,k+1,j)-zw(i,k,j))
      D0(i,j) = D0(i,j) + (v_canforce(i,k,j))*(zw(i,k+1,j)-zw(i,k,j))
    END DO

  END DO
  END DO

!-----------------------------------------------------------------------
! integrate to get the canopy stress term at each height
!-----------------------------------------------------------------------
  DO j = jbgn,jend
  DO i = ibgn,iend

    kmax = kcan(i,j)
    DO k = 1, kmax

      ! add up pieces
      DO kint =1,k
        u_can(i,k,j) = u_can(i,k,j) + (u_canforce(i,kint,j)) &
                          *(zw(i,kint+1,j)-zw(i,kint,j))
        v_can(i,k,j) = v_can(i,k,j) + (v_canforce(i,kint,j)) &
                          *(zw(i,kint+1,j)-zw(i,kint,j))
      END DO

      ! subtract off integration constant
      u_can(i,k,j) = u_can(i,k,j)-C0(i,j)
      v_can(i,k,j) = v_can(i,k,j)-D0(i,j)

    END DO

  END DO
  END DO

!-----------------------------------------------------------------------
!Scale so that canopy model supplements the stress provided by
!the series model to equal the applied stress 
!(usflx, vsflx) at the bottom boundary
!-----------------------------------------------------------------------

  DO j = jbgn,jend
  DO i = ibgn,iend

    a0(i,j) = canfact*usflx(i,j)/(u_can(i,1,j) + 1.0e-18)
    b0(i,j) = canfact*vsflx(i,j)/(v_can(i,1,j) + 1.0e-18)

    DO k=1,kend
      u_can(i,k,j) = a0(i,j)*u_can(i,k,j)
      v_can(i,k,j) = b0(i,j)*v_can(i,k,j)  
    END DO

  END DO
  END DO

!Project canopy stress to e and f points, from k = 2 up

  DO j = jbgn,jend
  DO k = 2,kend
  DO i = ibgn,iend
!JDM1130    u_canopy(i,k,j) = fnm(k)*u_can(i,k,j) + fnp(k)*u_can(i,k-1,j)
!JDM1130    v_canopy(i,k,j) = fnm(k)*v_can(i,k,j) + fnp(k)*v_can(i,k-1,j)

   nwtau13(i,k,j) = fnm(k)*u_can(i,k,j) + fnp(k)*u_can(i,k-1,j) !JDM1130  
   nwtau23(i,k,j) = fnm(k)*v_can(i,k,j) + fnp(k)*v_can(i,k-1,j) !JDM1130  

  ENDDO
  ENDDO
  ENDDO

!  DO j = jbgn,jend
!  DO i = ibgn,iend
!    u_canopy(i,2,j) = u_canopy(i,2,j)*1.15
!    u_canopy(i,3,j) = u_canopy(i,3,j)*1.05
!    v_canopy(i,2,j) = v_canopy(i,2,j)*1.15
!    v_canopy(i,3,j) = v_canopy(i,3,j)*1.05
!  ENDDO
!  ENDDO


!JDM1130    DO j = jbgn,jend
!JDM1130    DO i = ibgn,iend
!JDM1130    DO k=2,kmax+1
!JDM1130      nwtau13(i,k,j) = u_canopy(i,k,j) 
!JDM1130      nwtau23(i,k,j) = v_canopy(i,k,j) 
!JDM1130  !    if ((i.eq.2).and.(j.eq.2).and.(k.eq.2)) print *, 'in lasd', nwtau13(i,k,j)
!JDM1130    END DO
!JDM1130    END DO
!JDM1130    END DO

 10 FORMAT(I3)
 100 FORMAT(18(x,I9))
 1000 FORMAT(i2,18(x,f9.5))

 RETURN

END SUBROUTINE drm_nw_canopy

!=======================================================================

SUBROUTINE writecheckm_drm( var, ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte  )

 IMPLICIT NONE

 INTEGER, INTENT( IN )            &
 :: ims, ime, jms, jme, kms, kme, &
    its, ite, jts, jte, kts, kte

 REAL, DIMENSION( ims:ime,kms:kme,jms:jme ), INTENT( IN )  &
 :: var

 INTEGER :: i, j, kk, jj

  INTEGER, DIMENSION( its:ite ) &
  :: indarr

!------------------------------------------------------------------------

  DO i = its,ite
    indarr(i)=i
  ENDDO

 jj = jts+3
 kk = 2

!   WRITE(*,*) 'ims, ime, jms, jme, kms, kme'
!   WRITE(*,*)  ims, ime, jms, jme, kms, kme
!   WRITE(*,*) 'its, ite, jts, jte, kts, kte'
!   WRITE(*,*)  its, ite, jts, jte, kts, kte
!   WRITE(*,*) 'kk',kk
!   WRITE(*,*) 'jj',jj

!   DO j = jte, jts -1

      WRITE(*,1000)jj,var(its:ite,kk,jj)

!   ENDDO
      WRITE(*,100)indarr(its:ite)

 100   FORMAT(2x,26(x,I12))
 1000   FORMAT(i2,26(x,e12.5))

END SUBROUTINE writecheckm_drm


!=======================================================================

SUBROUTINE writecheck_2d( var, ims, ime, jms, jme )

 IMPLICIT NONE

 INTEGER, INTENT( IN ) &
 :: ims, ime, jms, jme

 REAL, DIMENSION( ims:ime,jms:jme ), INTENT( IN )  &
 :: var

 INTEGER :: i, j, m

 PARAMETER (m=4)

 INTEGER, DIMENSION( ims+m:ime-m ) &
 :: indarr

!----------------------------------------------------------------------

 DO i = ims+m,ime-m
   indarr(i)=i
 ENDDO

 WRITE(*,*) 'ims, ime, jms, jme'
 WRITE(*,*)  ims, ime, jms, jme
 WRITE(*,*) 'j'

 DO j = jme-m, jms+m, -1
   WRITE(*,1000)j,var(ims+m:ime-m,j)
 ENDDO
 WRITE(*,100)indarr(ims+m:ime-m)

 100   FORMAT(2x,26(x,I9))
 1000   FORMAT(i2,26(x,f9.5))

END SUBROUTINE writecheck_2d


!=======================================================================

SUBROUTINE writecheck_t2d( var,                          &
                           ims, ime, jms, jme, kms, kme, &
                           its, ite, jts, jte, kts, kte  )

 IMPLICIT NONE

 INTEGER, INTENT( IN ) &
 :: its, ite, jts, jte, kts, kte, ims, ime, jms, jme, kms, kme

 REAL, DIMENSION( ims:ime,kms:kme,jms:jme ), INTENT( IN )  &
 :: var

 INTEGER :: i, j, k, m

 PARAMETER (m=2)

 INTEGER, DIMENSION( its-2:ite+2 ) &
 :: indarr

!----------------------------------------------------------------------

 k=5

 DO i = its-2,ite+2
   indarr(i)=i
 ENDDO

 WRITE(*,*) 'its, ite, jts, jte'
 WRITE(*,*)  its, ite, jts, jte
 WRITE(*,*) 'k ',k
 WRITE(*,*) 'j'

 DO j = jte+2, jts-2, -1
   WRITE(*,1000)j,var(its-2:ite+2,k,j)
 ENDDO
 WRITE(*,100)indarr(its-2:ite+2)

 100   FORMAT(2x,26(x,I9))
 1000   FORMAT(i2,26(x,f9.5))

END SUBROUTINE writecheck_t2d




END MODULE module_sfs_drm
