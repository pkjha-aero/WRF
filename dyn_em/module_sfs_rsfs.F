!WRF:MODEL_LAYER:PHYSICS

!==============================================================================
!
! © 2008. Lawrence Livermore National Security, LLC. All rights reserved.
! This work was produced at the Lawrence Livermore National Laboratory (LLNL) under
! contract no. DE-AC52-07NA27344 (Contract 44) between the U.S. Department of Energy (DOE)
! and Lawrence Livermore National Security, LLC (LLNS) for the operation of LLNL. Copyright
! is reserved to Lawrence Livermore National Security, LLC for purposes of controlled
! dissemination, commercialization through formal licensing, or other disposition under
! terms of Contract 44; DOE policies, regulations and orders; and U.S. statutes. The rights
! of the Federal Government are reserved under Contract 44.
!
! DISCLAIMER
! This work was prepared as an account of work sponsored by an agency of the United States
! Government. Neither the United States Government nor Lawrence Livermore National
! Security, LLC nor any of their employees, makes any warranty, express or implied, or
! assumes any liability or responsibility for the accuracy, completeness, or usefulness of
! any information, apparatus, product, or process disclosed, or represents that its use
! would not infringe privately-owned rights. Reference herein to any specific commercial
! products, process, or service by trade name, trademark, manufacturer or otherwise does
! not necessarily constitute or imply its endorsement, recommendation, or favoring by the
! United States Government or Lawrence Livermore National Security, LLC. The views and
! opinions of authors expressed herein do not necessarily state or reflect those of the
! United States Government or Lawrence Livermore National Security, LLC, and shall not be
! used for advertising or product endorsement purposes.
!
! LICENSING REQUIREMENTS
! Any use, reproduction, modification, or distribution of this software or documentation
! for commercial purposes requires a license from Lawrence Livermore National Security,
! LLC. Contact: Lawrence Livermore National Laboratory, Industrial Partnerships Office,
! P.O. Box 808, L-795, Livermore, CA 94551
!
!=============================================================================
!
! Modification History: 
!
! Implemented 08/2008 by Jeff Mirocha, jmirocha@llnl.gov
!
!=============================================================================

MODULE module_sfs_rsfs

CONTAINS

!=======================================================================

SUBROUTINE rec_0( urec, vrec, wrec, rcoef,             &
                  u, v, w, recl,                       &
                  ids, ide, jds, jde, kds, kde,        &
                  ims, ime, jms, jme, kms, kme,        &
                  ips, ipe, jps, jpe, kps, kpe,        &
                  its, ite, jts, jte, kts, kte         )

!-----------------------------------------------------------------------
! PURPOSE: Calculate coefficients for the terms in the series expansion
!          for the RSFS stresses. Assign the zeroth term in the series.
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: urec        & ! zeroth term of RSFS u-velocity      (m s^-1)
   , vrec        & ! zeroth term of RSFS v-velocity      (m s^-1)
   , wrec          ! zeroth term of RSFS w-velocity      (m s^-1)


  REAL, DIMENSION(0:10), INTENT( OUT ) &
  :: rcoef         !reconstruction filter coefficients

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: u           & !u-velocity                           (m s^-1)
   , v           & !v-velocity                           (m s^-1)
   , w             !w-velocity                           (m s^-1)


  INTEGER,                                               INTENT( IN  ) &
  :: recl          !reconstruction level

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER:: i, j, k

!-----------------------------------------------------------------------

  rcoef = 0.0

  SELECT CASE (recl)

    CASE (0)

      rcoef(0)=1.0D0

    CASE (1)

      rcoef(0)=2.0D0
      rcoef(1)=-1.0D0

    CASE (2)

      rcoef(0)=3.0D0
      rcoef(1)=-3.0D0
      rcoef(2)=1.0D0

    CASE (3)

      rcoef(0)=4.0D0
      rcoef(1)=-6.0D0
      rcoef(2)=4.0D0
      rcoef(3)=-1.0D0

    CASE (4)

      rcoef(0)=5.0D0
      rcoef(1)=-10.0D0
      rcoef(2)=10.0D0
      rcoef(3)=-5.0D0
      rcoef(4)=1.0D0

    CASE (5)

      rcoef(0)=6.0D0
      rcoef(1)=-15.0D0
      rcoef(2)=20.0D0
      rcoef(3)=-15.0D0
      rcoef(4)=6.0D0
      rcoef(5)=-1.0D0

    CASE DEFAULT

      WRITE(*,*)'Error-improperly specified reconstruction level in'
      WRITE(*,*)'dyn_em/module_reconstruct.F/subroutine rec_0'
      STOP

  END SELECT

  DO j = jts, jte   !calculate first term in series
  DO k = kts, kte
  DO i = its, ite
    urec(i,k,j) = rcoef(0)*u(i,k,j)
    vrec(i,k,j) = rcoef(0)*v(i,k,j)
    wrec(i,k,j) = rcoef(0)*w(i,k,j)
  END DO
  END DO
  END DO

  RETURN

END SUBROUTINE rec_0

!=====================================================================

SUBROUTINE rec_0_tq ( trec, qrec, rcoef,               &
                      t, q,  recl,                     &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      ips, ipe, jps, jpe, kps, kpe,    &
                      its, ite, jts, jte, kts, kte     )

!-----------------------------------------------------------------------
! PURPOSE: Calculate coefficients for the terms in the series expansion
!          for the RSFS stresses. Assign the zeroth term in the series.
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: trec        & ! zeroth term of RSFS theta           (K) 
   , qrec          ! zeroth term of RSFS QV              ( ) 

  REAL, DIMENSION(0:10), INTENT( OUT ) &
  :: rcoef         !reconstruction filter coefficients

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: t           & !potential temperature                (K)
   , q             !vapor mixing ratio                   ( )

  INTEGER,                                               INTENT( IN  ) &
  :: recl          !reconstruction level

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER:: i, j, k

!-----------------------------------------------------------------------

  rcoef = 0.0

  SELECT CASE (recl)

    CASE (0)

      rcoef(0)=1.0D0

    CASE (1)

      rcoef(0)=2.0D0
      rcoef(1)=-1.0D0

    CASE (2)

      rcoef(0)=3.0D0
      rcoef(1)=-3.0D0
      rcoef(2)=1.0D0

    CASE (3)

      rcoef(0)=4.0D0
      rcoef(1)=-6.0D0
      rcoef(2)=4.0D0
      rcoef(3)=-1.0D0

    CASE (4)

      rcoef(0)=5.0D0
      rcoef(1)=-10.0D0
      rcoef(2)=10.0D0
      rcoef(3)=-5.0D0
      rcoef(4)=1.0D0

    CASE (5)

      rcoef(0)=6.0D0
      rcoef(1)=-15.0D0
      rcoef(2)=20.0D0
      rcoef(3)=-15.0D0
      rcoef(4)=6.0D0
      rcoef(5)=-1.0D0

    CASE DEFAULT

      WRITE(*,*)'Error-improperly specified reconstruction level in'
      WRITE(*,*)'dyn_em/module_reconstruct.F/subroutine rec_0'
      STOP

  END SELECT

  DO j = jts, jte   !calculate first term in series
  DO k = kts, kte
  DO i = its, ite
    trec(i,k,j) = rcoef(0)*t(i,k,j)
    qrec(i,k,j) = rcoef(0)*q(i,k,j)
  END DO
  END DO
  END DO

  RETURN

END SUBROUTINE rec_0_tq

!=====================================================================

SUBROUTINE rec( urec, vrec, wrec,             &
                ufilt, vfilt, wfilt, coef,    &
                ids, ide, jds, jde, kds, kde, &
                ims, ime, jms, jme, kms, kme, &
                ips, ipe, jps, jpe, kps, kpe, &
                its, ite, jts, jte, kts, kte  )

!---------------------------------------------------------------------
! PURPOSE: Add current step of reconstruction term to the series
!          for the RSFS stresses.
!---------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),        INTENT(INOUT) &
  :: urec        & !reconstructed u-velocity           (m s^-1)
   , vrec        & !reconstructed v-velocity           (m s^-1)
   , wrec          !reconstructed w-velocity           (m s^-1)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),        INTENT( IN  ) &
  :: ufilt       & !filtered u-velocity                (m s^-1)
   , vfilt       & !filtered v-velocity                (m s^-1)
   , wfilt         !filtered w-velocity                (m s^-1)

  REAL,                                                INTENT( IN  ) &
  :: coef          !coefficients of series expansion expression

  INTEGER,                                             INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER:: i, j, k

!---------------------------------------------------------------------

  DO j = jts, jte   !calculate first term in series
  DO k = kts, kte
  DO i = its, ite
    urec(i,k,j) = urec(i,k,j) + coef*ufilt(i,k,j)
    vrec(i,k,j) = vrec(i,k,j) + coef*vfilt(i,k,j)
    wrec(i,k,j) = wrec(i,k,j) + coef*wfilt(i,k,j)
  END DO
  END DO
  END DO

  RETURN

END SUBROUTINE rec

!=======================================================================

SUBROUTINE rec_tq ( trec, qrec,                               &
                    tfilt, qfilt, coef,                       &
                    ids, ide, jds, jde, kds, kde,             &
                    ims, ime, jms, jme, kms, kme,             &
                    ips, ipe, jps, jpe, kps, kpe,             &
                    its, ite, jts, jte, kts, kte              )

!---------------------------------------------------------------------
! PURPOSE: Add current step of reconstruction term to the series
!          for the RSFS stresses.
!---------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),        INTENT(INOUT) &
  :: trec        &  !reconstructed temperature
   , qrec           !reconstructed moisture

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),        INTENT( IN  ) &
  :: tfilt       & !filtered temperature
   , qfilt         !filtered moisture

  REAL,                                                INTENT( IN  ) &
  :: coef          !coefficients of series expansion expression

  INTEGER,                                             INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER:: i, j, k

!---------------------------------------------------------------------

  DO j = jts, jte   !calculate first term in series
  DO k = kts, kte
  DO i = its, ite
    trec(i,k,j) = trec(i,k,j) + coef*tfilt(i,k,j)
    qrec(i,k,j) = qrec(i,k,j) + coef*qfilt(i,k,j)
  END DO
  END DO
  END DO

  RETURN

END SUBROUTINE rec_tq

!=======================================================================

SUBROUTINE uvw_s( us, vs, ws,                   &
                  u, v,                         &
                  ht, rdx, rdy,                 &
                  cf1, cf2, cf3,                &
                  rsfsvsbc,                     &
                  ids, ide, jds, jde, kds, kde, &
                  ims, ime, jms, jme, kms, kme, &
                  ips, ipe, jps, jpe, kps, kpe, &
                  its, ite, jts, jte, kts, kte  )

!-----------------------------------------------------------------------
! PURPOSE: This subroutine calculates the value of the velocities
!          at the surface that are required prior to filtering.
!          ws is obtained using the kinematic boundary condition,
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
  :: us          & ! surface u                           (m s^-1)
   , vs          & ! surface v                           (m s^-1)
   , ws            ! surface w                           (m s^-1)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: u           & !input velocity                       (m s^-1)
   , v             !input velocity                       (m s^-1)


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

    DO j=jts-1, jte+1 ! project u and v to surface
    DO i=its-1, ite+1
      us(i,j) = cf1*u(i,1,j)+cf2*u(i,2,j)+cf3*u(i,3,j) 
      vs(i,j) = cf1*v(i,1,j)+cf2*v(i,2,j)+cf3*v(i,3,j)
    ENDDO
    ENDDO

    DO j=jts, jte ! assign w(i,1,j)  
    DO i=its, ite
      ws(i,j)=                         &
                0.5*rdy*(              &
                (ht(i,j+1)-ht(i,j  ))  &
                *(vs(i,j+1))           &
                +(ht(i,j  )-ht(i,j-1)) &
                *(vs(i,j  ))  )        &
                +.5*rdx*(              &
                (ht(i+1,j)-ht(i,j  ))  &
                *(us(i+1,j))           &
                +(ht(i,j  )-ht(i-1,j)) &
                *(us(i,j))  )   
    ENDDO
    ENDDO

  ELSEIF ( rsfsvsbc .EQ. 1 ) THEN !ZERO VERTICAL GRADIENT

  DO j=jts-1, jte+1
  DO i=its-1, ite+1
    us(i,j) = u(i,1,j)
    vs(i,j) = v(i,1,j)
  ENDDO
  ENDDO

  DO j=jts, jte
  DO i=its, ite
    ws(i,j) =                        &
              0.5*rdy*(              &
              (ht(i,j+1)-ht(i,j  ))  &
              *(vs(i,j+1))           &
              +(ht(i,j  )-ht(i,j-1)) &
              *(vs(i,j  ))  )        &
              +.5*rdx*(              &
              (ht(i+1,j)-ht(i,j  ))  &
              *(us(i+1,j))           &
              +(ht(i,j  )-ht(i-1,j)) &
              *(us(i,j))  )
  ENDDO
  ENDDO

  ELSEIF ( rsfsvsbc .EQ. 2 ) THEN !NO SLIP

    DO j=jts, jte
    DO i=its, ite
      us(i,j) = 0.0
      vs(i,j) = 0.0
      ws(i,j) = 0.0  
    ENDDO
    ENDDO

  ENDIF

  RETURN

END SUBROUTINE uvw_s

!=======================================================================

SUBROUTINE tq_s(  ts, qs,                       &
                  t, q,                         &
                  ht, rdx, rdy,                 &
                  cf1, cf2, cf3,                &
                  rsfsvsbc,                     &
                  ids, ide, jds, jde, kds, kde, &
                  ims, ime, jms, jme, kms, kme, &
                  ips, ipe, jps, jpe, kps, kpe, &
                  its, ite, jts, jte, kts, kte  )

!-----------------------------------------------------------------------
! PURPOSE: This subroutine calculates the value of the velocities
!          at the surface that are required prior to filtering.
!          ws is obtained using the kinematic boundary condition,
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
  :: ts          & ! surface t                           (m s^-1)
   , qs            ! surface t                           (m s^-1)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: t           & !input theta                          (m s^-1)
   , q             !input theta                          (m s^-1)

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

    DO j=jts-1, jte+1 ! project u and v to surface
    DO i=its-1, ite+1
      ts(i,j) = cf1*t(i,1,j)+cf2*t(i,2,j)+cf3*t(i,3,j) 
      qs(i,j) = cf1*q(i,1,j)+cf2*q(i,2,j)+cf3*q(i,3,j)
    ENDDO
    ENDDO

  ELSEIF ( rsfsvsbc .EQ. 1 ) THEN !ZERO VERTICAL GRADIENT

  DO j=jts-1, jte+1
  DO i=its-1, ite+1
    ts(i,j) = t(i,1,j)
    qs(i,j) = q(i,1,j)
  ENDDO
  ENDDO

  ELSEIF ( rsfsvsbc .EQ. 2 ) THEN !NO SLIP

    DO j=jts, jte
    DO i=its, ite
      ts(i,j) = 0.0
      qs(i,j) = 0.0  
    ENDDO
    ENDDO

  ENDIF

  RETURN

END SUBROUTINE tq_s

!=======================================================================

SUBROUTINE update_w1( w, ws,                        &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      ips, ipe, jps, jpe, kps, kpe, &
                      its, ite, jts, jte, kts, kte  )

!---------------------------------------------------------------------
! PURPOSE: Update the surface index of the vertical velocity to be
!          ws from uvw_s
!---------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),        INTENT(INOUT) &
  :: w             !3-D vertical velocity array        (m s^-1)

  REAL, DIMENSION( ims:ime, jms:jme ),                 INTENT( IN  ) &
  :: ws            !kinematic w at the surface         (m s^-1)

  INTEGER,                                             INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER:: i, j

!---------------------------------------------------------------------

  DO j=jts, jte
  DO i=its, ite
    w(i,kts,j) = ws(i,j)
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE update_w1

!=======================================================================

SUBROUTINE proj_s( usc, vsc,           &
                   uusc, vvsc, wws,    &
                   usd, vsd, uvsd,     &
                   wse, uwse,          &
                   wsf, vwsf,          &
                   us, vs, ws,         &
                   ids, ide, jds, jde, &
                   ims, ime, jms, jme, &
                   ips, ipe, jps, jpe, &
                   its, ite, jts, jte  )

!-----------------------------------------------------------------------
! PURPOSE: Project velocities at the surface
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( IN  ) &
  :: us          & !zonal surface velocity               (m s^-1)
   , vs          & !meridional surface velocity          (m s^-1)
   , ws          

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( OUT ) &
  :: usc         & !projected surface velocity           (m s^-1)
   , vsc         & !projected surface velocity           (m s^-1)
   , uusc        & !projected surface velocity           (m s^-1)
   , vvsc        & !projected surface velocity           (m s^-1)
   , wws         & !projected surface velocity           (m s^-1)
   , usd         & !projected surface velocity           (m s^-1)
   , vsd         & !projected surface velocity           (m s^-1)
   , uvsd        & !projected surface velocity           (m s^-1)
   , wse         & !projected surface velocity           (m s^-1)
   , uwse        & !projected surface velocity           (m s^-1)
   , wsf         & !projected surface velocity           (m s^-1)
   , vwsf          !projected surface velocity           (m s^-1)

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, &
     ims, ime, jms, jme, &
     ips, ipe, jps, jpe, &
     its, ite, jts, jte

! LOCAL VARIABLES

  INTEGER:: i, j

!-----------------------------------------------------------------------

! project us and vs to c nodes-------------------------------------------------

  DO j=jts, jte
  DO i=its, ite
    usc(i,j) = 0.5*( us(i+1,j) + us(i,j) )
    vsc(i,j) = 0.5*( vs(i,j+1) + vs(i,j) )
    uusc(i,j) = usc(i,j)*usc(i,j)
    vvsc(i,j) = vsc(i,j)*vsc(i,j)
    usd(i,j) = 0.5*( us(i,j-1) + us(i,j) )
    vsd(i,j) = 0.5*( vs(i-1,j) + vs(i,j) )
    uvsd(i,j) = usd(i,j)*vsd(i,j)
    wse(i,j) = 0.5*( ws(i-1,j) + ws(i,j) )
    uwse(i,j) =us(i,j)*wse(i,j)
    wsf(i,j) = 0.5*( ws(i,j-1) + ws(i,j) )
    vwsf(i,j) =vs(i,j)*wsf(i,j)
    wws(i,j) = ws(i,j)*ws(i,j)
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE proj_s

!=======================================================================

SUBROUTINE proj_s_tq ( tsu, tsv,           &
                       utsu, vtsv, wtsw,   &
                       qsu, qsv,           &
                       uqsu, vqsv, wqsw,   &
                       us, vs, ws, ts, qs, &
                       ids, ide, jds, jde, &
                       ims, ime, jms, jme, &
                       ips, ipe, jps, jpe, &
                       its, ite, jts, jte  )

!-----------------------------------------------------------------------
! PURPOSE: Project velocities at the surface
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( IN  ) &
  :: us          & !zonal surface velocity               (m s^-1)
   , vs          & !meridional surface velocity          (m s^-1)
   , ws          & !vertical surface velocity            (m s^-1)
   , ts          & !surface potential temperature        (K)
   , qs            !surface moisture           

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( OUT ) &
  :: tsu         & !
   , tsv         & !
   , utsu        & !  
   , vtsv        & !
   , wtsw        & !
   , qsu         & !
   , qsv         & !
   , uqsu        & !  
   , vqsv        & !
   , wqsw          !


  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, &
     ims, ime, jms, jme, &
     ips, ipe, jps, jpe, &
     its, ite, jts, jte

! LOCAL VARIABLES

  INTEGER:: i, j

!-----------------------------------------------------------------------

! project ts and qs to c nodes-------------------------------------------------

  DO j=jts, jte
  DO i=its, ite

    tsu(i,j) = 0.5*( ts(i-1,j) + ts(i,j) )     
    tsv(i,j) = 0.5*( ts(i,j-1) + ts(i,j) )                     
    utsu(i,j) = us(i,j)*tsu(i,j)       
    vtsv(i,j) = vs(i,j)*tsv(i,j)         
    wtsw(i,j) = ws(i,j)*ts(i,j)      

    qsu(i,j) = 0.5*( qs(i-1,j) + qs(i,j) )     
    qsv(i,j) = 0.5*( qs(i,j-1) + qs(i,j) )                     
    uqsu(i,j) = us(i,j)*qsu(i,j)       
    vqsv(i,j) = vs(i,j)*qsv(i,j)         
    wqsw(i,j) = ws(i,j)*qs(i,j)      

  ENDDO
  ENDDO

  RETURN

END SUBROUTINE proj_s_tq

!=======================================================================

SUBROUTINE proj_c( var1c, var2c, var3c,           &
                   var4c, var5c, var6c,           &
                   var1, var2, var3, variable,    &
                   ids, ide, jds, jde, kds, kde,  &
                   ims, ime, jms, jme, kms, kme,  &
                   ips, ipe, jps, jpe, kps, kpe,  &
                   its, ite, jts, jte, kts, kte   )

!-----------------------------------------------------------------------
! PURPOSE: project u, v and w to c-nodes
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: var1c       & !urec projected to c                  (m s^-1)
   , var2c       & !vrec projected to c                  (m s^-1)
   , var3c       & !wrec projected to c                  (m s^-1)
   , var4c       & !var1c^2                              (m^2 s^-2)
   , var5c       & !var2c^2                              (m^2 s^-2)
   , var6c         !var3c^2                              (m^2 s^-2)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var1        & !urec at u nodes                      (m s^-1)
   , var2        & !vrec at v nodes                      (m s^-1)
   , var3          !wrec at w nodes                      (m s^-1)

  CHARACTER, INTENT( IN ) :: variable

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER:: i, j, k, kend

!-----------------------------------------------------------------------

  SELECT CASE (variable)

    CASE ('u') !project velocities to cell center

  kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts, kend
  DO i = its, ite

    var1c(i,k,j) = 0.5*( var1(i+1,k,j) + var1(i,k,j) )
    var2c(i,k,j) = 0.5*( var2(i,k,j+1) + var2(i,k,j) )
    var3c(i,k,j) = 0.5*( var3(i,k+1,j) + var3(i,k,j) )
    var4c(i,k,j) = var1c(i,k,j)*var1c(i,k,j)
    var5c(i,k,j) = var2c(i,k,j)*var2c(i,k,j)
    var6c(i,k,j) = var3c(i,k,j)*var3c(i,k,j)

  ENDDO
  ENDDO
  ENDDO

    CASE ('d') !project to cell center and divide deformations by two
   
      kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts, kend
  DO i = its, ite

        var1c(i,k,j)=0.125*(   var1(i+1,k,j)   + var1(i,k,j)   &   
                            + var1(i+1,k,j+1) + var1(i,k,j+1) )

        var2c(i,k,j)=0.125*(   var2(i+1,k,j)   + var2(i,k,j)   &  
                            + var2(i+1,k+1,j) + var2(i,k+1,j) )

        var3c(i,k,j)=0.125*(   var3(i,k,j+1) + var3(i,k,j)     & 
                            + var3(i,k+1,j+1) + var3(i,k+1,j) )

      ENDDO
      ENDDO
      ENDDO

    CASE ('c') !divide deformations by two
   
      kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts, kend
  DO i = its, ite

        var1c(i,k,j)=0.5*var1(i,k,j)

        var2c(i,k,j)=0.5*var2(i,k,j) 

        var3c(i,k,j)=0.5*var3(i,k,j)
 
      ENDDO
      ENDDO
      ENDDO

    CASE ('h') !project Hij to cell center
   
      kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts, kend
  DO i = its, ite
        var1c(i,k,j)=0.25*(   var1(i+1,k,j)   + var1(i,k,j)   &   
                            + var1(i+1,k,j+1) + var1(i,k,j+1) )

        var2c(i,k,j)=0.25*(   var2(i+1,k,j)   + var2(i,k,j)   &  
                            + var2(i+1,k+1,j) + var2(i,k+1,j) )

        var3c(i,k,j)=0.25*(   var3(i,k,j+1) + var3(i,k,j)     & 
                            + var3(i,k+1,j+1) + var3(i,k+1,j) )
      ENDDO
      ENDDO
      ENDDO


    CASE DEFAULT
 
  END SELECT     

  RETURN

END SUBROUTINE proj_c

!=======================================================================

SUBROUTINE proj_d( var1d, var2d, var3d,          &
                   var1, var2,                   &
                   ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   ips, ipe, jps, jpe, kps, kpe, &
                   its, ite, jts, jte, kts, kte  )

!-----------------------------------------------------------------------
! PURPOSE: Project urec and vrec to d-nodes
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: var1d       & !urec projected to d-nodes            (m s^-1)
   , var3d       & !vrec projected to d-nodes            (m s^-1)
   , var2d         !var1d*var3d                          (m^2 s^-2)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var1        & !urec at u-nodes                      (m s^-1)
   , var2          !vrec at v-nodes                      (m s^-1)

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER:: i, j, k, kend

!-----------------------------------------------------------------------

  kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts, kend
  DO i = its, ite
    var1d(i,k,j) = 0.5*( var1(i,k,j-1) + var1(i,k,j) )
    var2d(i,k,j) = 0.5*( var2(i-1,k,j) + var2(i,k,j) )
    var3d(i,k,j) =var1d(i,k,j)*var2d(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE proj_d

!=======================================================================

SUBROUTINE proj_e( var1e, var2e, var3e,          &
                   var1, var2, fnm, fnp,         &
                   ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   ips, ipe, jps, jpe, kps, kpe, &
                   its, ite, jts, jte, kts, kte  )

!-----------------------------------------------------------------------
! PURPOSE: Project urec and wrec to e-nodes
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT(INOUT) &
  :: var1e       & !urec projected to e-nodes            (m s^-1)
   , var2e       & !wrec projected to e-nodes            (m s^-1)
   , var3e         !var1e*var2e                          (m^2 s^-2)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var1        & !urec at u-nodes                      (m s^-1)
   , var2          !wrec at w-nodes                      (m s^-1)

  REAL, DIMENSION( kms:kme ),                            INTENT( IN  ) &
  :: fnm         & !vertical interpolation coefficients
   , fnp

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER:: i, j, k, kend

!-----------------------------------------------------------------------

  kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts+1, kend
  DO i = its, ite
    var1e(i,k,j) = fnm(k)*var1(i,k,j) + fnp(k)*var1(i,k-1,j)
    var2e(i,k,j) = 0.5*( var2(i-1,k,j) + var2(i,k,j) )
    var3e(i,k,j) = var1e(i,k,j)*var2e(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE proj_e

!=======================================================================

SUBROUTINE proj_f( var1f, var2f, var3f,          &
                   var1, var2, fnm, fnp,         &
                   ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   ips, ipe, jps, jpe, kps, kpe, &
                   its, ite, jts, jte, kts, kte  )

!-----------------------------------------------------------------------
! PURPOSE: Project vrec and wrec to f-nodes
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT(INOUT) &
  :: var1f       & !vrec projected to e-nodes            (m s^-1)
   , var2f       & !wrec projected to e-nodes            (m s^-1)
   , var3f         !var1f*var2f                          (m^2 s^-1)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var1        & !vrec at v-nodes                      (m s^-1)
   , var2          !wrec at w-nodes                      (m s^-1)

  REAL, DIMENSION( kms:kme ),                            INTENT( IN  ) &
  :: fnm         & !vertical interpolation coefficientt
   , fnp

 INTEGER,                                               INTENT( IN  ) &
 :: ids, ide, jds, jde, kds, kde, &
    ims, ime, jms, jme, kms, kme, &
    ips, ipe, jps, jpe, kps, kpe, &
    its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER:: i, j, k, kend

!-----------------------------------------------------------------------

  kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts+1, kend
  DO i = its, ite
    var1f(i,k,j) = fnm(k)*var1(i,k,j) + fnp(k)*var1(i,k-1,j)
    var2f(i,k,j) = 0.5*( var2(i,k,j-1) + var2(i,k,j) )
    var3f(i,k,j) = var1f(i,k,j)*var2f(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE proj_f

!=======================================================================

SUBROUTINE proj_tuvw( tatu, tatv, tatw,             &
                     utatu, vtatv, wtatw,          &
                     urec, vrec, wrec, trec,       &
                     fnm, fnp,                     &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     ips, ipe, jps, jpe, kps, kpe, &
                     its, ite, jts, jte, kts, kte  )

!-----------------------------------------------------------------------
! PURPOSE: Project trec to u, v and w-nodes
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT(INOUT) &
  :: tatu        & !t projected to u-nodes               (K)
   , tatv        & !t projected to v-nodes               (K)
   , tatw        & !t projected to w-nodes               (K)
   , utatu       & !u*tatu                               (K)
   , vtatv       & !v*tatv                               (K)
   , wtatw         !w*tatw                               (K)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: urec        & !urec at u-nodes                      (m s-1)
   , vrec        & !vrec at v-nodes                      (m s-1)
   , wrec        & !wrec at w-nodes                      (m s-1)
   , trec          !trec at t-nodes                      (K)

  REAL, DIMENSION( kms:kme ),                            INTENT( IN  ) &
  :: fnm         & !vertical interpolation coefficientt
   , fnp

 INTEGER,                                               INTENT( IN  ) &
 :: ids, ide, jds, jde, kds, kde, &
    ims, ime, jms, jme, kms, kme, &
    ips, ipe, jps, jpe, kps, kpe, &
    its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER:: i, j, k, kend

!-----------------------------------------------------------------------

  kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts, kend
  DO i = its, ite
    tatu(i,k,j) = 0.5*( trec(i-1,k,j) + trec(i,k,j) )
    tatv(i,k,j) = 0.5*( trec(i,k,j-1) + trec(i,k,j) )
    utatu(i,k,j) = urec(i,k,j)*tatu(i,k,j)
    vtatv(i,k,j) = vrec(i,k,j)*tatv(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  DO j = jts, jte
  DO k = kts+1, kend
  DO i = its, ite
    tatw(i,k,j) = fnm(k)*trec(i,k,j) + fnp(k)*trec(i,k-1,j)
    wtatw(i,k,j) = wrec(i,k,j)* tatw(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  DO j = jts, jte
  DO i = its, ite
    tatw(i,kts,j) = 0.0
    wtatw(i,kts,j) = 0.0
  ENDDO
  ENDDO


  RETURN

END SUBROUTINE proj_tuvw

SUBROUTINE proj_quvw( tatu, tatv, tatw,             &
                     utatu, vtatv, wtatw,          &
                     urec, vrec, wrec, trec,       &
                     fnm, fnp,                     &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     ips, ipe, jps, jpe, kps, kpe, &
                     its, ite, jts, jte, kts, kte  )

!-----------------------------------------------------------------------
! PURPOSE: Project trec to u, v and w-nodes
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT(INOUT) &
  :: tatu        & !t projected to u-nodes               (K)
   , tatv        & !t projected to v-nodes               (K)
   , tatw        & !t projected to w-nodes               (K)
   , utatu       & !u*tatu                               (K)
   , vtatv       & !v*tatv                               (K)
   , wtatw         !w*tatw                               (K)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: urec        & !urec at u-nodes                      (m s-1)
   , vrec        & !vrec at v-nodes                      (m s-1)
   , wrec        & !wrec at w-nodes                      (m s-1)
   , trec          !trec at t-nodes                      (K)

  REAL, DIMENSION( kms:kme ),                            INTENT( IN  ) &
  :: fnm         & !vertical interpolation coefficientt
   , fnp

 INTEGER,                                               INTENT( IN  ) &
 :: ids, ide, jds, jde, kds, kde, &
    ims, ime, jms, jme, kms, kme, &
    ips, ipe, jps, jpe, kps, kpe, &
    its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER:: i, j, k, kend

!-----------------------------------------------------------------------

  kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts, kend
  DO i = its, ite
    tatu(i,k,j) = 0.5*( trec(i-1,k,j) + trec(i,k,j) )
    tatv(i,k,j) = 0.5*( trec(i,k,j-1) + trec(i,k,j) )
    utatu(i,k,j) = urec(i,k,j)*tatu(i,k,j)
    vtatv(i,k,j) = vrec(i,k,j)*tatv(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  DO j = jts, jte
  DO k = kts+1, kend
  DO i = its, ite
    tatw(i,k,j) = fnm(k)*trec(i,k,j) + fnp(k)*trec(i,k-1,j)
    wtatw(i,k,j) = wrec(i,k,j)* tatw(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  DO j = jts, jte
  DO i = its, ite
    tatw(i,kts,j) = 0.0
    wtatw(i,kts,j) = 0.0
  ENDDO
  ENDDO


  RETURN

END SUBROUTINE proj_quvw

!=======================================================================

SUBROUTINE rsfs_filter( varfilt, var, vars, variable, ifilt, &
                        ids, ide, jds, jde, kds, kde,        &
                        ims, ime, jms, jme, kms, kme,        &
                        ips, ipe, jps, jpe, kps, kpe,        &
                        its, ite, jts, jte, kts, kte         )

!-----------------------------------------------------------------------
! PURPOSE: apply a 3-D tophat filter of width 2*<dx,dy,dz> to var.
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

    CASE ('hc') ! trec at cell center, centered in z................

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
!        vart(i,kend,j) = vart(i,kend-1,j)                         ! TOP: NVG
        vart(i,kend,j) = 2.0*vart(i,kend-1,j)-vart(i,kend-2,j)     ! TOP: KBC
!         vart(i,kbgn,j) = - vart(i,kbgn+1,j)                      ! SFC: P 0
!         vart(i,kbgn,j) =   vart(i,kbgn+1,j)                      ! SFC: NVG
        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)          ! SFC: KBC
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

END SUBROUTINE rsfs_filter

!=======================================================================

SUBROUTINE rmij( tau, var1, var2, var3, variable, &
                 ids, ide, jds, jde, kds, kde,    &
                 ims, ime, jms, jme, kms, kme,    &
                 ips, ipe, jps, jpe, kps, kpe,    &
                 its, ite, jts, jte, kts, kte     )

!-----------------------------------------------------------------------
! PURPOSE: Compute the RSFS stresses
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: tau           !RSFS stress                          (m^2 s^-2)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var1        & !filtered RSFS velocity               (m s^-1)
   , var2        & !filtered RSFS velocity               (m s^-1)
   , var3          !filtered RSFS velocity^2             (m^2 s^-2)

  CHARACTER,                                             INTENT( IN  ) &
  :: variable

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

   REAL, DIMENSION( kms:kme ) &
  :: fac    

  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend

!-----------------------------------------------------------------------

  kbgn = kts
  IF ( (variable .EQ. 'e') .OR. (variable .EQ. 'f') .OR. (variable .EQ. 'w') ) kbgn = kts+1

  DO j = jts, jte  ! this zeroes tau at the top level.
  DO i = its, ite
    tau(i,kte,j) = 0.0
  ENDDO
  ENDDO

!-----------------------------------------------------------------------

      DO j = jts, jte
      DO k =  kbgn, kte-1
      DO i = its, ite
        tau(i,k,j) = var3(i,k,j) - var1(i,k,j)*var2(i,k,j) 
      ENDDO
      ENDDO
      ENDDO

  IF ( variable .EQ. 'h' ) THEN

  kbgn = kts+1 

  DO j = jts, jte  ! this zeroes tau at the top level.
  DO i = its, ite
    tau(i,kte,j) = 0.0
    tau(i,kte-1,j) = 0.0
  ENDDO
  ENDDO

!-----------------------------------------------------------------------

      DO j = jts, jte
      DO k =  kbgn, kte-2
      DO i = its, ite
        tau(i,k,j) = var3(i,k,j) - var1(i,k,j)*var2(i,k,j) 
      ENDDO
      ENDDO
      ENDDO

ENDIF

  RETURN

END SUBROUTINE rmij

!=======================================================================

SUBROUTINE zero_rsfs( var11, var22, var33,          &
                      var12, var13, var23,          &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      ips, ipe, jps, jpe, kps, kpe, &
                      its, ite, jts, jte, kts, kte  )

!-----------------------------------------------------------------------
! PURPOSE: Zero the input tensor 
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),        INTENT( INOUT ) &
  :: var11    & !
   , var22    & !
   , var33    & !
   , var12    & !
   , var13    & !
   , var23      !

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER :: i, j, k

!-----------------------------------------------------------------------

  DO j = jts-1, jte+1
  DO k = kts, kte
  DO i = its-1, ite+1
    var11(i,k,j) = 0.0
    var22(i,k,j) = 0.0
    var33(i,k,j) = 0.0
    var12(i,k,j) = 0.0
    var13(i,k,j) = 0.0
    var23(i,k,j) = 0.0
  END DO
  END DO
  END DO

  RETURN

END SUBROUTINE zero_rsfs

!=======================================================================

 END MODULE module_sfs_rsfs
