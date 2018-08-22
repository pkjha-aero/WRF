!WRF:MODEL_LAYER:PHYSICS

!=============================================================================
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
! Implemented 10/2008 by Jeff Mirocha, jmirocha@llnl.gov
!
!=============================================================================


MODULE module_sfs_lasd

CONTAINS

SUBROUTINE proj_lasd_c( var1c, var2c, var3c,           &
                        var4c, var5c, var6c,           &
                        var7c, var8c, var9c,           &
                        var10c,var11c,var12c,          &
                        var1, var2, var3,              &
                        ids, ide, jds, jde, kds, kde,  &
                        ims, ime, jms, jme, kms, kme,  &
                        ips, ipe, jps, jpe, kps, kpe,  &
                        its, ite, jts, jte, kts, kte     )

!-----------------------------------------------------------------------
! PURPOSE: project u, v and w to c-nodes
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: var1c       & !u projected to c                     (m s^-1)
   , var2c       & !u projected to c                     (m s^-1)
   , var3c       & !v projected to c                     (m s^-1)
   , var4c       & !v projected to c                     (m s^-1)
   , var5c       & !w projected to c                     (m s^-1)
   , var6c       & !w projected to c                     (m s^-1)
   , var7c       & !L11 = uc*uc                                  (m^2 s^-2)
   , var8c       & !L22 = vc*vc                                  (m^2 s^-2)
   , var9c       & !L33 = wc*wc                                  (m^2 s^-2)
   , var10c      & !L12 = uc*vc                                  (m^2 s^-2)
   , var11c      & !L13 = uc*wc                                 (m^2 s^-2)
   , var12c        !L23 = vc*wc                                 (m^2 s^-2)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var1        & !urec at u nodes                      (m s^-1)
   , var2        & !vrec at v nodes                      (m s^-1)
   , var3          !wrec at w nodes                      (m s^-1)

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER:: i, j, k

!-----------------------------------------------------------------------

  DO j = jts, jte
  DO k = kts, kte-1
  DO i = its, ite
    var1c(i,k,j) = 0.5*( var1(i+1,k,j) + var1(i,k,j) )
    var2c(i,k,j) = var1c(i,k,j)
    var3c(i,k,j) = 0.5*( var2(i,k,j+1) + var2(i,k,j) )
    var4c(i,k,j) = var3c(i,k,j)
    var5c(i,k,j) = 0.5*( var3(i,k+1,j) + var3(i,k,j) )
    var6c(i,k,j) = var5c(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  DO j = jts,jte
  DO k = kts,kte-1
  DO i = its,ite
    var7c (i,k,j) = var1c(i,k,j)*var1c(i,k,j)
    var8c (i,k,j) = var3c(i,k,j)*var3c(i,k,j)
    var9c (i,k,j) = var5c(i,k,j)*var5c(i,k,j)
    var10c(i,k,j) = var1c(i,k,j)*var3c(i,k,j)
    var11c(i,k,j) = var1c(i,k,j)*var5c(i,k,j)
    var12c(i,k,j) = var3c(i,k,j)*var5c(i,k,j)
  ENDDO
  ENDDO
  ENDDO


  RETURN

END SUBROUTINE proj_lasd_c

!=======================================================================

SUBROUTINE lasd_filter( varfilt, var , vars, variable, dim,   &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        ips, ipe, jps, jpe, kps, kpe,         &
                        its, ite, jts, jte, kts, kte          )

!-----------------------------------------------------------------------
! PURPOSE: apply a 2-D tophat filter of width 2*<dx,dy,dz> to var.
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: varfilt       !Filtered variable

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var           !input variable

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( IN  ) &
  :: vars          !surface value

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

  CHARACTER (LEN=4):: variable  

  INTEGER :: dim

! LOCAL VARIABLES

  REAL, DIMENSION( its-1:ite+1, kts-1:kte+1, jts-1:jte+1 ) & !not going to work for open MP
  :: fx, fxy, vart            !variable filtered in x direction

  REAL :: a, b, c

  INTEGER :: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend

!-----------------------------------------------------------------------

 IF (variable == 'toph') THEN

 !Tophat-filter function coefficients

  a = 0.25
  b = 0.5
  c = 0.25

 ELSEIF (variable == 'test') THEN

 !Test-filter function coefficients

  a = 0.5
  b = 0.0
  c = 0.5

 ENDIF

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
        vart(i,kend,j) = vart(i,kend-1,j)                  ! TOP: NVG
        vart(i,kbgn,j) = vars(i,j)                         ! SFC: KBC
      ENDDO
      ENDDO

IF (dim .EQ. 2) THEN  ! 2D filter starts here

  kbgn = kts
  kend = kte-1

! Filter in x-direction
  DO j = jts-1, jte+1
  DO k = kbgn, kend
  DO i = its, ite
    fx(i,k,j) = a*vart(i-1,k,j) + b*vart(i,k,j) + c*vart(i+1,k,j)
  ENDDO
  ENDDO
  ENDDO

! Filter in y-direction
  DO j = jts, jte
  DO k = kbgn, kend
  DO i = its, ite
    varfilt(i,k,j) = a*fx(i,k,j-1)+ b*fx(i,k,j) + c*fx(i,k,j+1)
  END DO
  END DO
  ENDDO

ELSEIF (dim .EQ. 3) then   ! 3D filter starts here

 kbgn = kts-1
 kend = kte

! Filter in x-direction
  DO j = jts-1, jte+1
  DO k = kbgn, kend
  DO i = its, ite
    fx(i,k,j) = a*vart(i-1,k,j) + b*vart(i,k,j) + c*vart(i+1,k,j)
  ENDDO
  ENDDO
  ENDDO

! Filter in y-direction
  DO j = jts, jte
  DO k = kbgn, kend
  DO i = its, ite
    fxy(i,k,j) = a*fx(i,k,j-1)+ b*fx(i,k,j) + c*fx(i,k,j+1)
  END DO
  END DO
  ENDDO

! Filter in z-direction
  DO j = jts, jte
  DO k = kbgn+1, kend-1
  DO i = its, ite
    varfilt(i,k,j) = a*fxy(i,k+1,j)+ b*fxy(i,k,j) + c*fxy(i,k-1,j)
  END DO
  END DO
  ENDDO

ENDIF  ! 3D filter ends here


  RETURN

END SUBROUTINE lasd_filter


!=======================================================================

SUBROUTINE uvw_s_c_lasd ( usc, vsc, ws,                 &
                          uusc, vvsc, wws,              &
                          uvsd, uwse, vwsf,             &
                          u,                            &
                          v,                            &
                          w,                            &
                          uu,                           &
                          vv,                           &
                          ww,                           &
                          uv,                           &
                          uw,                           &
                          vw,                           &
                          ht, rdx, rdy,                 &
                          cf1, cf2, cf3,                &
                          lasdvsbc,                     &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe, &
                          its, ite, jts, jte, kts, kte  )

!-----------------------------------------------------------------------
! PURPOSE: This subroutine calculates the value of the velocities
!          at the surface that are required prior to test filtering 
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( OUT ) &
  :: usc         & ! surface u                           (m s^-1)
   , vsc         & ! surface v                           (m s^-1)
   , ws          & ! surface w                           (m s^-1)
   , uusc        & !
   , vvsc        & !
   , wws         & !
   , uvsd        & !
   , uwse        & !
   , vwsf        

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: u           &
   , v           &
   , w           &
   , uu          &
   , vv          &
   , ww          &
   , uv          &
   , uw          &
   , vw                   

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( IN  ) &
  :: ht            !terrain height                       (m)

  INTEGER,                                               INTENT( IN  ) &
  :: lasdvsbc

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

  IF ( lasdvsbc .EQ. 0 ) THEN !STANDARD WRF EXTRAPOLATION 

    DO j=jts, jte ! project u and v to surface
    DO i=its, ite

      usc(i,j) = cf1*u(i,1,j)   + cf2*u(i,2,j)   + cf3*u(i,3,j) 
      vsc(i,j) = cf1*v(i,1,j)   + cf2*v(i,2,j)   + cf3*v(i,3,j)
      ws(i,j)  = cf1*w(i,1,j)   + cf2*w(i,2,j)   + cf3*w(i,3,j)
      uusc(i,j)= cf1*uu(i,1,j)  + cf2*uu(i,2,j)  + cf3*uu(i,3,j)
      vvsc(i,j)= cf1*vv(i,1,j)  + cf2*vv(i,2,j)  + cf3*vv(i,3,j)
      wws(i,j) = cf1*ww(i,1,j)  + cf2*ww(i,2,j)  + cf3*ww(i,3,j)
      uvsd(i,j)= cf1*uv(i,1,j)  + cf2*uv(i,2,j)  + cf3*uv(i,3,j)
      uwse(i,j)= cf1*uw(i,1,j)  + cf2*uw(i,2,j)  + cf3*uw(i,3,j)
      vwsf(i,j)= cf1*vw(i,1,j)  + cf2*vw(i,2,j)  + cf3*vw(i,3,j)


    ENDDO
    ENDDO


  ELSEIF ( lasdvsbc .EQ. 1 ) THEN !ZERO VERTICAL GRADIENT

  DO j=jts, jte
  DO i=its, ite

    usc(i,j) = u(i,1,j)
    vsc(i,j) = v(i,1,j)
    ws(i,j)  = w(i,1,j)
    uusc(i,j)= uu(i,1,j)
    vvsc(i,j)= vv(i,1,j)
    wws(i,j) = ww(i,1,j)
    uvsd(i,j)= uv(i,1,j)
    uwse(i,j)= uw(i,1,j)
    vwsf(i,j)= vw(i,1,j)

  ENDDO
  ENDDO


  ELSEIF ( lasdvsbc .EQ. 2 ) THEN !NO SLIP

    DO j=jts, jte
    DO i=its, ite

      usc(i,j) = 0.0
      vsc(i,j) = 0.0   
      ws(i,j)  = 0.0  
      uusc(i,j)= 0.0
      vvsc(i,j)= 0.0
      wws(i,j) = 0.0  
      uvsd(i,j)= 0.0
      uwse(i,j)= 0.0
      vwsf(i,j)= 0.0

    ENDDO
    ENDDO

  ENDIF

  RETURN

END SUBROUTINE uvw_s_c_lasd


!=======================================================================

SUBROUTINE calc_Lij(  vart, var1, var2, var3,          &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      ips, ipe, jps, jpe, kps, kpe,    &
                      its, ite, jts, jte, kts, kte     )

!-----------------------------------------------------------------------
! PURPOSE: Compute Lij
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: vart           !Lij                                (m^2 s^-2)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var1        & !filtered     velocity               (m s^-1)
   , var2        & !filtered     velocity               (m s^-1)
   , var3          !filtered     velocity^2             (m^2 s^-2)

!  CHARACTER,                                             INTENT( IN  ) &
!  :: variable

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend

!-----------------------------------------------------------------------
!
!  kbgn = kts
!  IF ( (variable .EQ. 'e') .OR. (variable .EQ. 'f') ) kbgn = kts+1
!
!  DO j = jts, jte  ! this zeroes tau at the top level.
!  DO i = its, ite
!    tau(i,kte,j) = 0.0
!  ENDDO
!  ENDDO
!
!-----------------------------------------------------------------------

      DO j = jts, jte
      DO k = kts, kte-1
      DO i = its, ite
       vart(i,k,j) = var3(i,k,j) - var1(i,k,j)*var2(i,k,j)
      ENDDO
      ENDDO
      ENDDO

  RETURN

END SUBROUTINE calc_Lij

!=============================================================================

SUBROUTINE calc_half_sij_c ( ss11, ss22, ss33,                &
                             ss12, ss13, ss23,                &
                             s11, s22, s33,                   &
                             s12, s13, s23,                   &
                             s12_cc, s13_cc, s23_cc,          &
                             defor_opt,                       &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe,    &
                             its, ite, jts, jte, kts, kte     )

!-----------------------------------------------------------------------------
! PURPOSE: divide Sij by 2 and project them to cell centers
!-----------------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: ss11         & ! deformation element 11            (s-1)
   , ss22         & ! deformation element 22            (s-1)
   , ss33         & ! deformation element 33            (s-1)
   , ss12         & ! deformation element 12            (s-1)
   , ss13         & ! deformation element 13            (s-1)
   , ss23           ! deformation element 23            (s-1)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: s11          & ! 2*deformation element 11            (s-1)
   , s22          & ! 2*deformation element 22            (s-1)
   , s33          & ! 2*deformation element 33            (s-1)
   , s12          & ! 2*deformation element 12            (s-1)
   , s13          & ! 2*deformation element 13            (s-1)
   , s23            ! 2*deformation element 23            (s-1)
 
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: s12_cc       & ! 2*deformation element 12 @ cc       (s-1)
   , s13_cc       & ! 2*deformation element 13 @ cc       (s-1)
   , s23_cc         ! 2*deformation element 23 @ cc       (s-1)

  INTEGER,                                               INTENT( IN ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

  INTEGER,                                               INTENT( IN ) &
  :: defor_opt

! LOCAL VARIABLES

  INTEGER :: i, j, k

!-----------------------------------------------------------------------------

  IF ( defor_opt .EQ. 0 ) THEN 

    DO j=jts,jte
    DO k=kts,kte-1 !JDM changed to -1 08/16
    DO i=its,ite
      ss11(i,k,j) = 0.5*s11(i,k,j)
      ss22(i,k,j) = 0.5*s22(i,k,j)
      ss33(i,k,j) = 0.5*s33(i,k,j) 

!JDM 09/2018      ss12(i,k,j) = 0.25*( s12(i  ,k,j) + s12(i  ,k,j+1) + &
!JDM 09/2018                           s12(i+1,k,j) + s12(i+1,k,j+1) )
!JDM 09/2018
!JDM 09/2018      ss13(i,k,j) = 0.25*( s13(i  ,k+1,j) + s13(i  ,k,j) + &
!JDM 09/2018                           s13(i+1,k+1,j) + s13(i+1,k,j) )
!JDM 09/2018
!JDM 09/2018      ss23(i,k,j) = 0.25*( s23(i,k+1,j  ) + s23(i,k,j  ) + &
!JDM 09/2018                           s23(i,k+1,j+1) + s23(i,k,j+1) )

    END DO
    END DO
    END DO

    DO j=jts,jte
    DO k=kts,kte-1 !JDM changed to -1 08/16
    DO i=its,ite
     
      ss12(i,k,j) = 0.5*0.25*( s12(i  ,k,j) + s12(i  ,k,j+1) + &
                           s12(i+1,k,j) + s12(i+1,k,j+1) )

      ss13(i,k,j) = 0.5*0.25*( s13(i  ,k+1,j) + s13(i  ,k,j) + &
                           s13(i+1,k+1,j) + s13(i+1,k,j) )

      ss23(i,k,j) = 0.5*0.25*( s23(i,k+1,j  ) + s23(i,k,j  ) + &
                           s23(i,k+1,j+1) + s23(i,k,j+1) )
    END DO
    END DO
    END DO

  ELSE

    DO j=jts,jte
    DO k=kts,kte-1 !JDM changed to -1 08/16
    DO i=its,ite
      ss11(i,k,j) = 0.5*s11(i,k,j)
      ss22(i,k,j) = 0.5*s22(i,k,j)
      ss33(i,k,j) = 0.5*s33(i,k,j) 
      ss12(i,k,j) = 0.5*s12_cc(i,k,j)
      ss13(i,k,j) = 0.5*s13_cc(i,k,j) 
      ss23(i,k,j) = 0.5*s23_cc(i,k,j)
    END DO
    END DO
    END DO

  ENDIF

  RETURN

END SUBROUTINE calc_half_sij_c

!=============================================================================

SUBROUTINE calc_sijsij( sijsij,                          &                
                        ss11, ss22, ss33,                &
                        ss12, ss13, ss23,                &
                        ids, ide, jds, jde, kds, kde,    &
                        ims, ime, jms, jme, kms, kme,    &
                        ips, ipe, jps, jpe, kps, kpe,    &
                        its, ite, jts, jte, kts, kte     )

!-----------------------------------------------------------------------------
! PURPOSE: Compute |S|
!-----------------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: sijsij        ! S_ij*S_ij                           (s-2)
 
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: ss11         & ! deformation element 11            (s-1)
   , ss22         & ! deformation element 22            (s-1)
   , ss33         & ! deformation element 33            (s-1)
   , ss12         & ! deformation element 12            (s-1)
   , ss13         & ! deformation element 13            (s-1)
   , ss23           ! deformation element 23            (s-1)

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, ktf

!-----------------------------------------------------------------------------

  ktf = min(kte,kde-1)
  i_start = its
  i_end   = ite 
  j_start = jts
  j_end   = jte

  DO j=jts,jte
  DO k=kts,kte-1 !JDM changed to -1 08/16
  DO i=its,ite
    sijsij(i,k,j) = ss11(i,k,j)*ss11(i,k,j) + &
                    ss22(i,k,j)*ss22(i,k,j) + &
                    ss33(i,k,j)*ss33(i,k,j) + &
                2.0*ss12(i,k,j)*ss12(i,k,j) + &
                2.0*ss13(i,k,j)*ss13(i,k,j) + & 
                2.0*ss23(i,k,j)*ss23(i,k,j)
  END DO
  END DO
  END DO

  DO j=jts,jte
  DO k=kts,kte-1 !JDM changed to -1 08/16
  DO i=its,ite
    sijsij(i,k,j) = sqrt(2.0*sijsij(i,k,j))
  END DO
  END DO
  END DO

  RETURN

END SUBROUTINE calc_sijsij

!=============================================================================

SUBROUTINE calc_Sij_bar_hat( s11bar, s11hat, s22bar,           &
                             s22hat, s33bar, s33hat,           &
                             s12bar, s12hat, s13bar,           &
                             s13hat, s23bar, s23hat,           &
                             ss11, ss22, ss33,                 &
                             ss12, ss13, ss23,                 &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             ips, ipe, jps, jpe, kps, kpe,     &
                             its, ite, jts, jte, kts, kte      )

!-----------------------------------------------------------------------
! PURPOSE: calculate sijbar and sijhat
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: s11bar       & !S11bar                                (m s^-1)
   , s11hat       & !S11hat=S11bar                         (m s^-1)
   , s22bar       & !S22bar                                (m s^-1)
   , s22hat       & !S22hat=S22bar                         (m s^-1)
   , s33bar       & !S33bar                                (m s^-1)
   , s33hat       & !S33hat=S33bar                         (m s^-1)
   , s12bar       & !S12bar                                (m s^-1)
   , s12hat       & !S12hat=S12bar                         (m s^-1)
   , s13bar       & !S13bar                                (m s^-1)
   , s13hat       & !S13hat=S13bar                         (m s^-1)
   , s23bar       & !S23bar                                (m s^-1)
   , s23hat         !S23hat=S23bar                         (m s^-1)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: ss11         & !deformation element 11             (s-1)!
   , ss22         & !deformation element 22             (s-1)!                    
   , ss33         & !deformation element 33             (s-1)!
   , ss12         & !deformation element 12             (s-1)!
   , ss13         & !deformation element 13             (s-1)!                    
   , ss23           !deformation element 23             (s-1)!
 

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES
     
  INTEGER:: i, j, k

!-----------------------------------------------------------------------


  DO j=jts,jte
  DO k=kts,kte-1 !JDM changed to -1 08/16
  DO i=its,ite
    s11bar(i,k,j) =  ss11(i,k,j)
    s11hat(i,k,j) =  s11bar(i,k,j) 
    s22bar(i,k,j) =  ss22(i,k,j)
    s22hat(i,k,j) =  s22bar(i,k,j)
    s33bar(i,k,j) =  ss33(i,k,j)  
    s33hat(i,k,j) =  s33bar(i,k,j)
  END DO
  END DO
  END DO

  DO j=jts,jte
  DO k=kts,kte-1 !JDM changed to -1 08/16
  DO i=its,ite
    s12bar(i,k,j) = ss12(i,k,j) 
    s12hat(i,k,j) = s12bar(i,k,j)
  END DO
  END DO
  END DO

  DO j=jts,jte
  DO k=kts,kte-1 !JDM changed to -1 08/16
  DO i=its,ite
    s13bar(i,k,j) = ss13(i,k,j)
    s13hat(i,k,j) = s13bar(i,k,j)
  END DO
  END DO
  END DO

  DO j=jts,jte
  DO k=kts,kte-1 !JDM changed to -1 08/16
  DO i=its,ite
    s23bar(i,k,j) = ss23(i,k,j) 
    s23hat(i,k,j) = s23bar(i,k,j)
  END DO
  END DO
  END DO


  RETURN

END SUBROUTINE calc_Sij_bar_hat

!=============================================================================

SUBROUTINE calc_s_sij(  s_s11, s_s22, s_s33,             &
                        s_s12, s_s13, s_s23,             &
                        s11, s22, s33,                   &
                        s12, s13, s23,                   &
                        ss,                              &
                        ids, ide, jds, jde, kds, kde,    &
                        ims, ime, jms, jme, kms, kme,    &
                        ips, ipe, jps, jpe, kps, kpe,    &
                        its, ite, jts, jte, kts, kte     )

!-----------------------------------------------------------------------
! PURPOSE: Compute S_Sij
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: s_s11, s_s22, s_s33,      &   ! S*Sij             
     s_s12, s_s13, s_s23           ! S*Sij                 

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: s11,  s22,  s33     &  ! resolved Sij      
   , s12,  s13,  s23     &  ! resolved Sij 
   , ss                     ! |S|

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER:: i, j, k

!-----------------------------------------------------------------------

      DO j = jts, jte
      DO k = kts, kte-1 !JDM changed to -1 08/16
      DO i = its, ite
       s_s11(i,k,j) =  s11(i,k,j) * ss(i,k,j)
       s_s22(i,k,j) =  s22(i,k,j) * ss(i,k,j)
       s_s33(i,k,j) =  s33(i,k,j) * ss(i,k,j)
       s_s12(i,k,j) =  s12(i,k,j) * ss(i,k,j)
       s_s13(i,k,j) =  s13(i,k,j) * ss(i,k,j)
       s_s23(i,k,j) =  s23(i,k,j) * ss(i,k,j)       
      ENDDO
      ENDDO
      ENDDO

  RETURN

END SUBROUTINE calc_s_sij

!=======================================================================

SUBROUTINE calc_Gij(  g11, g22, g33, g12, g13, g23,    &
                      s_bar,                           &
                      s11_bar, s22_bar, s33_bar,       &
                      s12_bar, s13_bar, s23_bar,       &
                      rdzw, dx, dy,                    &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      ips, ipe, jps, jpe, kps, kpe,    &
                      its, ite, jts, jte, kts, kte     )

!-----------------------------------------------------------------------
! PURPOSE: Compute Gij
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( INOUT ) &
  :: g11, g22, g33, g12, g13, g23       !Gij            

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: s_bar                             & !test filtered S               
   , s11_bar, s22_bar, s33_bar        & !test filtered Sij           
   , s12_bar, s13_bar, s23_bar        & !test filtered Sij      
   , rdzw                               ! 1/dz at w-levels   

  REAL,                                                  INTENT( IN  ) &
  :: dx          & ! grid spacing in x                   (m)
   , dy            ! grid spacing in y                   (m)


  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte


! LOCAL VARIABLES

  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend

  REAL :: tf1, tf1_2, delta ! Model parameters, tf1=test filter-1
  
!-----------------------------------------------------------------------

  tf1=2.0  ! test filter 1 / grid size

  tf1_2=tf1**2

      DO j = jts, jte
      DO k = kts, kte-1 !JDM changed to -1 08/16
      DO i = its, ite

      delta = ( dx * dy / rdzw(i,k,j) )**0.33333333

       g11(i,k,j)=2*(delta**2)*(g11(i,k,j)             &
                 -tf1_2*s_bar(i,k,j)*s11_bar(i,k,j))
       g22(i,k,j)=2*(delta**2)*(g22(i,k,j)             &
                 -tf1_2*s_bar(i,k,j)*s22_bar(i,k,j))
       g33(i,k,j)=2*(delta**2)*(g33(i,k,j)             &
                 -tf1_2*s_bar(i,k,j)*s33_bar(i,k,j))
       g12(i,k,j)=2*(delta**2)*(g12(i,k,j)             &
                 -tf1_2*s_bar(i,k,j)*s12_bar(i,k,j))
       g13(i,k,j)=2*(delta**2)*(g13(i,k,j)             &
                 -tf1_2*s_bar(i,k,j)*s13_bar(i,k,j))
       g23(i,k,j)=2*(delta**2)*(g23(i,k,j)             &
                 -tf1_2*s_bar(i,k,j)*s23_bar(i,k,j))

      ENDDO
      ENDDO
      ENDDO

  RETURN

END SUBROUTINE calc_Gij

!=============================================================================

SUBROUTINE calc_Nij(  n11, n22, n33, n12, n13, n23,    &
                      s_hat,                           &
                      s11_hat, s22_hat, s33_hat,       &
                      s12_hat, s13_hat, s23_hat,       &
                      rdzw, dx, dy,                    &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      ips, ipe, jps, jpe, kps, kpe,    &
                      its, ite, jts, jte, kts, kte     )

!-----------------------------------------------------------------------
! PURPOSE: Compute Nij
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( INOUT ) &
  :: n11, n22, n33, n12, n13, n23       !Nij            

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: s_hat                            & !2*test filtered S               
   , s11_hat, s22_hat, s33_hat        & !2*test filtered Sij           
   , s12_hat, s13_hat, s23_hat        & !2*test filtered Sij      
   , rdzw                               ! 1/dz at w-levels   

  REAL,                                                  INTENT( IN  ) &
  :: dx          & ! grid spacing in x                   (m)
   , dy            ! grid spacing in y                   (m)


  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte


! LOCAL VARIABLES

  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend

  REAL :: tf2, tf2_2, delta ! Model parameters, tf2=test filter-2
  
!-----------------------------------------------------------------------

  tf2=4.0  ! test filter 2 / grid size

  tf2_2=tf2**2

      DO j = jts, jte
      DO k = kts, kte-1 !JDM changed to -1 08/16
      DO i = its, ite

      delta = ( dx * dy / rdzw(i,k,j) )**0.33333333

       n11(i,k,j)=2*(delta**2)*(n11(i,k,j)             &
                 -tf2_2*s_hat(i,k,j)*s11_hat(i,k,j))
       n22(i,k,j)=2*(delta**2)*(n22(i,k,j)             &
                 -tf2_2*s_hat(i,k,j)*s22_hat(i,k,j))
       n33(i,k,j)=2*(delta**2)*(n33(i,k,j)             &
                 -tf2_2*s_hat(i,k,j)*s33_hat(i,k,j))
       n12(i,k,j)=2*(delta**2)*(n12(i,k,j)             &
                 -tf2_2*s_hat(i,k,j)*s12_hat(i,k,j))
       n13(i,k,j)=2*(delta**2)*(n13(i,k,j)             &
                 -tf2_2*s_hat(i,k,j)*s13_hat(i,k,j))
       n23(i,k,j)=2*(delta**2)*(n23(i,k,j)             &
                 -tf2_2*s_hat(i,k,j)*s23_hat(i,k,j))

      ENDDO
      ENDDO
      ENDDO

  RETURN

END SUBROUTINE calc_Nij

!=============================================================================

SUBROUTINE calc_contraction   ( lg,                           &                
                                l11, l22, l33, l12, l13, l23, &
                                g11, g22, g33, g12, g13, g23, &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, kpe, &
                                its, ite, jts, jte, kts, kte  )

!-----------------------------------------------------------------------------
! PURPOSE: Compute LG, GG, QN, NN
!-----------------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: lg            ! L_ij*G_ij                           (s-2)
 
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: l11, l22, l33, l12, l13, l23         & ! Lij           (s-1)
   , g11, g22, g33, g12, g13, g23           ! Gij           (s-1)

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte


  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, ktf

!-----------------------------------------------------------------------------

  ktf = min(kte,kde-1)
  i_start = its
  i_end   = ite 
  j_start = jts
  j_end   = jte


  DO j=j_start,j_end
  DO k=kts,ktf
  DO i=i_start,i_end
    lg(i,k,j) =     l11(i,k,j)*g11(i,k,j) + &
                    l22(i,k,j)*g22(i,k,j) + &
                    l33(i,k,j)*g33(i,k,j) + &
                2.0*l12(i,k,j)*g12(i,k,j) + &
                2.0*l13(i,k,j)*g13(i,k,j) + &
                2.0*l23(i,k,j)*g23(i,k,j)
  END DO
  END DO
  END DO

  RETURN

END SUBROUTINE calc_contraction

!=============================================================================

SUBROUTINE calc_ev_lasd( cs,                           &
                         xkmh, xkmv,                   &
                         xkhh, xkhv,                   &
                         LG  , GG  , QN  , NN  ,       &                
                         F_LG, F_GG, F_QN, F_NN, SS,   &
                         prandtl,                      &
                         lasd_opt,                     &
                         nested,                       &
                         rdzw, dx , dy , dt,           & 
                         rdz,                          &             
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         ips, ipe, jps, jpe, kps, kpe, &
                         its, ite, jts, jte, kts, kte  )

!-----------------------------------------------------------------------------
! PURPOSE: Compute cs (dynamic Smagorinsky coefficient--actually c^2)
!-----------------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( INOUT ) &
  :: cs                          ! ( dynamic Smagorinsky coefficient )^2    
    
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( INOUT  ) &
  :: xkmh, xkmv, xkhh, xkhv  

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: LG, GG, QN, NN,  SS     !
  
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( INOUT  ) &
  :: F_LG, F_GG, F_QN, F_NN  !  --integrals--
  
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: rdzw, rdz                              ! 1/dz   

  REAL,                                                  INTENT( IN  ) &
  :: dx          & ! grid spacing in x               (m)
   , dy          & ! grid spacing in y               (m) 
   , dt          & ! physical time step              (sec)
   , prandtl

  LOGICAL,                                               INTENT( IN  ) & 
  :: nested

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER :: i, j, k

  REAL :: tf1, tf2, powcoeff, lagran_dt, delta, opftdelta, Tn, dumfac, &
          epsi2, epsi4, cs_opt2_2d, cs_opt2_4d, beta, betaclip, h, alfa, count

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                         &
  :: z                            ! distance to the wall 

  REAL, DIMENSION( kms:kme )                         &
  :: LG_avg, GG_avg, QN_avg, NN_avg        ! plane-averaged contractions   

  INTEGER :: lasd_opt

  REAL :: dxx, dyy, dd ! model parameters

!-----------------------------------------------------------------------------

  tf1=2.0  ! test filter 1 / grid size
  tf2=4.0  ! test filter 2 / grid size

  powcoeff  = -1.0/8.0       ! cut off coefficient for Beta=cs(2Delta)/cs(Delta)

  lagran_dt = dt             ! time step used in the lagrangian computations

  h = 15

!  c_s = config_flags%c_s
    
  DO j=jts,jte 
  DO k=kts,kte-1
  DO i=its,ite

  if (k.eq.kts) then 
   z(i,k,j) = 1.0/rdz(i,k,j)
  else
   z(i,k,j) = z(i,k-1,j) + 1.0/rdz(i,k,j)
  endif

  END DO
  END DO
  END DO

  DO k=kts,kte-1
 
  count = 0.0 
  LG_avg(k) = 0.0
  GG_avg(k) = 0.0
  QN_avg(k) = 0.0
  NN_avg(k) = 0.0 

  DO j=jts,jte 
  DO i=its,ite

  LG_avg(k) = LG_avg(k)+LG(i,k,j)
  GG_avg(k) = GG_avg(k)+GG(i,k,j)
  QN_avg(k) = QN_avg(k)+QN(i,k,j)
  NN_avg(k) = NN_avg(k)+NN(i,k,j)  
  count = count + 1.0

  END DO
  END DO

  LG_avg(k) = LG_avg(k)/count
  GG_avg(k) = GG_avg(k)/count
  QN_avg(k) = QN_avg(k)/count
  NN_avg(k) = NN_avg(k)/count

  END DO  


  if (lasd_opt.EQ.1) then ! scale-independent dynamic model

  DO j=jts,jte
  DO k=kts,kte-1
  DO i=its,ite

  delta = ( dx * dy / rdzw(i,k,j) )**0.33333333 
  opftdelta = 1.5*delta 

  F_LG(i,k,j) = max(real(1E-24),real(F_LG(i,k,j)))

  cs_opt2_2d = F_LG(i,k,j) / F_GG(i,k,j)
  cs_opt2_2d = max(real(1E-24),real(cs_opt2_2d))

  cs(i,k,j) = cs_opt2_2d
  cs(i,k,j) = max(real(1E-24),real(cs(i,k,j)))

  xkmh(i,k,j) = cs(i,k,j)*delta*delta*SS(i,k,j)
!JDM 11_29_2012   xkmh(i,k,j) =  max(0.1,cs(i,k,j)*delta*delta*SS(i,k,j))!JDM 11_29_2012 
  xkmv(i,k,j) = xkmh(i,k,j)
  xkhh(i,k,j) = xkmv(i,k,j)/prandtl
  xkhv(i,k,j) = xkhh(i,k,j) 

  END DO
  END DO
  END DO

  else if (lasd_opt.EQ.2) then ! Lagrangian-averaged scale-independent dynamic model 

  DO j=jts,jte
  DO k=kts,kte-1
  DO i=its,ite

  delta = ( dx * dy / rdzw(i,k,j) )**0.33333333 
  opftdelta = 1.5*delta 

  Tn = opftdelta*((F_LG(i,k,j)*F_GG(i,k,j))**powcoeff) 
  Tn = max(real(1E-24),real(Tn))
  dumfac = lagran_dt/Tn
  epsi2 = dumfac / (1.0+dumfac)

  F_LG(i,k,j) = (epsi2*LG(i,k,j) + (1.0-epsi2)*F_LG(i,k,j))
  F_LG(i,k,j) = max(real(1E-24),real(F_LG(i,k,j)))

  F_GG(i,k,j) = (epsi2*GG(i,k,j) + (1.0-epsi2)*F_GG(i,k,j))

  cs_opt2_2d = F_LG(i,k,j) / F_GG(i,k,j)
  cs_opt2_2d = max(real(1E-24),real(cs_opt2_2d))

  cs(i,k,j) = cs_opt2_2d
  cs(i,k,j) = max(real(1E-24),real(cs(i,k,j)))
  
  xkmh(i,k,j) = cs(i,k,j)*delta*delta*SS(i,k,j)
  xkmv(i,k,j) = xkmh(i,k,j)
  xkhh(i,k,j) = xkmv(i,k,j)/prandtl
  xkhv(i,k,j) = xkhh(i,k,j) 


  END DO
  END DO
  END DO

  else if (lasd_opt.EQ.3) then !Lagrangian-averaged scale-dependent dynamic model

  DO j=jts,jte
  DO k=kts,kte-1
  DO i=its,ite

!  dd = 10
!
!    dxx = dx
!    dyy = dy
!
!    IF ( nested )  THEN
!
!       IF ( (i-ids) .LE. dd ) THEN
!
!           dxx = dx*( (3.0 - 1.0)*( ( dd - float(i-ids) )/dd ) + 1.0 )
!           dyy = dy*( (3.0 - 1.0)*( ( dd - float(i-ids) )/dd ) + 1.0 )
!           if (( k .eq. 5 ) .and. ( j .eq. 10 )) print*,'k,dxx,dyy',k,dxx,dyy
!       ENDIF 
!       IF ( (j-jds) .LE. dd ) THEN
!
!           dxx = dx*( (3.0 - 1.0)*( ( dd - float(j-jds) )/dd ) + 1.0 )
!           dyy = dy*( (3.0 - 1.0)*( ( dd - float(j-jds) )/dd ) + 1.0 )
!
!       ENDIF 
!       IF ( (ide-i) .LE. dd ) THEN
!
!           dxx = dx*( (3.0 - 1.0)*( ( dd - float(ide-i) )/dd ) + 1.0 )
!           dyy = dy*( (3.0 - 1.0)*( ( dd - float(ide-i) )/dd ) + 1.0 )
!
!       ENDIF 
!       IF ( (jde-j) .LE. dd ) THEN
!
!           dxx = dx*( (3.0 - 1.0)*( ( dd - float(jde-j) )/dd ) + 1.0 )
!           dyy = dy*( (3.0 - 1.0)*( ( dd - float(jde-j) )/dd ) + 1.0 )
!
!       ENDIF 
!
!    ENDIF

  delta = ( dx * dy / rdzw(i,k,j) )**0.33333333
!  delta = ( (dx/3.0) * (dy/3.0) / rdzw(i,k,j) )**0.33333333  
  opftdelta = 1.5*delta 

  Tn = opftdelta*((F_LG(i,k,j)*F_GG(i,k,j))**powcoeff) 
  Tn = max(real(1E-24),real(Tn))
  dumfac = lagran_dt/Tn
  epsi2 = dumfac / (1.0+dumfac)

  Tn = opftdelta*((F_QN(i,k,j)*F_NN(i,k,j))**powcoeff) 
  Tn = max(real(1E-24),real(Tn))
  dumfac = lagran_dt/Tn
  epsi4 = dumfac / (1.0+dumfac)


  F_LG(i,k,j) = (epsi2*LG(i,k,j) + (1.0-epsi2)*F_LG(i,k,j))
  F_LG(i,k,j) = max(real(1E-24),real(F_LG(i,k,j)))

  F_GG(i,k,j) = (epsi2*GG(i,k,j) + (1.0-epsi2)*F_GG(i,k,j))

  F_QN(i,k,j) = (epsi4*QN(i,k,j) + (1.0-epsi4)*F_QN(i,k,j))
  F_QN(i,k,j) = max(real(1E-24),real(F_QN(i,k,j)))

  F_NN(i,k,j) = (epsi4*NN(i,k,j) + (1.0-epsi4)*F_NN(i,k,j))

  cs_opt2_2d = F_LG(i,k,j) / F_GG(i,k,j)
  cs_opt2_2d = max(real(1E-24),real(cs_opt2_2d))

  cs_opt2_4d = F_QN(i,k,j) / F_NN(i,k,j)
  cs_opt2_4d = max(real(1E-24),real(cs_opt2_4d))

  beta = (cs_opt2_4d/cs_opt2_2d)**(log(tf1)/(log(tf2)-log(tf1)))
  betaclip = max(real(beta),real(1.0/(tf1*tf2)))

  cs(i,k,j) = cs_opt2_2d/betaclip
  cs(i,k,j) = max(real(1E-24),real(cs(i,k,j)))
 
  xkmh(i,k,j) = cs(i,k,j)*delta*delta*SS(i,k,j)
  xkmv(i,k,j) = xkmh(i,k,j)
  xkhh(i,k,j) = xkmv(i,k,j)/prandtl
  xkhv(i,k,j) = xkhh(i,k,j) 

!  if ((i.eq.12).and.(j.eq.12)) then
!   print*, 'k=', k, 'QN', QN(i,k,j), 'F_QN', F_QN(i,k,j)
!   print*, 'k=', k, 'GG', GG(i,k,j), 'F_GG', F_GG(i,k,j)
!   print*, 'k=', k, 'NN', NN(i,k,j), 'F_NN', F_NN(i,k,j)
!  endif

  END DO
  END DO
  END DO

  else if (lasd_opt.EQ.4) then !Lagrangian-averaged scale-dependent dynamic model -- calculate the dynamic coefficient

  DO j=jts,jte
  DO k=kts,kte-1
  DO i=its,ite

  delta = ( dx * dy / rdzw(i,k,j) )**0.33333333 
  opftdelta = 1.5*delta 

  Tn = opftdelta*((F_LG(i,k,j)*F_GG(i,k,j))**powcoeff) 
  Tn = max(real(1E-24),real(Tn))
  dumfac = lagran_dt/Tn
  epsi2 = dumfac / (1.0+dumfac)

  Tn = opftdelta*((F_QN(i,k,j)*F_NN(i,k,j))**powcoeff) 
  Tn = max(real(1E-24),real(Tn))
  dumfac = lagran_dt/Tn
  epsi4 = dumfac / (1.0+dumfac)

  F_LG(i,k,j) = (epsi2*LG(i,k,j) + (1.0-epsi2)*F_LG(i,k,j))
  F_LG(i,k,j) = max(real(1E-24),real(F_LG(i,k,j)))

  F_GG(i,k,j) = (epsi2*GG(i,k,j) + (1.0-epsi2)*F_GG(i,k,j))

  F_QN(i,k,j) = (epsi4*QN(i,k,j) + (1.0-epsi4)*F_QN(i,k,j))
  F_QN(i,k,j) = max(real(1E-24),real(F_QN(i,k,j)))

  F_NN(i,k,j) = (epsi4*NN(i,k,j) + (1.0-epsi4)*F_NN(i,k,j))

  cs_opt2_2d = F_LG(i,k,j) / F_GG(i,k,j)
  cs_opt2_2d = max(real(1E-24),real(cs_opt2_2d))

  cs_opt2_4d = F_QN(i,k,j) / F_NN(i,k,j)
  cs_opt2_4d = max(real(1E-24),real(cs_opt2_4d))

  beta = (cs_opt2_4d/cs_opt2_2d)**(log(tf1)/(log(tf2)-log(tf1)))
  betaclip = max(real(beta),real(1.0/(tf1*tf2)))

  cs(i,k,j) = cs_opt2_2d/betaclip
  cs(i,k,j) = max(real(1E-24),real(cs(i,k,j)))
  
  END DO
  END DO
  END DO

  endif


  RETURN

END SUBROUTINE calc_ev_lasd

!=============================================================================

SUBROUTINE interpolag_sdep  ( F_LG, F_GG, F_QN, F_NN,       &
                              FF_LG, FF_GG, FF_QN, FF_NN,   &                
                              rdz, dx, dy, dt,              &
                              u, v, w,                      &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte  )

!-----------------------------------------------------------------------------
! PURPOSE: Compute interpolated F_LG, F_GG, F_QN and F_NN
!-----------------------------------------------------------------------------

  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: F_LG, F_GG, F_QN, F_NN            ! lagrangian integrals   

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN ) &
  :: FF_LG, FF_GG, FF_QN, FF_NN        ! arrays to do interpolations          
 
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: rdz                               ! 1/dz at uvw-levels   

  REAL,                                                  INTENT( IN  ) &
  :: dx          & ! grid spacing in x               (m)
   , dy          & ! grid spacing in y               (m) 
   , dt            ! physical time step              (sec)

 REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: u, v, w                           ! resolved velocities (m/s)  

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER :: i, j, k, jx, jy, jz, addx, addy, addz, jxaddx, jyaddy, jzaddz

  REAL :: uc, vc, wc, xp, yp, zp, comp_x, comp_y, comp_z, frac_x, frac_y, frac_z

!-----------------------------------------------------------------------------

  DO jy = jts, jte
  DO jz = kts, kte-1
  DO jx = its, ite

    uc = 0.5*( u(jx+1,jz  ,jy  ) + u(jx,jz,jy) )
    vc = 0.5*( v(jx  ,jz  ,jy+1) + v(jx,jz,jy) )
    wc = 0.5*( w(jx  ,jz+1,jy  ) + w(jx,jz,jy) )   

    xp = -uc * dt / dx
    yp = -vc * dt / dy
   if (wc.lt.0) then
    zp = -wc * dt * rdz(jx,jz+1,jy)
   else
    zp = -wc * dt * rdz(jx,jz  ,jy)
   endif

    addx = int(sign(1.0,xp))
    addy = int(sign(1.0,yp))
    addz = int(sign(1.0,zp))

    jxaddx = jx + addx
    jyaddy = jy + addy
    jzaddz = jz + addz

    comp_x = abs(xp) 
    comp_y = abs(yp)
    comp_z = abs(zp)
    frac_x = 1.0-comp_x
    frac_y = 1.0-comp_y
    frac_z = 1.0-comp_z

    if ((jz.eq.1).and.(wc.gt.0)) then

 F_LG(jx,jz,jy) =   frac_y*(FF_LG(jx, jz, jy    )*frac_x+FF_LG(jxaddx, jz, jy    )*comp_x) &
                  + comp_y*(FF_LG(jx, jz, jyaddy)*frac_x+FF_LG(jxaddx, jz, jyaddy)*comp_x)

 F_GG(jx,jz,jy) =   frac_y*(FF_GG(jx, jz, jy    )*frac_x+FF_GG(jxaddx, jz, jy    )*comp_x) &
                  + comp_y*(FF_GG(jx, jz, jyaddy)*frac_x+FF_GG(jxaddx, jz, jyaddy)*comp_x)

 F_QN(jx,jz,jy) =   frac_y*(FF_QN(jx, jz, jy    )*frac_x+FF_QN(jxaddx, jz, jy    )*comp_x) &
                  + comp_y*(FF_QN(jx, jz, jyaddy)*frac_x+FF_QN(jxaddx, jz, jyaddy)*comp_x)

 F_NN(jx,jz,jy) =   frac_y*(FF_NN(jx, jz, jy    )*frac_x+FF_NN(jxaddx, jz, jy    )*comp_x) &
                  + comp_y*(FF_NN(jx, jz, jyaddy)*frac_x+FF_NN(jxaddx, jz, jyaddy)*comp_x)

    else

 F_LG(jx,jz,jy) =   frac_x*frac_y*(FF_LG(jx    ,jz,jy    )*frac_z + FF_LG(jx    ,jzaddz,jy    )*comp_z) &
                  + frac_x*comp_y*(FF_LG(jx    ,jz,jyaddy)*frac_z + FF_LG(jx    ,jzaddz,jyaddy)*comp_z) &
                  + comp_x*frac_y*(FF_LG(jxaddx,jz,jy    )*frac_z + FF_LG(jxaddx,jzaddz,jy    )*comp_z) &
                  + comp_x*comp_y*(FF_LG(jxaddx,jz,jyaddy)*frac_z + FF_LG(jxaddx,jzaddz,jyaddy)*comp_z)

 F_GG(jx,jz,jy) =   frac_x*frac_y*(FF_GG(jx    ,jz,jy    )*frac_z + FF_GG(jx    ,jzaddz,jy    )*comp_z) &
                  + frac_x*comp_y*(FF_GG(jx    ,jz,jyaddy)*frac_z + FF_GG(jx    ,jzaddz,jyaddy)*comp_z) &
                  + comp_x*frac_y*(FF_GG(jxaddx,jz,jy    )*frac_z + FF_GG(jxaddx,jzaddz,jy    )*comp_z) &
                  + comp_x*comp_y*(FF_GG(jxaddx,jz,jyaddy)*frac_z + FF_GG(jxaddx,jzaddz,jyaddy)*comp_z)

 F_QN(jx,jz,jy) =   frac_x*frac_y*(FF_QN(jx    ,jz,jy    )*frac_z + FF_QN(jx    ,jzaddz,jy    )*comp_z) &
                  + frac_x*comp_y*(FF_QN(jx    ,jz,jyaddy)*frac_z + FF_QN(jx    ,jzaddz,jyaddy)*comp_z) &
                  + comp_x*frac_y*(FF_QN(jxaddx,jz,jy    )*frac_z + FF_QN(jxaddx,jzaddz,jy    )*comp_z) &
                  + comp_x*comp_y*(FF_QN(jxaddx,jz,jyaddy)*frac_z + FF_QN(jxaddx,jzaddz,jyaddy)*comp_z)

 F_NN(jx,jz,jy) =   frac_x*frac_y*(FF_NN(jx    ,jz,jy    )*frac_z + FF_NN(jx    ,jzaddz,jy    )*comp_z) &
                  + frac_x*comp_y*(FF_NN(jx    ,jz,jyaddy)*frac_z + FF_NN(jx    ,jzaddz,jyaddy)*comp_z) &
                  + comp_x*frac_y*(FF_NN(jxaddx,jz,jy    )*frac_z + FF_NN(jxaddx,jzaddz,jy    )*comp_z) &
                  + comp_x*comp_y*(FF_NN(jxaddx,jz,jyaddy)*frac_z + FF_NN(jxaddx,jzaddz,jyaddy)*comp_z)

    endif


  END DO
  END DO
  END DO

  RETURN

END SUBROUTINE interpolag_sdep

!=============================================================================

SUBROUTINE initialize_F  (   FF_LG,FF_GG,FF_QN,FF_NN,      &
                             F_LG, F_GG, F_QN, F_NN,       &  
                             GG, NN,                       &
                             itimestep,                    &
                             restart,                      &
                             ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             ips, ipe, jps, jpe, kps, kpe, &
                             its, ite, jts, jte, kts, kte  )


  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( INOUT ) &
  :: F_LG, F_GG, F_QN, F_NN,      &  ! F_LG, F_GG, F_QN, F_NN  --integrals--
     FF_LG, FF_GG, FF_QN, FF_NN      ! FF_LG, FF_GG, FF_QN, FF_NN --arrays to do interpolations--

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: GG , NN                 ! GG, NN

  INTEGER,                                               INTENT( IN  ) &
  :: itimestep

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER :: i, j, k

  LOGICAL :: restart

!-----------------------------------------------------------------------------

!    IF ( ( itimestep .eq. 1 ) .or. (restart .eq. true) ) THEN
    IF  ( itimestep .eq. 1 )  THEN

  DO j=jts,jte
  DO k=kts,kte-1
  DO i=its,ite

   F_GG(i,k,j) = max( real(1.0e-10),GG(i,k,j) )
   F_LG(i,k,j) = 0.03*F_GG(i,k,j)
   F_NN(i,k,j) = max( real(1.0e-10),NN(i,k,j) )
   F_QN(i,k,j) = 0.03*F_NN(i,k,j)

  END DO
  END DO
  END DO

    ENDIF

  DO j=jts,jte
  DO k=kts,kte-1
  DO i=its,ite

   FF_LG(i,k,j) = F_LG(i,k,j)
   FF_GG(i,k,j) = F_GG(i,k,j)
   FF_QN(i,k,j) = F_QN(i,k,j)
   FF_NN(i,k,j) = F_NN(i,k,j)

  END DO
  END DO
  END DO


  RETURN

END SUBROUTINE initialize_F

!=============================================================================

SUBROUTINE specified_F  (   FF_LG,FF_GG,FF_QN,FF_NN,      &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            ips, ipe, jps, jpe, kps, kpe, &
                            its, ite, jts, jte, kts, kte  )


  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( INOUT ) &
  :: FF_LG, FF_GG, FF_QN, FF_NN      ! FF_LG, FF_GG, FF_QN, FF_NN --arrays to do interpolations--

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

! LOCAL VARIABLES

  INTEGER :: i, j, k

!-----------------------------------------------------------------------------

   IF ( its == ids ) THEN

      DO k = kts, kte
      DO j = jts, jte

         FF_LG(its-1,k,j) = FF_LG(its,k,j)
         FF_GG(its-1,k,j) = FF_GG(its,k,j)
         FF_QN(its-1,k,j) = FF_QN(its,k,j)
         FF_NN(its-1,k,j) = FF_NN(its,k,j)

      ENDDO
      ENDDO

   ENDIF

   IF ( ite == ide ) THEN

      DO k = kts, kte
      DO j = jts, jte

         FF_LG(ite+1,k,j) = FF_LG(ite,k,j)
         FF_GG(ite+1,k,j) = FF_GG(ite,k,j)
         FF_QN(ite+1,k,j) = FF_QN(ite,k,j)
         FF_NN(ite+1,k,j) = FF_NN(ite,k,j)

      ENDDO
      ENDDO

   ENDIF

   IF ( jts == jds ) THEN

      DO k = kts, kte
      DO i = its-1, ite+1

         FF_LG(i,k,jts-1) = FF_LG(i,k,jts)
         FF_GG(i,k,jts-1) = FF_GG(i,k,jts)
         FF_QN(i,k,jts-1) = FF_QN(i,k,jts)
         FF_NN(i,k,jts-1) = FF_NN(i,k,jts)

      ENDDO
      ENDDO

   ENDIF

   IF ( jte == jde ) THEN

      DO k = kts, kte
      DO i = its-1, ite+1

         FF_LG(i,k,jte+1) = FF_LG(i,k,jte)
         FF_GG(i,k,jte+1) = FF_GG(i,k,jte)
         FF_QN(i,k,jte+1) = FF_QN(i,k,jte)
         FF_NN(i,k,jte+1) = FF_NN(i,k,jte)

      ENDDO
      ENDDO

   ENDIF

  RETURN

END SUBROUTINE specified_F

!=======================================================================

SUBROUTINE nw_canopy( nwtau13, nwtau23,             &
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
   , canheight & !
   , k_ref       ! KG

!-----------------------------------------------------------------------

  PI=3.14159265359
  ternopt = 1 
  g0=9.81     
  epsilon = 0.0000001
  vk = 0.4

  canheight = 1

!  canfact = 0.8 !!! JEFF OPTIMUM !!!
!  canfact = 0.66
!  canfact = 0.78 !!! OPTIMUM for 2-D filter!!!
!!  canfact = 0.9 !!! KG OPTIMUM 3-D filter -- NEUTRAL CASE!!!
!!  canfact = 0.5 !!! KG OPTIMUM 3-D filter -- UNSTABLE CASE!!!

!   canfact = 0.94 !!! KG OPTIMUM 3-D filter -- OUTER NEST!!!

!!   canfact = 0.78 !!! KG OPTIMUM 3-D filter -- OUTER NEST!!!

!!  if (nested)  then
!!   canfact = 0.65
!!  endif

!  if (nested)  then
!   canfact = 0.66
!  endif

!  IF ( dx .EQ. 99.0 ) THEN  !These work for dz=8.25; a=12 for 99 m domain, 4 for 33 m domain.
!
!    canfact = 0.90 
!
!  ENDIF
!
!  IF ( dx .EQ. 33.0 ) THEN!
!
!    canfact = 0.80
!
!  ENDIF
!
!  IF ( dx .EQ. 11.0 ) THEN!
!
!    canfact = 0.80
!
!  ENDIF
!
!
!canfact = 0.80 !JDM-12-2015 for alpha = 4
!!canfact = 0.67 !0.65 JDM-12-2015 for alpha = 1.3!

!  print*,dx,canfact


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

!  Hc = 3.0*dx !!! KG OPTIMUM !!!
  Hc = 4.0*dx !!! KG OPTIMUM --- NEUTRAL !!!
!  Hc = 3.0*dx !!! KG OPTIMUM --- UNSTABLE & STABLE !!!

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
!      a(i,k,j) = exp(-z(i,k,j)/(0.12*Hc)) !!! KG FINAL !!!
!      a(i,k,j) = (exp(-z(i,k,j)/(0.2*Hc)))**2 !!! KG FINAL !!!
!      a(i,k,j) = (exp(-z(i,k,j)/(0.18*Hc)))**2 !!! JEFF FINAL !!! Prior to 09/25.2017
!      a(i,k,j) = (exp(-z(i,k,j)/(1.0*Hc)))**2 !!
      a(i,k,j) = (exp(-z(i,k,j)/(1.0*Hc))) !!

    END DO
    DO k=kmax+1,kend
      a(i,k,j) = 0.0
    END DO
  END DO
  END DO


!-----------------------------------------------------------------------
! Define the canopy force dtau_can/dz and calculate the surface fluxes
!-----------------------------------------------------------------------

  k_ref = 1  ! reference k level at which the wall stress is calculated 

!JDM1130  DO j = jbgn, jend+1
!JDM1130   DO i = ibgn, iend+1
!JDM1130 
!JDM1130     zu1 = 2.0/( rdz(i-1,kts,j)+rdz(i,kts,j) )
!JDM1130     zv1 = 2.0/( rdz(i,kts,j-1)+rdz(i,kts,j) )
!JDM1130 
!JDM1130     Cd_u(i,j) = ( vk / LOG( ( zu1 + zrough )/zrough ) )**2
!JDM1130     Cd_v(i,j) = ( vk / LOG( ( zv1 + zrough )/zrough ) )**2
!JDM1130 
!JDM1130 
!JDM1130 !JDM    Cd_u(i,j)=( KARMAN /                                     &
!JDM1130 !JDM               LOG( (2./( rdz(i-1,kts,j) + rdz(i  ,kts,j) ) &   
!JDM1130 !JDM                          )/z_rough ) )**2
!JDM1130 !JDM
!JDM1130 !JDM    Cd_v(i,j)=( KARMAN /                                     &
!JDM1130 !JDM                LOG( (2./( rdz(i,kts,j-1) + rdz(i  ,kts,j) ) &   
!JDM1130 !JDM                          )/z_rough ) )**2
!JDM1130 
!JDM1130 !JDM!!KG    Cd_u(i,j)=( KARMAN /                                     &
!JDM1130 !JDM!!KG                LOG( (         z(i,k_ref,j)                      &   
!JDM1130 !JDM!!KG                              +z_rough )/z_rough ) )**2
!JDM1130 !JDM
!JDM1130 !JDM!!KG    Cd_v(i,j)=( KARMAN /                                     &
!JDM1130 !JDM!!KG                LOG( (         z(i,k_ref,j)                      &   
!JDM1130 !JDM!!KG                              +z_rough )/z_rough ) )**2
!JDM1130 !JDM
!JDM1130   ENDDO
!JDM1130   ENDDO
!JDM1130 
!JDM1130   DO j = jbgn,jend
!JDM1130   DO i = ibgn,iend
!JDM1130  
!JDM1130     V0_u=sqrt((u(i,kts,j)**2) +       &
!JDM1130              (((v(i  ,kts,j  )+       &
!JDM1130              v(i  ,kts,j+1)+          &
!JDM1130              v(i-1,kts,j  )+          &
!JDM1130              v(i-1,kts,j+1))/4.0)**2))+epsilon
!JDM1130 
!JDM1130     tao_xz=Cd_u(i,j)*V0_u*u(i,kts,j)
!JDM1130     usflx(i,j) = -tao_xz
!JDM1130 
!JDM1130     V0_v=sqrt((v(i,kts,j)**2) +       &
!JDM1130              (((u(i  ,kts,j  )+       &
!JDM1130              u(i  ,kts,j-1)+          &
!JDM1130              u(i+1,kts,j  )+          &
!JDM1130              u(i+1,kts,j-1))/4.0)**2))+epsilon
!JDM1130 
!JDM1130     tao_yz=Cd_v(i,j)*V0_v*v(i,kts,j)
!JDM1130     vsflx(i,j) = -tao_yz
!JDM1130 
!JDM1130 !JDM!!KG    V0_u=sqrt((u(i,k_ref,j)**2) +       &
!JDM1130 !JDM!!KG             (((v(i  ,k_ref,j  )+       &
!JDM1130 !JDM!!KG             v(i  ,k_ref,j+1)+          &
!JDM1130 !JDM!!KG             v(i-1,k_ref,j  )+          &
!JDM1130 !JDM!!KG             v(i-1,k_ref,j+1))/4.0)**2))+epsilon
!JDM1130 !JDM!!KG
!JDM1130 !JDM!!KG    tao_xz=Cd_u(i,j)*V0_u*u(i,k_ref,j)
!JDM1130 !JDM!!KG    usflx(i,j) = -tao_xz
!JDM1130 !JDM!!KG
!JDM1130 !JDM!!KG
!JDM1130 !JDM!!KG    V0_v=sqrt((v(i,k_ref,j)**2) +       &
!JDM1130 !JDM!!KG             (((u(i  ,k_ref,j  )+       &
!JDM1130 !JDM!!KG             u(i  ,k_ref,j-1)+          &
!JDM1130 !JDM!!KG             u(i+1,k_ref,j  )+          &
!JDM1130 !JDM!!KG             u(i+1,k_ref,j-1))/4.0)**2))+epsilon
!JDM1130 !JDM!!KG
!JDM1130 !JDM!!KG    tao_yz=Cd_v(i,j)*V0_v*v(i,k_ref,j)
!JDM1130 !JDM!!KG    vsflx(i,j) = -tao_yz
!JDM1130 !JDM!!KG
!JDM1130 !JDM!!KG    
!JDM1130   END DO
!JDM1130   END DO

!=====================================

    DO j = jbgn,jend
    DO i = ibgn,iend

       V0_u=0.
       tao_xz=0.
       V0_u=    sqrt((u(i,kts,j)**2) +         &
                        (((v(i  ,kts,j  )+          &
                           v(i  ,kts,j+1)+          &
                           v(i-1,kts,j  )+          &
                           v(i-1,kts,j+1))/4.0)**2))+epsilon
       ustar=0.5*(ust(i,j)+ust(i-1,j))
       tao_xz=ustar*ustar*u(i,kts,j)/V0_u

       usflx(i,j) = -tao_xz

!JDM!      IF ( ( i .EQ. 1 ) .AND. ( j .EQ. 1 ) ) THEN
!JDM!
!JDM!         print*,'ust(i,j), ust(i-1,j) ',ust(i,j), ust(i-1,j)
!JDM!
!JDM!      ENDIF

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
                           u(i+1,kts,j-1))/4.0)**2))+epsilon
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


!JDM1130  DO j = jbgn,jend
!JDM1130  DO i = ibgn,iend
!JDM1130  DO k=2,kmax+1
!JDM1130    nwtau13(i,k,j) = u_canopy(i,k,j) 
!JDM1130    nwtau23(i,k,j) = v_canopy(i,k,j) 
!JDM1130!    if ((i.eq.2).and.(j.eq.2).and.(k.eq.2)) print *, 'in lasd', nwtau13(i,k,j)
!JDM1130  END DO
!JDM1130  END DO
!JDM1130  END DO

 10 FORMAT(I3)
 100 FORMAT(18(x,I9))
 1000 FORMAT(i2,18(x,f9.5))

 RETURN

END SUBROUTINE nw_canopy



END MODULE module_sfs_lasd
