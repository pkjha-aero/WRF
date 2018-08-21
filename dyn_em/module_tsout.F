!WRF
!
MODULE module_tsout

USE module_dm, ONLY: wrf_dm_max_real !JDM

CONTAINS

!=======================================================================
!             cell_centered_calc.F
!
!  This subroutine calculates cell centered velocity componentes and TKE
!
!  Branko Kosovic, March 17, 2010
!  JDM-SWIFT Modified for column output for MMC, June, 2015

SUBROUTINE cell_centered_calc( uts, vts, wts, tts, pts, tkets, zts,       &
                               t11ts, t22ts, t33ts, t12ts, t13ts, t23ts,  &
                               h3ts, usts, z0ts,                          &
                               u, v, w, t, p, pb, tke, dz8w,              &
                               t11, t22, t33, t12, t13, t23,              &
                               h3, ust, z0,                               &
                               ts_loc_i, ts_loc_j,                        &
                               ids, ide, jds, jde, kds, kde,              &
                               ims, ime, jms, jme, kms, kme,              &
                               its, ite, jts, jte, kts, kte                )
!
   IMPLICIT NONE

   REAL, DIMENSION(kms:kme), INTENT( OUT ) :: uts, vts, wts,        &
                                              tts, pts, tkets, zts, &
                                              t11ts, t22ts, t33ts,  &
                                              t12ts, t13ts, t23ts,  &
                                              h3ts 

   REAL, INTENT( OUT ) :: usts, z0ts

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT( IN ) :: u, v, w,        &
                                                             t, p, pb,       &
                                                             tke, dz8w, t11, &
                                                             t22, t33, t12,  & 
                                                             t13, t23, h3 
                                                         
   REAL, DIMENSION(ims:ime,jms:jme), INTENT( IN ) :: ust, z0 

   INTEGER, INTENT( IN ) :: ts_loc_i, ts_loc_j
 
   INTEGER , INTENT( IN  ) :: ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte

! local variables
   INTEGER :: i, j, k, i_col, j_col
   REAL :: max_val

!-----------------------------------------------------------------------
! executable starts here
!

   DO k=kts,MIN(kte,kde-1)
      uts(k)  = -9999.0
      vts(k)  = -9999.0
      wts(k)  = -9999.0
      tts(k)  = -9999.0
      pts(k)  = -9999.0
      tkets(k)  = -9999.0
      t11ts(k) = -9999.0
      t22ts(k) = -9999.0
      t33ts(k) = -9999.0
      t12ts(k) = -9999.0
   ENDDO

   DO k=kts,MIN(kte,kde)
      zts(k)  = -9999.0
      t13ts(k) = -9999.0
      t23ts(k) = -9999.0
      h3ts(k)  = -9999.0
   ENDDO

   usts = -9999.0
   z0ts = -9999.0

IF ( ( ts_loc_i .GE. its ) .AND. ( ts_loc_i .LE. ite ) ) THEN

  IF ( ( ts_loc_j .GE. jts ) .AND. ( ts_loc_j .LE. jte ) ) THEN

     DO k=kts,MIN(kte,kde-1)
        uts(k)  = 0.5 * ( u(ts_loc_i,k,ts_loc_j) + u(ts_loc_i+1,k,ts_loc_j) )
        vts(k)  = 0.5 * ( v(ts_loc_i,k,ts_loc_j) + v(ts_loc_i,k,ts_loc_j+1) )
        wts(k)  = 0.5 * ( w(ts_loc_i,k,ts_loc_j) + w(ts_loc_i,k+1,ts_loc_j) )
        tts(k)  = t(ts_loc_i,k,ts_loc_j)
        pts(k)  = p(ts_loc_i,k,ts_loc_j) + pb(ts_loc_i,k,ts_loc_j)
        tkets(k) = tke(ts_loc_i,k,ts_loc_j)
        t11ts(k) = t11(ts_loc_i,k,ts_loc_j)
        t22ts(k) = t22(ts_loc_i,k,ts_loc_j)
        t33ts(k) = t33(ts_loc_i,k,ts_loc_j)
        t12ts(k) = 0.25 * (   t12(ts_loc_i,k,ts_loc_j)   + t12(ts_loc_i+1,k,ts_loc_j)  & 
                            + t12(ts_loc_i,k,ts_loc_j+1) + t12(ts_loc_i+1,k,ts_loc_j+1)  )
        t13ts(k) = 0.5 * ( t13(ts_loc_i,k,ts_loc_j) + t13(ts_loc_i+1,k,ts_loc_j) ) 
        t23ts(k) = 0.5 * ( t23(ts_loc_i,k,ts_loc_j) + t23(ts_loc_i,k,ts_loc_j+1) ) 
        h3ts(k)  = h3(ts_loc_i,k,ts_loc_j) 
     ENDDO

     t13ts(kde) = 0.5 * ( t13(ts_loc_i,kde,ts_loc_j) + t13(ts_loc_i+1,kde,ts_loc_j) ) 
     t23ts(kde) = 0.5 * ( t23(ts_loc_i,kde,ts_loc_j) + t23(ts_loc_i,kde,ts_loc_j+1) ) 
     h3ts(kde)  = h3(ts_loc_i,kde,ts_loc_j) 
  
     zts(kts) = 0.0
     DO k=kts+1,MIN(kte,kde)
        zts(k) = zts(k-1) + dz8w(ts_loc_i,k,ts_loc_j)
     ENDDO

     usts = ust(ts_loc_i,ts_loc_j)  
     z0ts = z0(ts_loc_i,ts_loc_j)  

  ENDIF

ENDIF

!   Obtain global maxima

    DO k=kts,MIN(kte,kde-1)
      uts(k) = wrf_dm_max_real( uts(k) )
      vts(k) = wrf_dm_max_real( vts(k) )
      wts(k) = wrf_dm_max_real( wts(k) )
      tts(k) = wrf_dm_max_real( tts(k) )
      pts(k) = wrf_dm_max_real( pts(k) )
      tkets(k) = wrf_dm_max_real( tkets(k) )

      t11ts(k) = wrf_dm_max_real( t11ts(k) )
      t22ts(k) = wrf_dm_max_real( t22ts(k) )
      t33ts(k) = wrf_dm_max_real( t33ts(k) )
      t12ts(k) = wrf_dm_max_real( t12ts(k) )
    ENDDO

    DO k=kts,MIN(kte,kde)
      zts(k) = wrf_dm_max_real( zts(k) )
      t13ts(k) = wrf_dm_max_real( t13ts(k) )
      t23ts(k) = wrf_dm_max_real( t23ts(k) )
      h3ts(k)  = wrf_dm_max_real( h3ts(k) )
    ENDDO

    usts = wrf_dm_max_real( usts )
    z0ts = wrf_dm_max_real( z0ts )

!     DO k=kts,MIN(kte,kde-1)
!       call wrf_dm_maxval_real(ucts(k),its,jts)
!       call wrf_dm_maxval_real(vcts(k),its,jts)
!       call wrf_dm_maxval_real(wcts(k),its,jts)
!       call wrf_dm_maxval_real(tcts(k),its,jts)
!       call wrf_dm_maxval_real(pcts(k),its,jts)
!       call wrf_dm_maxval_real(zets(k),its,jts)
!     ENDDO

     RETURN

END SUBROUTINE cell_centered_calc

END MODULE module_tsout
