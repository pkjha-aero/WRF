































































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





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: var1c       & 
   , var2c       & 
   , var3c       & 
   , var4c       & 
   , var5c       & 
   , var6c       & 
   , var7c       & 
   , var8c       & 
   , var9c       & 
   , var10c      & 
   , var11c      & 
   , var12c        

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var1        & 
   , var2        & 
   , var3          

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER:: i, j, k



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



SUBROUTINE lasd_filter( varfilt, var , vars, variable, dim,   &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        ips, ipe, jps, jpe, kps, kpe,         &
                        its, ite, jts, jte, kts, kte          )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: varfilt       

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var           

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( IN  ) &
  :: vars          

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

  CHARACTER (LEN=4):: variable  

  INTEGER :: dim



  REAL, DIMENSION( its-1:ite+1, kts-1:kte+1, jts-1:jte+1 ) & 
  :: fx, fxy, vart            

  REAL :: a, b, c

  INTEGER :: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend



 IF (variable == 'toph') THEN

 

  a = 0.25
  b = 0.5
  c = 0.25

 ELSEIF (variable == 'test') THEN

 

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
        vart(i,kend,j) = vart(i,kend-1,j)                  
        vart(i,kbgn,j) = vars(i,j)                         
      ENDDO
      ENDDO

IF (dim .EQ. 2) THEN  

  kbgn = kts
  kend = kte-1


  DO j = jts-1, jte+1
  DO k = kbgn, kend
  DO i = its, ite
    fx(i,k,j) = a*vart(i-1,k,j) + b*vart(i,k,j) + c*vart(i+1,k,j)
  ENDDO
  ENDDO
  ENDDO


  DO j = jts, jte
  DO k = kbgn, kend
  DO i = its, ite
    varfilt(i,k,j) = a*fx(i,k,j-1)+ b*fx(i,k,j) + c*fx(i,k,j+1)
  END DO
  END DO
  ENDDO

ELSEIF (dim .EQ. 3) then   

 kbgn = kts-1
 kend = kte


  DO j = jts-1, jte+1
  DO k = kbgn, kend
  DO i = its, ite
    fx(i,k,j) = a*vart(i-1,k,j) + b*vart(i,k,j) + c*vart(i+1,k,j)
  ENDDO
  ENDDO
  ENDDO


  DO j = jts, jte
  DO k = kbgn, kend
  DO i = its, ite
    fxy(i,k,j) = a*fx(i,k,j-1)+ b*fx(i,k,j) + c*fx(i,k,j+1)
  END DO
  END DO
  ENDDO


  DO j = jts, jte
  DO k = kbgn+1, kend-1
  DO i = its, ite
    varfilt(i,k,j) = a*fxy(i,k+1,j)+ b*fxy(i,k,j) + c*fxy(i,k-1,j)
  END DO
  END DO
  ENDDO

ENDIF  


  RETURN

END SUBROUTINE lasd_filter




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






  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( OUT ) &
  :: usc         & 
   , vsc         & 
   , ws          & 
   , uusc        & 
   , vvsc        & 
   , wws         & 
   , uvsd        & 
   , uwse        & 
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
  :: ht            

  INTEGER,                                               INTENT( IN  ) &
  :: lasdvsbc

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

  REAL,                                                  INTENT( IN  ) &
  :: rdx         & 
    ,rdy         & 
    ,cf1         & 
    ,cf2         & 
    ,cf3           
 


  INTEGER:: i, j



  IF ( lasdvsbc .EQ. 0 ) THEN 

    DO j=jts, jte 
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


  ELSEIF ( lasdvsbc .EQ. 1 ) THEN 

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


  ELSEIF ( lasdvsbc .EQ. 2 ) THEN 

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




SUBROUTINE calc_Lij(  vart, var1, var2, var3,          &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      ips, ipe, jps, jpe, kps, kpe,    &
                      its, ite, jts, jte, kts, kte     )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: vart           

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var1        & 
   , var2        & 
   , var3          




  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend














      DO j = jts, jte
      DO k = kts, kte-1
      DO i = its, ite
       vart(i,k,j) = var3(i,k,j) - var1(i,k,j)*var2(i,k,j)
      ENDDO
      ENDDO
      ENDDO

  RETURN

END SUBROUTINE calc_Lij



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





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: ss11         & 
   , ss22         & 
   , ss33         & 
   , ss12         & 
   , ss13         & 
   , ss23           

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: s11          & 
   , s22          & 
   , s33          & 
   , s12          & 
   , s13          & 
   , s23            
 
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: s12_cc       & 
   , s13_cc       & 
   , s23_cc         

  INTEGER,                                               INTENT( IN ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

  INTEGER,                                               INTENT( IN ) &
  :: defor_opt



  INTEGER :: i, j, k



  IF ( defor_opt .EQ. 0 ) THEN 

    DO j=jts,jte
    DO k=kts,kte-1 
    DO i=its,ite
      ss11(i,k,j) = 0.5*s11(i,k,j)
      ss22(i,k,j) = 0.5*s22(i,k,j)
      ss33(i,k,j) = 0.5*s33(i,k,j) 










    END DO
    END DO
    END DO

    DO j=jts,jte
    DO k=kts,kte-1 
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
    DO k=kts,kte-1 
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



SUBROUTINE calc_sijsij( sijsij,                          &                
                        ss11, ss22, ss33,                &
                        ss12, ss13, ss23,                &
                        ids, ide, jds, jde, kds, kde,    &
                        ims, ime, jms, jme, kms, kme,    &
                        ips, ipe, jps, jpe, kps, kpe,    &
                        its, ite, jts, jte, kts, kte     )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: sijsij        
 
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: ss11         & 
   , ss22         & 
   , ss33         & 
   , ss12         & 
   , ss13         & 
   , ss23           

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, ktf



  ktf = min(kte,kde-1)
  i_start = its
  i_end   = ite 
  j_start = jts
  j_end   = jte

  DO j=jts,jte
  DO k=kts,kte-1 
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
  DO k=kts,kte-1 
  DO i=its,ite
    sijsij(i,k,j) = sqrt(2.0*sijsij(i,k,j))
  END DO
  END DO
  END DO

  RETURN

END SUBROUTINE calc_sijsij



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





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: s11bar       & 
   , s11hat       & 
   , s22bar       & 
   , s22hat       & 
   , s33bar       & 
   , s33hat       & 
   , s12bar       & 
   , s12hat       & 
   , s13bar       & 
   , s13hat       & 
   , s23bar       & 
   , s23hat         

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: ss11         & 
   , ss22         & 
   , ss33         & 
   , ss12         & 
   , ss13         & 
   , ss23           
 

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte


     
  INTEGER:: i, j, k




  DO j=jts,jte
  DO k=kts,kte-1 
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
  DO k=kts,kte-1 
  DO i=its,ite
    s12bar(i,k,j) = ss12(i,k,j) 
    s12hat(i,k,j) = s12bar(i,k,j)
  END DO
  END DO
  END DO

  DO j=jts,jte
  DO k=kts,kte-1 
  DO i=its,ite
    s13bar(i,k,j) = ss13(i,k,j)
    s13hat(i,k,j) = s13bar(i,k,j)
  END DO
  END DO
  END DO

  DO j=jts,jte
  DO k=kts,kte-1 
  DO i=its,ite
    s23bar(i,k,j) = ss23(i,k,j) 
    s23hat(i,k,j) = s23bar(i,k,j)
  END DO
  END DO
  END DO


  RETURN

END SUBROUTINE calc_Sij_bar_hat



SUBROUTINE calc_s_sij(  s_s11, s_s22, s_s33,             &
                        s_s12, s_s13, s_s23,             &
                        s11, s22, s33,                   &
                        s12, s13, s23,                   &
                        ss,                              &
                        ids, ide, jds, jde, kds, kde,    &
                        ims, ime, jms, jme, kms, kme,    &
                        ips, ipe, jps, jpe, kps, kpe,    &
                        its, ite, jts, jte, kts, kte     )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: s_s11, s_s22, s_s33,      &   
     s_s12, s_s13, s_s23           

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: s11,  s22,  s33     &  
   , s12,  s13,  s23     &  
   , ss                     

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER:: i, j, k



      DO j = jts, jte
      DO k = kts, kte-1 
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



SUBROUTINE calc_Gij(  g11, g22, g33, g12, g13, g23,    &
                      s_bar,                           &
                      s11_bar, s22_bar, s33_bar,       &
                      s12_bar, s13_bar, s23_bar,       &
                      rdzw, dx, dy,                    &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      ips, ipe, jps, jpe, kps, kpe,    &
                      its, ite, jts, jte, kts, kte     )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( INOUT ) &
  :: g11, g22, g33, g12, g13, g23       

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: s_bar                             & 
   , s11_bar, s22_bar, s33_bar        & 
   , s12_bar, s13_bar, s23_bar        & 
   , rdzw                               

  REAL,                                                  INTENT( IN  ) &
  :: dx          & 
   , dy            


  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte




  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend

  REAL :: tf1, tf1_2, delta 
  


  tf1=2.0  

  tf1_2=tf1**2

      DO j = jts, jte
      DO k = kts, kte-1 
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



SUBROUTINE calc_Nij(  n11, n22, n33, n12, n13, n23,    &
                      s_hat,                           &
                      s11_hat, s22_hat, s33_hat,       &
                      s12_hat, s13_hat, s23_hat,       &
                      rdzw, dx, dy,                    &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      ips, ipe, jps, jpe, kps, kpe,    &
                      its, ite, jts, jte, kts, kte     )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( INOUT ) &
  :: n11, n22, n33, n12, n13, n23       

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: s_hat                            & 
   , s11_hat, s22_hat, s33_hat        & 
   , s12_hat, s13_hat, s23_hat        & 
   , rdzw                               

  REAL,                                                  INTENT( IN  ) &
  :: dx          & 
   , dy            


  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte




  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend

  REAL :: tf2, tf2_2, delta 
  


  tf2=4.0  

  tf2_2=tf2**2

      DO j = jts, jte
      DO k = kts, kte-1 
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



SUBROUTINE calc_contraction   ( lg,                           &                
                                l11, l22, l33, l12, l13, l23, &
                                g11, g22, g33, g12, g13, g23, &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, kpe, &
                                its, ite, jts, jte, kts, kte  )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: lg            
 
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: l11, l22, l33, l12, l13, l23         & 
   , g11, g22, g33, g12, g13, g23           

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte


  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, ktf



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





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( INOUT ) &
  :: cs                          
    
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( INOUT  ) &
  :: xkmh, xkmv, xkhh, xkhv  

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: LG, GG, QN, NN,  SS     
  
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( INOUT  ) &
  :: F_LG, F_GG, F_QN, F_NN  
  
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: rdzw, rdz                              

  REAL,                                                  INTENT( IN  ) &
  :: dx          & 
   , dy          & 
   , dt          & 
   , prandtl

  LOGICAL,                                               INTENT( IN  ) & 
  :: nested

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER :: i, j, k

  REAL :: tf1, tf2, powcoeff, lagran_dt, delta, opftdelta, Tn, dumfac, &
          epsi2, epsi4, cs_opt2_2d, cs_opt2_4d, beta, betaclip, h, alfa, count

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                         &
  :: z                            

  REAL, DIMENSION( kms:kme )                         &
  :: LG_avg, GG_avg, QN_avg, NN_avg        

  INTEGER :: lasd_opt

  REAL :: dxx, dyy, dd 



  tf1=2.0  
  tf2=4.0  

  powcoeff  = -1.0/8.0       

  lagran_dt = dt             

  h = 15


    
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


  if (lasd_opt.EQ.1) then 

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

  xkmv(i,k,j) = xkmh(i,k,j)
  xkhh(i,k,j) = xkmv(i,k,j)/prandtl
  xkhv(i,k,j) = xkhh(i,k,j) 

  END DO
  END DO
  END DO

  else if (lasd_opt.EQ.2) then 

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

  else if (lasd_opt.EQ.3) then 

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
 
  xkmh(i,k,j) = cs(i,k,j)*delta*delta*SS(i,k,j)
  xkmv(i,k,j) = xkmh(i,k,j)
  xkhh(i,k,j) = xkmv(i,k,j)/prandtl
  xkhv(i,k,j) = xkhh(i,k,j) 







  END DO
  END DO
  END DO

  else if (lasd_opt.EQ.4) then 

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



SUBROUTINE interpolag_sdep  ( F_LG, F_GG, F_QN, F_NN,       &
                              FF_LG, FF_GG, FF_QN, FF_NN,   &                
                              rdz, dx, dy, dt,              &
                              u, v, w,                      &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte  )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: F_LG, F_GG, F_QN, F_NN            

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN ) &
  :: FF_LG, FF_GG, FF_QN, FF_NN        
 
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: rdz                               

  REAL,                                                  INTENT( IN  ) &
  :: dx          & 
   , dy          & 
   , dt            

 REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: u, v, w                           

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER :: i, j, k, jx, jy, jz, addx, addy, addz, jxaddx, jyaddy, jzaddz

  REAL :: uc, vc, wc, xp, yp, zp, comp_x, comp_y, comp_z, frac_x, frac_y, frac_z



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
  :: F_LG, F_GG, F_QN, F_NN,      &  
     FF_LG, FF_GG, FF_QN, FF_NN      

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: GG , NN                 

  INTEGER,                                               INTENT( IN  ) &
  :: itimestep

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER :: i, j, k

  LOGICAL :: restart




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



SUBROUTINE specified_F  (   FF_LG,FF_GG,FF_QN,FF_NN,      &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            ips, ipe, jps, jpe, kps, kpe, &
                            its, ite, jts, jte, kts, kte  )


  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( INOUT ) &
  :: FF_LG, FF_GG, FF_QN, FF_NN      

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER :: i, j, k



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
























  
  IMPLICIT NONE                                         

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT(OUT) &    
  :: nwtau13    & 
   , nwtau23      

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT(IN) &    
  :: ust      

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &    
  :: u         & 
   , v         & 
   , w         & 
   , rdz       & 
   , rdzw        

  REAL, DIMENSION( kms:kme ),                            INTENT( IN  ) &
  :: fnm         &  
   , fnp

  REAL,                                                  INTENT( IN  ) &   
  :: dx          & 
   , canfact       
      
  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &                             
     ims, ime, jms, jme, kms, kme, &                               
     ips, ipe, jps, jpe, kps, kpe, &                              
     its, ite, jts, jte, kts, kte       



  REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ) &
  :: u_canforce & 
   , v_canforce & 
   , a          & 
   , zw         & 
   , z            

  REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ) &
  :: u_can       & 
   , v_can       &
   , u_canopy    &
   , v_canopy 

  REAL, DIMENSION( its-1:ite+1, jts-1:jte+1 ) &   
  :: usflx     & 
   , vsflx     & 
   , C0        & 
   , D0        & 
   , kcan      & 
   , a0        & 
   , b0          

  REAL, DIMENSION( its-1:ite+1, jts-1:jte+1) &  
  :: Cd_u      & 
   , Cd_v        

  REAL         &  
  :: PI        & 
   , g0        & 
   , Hc        & 
   , V0_u      & 
   , V0_v      & 
   , tao_xz    & 
   , tao_yz    & 
   , epsilon   & 
   , ustar     & 
   , vk        & 
   , zu1       & 
   , zv1         

  INTEGER      &  
  :: ibgn      & 
   , iend      & 
   , jbgn      & 
   , jend      & 
   , kbgn      & 
   , kend      & 
   , i         & 
   , j         & 
   , k         & 
   , kint      & 
   , kmax      & 
   , ternopt   & 
   , canheight & 
   , k_ref       



  PI=3.14159265359
  ternopt = 1 
  g0=9.81     
  epsilon = 0.0000001
  vk = 0.4

  canheight = 1












































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

  
  



















  Hc = 4.0*dx 


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










  IF (ternopt .NE. 0) THEN

    DO j=jbgn,jend
    DO i=ibgn,iend
    DO k=kbgn,kend  

      
      

      IF( zw(i,k,j) .GT. Hc ) THEN
        kcan(i,j) = k-1
        EXIT
      END IF

    END DO
    END DO
    END DO

  ELSE 

    DO k=kbgn,kend  

      IF( zw(1,k,1) .GT. Hc ) THEN
        kcan = k-1 
        EXIT
      END IF

    END DO

  ENDIF




         





  DO j=jbgn,jend
  DO i=ibgn,iend
    kmax = kcan(i,j)
    DO k=1,kmax






      a(i,k,j) = (exp(-z(i,k,j)/(0.18*Hc)))**2 



    END DO
    DO k=kmax+1,kend
      a(i,k,j) = 0.0
    END DO
  END DO
  END DO






  k_ref = 1  












































































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








  DO j = jbgn,jend
  DO i = ibgn,iend

    kmax = kcan(i,j)
    DO k = 1, kmax
      C0(i,j) = C0(i,j) + (u_canforce(i,k,j))*(zw(i,k+1,j)-zw(i,k,j))
      D0(i,j) = D0(i,j) + (v_canforce(i,k,j))*(zw(i,k+1,j)-zw(i,k,j))
    END DO

  END DO
  END DO




  DO j = jbgn,jend
  DO i = ibgn,iend

    kmax = kcan(i,j)
    DO k = 1, kmax

      
      DO kint =1,k
        u_can(i,k,j) = u_can(i,k,j) + (u_canforce(i,kint,j)) &
                          *(zw(i,kint+1,j)-zw(i,kint,j))
        v_can(i,k,j) = v_can(i,k,j) + (v_canforce(i,kint,j)) &
                          *(zw(i,kint+1,j)-zw(i,kint,j))
      END DO

      
      u_can(i,k,j) = u_can(i,k,j)-C0(i,j)
      v_can(i,k,j) = v_can(i,k,j)-D0(i,j)

    END DO

  END DO
  END DO







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



  DO j = jbgn,jend
  DO k = 2,kend
  DO i = ibgn,iend



   nwtau13(i,k,j) = fnm(k)*u_can(i,k,j) + fnp(k)*u_can(i,k-1,j) 
   nwtau23(i,k,j) = fnm(k)*v_can(i,k,j) + fnp(k)*v_can(i,k-1,j) 

  ENDDO
  ENDDO
  ENDDO





















 10 FORMAT(I3)
 100 FORMAT(18(x,I9))
 1000 FORMAT(i2,18(x,f9.5))

 RETURN

END SUBROUTINE nw_canopy



END MODULE module_sfs_lasd
