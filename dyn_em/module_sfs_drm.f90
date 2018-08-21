























MODULE module_sfs_drm

  IMPLICIT NONE

  CONTAINS



SUBROUTINE drm_filter( varfilt, var, vars, variable, ifilt, &
                        ids, ide, jds, jde, kds, kde,        &
                        ims, ime, jms, jme, kms, kme,        &
                        ips, ipe, jps, jpe, kps, kpe,        &
                        its, ite, jts, jte, kts, kte         )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: varfilt       

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var           

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( IN  ) &
  :: vars          

  CHARACTER(LEN=2),                                      INTENT( IN  ) &
  :: variable

  INTEGER,                                               INTENT( IN  ) &
  :: ifilt

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  REAL, DIMENSION( its-1:ite+1, kts-1:kte+1, jts-1:jte+1 ) &
  :: fx         &  
   , fxy        &  
   , vart          

  REAL :: a, b, c, du

  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend





  IF ( ifilt .EQ. 1 ) THEN      
    a = 0.25
    b = 0.5
    c = 0.25
  ELSEIF ( ifilt .EQ. 2 ) THEN  
    a = 1.0/6.0
    b = 4.0/6.0
    c = 1.0/6.0
  ELSEIF ( ifilt .EQ. 3 ) THEN 
    a = 0.5                    
    b = 0.0                    
    c = 0.5
  ELSE
    WRITE(*,*) 'Invalid filter type, ifilt=', ifilt
    STOP  
  ENDIF

  SELECT CASE (variable)


    CASE ('uu') 

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


        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)   
      ENDDO
      ENDDO


    CASE ('vv') 

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
        vart(i,kend,j) =  vart(i,kend-1,j)                  


        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)   
      ENDDO
      ENDDO


    CASE ('ww') 

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
        vart(i,kend,j) = 0.0                                


          vart(i,kbgn,j) = vars(i,j)                          

      END DO
      END DO



    CASE ('cw') 

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
        vart(i,kend,j) = - vart(i,kend-1,j)                 


        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)   
      ENDDO
      ENDDO


    CASE ('cc') 

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


        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)   
      ENDDO
      ENDDO


    CASE ('dd') 

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


        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)   
      END DO
      END DO


    CASE ('ee') 

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
        vart(i,kend,j) = vart(i,kend-1,j)                   


        vart(i,kbgn,j) = vars(i,j)                          
      END DO
      END DO



    CASE ('ew') 

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
        vart(i,kend,j) = 0.0                                


        vart(i,kbgn,j) = vars(i,j)                          
      END DO
      END DO



    CASE ('ff') 

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
        vart(i,kend,j) = vart(i,kend-1,j)                   


        vart(i,kbgn,j) = vars(i,j)                          
      END DO
      END DO



    CASE ('fw') 

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
        vart(i,kend,j) = 0.0                                


        vart(i,kbgn,j) = vars(i,j)                          
      END DO
      END DO



    CASE ('sh') 

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

        vart(i,kend,j) =  vart(i,kend-1,j)                    

        vart(i,kbgn,j) = vart(i,kbgn+1,j)                  
      ENDDO
      ENDDO


    CASE ('sv') 

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
        vart(i,kend,j) = - vart(i,kend-1,j)                  


        vart(i,kbgn,j) = vart(i,kbgn+1,j)                  
      ENDDO
      ENDDO


    CASE ('tc') 

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
        vart(i,kend,j) = - vart(i,kend-1,j)                  


        vart(i,kbgn,j) = vart(i,kbgn+1,j)                  
      ENDDO
      ENDDO


    CASE ('td') 

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
        vart(i,kend,j) = - vart(i,kend-1,j)                  


        vart(i,kbgn,j) = vart(i,kbgn+1,j)                    
      ENDDO
      ENDDO


    CASE ('te') 

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
        vart(i,kend,j) = 0.0                                 

        vart(i,kbgn,j) = vart(i,kbgn+1,j) 
      ENDDO
      ENDDO


    CASE ('tf') 

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
        vart(i,kend,j) = 0.0                                 

        vart(i,kbgn,j) = vart(i,kbgn+1,j) 
      ENDDO
      ENDDO


    CASE ('kc') 

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

        vart(i,kend,j) =   vart(i,kend-1,j)                  
        vart(i,kbgn,j) =  vart(i,kbgn+1,j)                 
      ENDDO
      ENDDO



    CASE DEFAULT
 
      WRITE(*,*)'Error-improperly specified variable in'
      WRITE(*,*)'dyn_em/module_reconstruct.F/subroutine drm_filter'
      STOP

  END SELECT




  DO j = jts-1, jte+1
  DO k = kbgn, kend
  DO i = its, ite
    fx(i,k,j) = a*vart(i+1,k,j) + b*vart(i,k,j) + c*vart(i-1,k,j)
  ENDDO
  ENDDO
  ENDDO


  DO j = jts, jte
  DO k = kbgn, kend
  DO i = its, ite
    fxy(i,k,j) = a*fx(i,k,j+1) + b*fx(i,k,j) + c*fx(i,k,j-1)
  END DO
  END DO
  ENDDO


  DO j = jts, jte
  DO k = kbgn+1, kend-1
  DO i = its, ite
    varfilt(i,k,j) = a*fxy(i,k+1,j) + b*fxy(i,k,j) + c*fxy(i,k-1,j)
  ENDDO
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE drm_filter



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

















  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( OUT ) &
  :: usc         & 
   , vsc         & 
   , ws          & 
   , uusc        & 
   , vvsc        & 
   , wws         & 
   , usd         & 
   , vsd         & 
   , uvsd        & 
   , wse         & 
   , uwse        & 
   , wsf         & 
   , vwsf          

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
  :: ht            

  INTEGER,                                               INTENT( IN  ) &
  :: rsfsvsbc

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



  IF ( rsfsvsbc .EQ. 0 ) THEN 

    DO j=jts, jte 
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


  ELSEIF ( rsfsvsbc .EQ. 1 ) THEN 

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


  ELSEIF ( rsfsvsbc .EQ. 2 ) THEN 

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




SUBROUTINE hij( h11, h22, h33, h12, h13, h23, &
                tgrtau11, tgrtau22, tgrtau33,    &
                tgrtau12, tgrtau13, tgrtau23,    &
                tfrtau11, tfrtau22, tfrtau33, &
                tfrtau12, tfrtau13, tfrtau23, &
                ids, ide, jds, jde, kds, kde, & 
                ims, ime, jms, jme, kms, kme, & 
                ips, ipe, jps, jpe, kps, kpe, & 
                its, ite, jts, jte, kts, kte  )  





  IMPLICIT NONE                                               

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &   
  :: h11         & 
   , h22         & 
   , h33         & 
   , h12         & 
   , h13         & 
   , h23           

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &   
  :: tgrtau11    & 
   , tgrtau22    & 
   , tgrtau33    & 
   , tgrtau12    & 
   , tgrtau13    & 
   , tgrtau23    & 
   , tfrtau11    & 
   , tfrtau22    & 
   , tfrtau33    & 
   , tfrtau12    & 
   , tfrtau13    & 
   , tfrtau23      

  INTEGER,                                               INTENT( IN  ) & 
  :: ids, ide, jds, jde, kds, kde, &                              
     ims, ime, jms, jme, kms, kme, &                               
     ips, ipe, jps, jpe, kps, kpe, &                              
     its, ite, jts, jte, kts, kte       



  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend



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


  ENDDO
  ENDDO

  RETURN

END SUBROUTINE hij



SUBROUTINE product( prod, var1, var2,             & 
                    ids, ide, jds, jde, kds, kde, & 
                    ims, ime, jms, jme, kms, kme, & 
                    ips, ipe, jps, jpe, kps, kpe, & 
                    its, ite, jts, jte, kts, kte  ) 





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
       

    
  INTEGER :: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend


   
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






  IMPLICIT NONE  
                                             
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &   
  :: drmn        & 
   , drmd      
 
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &   
  :: h11         & 
   , h22         & 
   , h33         & 
   , h12         & 
   , h13         & 
   , h23         & 
   , tfs11       & 
   , tfs22       & 
   , tfs33       & 
   , tfs12       & 
   , tfs13       & 
   , tfs23       & 
   , tfu         & 
   , tfv         & 
   , tfw         & 
   , tfuu        & 
   , tfvv        & 
   , tfww        & 
   , tfuv        & 
   , tfuw        & 
   , tfvw          

  INTEGER,                                               INTENT( IN  ) & 
  :: drm_opt

  INTEGER,                                               INTENT( IN  ) & 
  :: ids, ide, jds, jde, kds, kde, &                              
     ims, ime, jms, jme, kms, kme, &                               
     ips, ipe, jps, jpe, kps, kpe, &                              
     its, ite, jts, jte, kts, kte       



  REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ) &   
  :: l11         & 
   , l22         & 
   , l33         & 
   , l12         & 
   , l13         & 
   , l23         & 
   , st11        & 
   , st22        & 
   , st33        & 
   , st12        & 
   , st13        & 
   , st23          

  REAL :: ddiff, tem, temx        

  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend



  kbgn = kts
  kend = MIN( kte, kde )-1  


  ddiff = 1.0 - 2.0**(4.0/3.0)  
			        

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

    
    tem  = ( l11(i,k,j) + l22(i,k,j) + l33(i,k,j) )/3.0
    temx = ( st11(i,k,j) + st22(i,k,j) + st33(i,k,j) )/3.0

    
    drmn(i,k,j) = ( l11(i,k,j) - tem )*( tfs11(i,k,j) - temx ) +  &
                  ( l22(i,k,j) - tem )*( tfs22(i,k,j) - temx ) +  &
                  ( l33(i,k,j) - tem )*( tfs33(i,k,j) - temx ) +  &
                  2.0*( l12(i,k,j)*st12(i,k,j) +                  &
                  l13(i,k,j)*st13(i,k,j) + l23(i,k,j)*st23(i,k,j) )

    
    drmd(i,k,j) = ( st11(i,k,j) - temx )**2 +      &
                  ( st22(i,k,j) - temx )**2 +      &
                  ( st33(i,k,j) - temx )**2 +      &
                  2.0*( st12(i,k,j)**2 +         &
                  st13(i,k,j)**2 + st23(i,k,j)**2 )



  ENDDO
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE drmnd




SUBROUTINE drmevc( xkmv, xkhv, xkmh, xkhh,       &
                   drmn, drmd,                   &      
                   ids, ide, jds, jde, kds, kde, & 
                   ims, ime, jms, jme, kms, kme, & 
                   ips, ipe, jps, jpe, kps, kpe, & 
                   its, ite, jts, jte, kts, kte  )  





  IMPLICIT NONE                                               
      
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT(INOUT) &   
  :: xkmv        & 
   , xkhv        & 
   , xkmh        & 
   , xkhh          
  
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &   
  :: drmn        & 
   , drmd          
  
  INTEGER,                                               INTENT( IN  ) & 
  :: ids, ide, jds, jde, kds, kde, &                              
     ims, ime, jms, jme, kms, kme, &                               
     ips, ipe, jps, jpe, kps, kpe, &                              
     its, ite, jts, jte, kts, kte       



  REAL :: eps, tem, pr

  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend














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

    xkmh(i,k,j)=xkmv(i,k,j)
    xkhv(i,k,j)=xkmv(i,k,j)/pr
    xkhh(i,k,j)=xkhv(i,k,j)



  
  ENDDO
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE drmevc




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
   , canshape  & 
   , canheight & 
   , k_ref       



  PI=3.14159265359
  ternopt = 1 
  g0=9.81     
  epsilon = 0.0000001
  vk = 0.4

  canshape = 2
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




       a(i,k,j) = exp(-z(i,k,j)/(0.18*Hc)) 

    END DO
    DO k=kmax+1,kend
      a(i,k,j) = 0.0
    END DO
  END DO
  END DO

































 













































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

END SUBROUTINE drm_nw_canopy



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



  DO i = its,ite
    indarr(i)=i
  ENDDO

 jj = jts+3
 kk = 2










      WRITE(*,1000)jj,var(its:ite,kk,jj)


      WRITE(*,100)indarr(its:ite)

 100   FORMAT(2x,26(x,I12))
 1000   FORMAT(i2,26(x,e12.5))

END SUBROUTINE writecheckm_drm




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
