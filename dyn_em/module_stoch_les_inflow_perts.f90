






















 
    MODULE module_stoch_les_inflow_perts

    USE module_model_constants    

    USE MODULE_DM

    CONTAINS



    SUBROUTINE force_down_meso_pblh( m_pblh, pblh,                        &
                                     ids, ide, jds, jde, kds, kde,        &
                                     ims, ime, jms, jme, kms, kme,        &
                                     its, ite, jts, jte, kts, kte         )






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
 


    END SUBROUTINE force_down_meso_pblh


 
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



    IMPLICIT NONE

    INCLUDE 'mpif.h'

    INTEGER, INTENT( IN    )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) :: t      
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) :: pert_t 
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN    ) :: u      
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN    ) :: v      
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN    ) :: rdz    
    REAL , DIMENSION( ims:ime, jms:jme ),          INTENT( IN    ) :: m_pblh 

    INTEGER , DIMENSION( kms:kme ), INTENT( INOUT ) :: prtseed         

    INTEGER,         INTENT( IN    ) :: les_pert_opt                   
    INTEGER,         INTENT( IN    ) :: m_pblh_opt                     
    REAL,            INTENT( INOUT ) :: prttms                         
    REAL,            INTENT( INOUT ) :: prtdt                          
    REAL,            INTENT( INOUT ) :: prtz                           
    INTEGER,         INTENT( INOUT ) :: prtnk                          
    REAL,            INTENT( IN    ) :: dt                             
    REAL,            INTENT( IN    ) :: dx                             

    REAL, DIMENSION( its:ite, kts:kte, jts:jte ) :: z  
    REAL, DIMENSION( kts:kte ) :: sf                   

    INTEGER :: i, j, k, l, m, n               
    INTEGER :: i_start, i_end, j_start, j_end 
    INTEGER :: itilemid, jtilemid             
    INTEGER :: i_seed                         
    INTEGER :: sum                            
    INTEGER :: ngc_h, ngc_v                   
    INTEGER :: ni, nj                         
    INTEGER :: ncells_h, ncells_v             
    INTEGER :: k_pert_start, k_pert_end       
    INTEGER :: north, south, east, west, nsew 
    INTEGER :: k_wg                           
    INTEGER :: k_ws                           
    REAL    :: h_wg                           
    REAL    :: h_ws                           
    REAL    :: mpblhpsum, mpblhpavg, mpblhdsum, mpblhdavg                         
    REAL    :: uwsgpsum, vwsgpsum, uwsgpsumdsum, vwsgpsumdsum                     
    REAL    :: wsgpsum, wsgpsumdsum, wsgpavgdavg                                  
    REAL    :: wsspsum, wsspsumdsum, wsspavgdavg                                  
    REAL    :: num_pts_in_sum, counter                                            
    REAL    :: tpertmag                       
    REAL    :: angles, anglespsum, anglespsumdsum, anglespavgdavg 
    REAL    :: dz                             
    REAL    :: pblh                           
    REAL    :: sf2                            
    REAL    :: ek_opt = 0.20                  
    REAL    :: pio2 = piconst/2.0
    REAL    :: pio4 = piconst/4.0

    INTEGER, dimension( : ), allocatable :: seed                 
    REAL, dimension( :, :, : ), allocatable :: pxs,pxe,pys,pye   
  
    INTEGER :: ierr
    INTEGER :: tag
    INTEGER :: master
    INTEGER :: status(MPI_STATUS_SIZE)

    master = 0
    tag    = 0



    prttms = prttms + dt



    IF (  prttms .GE. prtdt ) THEN 

       print*,'Computing new perturbations'







       prttms = dt

       i_start = its
       i_end   = MIN(ite,ide-1)
       j_start = jts
       j_end   = MIN(jte,jde-1)

       ngc_h = 8      
       ngc_v = 4      
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


       pblh = 1000.0 






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


mpblhdsum  = wrf_dm_sum_real(mpblhpsum)

              
          counter = float(ide-ids)*float(jde-jds) 

          mpblhdavg = mpblhdsum/counter
          
          pblh = MAX(100.0,MIN(mpblhdavg,3000.0))
                   


       ENDIF 

       print*,'pblh',pblh


       prtz = 0.9*pblh 
       h_wg = 1.10*pblh 
       h_ws = 0.05*pblh
       sf2 = 4.0*prtz/27.0 
   


       itilemid = (ite+its)/2
       jtilemid = (jte+jts)/2




       prtnk = 1

       DO k=kts,kte - 1  

          sf(k) = 1.0 
          IF ( z(itilemid,k,jtilemid) .LE. prtz ) prtnk = k
          IF ( z(itilemid,k,jtilemid) .LE. h_wg ) k_wg = k
          IF ( z(itilemid,k,jtilemid) .LE. h_ws ) k_ws = k
          IF ( z(itilemid,k,jtilemid) .GT. prtz ) sf(k) = 0.0

         END DO

        k_ws = 4

        sf(1) = 0.0
        sf(2) = 0.33
        sf(3) = 0.67








       uwsgpsum = 0.0
       vwsgpsum = 0.0

       IF ( its .EQ. ids ) THEN 

          DO j=j_start, j_end

             uwsgpsum = uwsgpsum + u(ids,k_wg,j)
             vwsgpsum = vwsgpsum + v(ids,k_wg,j)

          END DO

       ENDIF

       IF ( ite .EQ. ide ) THEN 

          DO j=j_start, j_end
      
             uwsgpsum = uwsgpsum + u(ide,k_wg,j)
             vwsgpsum = vwsgpsum + v(ide,k_wg,j)

          END DO

       ENDIF

       IF ( jts .EQ. jds ) THEN 

          DO i=i_start, i_end
      
             uwsgpsum = uwsgpsum + u(i,k_wg,jds)
             vwsgpsum = vwsgpsum + v(i,k_wg,jds)

          END DO

       ENDIF

       IF ( jte .EQ. jde ) THEN 

          DO i=i_start, i_end

             uwsgpsum = uwsgpsum + u(i,k_wg,jde)
             vwsgpsum = vwsgpsum + v(i,k_wg,jde)

          END DO

       ENDIF




uwsgpsumdsum  = wrf_dm_sum_real(uwsgpsum)

vwsgpsumdsum  = wrf_dm_sum_real(vwsgpsum)

   
       north = 0
       east = 0
       south = 0
       west = 0

       IF ( uwsgpsumdsum .GT. 0.0 ) west = 1
       IF ( uwsgpsumdsum .LT. 0.0 ) east = 1
       IF ( vwsgpsumdsum .GT. 0.0 ) south = 1
       IF ( vwsgpsumdsum .LT. 0.0 ) north = 1
           











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



       wsspsum = 0.0
       wsgpsum = 0.0
       anglespsum = 0.0
       counter = 0.0
       num_pts_in_sum = 0.0

       IF ( west .EQ. 1 ) THEN

          IF ( its .EQ. ids ) THEN 

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

          IF ( ite .EQ. ide ) THEN 

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

          IF ( jts .EQ. jds ) THEN 

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

          IF ( jte .EQ. jde ) THEN 

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




wsspsumdsum  = wrf_dm_sum_real(wsspsum)

wsgpsumdsum  = wrf_dm_sum_real(wsgpsum)

anglespsumdsum  = wrf_dm_sum_real(anglespsum)

counter = wrf_dm_sum_real(num_pts_in_sum)


       wsgpavgdavg = wsgpsumdsum/counter

       wsspavgdavg = wsspsumdsum/counter

       anglespavgdavg = anglespsumdsum/counter



     
       IF (wsspavgdavg .EQ. 0.0) THEN 

          print*,'something wrong in stoch_les_inflow_perts'
          STOP

       ENDIF

       tpertmag = (wsgpavgdavg*wsgpavgdavg)/(ek_opt*cp)
    
       print*,'tpertmag',tpertmag

       prtdt = 0.875*(1.0/cos(anglespavgdavg))*ngc_h*ncells_h*dx/wsspavgdavg  
                                                                              



       print*,'prtdt',prtdt

  


       ni = (ide-1)/ngc_h 
       nj = (jde-1)/ngc_h  


 
       ncells_v = kte-1 



        ALLOCATE( pxs(1:nj,1:ncells_v,1:ncells_h) )
        ALLOCATE( pxe(1:nj,1:ncells_v,1:ncells_h) )
        ALLOCATE( pys(1:ni,1:ncells_v,1:ncells_h) )
        ALLOCATE( pye(1:ni,1:ncells_v,1:ncells_h) )

       CALL RANDOM_SEED(size=i_seed)  

       ALLOCATE( seed(1:i_seed) )     


       IF ( mytask .EQ. master ) THEN

          sum = 0                     
          DO k = 1,i_seed             
             seed(k) = prtseed(k)
             sum = sum + seed(k)
          END DO

          IF (sum .EQ. 0 ) THEN       

             print*,'calling random seed for first time'   
             CALL RANDOM_SEED(get=seed)

          ENDIF
 
       ENDIF 

       CALL MPI_BCAST(seed,i_seed,MPI_REAL,master,MPI_COMM_WORLD,ierr) 



       CALL RANDOM_SEED(put=seed) 

       CALL RANDOM_NUMBER(pxs)
       CALL RANDOM_NUMBER(pxe)
       CALL RANDOM_NUMBER(pys)
       CALL RANDOM_NUMBER(pye)



       IF ( mytask .EQ. master ) THEN

          CALL RANDOM_SEED(get=seed)

             DO k = 1,i_seed
                prtseed(k) = seed(k)
             ENDDO
  
       ENDIF



       k_pert_start = 2 
       k_pert_end = prtnk

       DO k = kts,kte  
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



                   DO i = its, MIN(ite, ide-1)



                      m = (i-1)/ngc_h+1

                      IF ( m .LE. ncells_h ) THEN

                         pert_t(i,k,j) =  (pxs(((j-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag 

                         t(i,k,j) = t(i,k,j) + (pxs(((j-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

                      ENDIF

                   END DO 

                END DO 

             END DO 

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

                   END DO 

                END DO 

             END DO 

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




                        pert_t(i,k,j) = (pys(((i-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

                         t(i,k,j) = t(i,k,j) + (pys(((i-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag

                      ENDIF

                   END DO 

                END DO 

             END DO 

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

                   END DO 

                END DO 

             END DO 

          ENDIF

       ENDIF

       DEALLOCATE(seed)

       DEALLOCATE( pxs )
       DEALLOCATE( pxe )
       DEALLOCATE( pys )
       DEALLOCATE( pye )

    ENDIF 



END SUBROUTINE calc_pert_t




 
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







    IMPLICIT NONE

    INCLUDE 'mpif.h'

    INTEGER, INTENT( IN    )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) :: pert_t 
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN    ) :: u      
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN    ) :: v      
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) :: ru_tendf  
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) :: rv_tendf  
    REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN    ) :: rdz    
    REAL , DIMENSION( ims:ime, jms:jme ),          INTENT( IN    ) :: m_pblh 

    REAL , DIMENSION( ims:ime, jms:jme ),          INTENT( IN    ) :: muu
    REAL , DIMENSION( ims:ime, jms:jme ),          INTENT( IN    ) :: muv



    INTEGER , DIMENSION( kms:kme ), INTENT( INOUT ) :: prtseed         

    INTEGER,         INTENT( IN    ) :: les_pert_opt                   
    INTEGER,         INTENT( IN    ) :: m_pblh_opt                     
    REAL,            INTENT( INOUT ) :: prttms                         
    REAL,            INTENT( INOUT ) :: prtdt                          
    REAL,            INTENT( INOUT ) :: prtz                           
    INTEGER,         INTENT( INOUT ) :: prtnk                          
    REAL,            INTENT( IN    ) :: dt                             
    REAL,            INTENT( IN    ) :: dx                             

    REAL, DIMENSION( its:ite, kts:kte, jts:jte ) :: z  
    REAL, DIMENSION( kts:kte ) :: sf                   

    INTEGER :: i, j, k, l, m, n               
    INTEGER :: i_start, i_end, j_start, j_end 
    INTEGER :: itilemid, jtilemid             
    INTEGER :: i_seed                         
    INTEGER :: sum                            
    INTEGER :: ngc_h, ngc_v                   
    INTEGER :: ni, nj                         
    INTEGER :: ncells_h, ncells_v             
    INTEGER :: k_pert_start, k_pert_end       
    INTEGER :: north, south, east, west, nsew 
    INTEGER :: k_wg                           
    INTEGER :: k_ws                           
    REAL    :: h_wg                           
    REAL    :: h_ws                           
    REAL    :: mpblhpsum, mpblhpavg, mpblhdsum, mpblhdavg                         
    REAL    :: uwsgpsum, vwsgpsum, uwsgpsumdsum, vwsgpsumdsum                     
    REAL    :: wsgpsum, wsgpsumdsum, wsgpavgdavg                                  
    REAL    :: wsspsum, wsspsumdsum, wsspavgdavg                                  
    REAL    :: num_pts_in_sum, counter                                            
    REAL    :: tpertmag                       
    REAL    :: angles, anglespsum, anglespsumdsum, anglespavgdavg 
    REAL    :: dz                             
    REAL    :: pblh                           
    REAL    :: sf2                            
    REAL    :: ek_opt = 0.20                  
    REAL    :: pio2 = piconst/2.0
    REAL    :: pio4 = piconst/4.0

    INTEGER, dimension( : ), allocatable :: seed                 
    REAL, dimension( :, :, : ), allocatable :: pxs,pxe,pys,pye   
  
    INTEGER :: ierr
    INTEGER :: tag
    INTEGER :: master
    INTEGER :: status(MPI_STATUS_SIZE)

    master = 0
    tag    = 0

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)



    prttms = prttms + dt



    IF (  prttms .GE. prtdt ) THEN 

       print*,'Computing new perturbations'







       prttms = dt

       ngc_h = 8      
       ngc_v = 4      
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


       pblh = 1000.0 






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


mpblhdsum  = wrf_dm_sum_real(mpblhpsum)

              
          counter = float(ide-ids)*float(jde-jds) 

          mpblhdavg = mpblhdsum/counter
          
          pblh = MAX(100.0,MIN(mpblhdavg,3000.0))
                   


       ENDIF 

       print*,'pblh',pblh


       prtz = 0.9*pblh 
       h_wg = 1.10*pblh 
       h_ws = 0.05*pblh
       sf2 = 4.0*prtz/27.0 
   


       itilemid = (ite+its)/2
       jtilemid = (jte+jts)/2




       prtnk = 1

       DO k=kts,kte - 1  

          sf(k) = 1.0 
          IF ( z(itilemid,k,jtilemid) .LE. prtz ) prtnk = k
          IF ( z(itilemid,k,jtilemid) .LE. h_wg ) k_wg = k
          IF ( z(itilemid,k,jtilemid) .LE. h_ws ) k_ws = k
          IF ( z(itilemid,k,jtilemid) .GT. prtz ) sf(k) = 0.0

         END DO

        k_ws = 4

        sf(1) = 0.0
        sf(2) = 0.33
        sf(3) = 0.67








       uwsgpsum = 0.0
       vwsgpsum = 0.0

       IF ( its .EQ. ids ) THEN 

          DO j=j_start, j_end

             uwsgpsum = uwsgpsum + u(ids,k_wg,j)
             vwsgpsum = vwsgpsum + v(ids,k_wg,j)

          END DO

       ENDIF

       IF ( ite .EQ. ide ) THEN 

          DO j=j_start, j_end
      
             uwsgpsum = uwsgpsum + u(ide,k_wg,j)
             vwsgpsum = vwsgpsum + v(ide,k_wg,j)

          END DO

       ENDIF

       IF ( jts .EQ. jds ) THEN 

          DO i=i_start, i_end
      
             uwsgpsum = uwsgpsum + u(i,k_wg,jds)
             vwsgpsum = vwsgpsum + v(i,k_wg,jds)

          END DO

       ENDIF

       IF ( jte .EQ. jde ) THEN 

          DO i=i_start, i_end

             uwsgpsum = uwsgpsum + u(i,k_wg,jde)
             vwsgpsum = vwsgpsum + v(i,k_wg,jde)

          END DO

       ENDIF




uwsgpsumdsum  = wrf_dm_sum_real(uwsgpsum)

vwsgpsumdsum  = wrf_dm_sum_real(vwsgpsum)

   
       north = 0
       east = 0
       south = 0
       west = 0

       IF ( uwsgpsumdsum .GT. 0.0 ) west = 1
       IF ( uwsgpsumdsum .LT. 0.0 ) east = 1
       IF ( vwsgpsumdsum .GT. 0.0 ) south = 1
       IF ( vwsgpsumdsum .LT. 0.0 ) north = 1
           











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



       wsspsum = 0.0
       wsgpsum = 0.0
       anglespsum = 0.0
       counter = 0.0
       num_pts_in_sum = 0.0

       IF ( west .EQ. 1 ) THEN

          IF ( its .EQ. ids ) THEN 

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

          IF ( ite .EQ. ide ) THEN 

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

          IF ( jts .EQ. jds ) THEN 

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

          IF ( jte .EQ. jde ) THEN 

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




wsspsumdsum  = wrf_dm_sum_real(wsspsum)

wsgpsumdsum  = wrf_dm_sum_real(wsgpsum)

anglespsumdsum  = wrf_dm_sum_real(anglespsum)

counter = wrf_dm_sum_real(num_pts_in_sum)


       wsgpavgdavg = wsgpsumdsum/counter

       wsspavgdavg = wsspsumdsum/counter

       anglespavgdavg = anglespsumdsum/counter



     
       IF (wsspavgdavg .EQ. 0.0) THEN 

          print*,'something wrong in stoch_les_inflow_perts'
          STOP

       ENDIF

       tpertmag = (wsgpavgdavg*wsgpavgdavg)/(ek_opt*cp)
    
       print*,'tpertmag',tpertmag

       prtdt = 0.875*(1.0/cos(anglespavgdavg))*ngc_h*ncells_h*dx/wsspavgdavg  
                                                                              



       print*,'prtdt',prtdt

  


       ni = (ide-1)/ngc_h 
       nj = (jde-1)/ngc_h  


 
       ncells_v = kte-1 



        ALLOCATE( pxs(1:nj,1:ncells_v,1:ncells_h) )
        ALLOCATE( pxe(1:nj,1:ncells_v,1:ncells_h) )
        ALLOCATE( pys(1:ni,1:ncells_v,1:ncells_h) )
        ALLOCATE( pye(1:ni,1:ncells_v,1:ncells_h) )

       CALL RANDOM_SEED(size=i_seed)  

       ALLOCATE( seed(1:i_seed) )     


       IF ( mytask .EQ. master ) THEN

          sum = 0                     
          DO k = 1,i_seed             
             seed(k) = prtseed(k)
             sum = sum + seed(k)
          END DO

          IF (sum .EQ. 0 ) THEN       

             print*,'calling random seed for first time'   
             CALL RANDOM_SEED(get=seed)

          ENDIF
 
       ENDIF 

       CALL MPI_BCAST(seed,i_seed,MPI_REAL,master,MPI_COMM_WORLD,ierr) 



       CALL RANDOM_SEED(put=seed) 

       CALL RANDOM_NUMBER(pxs)
       CALL RANDOM_NUMBER(pxe)
       CALL RANDOM_NUMBER(pys)
       CALL RANDOM_NUMBER(pye)



       IF ( mytask .EQ. master ) THEN

          CALL RANDOM_SEED(get=seed)

             DO k = 1,i_seed
                prtseed(k) = seed(k)
             ENDDO
  
       ENDIF



       k_pert_start = 2 
       k_pert_end = prtnk

       DO k = kts,kte  
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



                   DO i = its, MIN(ite, ide-1)



                      m = (i-1)/ngc_h+1

                      IF ( m .LE. ncells_h ) THEN

                         pert_t(i,k,j) =  (pxs(((j-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag 



                      ENDIF

                   END DO 

                END DO 

             END DO 

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



                      ENDIF

                   END DO 

                END DO 

             END DO 

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




                        pert_t(i,k,j) = (pys(((i-1)/ngc_h+1),n,m)-0.5)*sf(k)*2.0*tpertmag



                      ENDIF

                   END DO 

                END DO 

             END DO 

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



                      ENDIF

                   END DO 

                END DO 

             END DO 

          ENDIF

       ENDIF

       DEALLOCATE(seed)

       DEALLOCATE( pxs )
       DEALLOCATE( pxe )
       DEALLOCATE( pys )
       DEALLOCATE( pye )

    ENDIF 


    DO j=j_start, j_end

       DO i=i_start, i_end

          DO k=kts+1,kte-1  

             ru_tendf(i,k,j) = ru_tendf(i,k,j) + 0.01*pert_t(i,k,j)*muu(i,j)
             rv_tendf(i,k,j) = rv_tendf(i,k,j) + 0.01*pert_t(i,k,j)*muv(i,j)

          END DO 

       END DO 

    END DO 





END SUBROUTINE calc_pert_uv





    END MODULE module_stoch_les_inflow_perts



