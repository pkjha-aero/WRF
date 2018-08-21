






























































MODULE module_sfs_driver

CONTAINS



SUBROUTINE sfs_driver( grid, config_flags,                   &
                       nba_mij, n_nba_mij,                   & 
                       nba_rij, n_nba_rij,                   & 
                       lasd_uvw_bar_hat, n_lasd_uvw_bar_hat, & 
                       lasd_s_bar_hat, n_lasd_s_bar_hat,     & 
                       lasd_Sij, n_lasd_Sij,                 & 
                       lasd_Lij, n_lasd_Lij,                 & 
                       lasd_Qij, n_lasd_Qij,                 & 
                       lasd_Gij, n_lasd_Gij,                 & 
                       lasd_Nij, n_lasd_Nij,                 & 
                       lasd_TC, n_lasd_TC,                   & 
                       lasd_LAGR, n_lasd_LAGR,               & 
                       rsfs_rec, n_rsfs_rec,                 & 
                       rsfs_filt, n_rsfs_filt,               & 
                       rsfs_prec, n_rsfs_prec,               & 
                       rsfs_rmij, n_rsfs_rmij,               & 
                       qvapor,                               &                               
                       tfu_drm, n_tfu_drm,                   & 
                       uc_drm, n_uc_drm,                   & 
                       tfuc_drm, n_tfuc_drm,               & 
                       sijc_drm, n_sijc_drm,               & 
                       tfsij_drm, n_tfsij_drm,             & 
                       tfsijc_drm, n_tfsijc_drm,           & 
                       hij_drm, n_hij_drm,                 & 
                       hijc_drm, n_hijc_drm,               & 
                       tfrmij_drm, n_tfrmij_drm,           & 
                       tgrmij_drm, n_tgrmij_drm,           & 
                       evc_drm, n_evc_drm                   )










  USE module_domain, ONLY : domain, get_ijk_from_grid
  USE module_configure, ONLY : grid_config_rec_type, model_config_rec
  USE module_tiles
  USE module_machine
  USE module_state_description
  USE module_model_constants

  USE module_bc



  USE module_sfs_nba
  USE module_sfs_drm
  USE module_sfs_rsfs
  USE module_sfs_lasd
   USE module_dm
   USE module_comm_dm, ONLY : &
                           HALO_EM_NBA_RIJ_sub   &
                          ,PERIOD_EM_NBA_RIJ_sub   &
                          ,HALO_EM_NBA_MIJ_sub   &
                          ,PERIOD_EM_NBA_MIJ_sub  &
                          ,HALO_EM_DEFOR_CC_sub   &    
                          ,PERIOD_EM_DEFOR_CC_sub  &    
                          ,HALO_EM_LASD_UVW_BAR_HAT_sub  &
                          ,PERIOD_EM_LASD_UVW_BAR_HAT_sub  &
                          ,HALO_EM_LASD_S_BAR_HAT_sub  &
                          ,PERIOD_EM_LASD_S_BAR_HAT_sub  &
                          ,HALO_EM_LASD_SIJ_sub  & 
                          ,PERIOD_EM_LASD_SIJ_sub  & 
                          ,HALO_EM_LASD_LIJ_sub  &
                          ,PERIOD_EM_LASD_LIJ_sub  &
                          ,HALO_EM_LASD_QIJ_sub  & 
                          ,PERIOD_EM_LASD_QIJ_sub  &
                          ,HALO_EM_LASD_GIJ_sub  & 
                          ,PERIOD_EM_LASD_GIJ_sub  &
                          ,HALO_EM_LASD_NIJ_sub  & 
                          ,PERIOD_EM_LASD_NIJ_sub  & 
                          ,HALO_EM_LASD_TC_sub  & 
                          ,PERIOD_EM_LASD_TC_sub  &
                          ,HALO_EM_LASD_LAGR_sub  & 
                          ,PERIOD_EM_LASD_LAGR_sub  &
                          ,HALO_EM_EV_sub  &
                          ,PERIOD_EM_EV_sub  &
                          ,HALO_EM_LASD_PUVWS_sub  &
                          ,PERIOD_EM_LASD_PUVWS_sub  &
                          ,HALO_EM_RSFS_REC_sub  &
                          ,PERIOD_EM_RSFS_REC_sub  &
                          ,HALO_EM_RSFS_FILT_sub  &  
                          ,PERIOD_EM_RSFS_FILT_sub  & 
                          ,HALO_EM_RSFS_PREC_sub  & 
                          ,PERIOD_EM_RSFS_PREC_sub  &
                          ,HALO_EM_RSFS_RMIJ_sub   &
                          ,PERIOD_EM_RSFS_RMIJ_sub  & 
                          ,HALO_EM_RSFS_UVWS_sub  &
                          ,PERIOD_EM_RSFS_UVWS_sub  &
                          ,HALO_EM_RSFS_PUVWS_sub  &
                          ,PERIOD_EM_RSFS_PUVWS_sub  &
                          ,HALO_EM_TFU_DRM_sub  &
                          ,PERIOD_EM_TFU_DRM_sub  &
                          ,HALO_EM_UC_DRM_sub  &
                          ,PERIOD_EM_UC_DRM_sub  &
                          ,HALO_EM_TFUC_DRM_sub  &
                          ,PERIOD_EM_TFUC_DRM_sub  &
                          ,HALO_EM_TFRMIJ_DRM_sub  &
                          ,PERIOD_EM_TFRMIJ_DRM_sub  &
                          ,HALO_EM_SIJC_DRM_sub   &
                          ,PERIOD_EM_SIJC_DRM_sub   &
                          ,HALO_EM_HIJ_DRM_sub   &
                          ,PERIOD_EM_HIJ_DRM_sub  &
                          ,HALO_EM_HIJC_DRM_sub  &
                          ,PERIOD_EM_HIJC_DRM_sub  &
                          ,HALO_EM_TFSIJ_DRM_sub  &
                          ,PERIOD_EM_TFSIJ_DRM_sub  &
                          ,HALO_EM_TFSIJC_DRM_sub  &
                          ,PERIOD_EM_TFSIJC_DRM_sub   &
                          ,HALO_EM_EVC_DRM_sub   &
                          ,PERIOD_EM_EVC_DRM_sub   &
                          ,HALO_EM_NWTAU_sub   &
                          ,PERIOD_EM_NWTAU_sub   

  IMPLICIT NONE



  TYPE(domain) , TARGET          :: grid

  TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

  INTEGER, INTENT(  IN ) :: n_nba_mij, n_nba_rij, &
                            n_rsfs_rec, n_rsfs_filt, n_rsfs_prec, n_rsfs_rmij, &
                            n_lasd_uvw_bar_hat,n_lasd_s_bar_hat,n_lasd_Sij,n_lasd_Lij,n_lasd_Qij, &
                            n_lasd_Gij, n_lasd_Nij, n_lasd_TC, n_lasd_LAGR, n_tfu_drm, n_uc_drm, n_tfuc_drm, &
                            n_sijc_drm, n_tfsij_drm, n_tfsijc_drm, n_hij_drm,  &
                            n_hijc_drm, n_tfrmij_drm, n_tgrmij_drm, n_evc_drm

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_nba_mij) &
  :: nba_mij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_nba_rij) &
  :: nba_rij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_lasd_uvw_bar_hat) &
  :: lasd_uvw_bar_hat

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_lasd_s_bar_hat) &
  :: lasd_s_bar_hat

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_lasd_Sij) &
  :: lasd_Sij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_lasd_Lij) &
  :: lasd_Lij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_lasd_Qij) &
  :: lasd_Qij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_lasd_Gij) &
  :: lasd_Gij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_lasd_Nij) &
  :: lasd_Nij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_lasd_TC) &
  :: lasd_TC

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_lasd_LAGR) &
  :: lasd_LAGR

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_rsfs_rec) &
  :: rsfs_rec

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_rsfs_filt) &
  :: rsfs_filt

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_rsfs_prec) &
  :: rsfs_prec

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_rsfs_rmij) &
  :: rsfs_rmij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) &
  :: qvapor

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_tfu_drm) &
  :: tfu_drm

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_uc_drm) &
  :: uc_drm

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_tfuc_drm) &
  :: tfuc_drm

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_sijc_drm) &
  :: sijc_drm

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_tfsij_drm) &
  :: tfsij_drm

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_tfsijc_drm) &
  :: tfsijc_drm

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_hij_drm) &
  :: hij_drm

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_hijc_drm) &
  :: hijc_drm

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_tfrmij_drm) &
  :: tfrmij_drm

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_tgrmij_drm) &
  :: tgrmij_drm

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_evc_drm) &
  :: evc_drm




  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) &
  :: temp
  


  INTEGER :: k_start , k_end, its, ite, jts, jte
  INTEGER :: ids , ide , jds , jde , kds , kde , &
             ims , ime , jms , jme , kms , kme , &
             ips , ipe , jps , jpe , kps , kpe

  INTEGER :: imsx, imex, jmsx, jmex, kmsx, kmex, &
             ipsx, ipex, jpsx, jpex, kpsx, kpex, &
             imsy, imey, jmsy, jmey, kmsy, kmey, &
             ipsy, ipey, jpsy, jpey, kpsy, kpey
 
  INTEGER :: ij, i, j, k



  INTEGER :: recn

  REAL, DIMENSION(0:10) :: rcoef



   INTEGER :: itst
   INTEGER :: iexp
   INTEGER :: switch 



  CALL get_ijk_from_grid ( grid ,                              &
                           ids, ide, jds, jde, kds, kde,       &
                           ims, ime, jms, jme, kms, kme,       &
                           ips, ipe, jps, jpe, kps, kpe,       &
                           imsx, imex, jmsx, jmex, kmsx, kmex, &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex, &
                           imsy, imey, jmsy, jmey, kmsy, kmey, &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey  )

  k_start         = kps
  k_end           = kpe

  iexp = 1
  itst = 3




  CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

  IF ( (config_flags%sfs_opt .EQ. 1) .OR. (config_flags%sfs_opt .EQ. 2) ) THEN






      


























    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

        CALL calc_mij_constants( )

    ENDDO 
    !$OMP END PARALLEL DO






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

        CALL calc_smnsmn( nba_rij(ims,kms,jms,P_smnsmn),    &
                          grid%defor11, grid%defor22,       &
                          grid%defor33, grid%defor12,       &
                          grid%defor13, grid%defor23,       &
                          grid%defor12_cc,                  &
                          grid%defor13_cc,                  &
                          grid%defor23_cc,                  &
                     nba_rij(ims,kms,jms,P_r12_cc),    &
                     nba_rij(ims,kms,jms,P_r13_cc),    &
                     nba_rij(ims,kms,jms,P_r23_cc),    &
                          config_flags,                     &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          ips, ipe, jps, jpe, kps, kpe,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end                )

    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_NBA_RIJ_sub ( grid, &
  num_nba_rij, &
  nba_rij, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_NBA_RIJ_sub ( grid, &
  config_flags, &
  num_nba_rij, &
  nba_rij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








CALL HALO_EM_DEFOR_CC_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_DEFOR_CC_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

        CALL set_physical_bc3d_m( nba_rij(ims,kms,jms,P_r12), 'd',  &
                                config_flags,                     &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end                )

        CALL set_physical_bc3d_m( nba_rij(ims,kms,jms,P_r13), 'e',  &
                                config_flags,                     &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end                )

        CALL set_physical_bc3d_m( nba_rij(ims,kms,jms,P_r23), 'f',  &
                                config_flags,                     &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end                )

IF (config_flags%defor_opt .GT. 0) THEN

        CALL set_physical_bc3d_m( nba_rij(ims,kms,jms,P_r12_cc), 'p',  &
                                  config_flags,                     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start    , k_end                )

        CALL set_physical_bc3d_m( nba_rij(ims,kms,jms,P_r13_cc), 'p',  &
                                  config_flags,                     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start    , k_end                )

        CALL set_physical_bc3d_m( nba_rij(ims,kms,jms,P_r23_cc), 'p',  &
                                  config_flags,                     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start    , k_end                )

        CALL set_physical_bc3d_m( grid%defor12_cc, 'p',  &
                                  config_flags,                     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start    , k_end                )

        CALL set_physical_bc3d_m( grid%defor13_cc, 'p',  &
                                  config_flags,                     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start    , k_end                )

        CALL set_physical_bc3d_m( grid%defor23_cc, 'p',  &
                                  config_flags,                     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start    , k_end                )

ENDIF

        CALL set_physical_bc3d_m( nba_rij(ims,kms,jms,P_smnsmn), 'p', &
                                  config_flags,                       &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                  )

    ENDDO 
    !$OMP END PARALLEL DO






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL calc_mii( nba_mij(ims,kms,jms,P_m11),       &
                     nba_mij(ims,kms,jms,P_m22),       &
                     nba_mij(ims,kms,jms,P_m33),       &
                     grid%defor11, grid%defor22,       &
                     grid%defor33, grid%defor12,       &
                     grid%defor13, grid%defor23,       &
                     grid%defor12_cc,                  &
                     grid%defor13_cc,                  &
                     grid%defor23_cc,                  &
                     nba_rij(ims,kms,jms,P_r12),       &
                     nba_rij(ims,kms,jms,P_r13),       &
                     nba_rij(ims,kms,jms,P_r23),       &
                     nba_rij(ims,kms,jms,P_r12_cc),    &
                     nba_rij(ims,kms,jms,P_r13_cc),    &
                     nba_rij(ims,kms,jms,P_r23_cc),    &
                     nba_rij(ims,kms,jms,P_smnsmn),    &
                     grid%tke_2,                       & 
                     grid%rdzw, grid%dx, grid%dy,      &
                     config_flags,                     &
                     ids, ide, jds, jde, kds, kde,     &
                     ims, ime, jms, jme, kms, kme,     &
                     ips, ipe, jps, jpe, kps, kpe,     &
                     grid%i_start(ij), grid%i_end(ij), &
                     grid%j_start(ij), grid%j_end(ij), &
                     k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL calc_m12( nba_mij(ims,kms,jms,P_m12),       &
                     grid%defor11, grid%defor22,       &
                     grid%defor12, grid%defor13,       &
                     grid%defor23,                     &
                     grid%defor12_cc,                  &
                     grid%defor13_cc,                  &
                     grid%defor23_cc,                  &
                     nba_rij(ims,kms,jms,P_r12),       &
                     nba_rij(ims,kms,jms,P_r13),       &
                     nba_rij(ims,kms,jms,P_r23),       &
                     nba_rij(ims,kms,jms,P_r12_cc),    &
                     nba_rij(ims,kms,jms,P_r13_cc),    &
                     nba_rij(ims,kms,jms,P_r23_cc),    &
                     nba_rij(ims,kms,jms,P_smnsmn),    &
                     grid%tke_2,                       & 
                     grid%rdzw, grid%dx, grid%dy,      &
                     config_flags,                     &
                     ids, ide, jds, jde, kds, kde,     &
                     ims, ime, jms, jme, kms, kme,     &
                     ips, ipe, jps, jpe, kps, kpe,     &
                     grid%i_start(ij), grid%i_end(ij), &
                     grid%j_start(ij), grid%j_end(ij), &
                     k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL calc_m13( nba_mij(ims,kms,jms,P_m13),       &
                     grid%defor11, grid%defor33,       &
                     grid%defor12, grid%defor13,       &
                     grid%defor23,                     &
                     grid%defor12_cc,                  &
                     grid%defor13_cc,                  &
                     grid%defor23_cc,                  &
                     nba_rij(ims,kms,jms,P_r12),       &
                     nba_rij(ims,kms,jms,P_r13),       &
                     nba_rij(ims,kms,jms,P_r23),       &
                     nba_rij(ims,kms,jms,P_r12_cc),    &
                     nba_rij(ims,kms,jms,P_r13_cc),    &
                     nba_rij(ims,kms,jms,P_r23_cc),    &
                     nba_rij(ims,kms,jms,P_smnsmn),    &
                     grid%tke_2,                       & 
                     grid%rdzw, grid%dx, grid%dy,      &
                     grid%fnm, grid%fnp,               &
                     config_flags,                     &
                     ids, ide, jds, jde, kds, kde,     &
                     ims, ime, jms, jme, kms, kme,     &
                     ips, ipe, jps, jpe, kps, kpe,     &
                     grid%i_start(ij), grid%i_end(ij), &
                     grid%j_start(ij), grid%j_end(ij), &
                     k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO





    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL calc_m23( nba_mij(ims,kms,jms,P_m23),       &
                     grid%defor22, grid%defor33,       &
                     grid%defor12, grid%defor13,       &
                     grid%defor23,                     &
                     grid%defor12_cc,                  &
                     grid%defor13_cc,                  &
                     grid%defor23_cc,                  &
                     nba_rij(ims,kms,jms,P_r12),       &
                     nba_rij(ims,kms,jms,P_r13),       &
                     nba_rij(ims,kms,jms,P_r23),       &
                     nba_rij(ims,kms,jms,P_r12_cc),    &
                     nba_rij(ims,kms,jms,P_r13_cc),    &
                     nba_rij(ims,kms,jms,P_r23_cc),    &
                     nba_rij(ims,kms,jms,P_smnsmn),    &
                     grid%tke_2,                       & 
                     grid%rdzw, grid%dx, grid%dy,      &
                     grid%fnm, grid%fnp,               &
                     config_flags,                     &
                     ids, ide, jds, jde, kds, kde,     &
                     ims, ime, jms, jme, kms, kme,     &
                     ips, ipe, jps, jpe, kps, kpe,     &
                     grid%i_start(ij), grid%i_end(ij), &
                     grid%j_start(ij), grid%j_end(ij), &
                     k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO











CALL HALO_EM_NBA_MIJ_sub ( grid, &
  num_nba_mij, &
  nba_mij, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_NBA_MIJ_sub ( grid, &
  config_flags, &
  num_nba_mij, &
  nba_mij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL set_physical_bc3d_m( nba_mij(ims,kms,jms,P_m11), 'p',  &
                                config_flags,                     &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end                )
      
      CALL set_physical_bc3d_m( nba_mij(ims,kms,jms,P_m22), 'p',  &
                                config_flags,                     &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end                )
      
      CALL set_physical_bc3d_m( nba_mij(ims,kms,jms,P_m33), 'p',  &
                                config_flags,                     &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end                )

      CALL set_physical_bc3d_m( nba_mij(ims,kms,jms,P_m12), 'd',  &
                                config_flags,                     &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end                )

      CALL set_physical_bc3d_m( nba_mij(ims,kms,jms,P_m13), 'e',  &
                                config_flags,                     &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end                )

      CALL set_physical_bc3d_m( nba_mij(ims,kms,jms,P_m23), 'f',  &
                                config_flags,                     &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end                )

    ENDDO 
    !$OMP END PARALLEL DO

!    !$OMP PARALLEL DO   &
!    !$OMP PRIVATE ( ij )












!    !$OMP END PARALLEL DO







  ENDIF 

  IF ( config_flags%lasd_opt .GT. 0 ) THEN












    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL proj_lasd_c( lasd_uvw_bar_hat(ims,kms,jms,P_ubar), &
                        lasd_uvw_bar_hat(ims,kms,jms,P_uhat), &
                        lasd_uvw_bar_hat(ims,kms,jms,P_vbar), &
                        lasd_uvw_bar_hat(ims,kms,jms,P_vhat), &
                        lasd_uvw_bar_hat(ims,kms,jms,P_wbar), &
                        lasd_uvw_bar_hat(ims,kms,jms,P_what), &
                        lasd_Lij(ims,kms,jms,P_L11),          &
                        lasd_Lij(ims,kms,jms,P_L22),          &
                        lasd_Lij(ims,kms,jms,P_L33),          &
                        lasd_Lij(ims,kms,jms,P_L12),          &
                        lasd_Lij(ims,kms,jms,P_L13),          &
                        lasd_Lij(ims,kms,jms,P_L23),          &
                        grid%u_2,grid%v_2,grid%w_2,           &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        ips, ipe, jps, jpe, kps, kpe,         &
                        grid%i_start(ij), grid%i_end(ij),     &
                        grid%j_start(ij), grid%j_end(ij),     &
                        k_start, k_end                        )


    ENDDO 
    !$OMP END PARALLEL DO



  






















CALL HALO_EM_LASD_UVW_BAR_HAT_sub ( grid, &
  num_lasd_uvw_bar_hat, &
  lasd_uvw_bar_hat, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_LASD_LIJ_sub ( grid, &
  num_lasd_lij, &
  lasd_lij, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_UVW_BAR_HAT_sub ( grid, &
  config_flags, &
  num_lasd_uvw_bar_hat, &
  lasd_uvw_bar_hat, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_LIJ_sub ( grid, &
  config_flags, &
  num_lasd_lij, &
  lasd_lij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )



        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( lasd_uvw_bar_hat(ims,kms,jms,P_ubar), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_uvw_bar_hat(ims,kms,jms,P_uhat), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_uvw_bar_hat(ims,kms,jms,P_vbar), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_uvw_bar_hat(ims,kms,jms,P_vhat), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_uvw_bar_hat(ims,kms,jms,P_wbar), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_uvw_bar_hat(ims,kms,jms,P_what), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L11), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L22), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L33), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L12), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L13), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L23), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

    ENDDO 
    !$OMP END PARALLEL DO


  IF (  config_flags%filter_opt .EQ. 3  ) THEN








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL uvw_s_c_lasd( grid%usc, grid%vsc, grid%ws,            &
                         grid%uusc, grid%vvsc, grid%wws,         &
                         grid%uvsd, grid%uwse, grid%vwsf,        &
                         lasd_uvw_bar_hat(ims,kms,jms,P_ubar),   &
                         lasd_uvw_bar_hat(ims,kms,jms,P_vbar),   &
                         lasd_uvw_bar_hat(ims,kms,jms,P_wbar),   &
                         lasd_Lij(ims,kms,jms,P_L11),            &
                         lasd_Lij(ims,kms,jms,P_L22),            &
                         lasd_Lij(ims,kms,jms,P_L33),            &
                         lasd_Lij(ims,kms,jms,P_L12),            &
                         lasd_Lij(ims,kms,jms,P_L13),            &
                         lasd_Lij(ims,kms,jms,P_L23),            &
                         grid%ht, grid%rdx, grid%rdy,            &
                         grid%cf1, grid%cf2, grid%cf3,           &
                         config_flags%lasdvsbc_opt,              &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         ips, ipe, jps, jpe, kps, kpe,           &
                         grid%i_start(ij), grid%i_end(ij),       &
                         grid%j_start(ij), grid%j_end(ij),       &
                         k_start, k_end                          )

    ENDDO 
    !$OMP END PARALLEL DO


























CALL HALO_EM_LASD_PUVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_PUVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )



    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 


      CALL set_physical_bc2d_m( grid%usc, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vsc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )
 
      CALL set_physical_bc2d_m( grid%ws, 't', config_flags,       &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uusc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vvsc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wws, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uvsd, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uwse, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vwsf, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

    ENDDO 
    !$OMP END PARALLEL DO

  ENDIF  









        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL lasd_filter( lasd_uvw_bar_hat(ims,kms,jms,P_ubar), &
                            lasd_uvw_bar_hat(ims,kms,jms,P_ubar), & 
                            grid%usc,                             & 
                            'toph', config_flags%filter_opt,      &                          
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        ) 

          CALL lasd_filter( lasd_uvw_bar_hat(ims,kms,jms,P_vbar), &
                            lasd_uvw_bar_hat(ims,kms,jms,P_vbar), &
                            grid%vsc,                             & 
                            'toph', config_flags%filter_opt,      &                             
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_uvw_bar_hat(ims,kms,jms,P_wbar), &
                            lasd_uvw_bar_hat(ims,kms,jms,P_wbar), & 
                            grid%ws,                              & 
                            'toph', config_flags%filter_opt,      &                            
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Lij(ims,kms,jms,P_L11),          &
                            lasd_Lij(ims,kms,jms,P_L11),          &
                            grid%uusc,                            &   
                            'toph', config_flags%filter_opt,      &                           
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Lij(ims,kms,jms,P_L22),          &
                            lasd_Lij(ims,kms,jms,P_L22),          & 
                            grid%vvsc,                            &  
                            'toph', config_flags%filter_opt,      &                           
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Lij(ims,kms,jms,P_L33),          &
                            lasd_Lij(ims,kms,jms,P_L33),          & 
                            grid%wws,                             &   
                            'toph', config_flags%filter_opt,      &                           
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Lij(ims,kms,jms,P_L12),          &
                            lasd_Lij(ims,kms,jms,P_L12),          &
                            grid%uvsd,                            & 
                            'toph', config_flags%filter_opt,      &                             
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Lij(ims,kms,jms,P_L13),          &
                            lasd_Lij(ims,kms,jms,P_L13),          & 
                            grid%uwse,                            & 
                            'toph', config_flags%filter_opt,      &                            
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Lij(ims,kms,jms,P_L23),          &
                            lasd_Lij(ims,kms,jms,P_L23),          &
                            grid%vwsf,                            & 
                            'toph', config_flags%filter_opt,      &                             
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )
 

        ENDDO 
        !$OMP END PARALLEL DO













CALL HALO_EM_LASD_UVW_BAR_HAT_sub ( grid, &
  num_lasd_uvw_bar_hat, &
  lasd_uvw_bar_hat, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_LASD_LIJ_sub ( grid, &
  num_lasd_lij, &
  lasd_lij, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_UVW_BAR_HAT_sub ( grid, &
  config_flags, &
  num_lasd_uvw_bar_hat, &
  lasd_uvw_bar_hat, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_LIJ_sub ( grid, &
  config_flags, &
  num_lasd_lij, &
  lasd_lij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( lasd_uvw_bar_hat(ims,kms,jms,P_ubar), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_uvw_bar_hat(ims,kms,jms,P_vbar), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_uvw_bar_hat(ims,kms,jms,P_wbar), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L11), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L22), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L33), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L12), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L13), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L23), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

    ENDDO 
    !$OMP END PARALLEL DO


  IF (  config_flags%filter_opt .EQ. 3  ) THEN








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL uvw_s_c_lasd( grid%usc, grid%vsc, grid%ws,            &
                         grid%uusc, grid%vvsc, grid%wws,         &
                         grid%uvsd, grid%uwse, grid%vwsf,        &
                         lasd_uvw_bar_hat(ims,kms,jms,P_ubar),   &
                         lasd_uvw_bar_hat(ims,kms,jms,P_vbar),   &
                         lasd_uvw_bar_hat(ims,kms,jms,P_wbar),   &
                         lasd_Lij(ims,kms,jms,P_L11),            &
                         lasd_Lij(ims,kms,jms,P_L22),            &
                         lasd_Lij(ims,kms,jms,P_L33),            &
                         lasd_Lij(ims,kms,jms,P_L12),            &
                         lasd_Lij(ims,kms,jms,P_L13),            &
                         lasd_Lij(ims,kms,jms,P_L23),            &
                         grid%ht, grid%rdx, grid%rdy,            &
                         grid%cf1, grid%cf2, grid%cf3,           &
                         config_flags%lasdvsbc_opt,              &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         ips, ipe, jps, jpe, kps, kpe,           &
                         grid%i_start(ij), grid%i_end(ij),       &
                         grid%j_start(ij), grid%j_end(ij),       &
                         k_start, k_end                          )

    ENDDO 
    !$OMP END PARALLEL DO














CALL HALO_EM_LASD_PUVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_PUVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 


      CALL set_physical_bc2d_m( grid%usc, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vsc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )
 
      CALL set_physical_bc2d_m( grid%ws, 't', config_flags,       &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uusc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vvsc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wws, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uvsd, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uwse, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vwsf, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

    ENDDO 
    !$OMP END PARALLEL DO


  ENDIF  








        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL lasd_filter( lasd_uvw_bar_hat(ims,kms,jms,P_uhat), &
                            lasd_uvw_bar_hat(ims,kms,jms,P_ubar), & 
                            grid%usc,                             &
                            'test', config_flags%filter_opt,      &                              
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        ) 

          CALL lasd_filter( lasd_uvw_bar_hat(ims,kms,jms,P_vhat), &
                            lasd_uvw_bar_hat(ims,kms,jms,P_vbar), &
                            grid%vsc,                             & 
                            'test', config_flags%filter_opt,      &                               
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_uvw_bar_hat(ims,kms,jms,P_what), &
                            lasd_uvw_bar_hat(ims,kms,jms,P_wbar), &
                            grid%ws,                              &
                            'test', config_flags%filter_opt,      &                                
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         &
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Qij(ims,kms,jms,P_Q11),          &
                            lasd_Lij(ims,kms,jms,P_L11),          &
                            grid%uusc,                            & 
                            'test', config_flags%filter_opt,      &                              
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Qij(ims,kms,jms,P_Q22),          &
                            lasd_Lij(ims,kms,jms,P_L22),          &
                            grid%vvsc,                            &
                            'test', config_flags%filter_opt,      &                                
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Qij(ims,kms,jms,P_Q33),          &
                            lasd_Lij(ims,kms,jms,P_L33),          &
                            grid%wws,                             & 
                            'test', config_flags%filter_opt,      &                               
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Qij(ims,kms,jms,P_Q12),          &
                            lasd_Lij(ims,kms,jms,P_L12),          &  
                            grid%uvsd,                            &
                            'test', config_flags%filter_opt,      &                              
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Qij(ims,kms,jms,P_Q13),          &
                            lasd_Lij(ims,kms,jms,P_L13),          & 
                            grid%uwse,                            &
                            'test', config_flags%filter_opt,      &                              
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Qij(ims,kms,jms,P_Q23),          &
                            lasd_Lij(ims,kms,jms,P_L23),          & 
                            grid%vwsf,                            &
                            'test', config_flags%filter_opt,      &                               
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )
 

        ENDDO 
        !$OMP END PARALLEL DO







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL calc_Lij ( lasd_Lij(ims,kms,jms,P_L11),          &
                 lasd_uvw_bar_hat(ims,kms,jms,P_ubar), &
                 lasd_uvw_bar_hat(ims,kms,jms,P_ubar), &
                 lasd_Lij(ims,kms,jms,P_L11),          &
                 ids, ide, jds, jde, kds, kde,         &
                 ims, ime, jms, jme, kms, kme,         &
                 ips, ipe, jps, jpe, kps, kpe,         &
                 grid%i_start(ij), grid%i_end(ij),     &
                 grid%j_start(ij), grid%j_end(ij),     &
                 k_start, k_end                        )

      CALL calc_Lij ( lasd_Lij(ims,kms,jms,P_L22),          &
                 lasd_uvw_bar_hat(ims,kms,jms,P_vbar), &
                 lasd_uvw_bar_hat(ims,kms,jms,P_vbar), &
                 lasd_Lij(ims,kms,jms,P_L22),          &
                 ids, ide, jds, jde, kds, kde,         &
                 ims, ime, jms, jme, kms, kme,         &
                 ips, ipe, jps, jpe, kps, kpe,         &
                 grid%i_start(ij), grid%i_end(ij),     &
                 grid%j_start(ij), grid%j_end(ij),     &
                 k_start, k_end                        )
 
      CALL calc_Lij ( lasd_Lij(ims,kms,jms,P_L33),          &
                 lasd_uvw_bar_hat(ims,kms,jms,P_wbar), &
                 lasd_uvw_bar_hat(ims,kms,jms,P_wbar), &
                 lasd_Lij(ims,kms,jms,P_L33),          &
                 ids, ide, jds, jde, kds, kde,         &
                 ims, ime, jms, jme, kms, kme,         &
                 ips, ipe, jps, jpe, kps, kpe,         &
                 grid%i_start(ij), grid%i_end(ij),     &
                 grid%j_start(ij), grid%j_end(ij),     &
                 k_start, k_end                        )

      CALL calc_Lij ( lasd_Lij(ims,kms,jms,P_L12),          &
                 lasd_uvw_bar_hat(ims,kms,jms,P_ubar), &
                 lasd_uvw_bar_hat(ims,kms,jms,P_vbar), &
                 lasd_Lij(ims,kms,jms,P_L12),          &
                 ids, ide, jds, jde, kds, kde,         &
                 ims, ime, jms, jme, kms, kme,         &
                 ips, ipe, jps, jpe, kps, kpe,         &
                 grid%i_start(ij), grid%i_end(ij),     &
                 grid%j_start(ij), grid%j_end(ij),     &
                 k_start, k_end                        )

      CALL calc_Lij ( lasd_Lij(ims,kms,jms,P_L13),          &
                 lasd_uvw_bar_hat(ims,kms,jms,P_ubar), &
                 lasd_uvw_bar_hat(ims,kms,jms,P_wbar), &
                 lasd_Lij(ims,kms,jms,P_L13),          &
                 ids, ide, jds, jde, kds, kde,         &
                 ims, ime, jms, jme, kms, kme,         &
                 ips, ipe, jps, jpe, kps, kpe,         &
                 grid%i_start(ij), grid%i_end(ij),     &
                 grid%j_start(ij), grid%j_end(ij),     &
                 k_start, k_end                        )

      CALL calc_Lij ( lasd_Lij(ims,kms,jms,P_L23),          &
                 lasd_uvw_bar_hat(ims,kms,jms,P_vbar), &
                 lasd_uvw_bar_hat(ims,kms,jms,P_wbar), &
                 lasd_Lij(ims,kms,jms,P_L23),          &
                 ids, ide, jds, jde, kds, kde,         &
                 ims, ime, jms, jme, kms, kme,         &
                 ips, ipe, jps, jpe, kps, kpe,         &
                 grid%i_start(ij), grid%i_end(ij),     &
                 grid%j_start(ij), grid%j_end(ij),     &
                 k_start, k_end                        )

    ENDDO 
    !$OMP END PARALLEL DO






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL calc_Lij ( lasd_Qij(ims,kms,jms,P_Q11),          &
                 lasd_uvw_bar_hat(ims,kms,jms,P_uhat), &
                 lasd_uvw_bar_hat(ims,kms,jms,P_uhat), &
                 lasd_Qij(ims,kms,jms,P_Q11),          &
                 ids, ide, jds, jde, kds, kde,         &
                 ims, ime, jms, jme, kms, kme,         &
                 ips, ipe, jps, jpe, kps, kpe,         &
                 grid%i_start(ij), grid%i_end(ij),     &
                 grid%j_start(ij), grid%j_end(ij),     &
                 k_start, k_end                        )

      CALL calc_Lij ( lasd_Qij(ims,kms,jms,P_Q22),          &
                 lasd_uvw_bar_hat(ims,kms,jms,P_vhat), &
                 lasd_uvw_bar_hat(ims,kms,jms,P_vhat), &
                 lasd_Qij(ims,kms,jms,P_Q22),          &
                 ids, ide, jds, jde, kds, kde,         &
                 ims, ime, jms, jme, kms, kme,         &
                 ips, ipe, jps, jpe, kps, kpe,         &
                 grid%i_start(ij), grid%i_end(ij),     &
                 grid%j_start(ij), grid%j_end(ij),     &
                 k_start, k_end                        )
 
      CALL calc_Lij ( lasd_Qij(ims,kms,jms,P_Q33),          &
                 lasd_uvw_bar_hat(ims,kms,jms,P_what), &
                 lasd_uvw_bar_hat(ims,kms,jms,P_what), &
                 lasd_Qij(ims,kms,jms,P_Q33),          &
                 ids, ide, jds, jde, kds, kde,         &
                 ims, ime, jms, jme, kms, kme,         &
                 ips, ipe, jps, jpe, kps, kpe,         &
                 grid%i_start(ij), grid%i_end(ij),     &
                 grid%j_start(ij), grid%j_end(ij),     &
                 k_start, k_end                        )

      CALL calc_Lij ( lasd_Qij(ims,kms,jms,P_Q12),          &
                 lasd_uvw_bar_hat(ims,kms,jms,P_uhat), &
                 lasd_uvw_bar_hat(ims,kms,jms,P_vhat), &
                 lasd_Qij(ims,kms,jms,P_Q12),          &
                 ids, ide, jds, jde, kds, kde,         &
                 ims, ime, jms, jme, kms, kme,         &
                 ips, ipe, jps, jpe, kps, kpe,         &
                 grid%i_start(ij), grid%i_end(ij),     &
                 grid%j_start(ij), grid%j_end(ij),     &
                 k_start, k_end                        )

      CALL calc_Lij ( lasd_Qij(ims,kms,jms,P_Q13),          &
                 lasd_uvw_bar_hat(ims,kms,jms,P_uhat), &
                 lasd_uvw_bar_hat(ims,kms,jms,P_what), &
                 lasd_Qij(ims,kms,jms,P_Q13),          &
                 ids, ide, jds, jde, kds, kde,         &
                 ims, ime, jms, jme, kms, kme,         &
                 ips, ipe, jps, jpe, kps, kpe,         &
                 grid%i_start(ij), grid%i_end(ij),     &
                 grid%j_start(ij), grid%j_end(ij),     &
                 k_start, k_end                        )

      CALL calc_Lij ( lasd_Qij(ims,kms,jms,P_Q23),          &
                 lasd_uvw_bar_hat(ims,kms,jms,P_vhat), &
                 lasd_uvw_bar_hat(ims,kms,jms,P_what), &
                 lasd_Qij(ims,kms,jms,P_Q23),          &
                 ids, ide, jds, jde, kds, kde,         &
                 ims, ime, jms, jme, kms, kme,         &
                 ips, ipe, jps, jpe, kps, kpe,         &
                 grid%i_start(ij), grid%i_end(ij),     &
                 grid%j_start(ij), grid%j_end(ij),     &
                 k_start, k_end                        )

    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_LASD_LIJ_sub ( grid, &
  num_lasd_lij, &
  lasd_lij, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_LIJ_sub ( grid, &
  config_flags, &
  num_lasd_lij, &
  lasd_lij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_LASD_QIJ_sub ( grid, &
  num_lasd_qij, &
  lasd_qij, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_QIJ_sub ( grid, &
  config_flags, &
  num_lasd_qij, &
  lasd_qij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L11), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L22), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L33), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L12), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L13), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Lij(ims,kms,jms,P_L23), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Qij(ims,kms,jms,P_Q11), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Qij(ims,kms,jms,P_Q22), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Qij(ims,kms,jms,P_Q33), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Qij(ims,kms,jms,P_Q12), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Qij(ims,kms,jms,P_Q13), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Qij(ims,kms,jms,P_Q23), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )
 
    ENDDO 
    !$OMP END PARALLEL DO






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

        CALL calc_half_sij_c( lasd_Sij(ims,kms,jms,P_S11),       &
                              lasd_Sij(ims,kms,jms,P_S22),       &
                              lasd_Sij(ims,kms,jms,P_S33),       &
                              lasd_Sij(ims,kms,jms,P_S12),       &
                              lasd_Sij(ims,kms,jms,P_S13),       &
                              lasd_Sij(ims,kms,jms,P_S23),       &
                              grid%defor11, grid%defor22,        &
                              grid%defor33, grid%defor12,        &
                              grid%defor13, grid%defor23,        &
                              grid%defor12_cc,                   &
                              grid%defor13_cc,                   &
                              grid%defor23_cc,                   &
                              config_flags%defor_opt,            &
                              ids, ide, jds, jde, kds, kde,      &
                              ims, ime, jms, jme, kms, kme,      &
                              ips, ipe, jps, jpe, kps, kpe,      &
                              grid%i_start(ij), grid%i_end(ij),  &
                              grid%j_start(ij), grid%j_end(ij),  &
                              k_start    , k_end                 )

    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_LASD_SIJ_sub ( grid, &
  num_lasd_sij, &
  lasd_sij, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_SIJ_sub ( grid, &
  config_flags, &
  num_lasd_sij, &
  lasd_sij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( lasd_Sij(ims,kms,jms,P_S11), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Sij(ims,kms,jms,P_S22), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Sij(ims,kms,jms,P_S33), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Sij(ims,kms,jms,P_S12), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Sij(ims,kms,jms,P_S13), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_Sij(ims,kms,jms,P_S23), 't',    &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )
 
    ENDDO 
    !$OMP END PARALLEL DO









    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

        CALL calc_sijsij( lasd_TC(ims,kms,jms,P_SS),        &
                          lasd_Sij(ims,kms,jms,P_S11),      &
                          lasd_Sij(ims,kms,jms,P_S22),      &
                          lasd_Sij(ims,kms,jms,P_S33),      &
                          lasd_Sij(ims,kms,jms,P_S12),      &
                          lasd_Sij(ims,kms,jms,P_S13),      &
                          lasd_Sij(ims,kms,jms,P_S23),      &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          ips, ipe, jps, jpe, kps, kpe,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end                )

    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_LASD_TC_sub ( grid, &
  num_lasd_tc, &
  lasd_tc, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_TC_sub ( grid, &
  config_flags, &
  num_lasd_tc, &
  lasd_tc, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( lasd_TC(ims,kms,jms,P_SS), 't',      &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )
 
    ENDDO 
    !$OMP END PARALLEL DO







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL calc_Sij_bar_hat( lasd_s_bar_hat(ims,kms,jms,P_s11bar), &
                             lasd_s_bar_hat(ims,kms,jms,P_s11hat), &
                             lasd_s_bar_hat(ims,kms,jms,P_s22bar), &
                             lasd_s_bar_hat(ims,kms,jms,P_s22hat), &
                             lasd_s_bar_hat(ims,kms,jms,P_s33bar), &
                             lasd_s_bar_hat(ims,kms,jms,P_s33hat), &
                             lasd_s_bar_hat(ims,kms,jms,P_s12bar), &
                             lasd_s_bar_hat(ims,kms,jms,P_s12hat), &
                             lasd_s_bar_hat(ims,kms,jms,P_s13bar), &
                             lasd_s_bar_hat(ims,kms,jms,P_s13hat), &
                             lasd_s_bar_hat(ims,kms,jms,P_s23bar), &
                             lasd_s_bar_hat(ims,kms,jms,P_s23hat), &
                             lasd_Sij(ims,kms,jms,P_S11),          &
                             lasd_Sij(ims,kms,jms,P_S22),          &
                             lasd_Sij(ims,kms,jms,P_S33),          &
                             lasd_Sij(ims,kms,jms,P_S12),          &
                             lasd_Sij(ims,kms,jms,P_S13),          &
                             lasd_Sij(ims,kms,jms,P_S23),          &
                             ids, ide, jds, jde, kds, kde,         &
                             ims, ime, jms, jme, kms, kme,         &
                             ips, ipe, jps, jpe, kps, kpe,         &
                             grid%i_start(ij), grid%i_end(ij),     &
                             grid%j_start(ij), grid%j_end(ij),     &
                             k_start, k_end                        )


    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_LASD_S_BAR_HAT_sub ( grid, &
  num_lasd_s_bar_hat, &
  lasd_s_bar_hat, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_S_BAR_HAT_sub ( grid, &
  config_flags, &
  num_lasd_s_bar_hat, &
  lasd_s_bar_hat, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s11bar), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s11hat), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s22bar), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s22hat), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s33bar), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s33hat), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s12bar), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s12hat), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s13bar), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s13hat), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s23bar), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s23hat), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

 
    ENDDO 
    !$OMP END PARALLEL DO








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL calc_s_sij ( lasd_Gij(ims,kms,jms,P_G11),          &
                        lasd_Gij(ims,kms,jms,P_G22),          &
                        lasd_Gij(ims,kms,jms,P_G33),          &
                        lasd_Gij(ims,kms,jms,P_G12),          &
                        lasd_Gij(ims,kms,jms,P_G13),          &
                        lasd_Gij(ims,kms,jms,P_G23),          &
                        lasd_s_bar_hat(ims,kms,jms,P_s11bar), &
                        lasd_s_bar_hat(ims,kms,jms,P_s22bar), &
                        lasd_s_bar_hat(ims,kms,jms,P_s33bar), &
                        lasd_s_bar_hat(ims,kms,jms,P_s12bar), &
                        lasd_s_bar_hat(ims,kms,jms,P_s13bar), &
                        lasd_s_bar_hat(ims,kms,jms,P_s23bar), &
                        lasd_TC(ims,kms,jms,P_SS),            &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        ips, ipe, jps, jpe, kps, kpe,         &
                        grid%i_start(ij), grid%i_end(ij),     &
                        grid%j_start(ij), grid%j_end(ij),     &
                        k_start, k_end                        )


    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_LASD_GIJ_sub ( grid, &
  num_lasd_gij, &
  lasd_gij, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_GIJ_sub ( grid, &
  config_flags, &
  num_lasd_gij, &
  lasd_gij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G11), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G22), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G33), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G12), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G13), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G23), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )


    ENDDO 
    !$OMP END PARALLEL DO




  IF (  config_flags%filter_opt .EQ. 3  ) THEN








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL uvw_s_c_lasd( grid%usc, grid%vsc, grid%ws,            & 
                         grid%uusc, grid%vvsc, grid%wws,         &
                         grid%uvsd, grid%uwse, grid%vwsf,        &
                         lasd_uvw_bar_hat(ims,kms,jms,P_ubar),   & 
                         lasd_uvw_bar_hat(ims,kms,jms,P_vbar),   & 
                         lasd_uvw_bar_hat(ims,kms,jms,P_wbar),   & 
                         lasd_Gij(ims,kms,jms,P_G11),            &
                         lasd_Gij(ims,kms,jms,P_G22),            &
                         lasd_Gij(ims,kms,jms,P_G33),            &
                         lasd_Gij(ims,kms,jms,P_G12),            &
                         lasd_Gij(ims,kms,jms,P_G13),            &
                         lasd_Gij(ims,kms,jms,P_G23),            &
                         grid%ht, grid%rdx, grid%rdy,            &
                         grid%cf1, grid%cf2, grid%cf3,           &
                         config_flags%lasdvsbc_opt,              &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         ips, ipe, jps, jpe, kps, kpe,           &
                         grid%i_start(ij), grid%i_end(ij),       &
                         grid%j_start(ij), grid%j_end(ij),       &
                         k_start, k_end                          )

    ENDDO 
    !$OMP END PARALLEL DO














CALL HALO_EM_LASD_PUVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_PUVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 


      CALL set_physical_bc2d_m( grid%uusc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vvsc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wws, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uvsd, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uwse, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vwsf, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

    ENDDO 
    !$OMP END PARALLEL DO


  ENDIF  








        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL lasd_filter( lasd_Gij(ims,kms,jms,P_G11),          &
                            lasd_Gij(ims,kms,jms,P_G11),          & 
                            grid%uusc,                            &
                            'toph', config_flags%filter_opt,      &                               
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        ) 

          CALL lasd_filter( lasd_Gij(ims,kms,jms,P_G22),          &
                            lasd_Gij(ims,kms,jms,P_G22),          & 
                            grid%vvsc,                            &
                            'toph', config_flags%filter_opt,      &               
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Gij(ims,kms,jms,P_G33),          &
                            lasd_Gij(ims,kms,jms,P_G33),          & 
                            grid%wws,                             &
                            'toph', config_flags%filter_opt,      &               
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Gij(ims,kms,jms,P_G12),          &
                            lasd_Gij(ims,kms,jms,P_G12),          & 
                            grid%uvsd,                            &   
                            'toph', config_flags%filter_opt,      &            
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Gij(ims,kms,jms,P_G13),          &
                            lasd_Gij(ims,kms,jms,P_G13),          &  
                            grid%uwse,                            &
                            'toph', config_flags%filter_opt,      &              
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Gij(ims,kms,jms,P_G23),          &
                            lasd_Gij(ims,kms,jms,P_G23),          &
                            grid%vwsf,                            & 
                            'toph', config_flags%filter_opt,      &               
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )


        ENDDO 
        !$OMP END PARALLEL DO












CALL HALO_EM_LASD_GIJ_sub ( grid, &
  num_lasd_gij, &
  lasd_gij, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_GIJ_sub ( grid, &
  config_flags, &
  num_lasd_gij, &
  lasd_gij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G11), 't',            &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G22), 't',            &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G33), 't',            &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G12), 't',            &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G13), 't',            &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G23), 't',            &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )


    ENDDO 
    !$OMP END PARALLEL DO


  IF (  config_flags%filter_opt .EQ. 3  ) THEN








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL uvw_s_c_lasd( grid%usc, grid%vsc, grid%ws,            & 
                         grid%uusc, grid%vvsc, grid%wws,         &
                         grid%uvsd, grid%uwse, grid%vwsf,        &
                         lasd_uvw_bar_hat(ims,kms,jms,P_ubar),   & 
                         lasd_uvw_bar_hat(ims,kms,jms,P_vbar),   & 
                         lasd_uvw_bar_hat(ims,kms,jms,P_wbar),   & 
                         lasd_Gij(ims,kms,jms,P_G11),            &
                         lasd_Gij(ims,kms,jms,P_G22),            &
                         lasd_Gij(ims,kms,jms,P_G33),            &
                         lasd_Gij(ims,kms,jms,P_G12),            &
                         lasd_Gij(ims,kms,jms,P_G13),            &
                         lasd_Gij(ims,kms,jms,P_G23),            &
                         grid%ht, grid%rdx, grid%rdy,            &
                         grid%cf1, grid%cf2, grid%cf3,           &
                         config_flags%lasdvsbc_opt,              &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         ips, ipe, jps, jpe, kps, kpe,           &
                         grid%i_start(ij), grid%i_end(ij),       &
                         grid%j_start(ij), grid%j_end(ij),       &
                         k_start, k_end                          )

    ENDDO 
    !$OMP END PARALLEL DO














CALL HALO_EM_LASD_PUVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_PUVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 


      CALL set_physical_bc2d_m( grid%uusc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vvsc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wws, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uvsd, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uwse, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vwsf, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

    ENDDO 
    !$OMP END PARALLEL DO


  ENDIF  








        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL lasd_filter( lasd_Nij(ims,kms,jms,P_N11),          &
                            lasd_Gij(ims,kms,jms,P_G11),          & 
                            grid%uusc,                            &
                            'test', config_flags%filter_opt,      &                                
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        ) 

          CALL lasd_filter( lasd_Nij(ims,kms,jms,P_N22),          &
                            lasd_Gij(ims,kms,jms,P_G22),          & 
                            grid%vvsc,                            &
                            'test', config_flags%filter_opt,      &                
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Nij(ims,kms,jms,P_N33),          &
                            lasd_Gij(ims,kms,jms,P_G33),          & 
                            grid%wws,                             &
                            'test', config_flags%filter_opt,      &                
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Nij(ims,kms,jms,P_N12),          &
                            lasd_Gij(ims,kms,jms,P_G12),          &
                            grid%uvsd,                            &
                            'test', config_flags%filter_opt,      &                 
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Nij(ims,kms,jms,P_N13),          &
                            lasd_Gij(ims,kms,jms,P_G13),          &
                            grid%uwse,                            &  
                            'test', config_flags%filter_opt,      &               
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_Nij(ims,kms,jms,P_N23),          &
                            lasd_Gij(ims,kms,jms,P_G23),          & 
                            grid%vwsf,                            &
                            'test', config_flags%filter_opt,      &                
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )


        ENDDO 
        !$OMP END PARALLEL DO












CALL HALO_EM_LASD_NIJ_sub ( grid, &
  num_lasd_nij, &
  lasd_nij, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_NIJ_sub ( grid, &
  config_flags, &
  num_lasd_nij, &
  lasd_nij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

       CALL set_physical_bc3d_m( lasd_Nij(ims,kms,jms,P_N11), 't',            &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Nij(ims,kms,jms,P_N22), 't',            &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Nij(ims,kms,jms,P_N33), 't',            &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Nij(ims,kms,jms,P_N12), 't',            &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Nij(ims,kms,jms,P_N13), 't',            &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Nij(ims,kms,jms,P_N23), 't',            &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )


    ENDDO 
    !$OMP END PARALLEL DO


  IF (  config_flags%filter_opt .EQ. 3  ) THEN








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL uvw_s_c_lasd( grid%usc, grid%vsc, grid%ws,            & 
                         grid%uusc, grid%vvsc, grid%wws,         &
                         grid%uvsd, grid%uwse, grid%vwsf,        &
                         lasd_uvw_bar_hat(ims,kms,jms,P_ubar),   & 
                         lasd_uvw_bar_hat(ims,kms,jms,P_vbar),   & 
                         lasd_uvw_bar_hat(ims,kms,jms,P_wbar),   & 
                         lasd_s_bar_hat(ims,kms,jms,P_s11bar),   &
                         lasd_s_bar_hat(ims,kms,jms,P_s22bar),   &
                         lasd_s_bar_hat(ims,kms,jms,P_s33bar),   &
                         lasd_s_bar_hat(ims,kms,jms,P_s12bar),   &
                         lasd_s_bar_hat(ims,kms,jms,P_s13bar),   &
                         lasd_s_bar_hat(ims,kms,jms,P_s23bar),   &
                         grid%ht, grid%rdx, grid%rdy,            &
                         grid%cf1, grid%cf2, grid%cf3,           &
                         config_flags%lasdvsbc_opt,              &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         ips, ipe, jps, jpe, kps, kpe,           &
                         grid%i_start(ij), grid%i_end(ij),       &
                         grid%j_start(ij), grid%j_end(ij),       &
                         k_start, k_end                          )

    ENDDO 
    !$OMP END PARALLEL DO














CALL HALO_EM_LASD_PUVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_PUVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 


      CALL set_physical_bc2d_m( grid%uusc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vvsc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wws, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uvsd, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uwse, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vwsf, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

    ENDDO 
    !$OMP END PARALLEL DO


  ENDIF  







        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL lasd_filter( lasd_s_bar_hat(ims,kms,jms,P_s11bar), &
                            lasd_s_bar_hat(ims,kms,jms,P_s11bar), & 
                            grid%uusc,                            &  
                            'toph',  config_flags%filter_opt,     &                              
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        ) 

          CALL lasd_filter( lasd_s_bar_hat(ims,kms,jms,P_s22bar), &
                            lasd_s_bar_hat(ims,kms,jms,P_s22bar), & 
                            grid%vvsc,                            &
                            'toph',  config_flags%filter_opt,     &                              
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_s_bar_hat(ims,kms,jms,P_s33bar), & 
                            lasd_s_bar_hat(ims,kms,jms,P_s33bar), &
                            grid%wws,                             &  
                            'toph',  config_flags%filter_opt,     &                            
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_s_bar_hat(ims,kms,jms,P_s12bar), &
                            lasd_s_bar_hat(ims,kms,jms,P_s12bar), & 
                            grid%uvsd,                            &
                            'toph',  config_flags%filter_opt,     &                             
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_s_bar_hat(ims,kms,jms,P_s13bar), &
                            lasd_s_bar_hat(ims,kms,jms,P_s13bar), &
                            grid%uwse,                            &  
                            'toph',  config_flags%filter_opt,     &                           
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_s_bar_hat(ims,kms,jms,P_s23bar), &
                            lasd_s_bar_hat(ims,kms,jms,P_s23bar), & 
                            grid%vwsf,                            &
                            'toph',  config_flags%filter_opt,     &                           
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )


        ENDDO 
        !$OMP END PARALLEL DO












CALL HALO_EM_LASD_S_BAR_HAT_sub ( grid, &
  num_lasd_s_bar_hat, &
  lasd_s_bar_hat, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_S_BAR_HAT_sub ( grid, &
  config_flags, &
  num_lasd_s_bar_hat, &
  lasd_s_bar_hat, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s11bar), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )


       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s22bar), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s33bar), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )


       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s12bar), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s13bar), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s23bar), 't',   &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )


    ENDDO 
    !$OMP END PARALLEL DO


  IF (  config_flags%filter_opt .EQ. 3  ) THEN








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL uvw_s_c_lasd( grid%usc, grid%vsc, grid%ws,            & 
                         grid%uusc, grid%vvsc, grid%wws,         &
                         grid%uvsd, grid%uwse, grid%vwsf,        &
                         lasd_uvw_bar_hat(ims,kms,jms,P_ubar),   & 
                         lasd_uvw_bar_hat(ims,kms,jms,P_vbar),   & 
                         lasd_uvw_bar_hat(ims,kms,jms,P_wbar),   & 
                         lasd_s_bar_hat(ims,kms,jms,P_s11bar),   &
                         lasd_s_bar_hat(ims,kms,jms,P_s22bar),   &
                         lasd_s_bar_hat(ims,kms,jms,P_s33bar),   &
                         lasd_s_bar_hat(ims,kms,jms,P_s12bar),   &
                         lasd_s_bar_hat(ims,kms,jms,P_s13bar),   &
                         lasd_s_bar_hat(ims,kms,jms,P_s23bar),   &
                         grid%ht, grid%rdx, grid%rdy,            &
                         grid%cf1, grid%cf2, grid%cf3,           &
                         config_flags%lasdvsbc_opt,              &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         ips, ipe, jps, jpe, kps, kpe,           &
                         grid%i_start(ij), grid%i_end(ij),       &
                         grid%j_start(ij), grid%j_end(ij),       &
                         k_start, k_end                          )

    ENDDO 
    !$OMP END PARALLEL DO














CALL HALO_EM_LASD_PUVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_PUVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 


      CALL set_physical_bc2d_m( grid%uusc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vvsc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wws, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uvsd, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uwse, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vwsf, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

    ENDDO 
    !$OMP END PARALLEL DO


  ENDIF  







        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL lasd_filter( lasd_s_bar_hat(ims,kms,jms,P_s11hat), &
                            lasd_s_bar_hat(ims,kms,jms,P_s11bar), & 
                            grid%uusc,                            &    
                            'test', config_flags%filter_opt,      &                           
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        ) 

          CALL lasd_filter( lasd_s_bar_hat(ims,kms,jms,P_s22hat), &
                            lasd_s_bar_hat(ims,kms,jms,P_s22bar), &
                            grid%vvsc,                            &  
                            'test', config_flags%filter_opt,      &                                    
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_s_bar_hat(ims,kms,jms,P_s33hat), & 
                            lasd_s_bar_hat(ims,kms,jms,P_s33bar), &
                            grid%wws,                             &   
                            'test',  config_flags%filter_opt,     &                                   
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_s_bar_hat(ims,kms,jms,P_s12hat), &
                            lasd_s_bar_hat(ims,kms,jms,P_s12bar), &
                            grid%uvsd,                            &  
                            'test', config_flags%filter_opt,      &                                   
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_s_bar_hat(ims,kms,jms,P_s13hat), &
                            lasd_s_bar_hat(ims,kms,jms,P_s13bar), & 
                            grid%uwse,                            &  
                            'test', config_flags%filter_opt,      &                                
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )

          CALL lasd_filter( lasd_s_bar_hat(ims,kms,jms,P_s23hat), &
                            lasd_s_bar_hat(ims,kms,jms,P_s23bar), &  
                            grid%vwsf,                            &  
                            'test', config_flags%filter_opt,      &                            
                            ids, ide, jds, jde, kds, kde,         & 
                            ims, ime, jms, jme, kms, kme,         & 
                            ips, ipe, jps, jpe, kps, kpe,         & 
                            grid%i_start(ij), grid%i_end(ij),     & 
                            grid%j_start(ij), grid%j_end(ij),     & 
                            k_start, k_end                        )


        ENDDO 
        !$OMP END PARALLEL DO












CALL HALO_EM_LASD_S_BAR_HAT_sub ( grid, &
  num_lasd_s_bar_hat, &
  lasd_s_bar_hat, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_S_BAR_HAT_sub ( grid, &
  config_flags, &
  num_lasd_s_bar_hat, &
  lasd_s_bar_hat, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s11hat), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )


       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s22hat), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s33hat), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )


       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s12hat), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s13hat), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_s_bar_hat(ims,kms,jms,P_s23hat), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )


    ENDDO 
    !$OMP END PARALLEL DO







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

        CALL calc_sijsij  ( lasd_TC(ims,kms,jms,P_SS_BAR),        &
                            lasd_s_bar_hat(ims,kms,jms,P_s11bar), &
                            lasd_s_bar_hat(ims,kms,jms,P_s22bar), &
                            lasd_s_bar_hat(ims,kms,jms,P_s33bar), &
                            lasd_s_bar_hat(ims,kms,jms,P_s12bar), &
                            lasd_s_bar_hat(ims,kms,jms,P_s13bar), &
                            lasd_s_bar_hat(ims,kms,jms,P_s23bar), &
                            ids, ide, jds, jde, kds, kde,         &
                            ims, ime, jms, jme, kms, kme,         &
                            ips, ipe, jps, jpe, kps, kpe,         &
                            grid%i_start(ij), grid%i_end(ij),     &
                            grid%j_start(ij), grid%j_end(ij),     &
                            k_start    , k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_LASD_TC_sub ( grid, &
  num_lasd_tc, &
  lasd_tc, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_TC_sub ( grid, &
  config_flags, &
  num_lasd_tc, &
  lasd_tc, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( lasd_TC(ims,kms,jms,P_SS_BAR), 't',  &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )
 
    ENDDO 
    !$OMP END PARALLEL DO






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL calc_Gij ( lasd_Gij(ims,kms,jms,P_G11),           &
                      lasd_Gij(ims,kms,jms,P_G22),           &
                      lasd_Gij(ims,kms,jms,P_G33),           &
                      lasd_Gij(ims,kms,jms,P_G12),           &
                      lasd_Gij(ims,kms,jms,P_G13),           &
                      lasd_Gij(ims,kms,jms,P_G23),           &
                      lasd_TC(ims,kms,jms,P_SS_BAR),        &
                      lasd_s_bar_hat(ims,kms,jms,P_s11bar),  &
                      lasd_s_bar_hat(ims,kms,jms,P_s22bar),  &
                      lasd_s_bar_hat(ims,kms,jms,P_s33bar),  &
                      lasd_s_bar_hat(ims,kms,jms,P_s12bar),  &
                      lasd_s_bar_hat(ims,kms,jms,P_s13bar),  &
                      lasd_s_bar_hat(ims,kms,jms,P_s23bar),  &
                      grid%rdzw, grid%dx, grid%dy,           &
                      ids, ide, jds, jde, kds, kde,          &
                      ims, ime, jms, jme, kms, kme,          &
                      ips, ipe, jps, jpe, kps, kpe,          &
                      grid%i_start(ij), grid%i_end(ij),      &
                      grid%j_start(ij), grid%j_end(ij),      &
                      k_start, k_end                         )


    ENDDO 
    !$OMP END PARALLEL DO







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

        CALL calc_sijsij  ( lasd_TC(ims,kms,jms,P_SS_HAT),        &
                            lasd_s_bar_hat(ims,kms,jms,P_s11hat), &
                            lasd_s_bar_hat(ims,kms,jms,P_s22hat), &
                            lasd_s_bar_hat(ims,kms,jms,P_s33hat), &
                            lasd_s_bar_hat(ims,kms,jms,P_s12hat), &
                            lasd_s_bar_hat(ims,kms,jms,P_s13hat), &
                            lasd_s_bar_hat(ims,kms,jms,P_s23hat), &
                            ids, ide, jds, jde, kds, kde,         &
                            ims, ime, jms, jme, kms, kme,         &
                            ips, ipe, jps, jpe, kps, kpe,         &
                            grid%i_start(ij), grid%i_end(ij),     &
                            grid%j_start(ij), grid%j_end(ij),     &
                            k_start    , k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_LASD_TC_sub ( grid, &
  num_lasd_tc, &
  lasd_tc, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_TC_sub ( grid, &
  config_flags, &
  num_lasd_tc, &
  lasd_tc, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( lasd_TC(ims,kms,jms,P_SS_HAT), 't',  &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )
 
    ENDDO 
    !$OMP END PARALLEL DO







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL calc_Nij ( lasd_Nij(ims,kms,jms,P_N11),           &
                      lasd_Nij(ims,kms,jms,P_N22),           &
                      lasd_Nij(ims,kms,jms,P_N33),           &
                      lasd_Nij(ims,kms,jms,P_N12),           &
                      lasd_Nij(ims,kms,jms,P_N13),           &
                      lasd_Nij(ims,kms,jms,P_N23),           &
                      lasd_TC(ims,kms,jms,P_SS_HAT),         &
                      lasd_s_bar_hat(ims,kms,jms,P_s11hat),  &
                      lasd_s_bar_hat(ims,kms,jms,P_s22hat),  &
                      lasd_s_bar_hat(ims,kms,jms,P_s33hat),  &
                      lasd_s_bar_hat(ims,kms,jms,P_s12hat),  &
                      lasd_s_bar_hat(ims,kms,jms,P_s13hat),  &
                      lasd_s_bar_hat(ims,kms,jms,P_s23hat),  &
                      grid%rdzw, grid%dx, grid%dy,           &
                      ids, ide, jds, jde, kds, kde,          &
                      ims, ime, jms, jme, kms, kme,          &
                      ips, ipe, jps, jpe, kps, kpe,          &
                      grid%i_start(ij), grid%i_end(ij),      &
                      grid%j_start(ij), grid%j_end(ij),      &
                      k_start, k_end                         )


    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_LASD_GIJ_sub ( grid, &
  num_lasd_gij, &
  lasd_gij, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_GIJ_sub ( grid, &
  config_flags, &
  num_lasd_gij, &
  lasd_gij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_LASD_NIJ_sub ( grid, &
  num_lasd_nij, &
  lasd_nij, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_NIJ_sub ( grid, &
  config_flags, &
  num_lasd_nij, &
  lasd_nij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 



       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G11), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G22), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G33), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G12), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G13), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Gij(ims,kms,jms,P_G23), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Nij(ims,kms,jms,P_N11), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Nij(ims,kms,jms,P_N22), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Nij(ims,kms,jms,P_N33), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Nij(ims,kms,jms,P_N12), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Nij(ims,kms,jms,P_N13), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )

       CALL set_physical_bc3d_m( lasd_Nij(ims,kms,jms,P_N23), 't', &
                               config_flags,                                &
                               ids, ide, jds, jde, kds, kde,                &
                               ims, ime, jms, jme, kms, kme,                &
                               ips, ipe, jps, jpe, kps, kpe,                &
                               grid%i_start(ij), grid%i_end(ij),            &
                               grid%j_start(ij), grid%j_end(ij),            &
                               k_start, k_end                               )


    ENDDO 
    !$OMP END PARALLEL DO

 





    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL calc_contraction ( lasd_TC(ims,kms,jms,P_LG),        &
                              lasd_Lij(ims,kms,jms,P_L11),      &
                              lasd_Lij(ims,kms,jms,P_L22),      &
                              lasd_Lij(ims,kms,jms,P_L33),      &
                              lasd_Lij(ims,kms,jms,P_L12),      &
                              lasd_Lij(ims,kms,jms,P_L13),      &
                              lasd_Lij(ims,kms,jms,P_L23),      &
                              lasd_Gij(ims,kms,jms,P_G11),      &
                              lasd_Gij(ims,kms,jms,P_G22),      &
                              lasd_Gij(ims,kms,jms,P_G33),      &
                              lasd_Gij(ims,kms,jms,P_G12),      &
                              lasd_Gij(ims,kms,jms,P_G13),      &
                              lasd_Gij(ims,kms,jms,P_G23),      &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                    )


    ENDDO 
    !$OMP END PARALLEL DO






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL calc_contraction ( lasd_TC(ims,kms,jms,P_GG),        &
                              lasd_Gij(ims,kms,jms,P_G11),      &
                              lasd_Gij(ims,kms,jms,P_G22),      &
                              lasd_Gij(ims,kms,jms,P_G33),      &
                              lasd_Gij(ims,kms,jms,P_G12),      &
                              lasd_Gij(ims,kms,jms,P_G13),      &
                              lasd_Gij(ims,kms,jms,P_G23),      &
                              lasd_Gij(ims,kms,jms,P_G11),      &
                              lasd_Gij(ims,kms,jms,P_G22),      &
                              lasd_Gij(ims,kms,jms,P_G33),      &
                              lasd_Gij(ims,kms,jms,P_G12),      &
                              lasd_Gij(ims,kms,jms,P_G13),      &
                              lasd_Gij(ims,kms,jms,P_G23),      &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                    )


    ENDDO 
    !$OMP END PARALLEL DO






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL calc_contraction ( lasd_TC(ims,kms,jms,P_QN),        &
                              lasd_Qij(ims,kms,jms,P_Q11),      &
                              lasd_Qij(ims,kms,jms,P_Q22),      &
                              lasd_Qij(ims,kms,jms,P_Q33),      &
                              lasd_Qij(ims,kms,jms,P_Q12),      &
                              lasd_Qij(ims,kms,jms,P_Q13),      &
                              lasd_Qij(ims,kms,jms,P_Q23),      &
                              lasd_Nij(ims,kms,jms,P_N11),      &
                              lasd_Nij(ims,kms,jms,P_N22),      &
                              lasd_Nij(ims,kms,jms,P_N33),      &
                              lasd_Nij(ims,kms,jms,P_N12),      &
                              lasd_Nij(ims,kms,jms,P_N13),      &
                              lasd_Nij(ims,kms,jms,P_N23),      &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                    )


    ENDDO 
    !$OMP END PARALLEL DO






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL calc_contraction ( lasd_TC(ims,kms,jms,P_NN),        &
                              lasd_Nij(ims,kms,jms,P_N11),      &
                              lasd_Nij(ims,kms,jms,P_N22),      &
                              lasd_Nij(ims,kms,jms,P_N33),      &
                              lasd_Nij(ims,kms,jms,P_N12),      &
                              lasd_Nij(ims,kms,jms,P_N13),      &
                              lasd_Nij(ims,kms,jms,P_N23),      &
                              lasd_Nij(ims,kms,jms,P_N11),      &
                              lasd_Nij(ims,kms,jms,P_N22),      &
                              lasd_Nij(ims,kms,jms,P_N33),      &
                              lasd_Nij(ims,kms,jms,P_N12),      &
                              lasd_Nij(ims,kms,jms,P_N13),      &
                              lasd_Nij(ims,kms,jms,P_N23),      &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                    )


    ENDDO 
    !$OMP END PARALLEL DO







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

     CALL initialize_F  (lasd_TC(ims,kms,jms,P_FF_LG),        &
                         lasd_TC(ims,kms,jms,P_FF_GG),        &
                         lasd_TC(ims,kms,jms,P_FF_QN),        &
                         lasd_TC(ims,kms,jms,P_FF_NN),        &
                         lasd_LAGR(ims,kms,jms,P_F_LG),       &
                         lasd_LAGR(ims,kms,jms,P_F_GG),       &
                         lasd_LAGR(ims,kms,jms,P_F_QN),       &
                         lasd_LAGR(ims,kms,jms,P_F_NN),       &
                         lasd_TC(ims,kms,jms,P_GG),           &
                         lasd_TC(ims,kms,jms,P_NN),           &
                         grid%itimestep,                      &
                         config_flags%restart,                &
                         ids, ide, jds, jde, kds, kde,        &
                         ims, ime, jms, jme, kms, kme,        &
                         ips, ipe, jps, jpe, kps, kpe,        &
                         grid%i_start(ij), grid%i_end(ij),    &
                         grid%j_start(ij), grid%j_end(ij),    &
                         k_start, k_end                       )


    ENDDO 
    !$OMP END PARALLEL DO
 











CALL HALO_EM_LASD_TC_sub ( grid, &
  num_lasd_tc, &
  lasd_tc, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_LASD_TC_sub ( grid, &
  config_flags, &
  num_lasd_tc, &
  lasd_tc, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( lasd_TC(ims,kms,jms,P_FF_LG), 't',   &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_TC(ims,kms,jms,P_FF_GG), 't',   &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_TC(ims,kms,jms,P_FF_NN), 't',   &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( lasd_TC(ims,kms,jms,P_FF_QN), 't',   &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       )

    ENDDO 
    !$OMP END PARALLEL DO







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

     CALL interpolag_sdep ( lasd_LAGR(ims,kms,jms,P_F_LG),         &
                            lasd_LAGR(ims,kms,jms,P_F_GG),         &
                            lasd_LAGR(ims,kms,jms,P_F_QN),         &
                            lasd_LAGR(ims,kms,jms,P_F_NN),         &
                            lasd_TC(ims,kms,jms,P_FF_LG),        &
                            lasd_TC(ims,kms,jms,P_FF_GG),        &
                            lasd_TC(ims,kms,jms,P_FF_QN),        &
                            lasd_TC(ims,kms,jms,P_FF_NN),        &
                            grid%rdz, grid%dx, grid%dy, grid%dt, &
                            grid%u_2,grid%v_2,grid%w_2,          &
                            ids, ide, jds, jde, kds, kde,        &
                            ims, ime, jms, jme, kms, kme,        &
                            ips, ipe, jps, jpe, kps, kpe,        &
                            grid%i_start(ij), grid%i_end(ij),    &
                            grid%j_start(ij), grid%j_end(ij),    &
                            k_start, k_end                       )

    ENDDO 
    !$OMP END PARALLEL DO







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

          CALL calc_ev_lasd (  lasd_TC(ims,kms,jms,P_CS),           &
                               grid%xkmh, grid%xkmv,                &
                               grid%xkhh, grid%xkhv,                &
                               lasd_TC(ims,kms,jms,P_LG),           &
                               lasd_TC(ims,kms,jms,P_GG),           &
                               lasd_TC(ims,kms,jms,P_QN),           &
                               lasd_TC(ims,kms,jms,P_NN),           &
                               lasd_LAGR(ims,kms,jms,P_F_LG),       &
                               lasd_LAGR(ims,kms,jms,P_F_GG),       &
                               lasd_LAGR(ims,kms,jms,P_F_QN),       &
                               lasd_LAGR(ims,kms,jms,P_F_NN),       &
                               lasd_TC(ims,kms,jms,P_SS),           &
                               prandtl,                             &
                               config_flags%lasd_opt,               &
                               config_flags%nested,                 &
                               grid%rdzw, grid%dx, grid%dy, grid%dt,&
                               grid%rdz,                            &
                               ids, ide, jds, jde, kds, kde,        &
                               ims, ime, jms, jme, kms, kme,        &
                               ips, ipe, jps, jpe, kps, kpe,        &
                               grid%i_start(ij), grid%i_end(ij),    &
                               grid%j_start(ij), grid%j_end(ij),    &
                               k_start, k_end                       )

    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_EV_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_EV_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( grid%xkmh, 't', config_flags,     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                    )

          CALL set_physical_bc3d_m( grid%xkhh, 't', config_flags,     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                    )

          CALL set_physical_bc3d_m( grid%xkmv, 't', config_flags,     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                    )

          CALL set_physical_bc3d_m( grid%xkhv, 't', config_flags,     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO


ENDIF 

  IF ( ( config_flags%drm_opt .EQ. 0 ) .AND. ( config_flags%can_opt .EQ. 1 ) ) THEN








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

          CALL nw_canopy( grid%nwtau13, grid%nwtau23,       &
                          grid%u_2, grid%v_2,               &
                          grid%w_2, grid%rdz,               &
                          grid%rdzw, grid%dx,               &
                          grid%fnm, grid%fnp,               &
                          config_flags%canfact,             &
                          grid%ustm,                        &         
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          ips, ipe, jps, jpe, kps, kpe,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_NWTAU_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_NWTAU_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( grid%nwtau13, 'e', config_flags,  &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                    )

          CALL set_physical_bc3d_m( grid%nwtau23, 'f', config_flags,  &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO



  ENDIF


  IF ( config_flags%rsfs_opt .LT. 6 ) THEN













































































    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rec_0( rsfs_rec(ims,kms,jms,P_urec),           &
                  rsfs_rec(ims,kms,jms,P_vrec),           &
                  rsfs_rec(ims,kms,jms,P_wrec),           &
                  rcoef,                                  & 
                  grid%u_2, grid%v_2, grid%w_2,           &
                  config_flags%rsfs_opt,                  &
                  ids, ide, jds, jde, kds, kde,           &
                  ims, ime, jms, jme, kms, kme,           &
                  ips, ipe, jps, jpe, kps, kpe,           &
                  grid%i_start(ij), grid%i_end(ij),       &
                  grid%j_start(ij), grid%j_end(ij),       &
                  k_start, k_end                          )

      CALL rec_0_tq( rsfs_rec(ims,kms,jms,P_trec),           &
                     rsfs_rec(ims,kms,jms,P_qrec),           &
                     rcoef,                                  & 
                     grid%t_2, qvapor,                       &
                     config_flags%rsfs_opt,                  &
                     ids, ide, jds, jde, kds, kde,           &
                     ims, ime, jms, jme, kms, kme,           &
                     ips, ipe, jps, jpe, kps, kpe,           &
                     grid%i_start(ij), grid%i_end(ij),       &
                     grid%j_start(ij), grid%j_end(ij),       &
                     k_start, k_end                          )

    ENDDO 
    !$OMP END PARALLEL DO







    DO recn = 1, config_flags%rsfs_opt  



      IF (recn .EQ. 1 ) THEN 













        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL uvw_s( grid%us, grid%vs, grid%ws,                      &
                      grid%u_2, grid%v_2,                             &
                      grid%ht, grid%rdx, grid%rdy,                    &
                      grid%cf1, grid%cf2, grid%cf3,                   &
                      config_flags%rsfsvsbc_opt,                      &
                      ids, ide, jds, jde, kds, kde,                   &
                      ims, ime, jms, jme, kms, kme,                   &
                      ips, ipe, jps, jpe, kps, kpe,                   &
                      grid%i_start(ij), grid%i_end(ij),               &  
                      grid%j_start(ij), grid%j_end(ij),               &
                      k_start, k_end                                  )

          CALL tq_s(  grid%ts, grid%qss,                              &
                      grid%t_2, qvapor,                               &
                      grid%ht, grid%rdx, grid%rdy,                    &
                      grid%cf1, grid%cf2, grid%cf3,                   &
                      config_flags%rsfsvsbc_opt,                      &
                      ids, ide, jds, jde, kds, kde,                   &
                      ims, ime, jms, jme, kms, kme,                   &
                      ips, ipe, jps, jpe, kps, kpe,                   &
                      grid%i_start(ij), grid%i_end(ij),               &  
                      grid%j_start(ij), grid%j_end(ij),               &
                      k_start, k_end                                  )

        ENDDO 
        !$OMP END PARALLEL DO







CALL HALO_EM_RSFS_UVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_UVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc2d_m( grid%us, 'u', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%vs, 'v', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%ws, 'w', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%ts, 't', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%qss, 't', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )


        ENDDO 
        !$OMP END PARALLEL DO







   IF (config_flags%nested) THEN 

        DO i = ims, ime
        DO k = kms, kme
        DO j = jms, jme
           rsfs_rmij(i,k,j,P_rm11)=grid%u_2(i,k,j)
           rsfs_rmij(i,k,j,P_rm22)=grid%v_2(i,k,j)
           rsfs_rmij(i,k,j,P_rm33)=grid%w_2(i,k,j)
           rsfs_rmij(i,k,j,P_rm12)=grid%t_2(i,k,j)
           rsfs_rmij(i,k,j,P_rm13)=qvapor(i,k,j)
        ENDDO
        ENDDO
        ENDDO







CALL HALO_EM_RSFS_RMIJ_sub ( grid, &
  num_rsfs_rmij, &
  rsfs_rmij, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_RMIJ_sub ( grid, &
  config_flags, &
  num_rsfs_rmij, &
  rsfs_rmij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rm11), 'u', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 

          CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rm22), 'v', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 


          CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rm33), 'w', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 

          CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rm12), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 

          CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rm13), 't', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 

        ENDDO 
        !$OMP END PARALLEL DO


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_ufilt),   &
                            rsfs_rmij(ims,kms,jms,P_rm11),    &
                            grid%us, 'uu', iexp,              &    
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_vfilt),   &
                            rsfs_rmij(ims,kms,jms,P_rm22),    &
                            grid%vs, 'vv', iexp,              &
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_wfilt),   &
                            rsfs_rmij(ims,kms,jms,P_rm33),    &
                            grid%ws, 'ww', iexp,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_tfilt),   &
                            rsfs_rmij(ims,kms,jms,P_rm12),     &
                            grid%ts, 'cc',  iexp,             & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_qfilt),   &
                            rsfs_rmij(ims,kms,jms,P_rm13),    &
                            grid%qss, 'cc',  iexp,            & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

        ENDDO 
        !$OMP END PARALLEL DO


   ELSE 


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_ufilt),   &
                            grid%u_2,                         &
                            grid%us, 'uu', iexp,              &
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_vfilt),   &
                            grid%v_2,                         &
                            grid%vs, 'vv', iexp,              &
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_wfilt),   &
                            grid%w_2,                         &
                            grid%ws, 'ww', iexp,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_tfilt),   &
                            grid%t_2,                         &
                            grid%ts, 'cc',  iexp,             & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_qfilt),   &
                            qvapor,                           &
                            grid%qss, 'cc',  iexp,            & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

        ENDDO 
        !$OMP END PARALLEL DO

   ENDIF

      ELSE 

















CALL HALO_EM_RSFS_FILT_sub ( grid, &
  num_rsfs_filt, &
  rsfs_filt, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_FILT_sub ( grid, &
  config_flags, &
  num_rsfs_filt, &
  rsfs_filt, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( rsfs_filt(ims,kms,jms,P_ufilt), 'u', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 

          CALL set_physical_bc3d_m( rsfs_filt(ims,kms,jms,P_vfilt), 'v', &
                                  config_flags,                        &  
                                  ids, ide, jds, jde, kds, kde,        &  
                                  ims, ime, jms, jme, kms, kme,        &  
                                  ips, ipe, jps, jpe, kps, kpe,        &  
                                  grid%i_start(ij), grid%i_end(ij),    &  
                                  grid%j_start(ij), grid%j_end(ij),    &  
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( rsfs_filt(ims,kms,jms,P_wfilt), 'w', &
                                  config_flags,                        &  
                                  ids, ide, jds, jde, kds, kde,        &  
                                  ims, ime, jms, jme, kms, kme,        &  
                                  ips, ipe, jps, jpe, kps, kpe,        & 
                                  grid%i_start(ij), grid%i_end(ij),    & 
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 

          CALL set_physical_bc3d_m( rsfs_filt(ims,kms,jms,P_tfilt), 't', &
                                  config_flags,                        &  
                                  ids, ide, jds, jde, kds, kde,        &  
                                  ims, ime, jms, jme, kms, kme,        &  
                                  ips, ipe, jps, jpe, kps, kpe,        & 
                                  grid%i_start(ij), grid%i_end(ij),    & 
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 

          CALL set_physical_bc3d_m( rsfs_filt(ims,kms,jms,P_qfilt), 't', &
                                  config_flags,                        &  
                                  ids, ide, jds, jde, kds, kde,        &  
                                  ims, ime, jms, jme, kms, kme,        &  
                                  ips, ipe, jps, jpe, kps, kpe,        & 
                                  grid%i_start(ij), grid%i_end(ij),    & 
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 

        ENDDO 
        !$OMP END PARALLEL DO






        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL uvw_s( grid%us, grid%vs, grid%ws,                      &
                      rsfs_filt(ims,kms,jms,P_ufilt),                 &
                      rsfs_filt(ims,kms,jms,P_vfilt),                 &
                      grid%ht, grid%rdx, grid%rdy,                    &
                      grid%cf1, grid%cf2, grid%cf3,                   &
                      config_flags%rsfsvsbc_opt,                      &
                      ids, ide, jds, jde, kds, kde,                   &
                      ims, ime, jms, jme, kms, kme,                   &
                      ips, ipe, jps, jpe, kps, kpe,                   &
                      grid%i_start(ij), grid%i_end(ij),               &  
                      grid%j_start(ij), grid%j_end(ij),               &
                      k_start, k_end                                  )

          CALL tq_s(  grid%ts, grid%qss,                              &
                      rsfs_filt(ims,kms,jms,P_tfilt),                 &
                      rsfs_filt(ims,kms,jms,P_qfilt),                 &
                      grid%ht, grid%rdx, grid%rdy,                    &
                      grid%cf1, grid%cf2, grid%cf3,                   &
                      config_flags%rsfsvsbc_opt,                      &
                      ids, ide, jds, jde, kds, kde,                   &
                      ims, ime, jms, jme, kms, kme,                   &
                      ips, ipe, jps, jpe, kps, kpe,                   &
                      grid%i_start(ij), grid%i_end(ij),               &  
                      grid%j_start(ij), grid%j_end(ij),               &
                      k_start, k_end                                  )

        ENDDO 
        !$OMP END PARALLEL DO












CALL HALO_EM_RSFS_UVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_UVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc2d_m( grid%us, 'u', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%vs, 'v', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%ws, 'w', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%ts, 't', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%qss, 't', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

        ENDDO 
        !$OMP END PARALLEL DO






        DO i = ims, ime
        DO k = kms, kme
        DO j = jms, jme
           rsfs_filt(i,k,j,P_temp)=rsfs_filt(i,k,j,P_ufilt)
        ENDDO
        ENDDO
        ENDDO

        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_ufilt),   &
                            rsfs_filt(ims,kms,jms,P_temp),    &
                            grid%us, 'uu', iexp,              &     
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    )

         ENDDO 
        !$OMP END PARALLEL DO

        DO i = ims, ime
        DO k = kms, kme
        DO j = jms, jme
           rsfs_filt(i,k,j,P_temp)=rsfs_filt(i,k,j,P_vfilt)
        ENDDO
        ENDDO
        ENDDO

        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_vfilt),   &
                            rsfs_filt(ims,kms,jms,P_temp),    &
                            grid%vs, 'vv', iexp,              &     
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     &    
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

        ENDDO 
        !$OMP END PARALLEL DO

        DO i = ims, ime
        DO k = kms, kme
        DO j = jms, jme
           rsfs_filt(i,k,j,P_temp)=rsfs_filt(i,k,j,P_wfilt)
        ENDDO
        ENDDO
        ENDDO

        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_wfilt),   &
                            rsfs_filt(ims,kms,jms,P_temp),    &
                            grid%ws, 'ww', iexp,              &
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

        ENDDO 
        !$OMP END PARALLEL DO

        DO i = ims, ime
        DO k = kms, kme
        DO j = jms, jme
           rsfs_filt(i,k,j,P_temp)=rsfs_filt(i,k,j,P_tfilt)
        ENDDO
        ENDDO
        ENDDO

        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_tfilt),   &
                            rsfs_filt(ims,kms,jms,P_temp),    &
                            grid%ws, 'cc', iexp,              &
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

        ENDDO 
        !$OMP END PARALLEL DO

        DO i = ims, ime
        DO k = kms, kme
        DO j = jms, jme
           rsfs_filt(i,k,j,P_temp)=rsfs_filt(i,k,j,P_qfilt)
        ENDDO
        ENDDO
        ENDDO

        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_qfilt),   &
                            rsfs_filt(ims,kms,jms,P_temp),    &
                            grid%ws, 'cc', iexp,              &
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

        ENDDO 
        !$OMP END PARALLEL DO


      ENDIF 





      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )
      DO ij = 1 , grid%num_tiles 

        CALL rec( rsfs_rec(ims,kms,jms,P_urec),     &
                  rsfs_rec(ims,kms,jms,P_vrec),     &
                  rsfs_rec(ims,kms,jms,P_wrec),     &
                  rsfs_filt(ims,kms,jms,P_ufilt),   &
                  rsfs_filt(ims,kms,jms,P_vfilt),   &
                  rsfs_filt(ims,kms,jms,P_wfilt),   &
                  rcoef(recn),                      &
                  ids, ide, jds, jde, kds, kde,     & 
                  ims, ime, jms, jme, kms, kme,     &
                  ips, ipe, jps, jpe, kps, kpe,     & 
                  grid%i_start(ij), grid%i_end(ij), & 
                  grid%j_start(ij), grid%j_end(ij), & 
                  k_start, k_end                    )

        CALL rec_tq( rsfs_rec(ims,kms,jms,P_trec),     &
                     rsfs_rec(ims,kms,jms,P_qrec),     &
                     rsfs_filt(ims,kms,jms,P_tfilt),   &
                     rsfs_filt(ims,kms,jms,P_qfilt),   &
                     rcoef(recn),                      &
                     ids, ide, jds, jde, kds, kde,     & 
                     ims, ime, jms, jme, kms, kme,     &
                     ips, ipe, jps, jpe, kps, kpe,     & 
                     grid%i_start(ij), grid%i_end(ij), & 
                     grid%j_start(ij), grid%j_end(ij), & 
                     k_start, k_end                    )

      ENDDO 
      !$OMP END PARALLEL DO



    ENDDO 























CALL HALO_EM_RSFS_REC_sub ( grid, &
  num_rsfs_rec, &
  rsfs_rec, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_REC_sub ( grid, &
  config_flags, &
  num_rsfs_rec, &
  rsfs_rec, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL set_physical_bc3d_m( rsfs_rec(ims,kms,jms,P_urec), 'u', &
                              config_flags,                      &
                              ids, ide, jds, jde, kds, kde,      &
                              ims, ime, jms, jme, kms, kme,      &
                              ips, ipe, jps, jpe, kps, kpe,      &
                              grid%i_start(ij), grid%i_end(ij),  &
                              grid%j_start(ij), grid%j_end(ij),  &
                              k_start, k_end                     )

      CALL set_physical_bc3d_m( rsfs_rec(ims,kms,jms,P_vrec), 'v', &
                              config_flags,                      &
                              ids, ide, jds, jde, kds, kde,      &
                              ims, ime, jms, jme, kms, kme,      &
                              ips, ipe, jps, jpe, kps, kpe,      &
                              grid%i_start(ij), grid%i_end(ij),  &
                              grid%j_start(ij), grid%j_end(ij),  &
                              k_start, k_end                     )

      CALL set_physical_bc3d_m( rsfs_rec(ims,kms,jms,P_wrec), 'w', &
                              config_flags,                      &
                              ids, ide, jds, jde, kds, kde,      &
                              ims, ime, jms, jme, kms, kme,      &
                              ips, ipe, jps, jpe, kps, kpe,      &
                              grid%i_start(ij), grid%i_end(ij),  &
                              grid%j_start(ij), grid%j_end(ij),  &
                              k_start, k_end                     )

      CALL set_physical_bc3d_m( rsfs_rec(ims,kms,jms,P_trec), 't', &
                              config_flags,                      &
                              ids, ide, jds, jde, kds, kde,      &
                              ims, ime, jms, jme, kms, kme,      &
                              ips, ipe, jps, jpe, kps, kpe,      &
                              grid%i_start(ij), grid%i_end(ij),  &
                              grid%j_start(ij), grid%j_end(ij),  &
                              k_start, k_end                     )

      CALL set_physical_bc3d_m( rsfs_rec(ims,kms,jms,P_qrec), 't', &
                              config_flags,                      &
                              ids, ide, jds, jde, kds, kde,      &
                              ims, ime, jms, jme, kms, kme,      &
                              ips, ipe, jps, jpe, kps, kpe,      &
                              grid%i_start(ij), grid%i_end(ij),  &
                              grid%j_start(ij), grid%j_end(ij),  &
                              k_start, k_end                     )

    ENDDO 
    !$OMP END PARALLEL DO






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

          CALL uvw_s( grid%us, grid%vs, grid%ws,                      &
                      grid%u_2, grid%v_2,                             &
                      grid%ht, grid%rdx, grid%rdy,                    &
                      grid%cf1, grid%cf2, grid%cf3,                   &
                      config_flags%rsfsvsbc_opt,                      &
                      ids, ide, jds, jde, kds, kde,                   &
                      ims, ime, jms, jme, kms, kme,                   &
                      ips, ipe, jps, jpe, kps, kpe,                   &
                      grid%i_start(ij), grid%i_end(ij),               &  
                      grid%j_start(ij), grid%j_end(ij),               &
                      k_start, k_end                                  )

          CALL tq_s(  grid%ts, grid%qss,                              &
                      grid%t_2, qvapor,                               &
                      grid%ht, grid%rdx, grid%rdy,                    &
                      grid%cf1, grid%cf2, grid%cf3,                   &
                      config_flags%rsfsvsbc_opt,                      &
                      ids, ide, jds, jde, kds, kde,                   &
                      ims, ime, jms, jme, kms, kme,                   &
                      ips, ipe, jps, jpe, kps, kpe,                   &
                      grid%i_start(ij), grid%i_end(ij),               &  
                      grid%j_start(ij), grid%j_end(ij),               &
                      k_start, k_end                                  )

    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_RSFS_UVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_UVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL set_physical_bc2d_m( grid%us, 'u', config_flags,       &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vs, 'v', config_flags,       &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%ws, 'w', config_flags,       &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%ts, 't', config_flags,       &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%qss, 't', config_flags,       &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

    ENDDO 
    !$OMP END PARALLEL DO







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL update_w1( rsfs_rec(ims,kms,jms,P_wrec), grid%ws, &
                      ids, ide, jds, jde, kds, kde,          &
                      ims, ime, jms, jme, kms, kme,          &
                      ips, ipe, jps, jpe, kps, kpe,          &
                      grid%i_start(ij), grid%i_end(ij),      &
                      grid%j_start(ij), grid%j_end(ij),      &
                      k_start, k_end                         )

    ENDDO 
    !$OMP END PARALLEL DO








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL proj_c( rsfs_prec(ims,kms,jms,P_urecc),    &
                   rsfs_prec(ims,kms,jms,P_vrecc),    &
                   rsfs_prec(ims,kms,jms,P_wrecc),    &
                   rsfs_prec(ims,kms,jms,P_uurecc),   &
                   rsfs_prec(ims,kms,jms,P_vvrecc),   &
                   rsfs_prec(ims,kms,jms,P_wwrecc),   &
                   rsfs_rec(ims,kms,jms,P_urec),      &
                   rsfs_rec(ims,kms,jms,P_vrec),      &
                   rsfs_rec(ims,kms,jms,P_wrec), 'u', &
                   ids, ide, jds, jde, kds, kde,      &
                   ims, ime, jms, jme, kms, kme,      &
                   ips, ipe, jps, jpe, kps, kpe,      &
                   grid%i_start(ij), grid%i_end(ij),  &
                   grid%j_start(ij), grid%j_end(ij),  &
                   k_start, k_end                     )

      CALL proj_d( rsfs_prec(ims,kms,jms,P_urecd),   &
                   rsfs_prec(ims,kms,jms,P_vrecd),   &
                   rsfs_prec(ims,kms,jms,P_uvrecd),  &
                   rsfs_rec(ims,kms,jms,P_urec),     &
                   rsfs_rec(ims,kms,jms,P_vrec),     &
                   ids, ide, jds, jde, kds, kde,     &
                   ims, ime, jms, jme, kms, kme,     &
                   ips, ipe, jps, jpe, kps, kpe,     &
                   grid%i_start(ij), grid%i_end(ij), &
                   grid%j_start(ij), grid%j_end(ij), &
                   k_start, k_end                    )

      CALL proj_e( rsfs_prec(ims,kms,jms,P_urece),   &
                   rsfs_prec(ims,kms,jms,P_wrece),   &
                   rsfs_prec(ims,kms,jms,P_uwrece),  &
                   rsfs_rec(ims,kms,jms,P_urec),     &
                   rsfs_rec(ims,kms,jms,P_wrec),     &
                   grid%fnm, grid%fnp,               &
                   ids, ide, jds, jde, kds, kde,     &
                   ims, ime, jms, jme, kms, kme,     &
                   ips, ipe, jps, jpe, kps, kpe,     &
                   grid%i_start(ij), grid%i_end(ij), &
                   grid%j_start(ij), grid%j_end(ij), &
                   k_start, k_end                    )

      CALL proj_f( rsfs_prec(ims,kms,jms,P_vrecf),   &
                   rsfs_prec(ims,kms,jms,P_wrecf),   &
                   rsfs_prec(ims,kms,jms,P_vwrecf),  &
                   rsfs_rec(ims,kms,jms,P_vrec),     &
                   rsfs_rec(ims,kms,jms,P_wrec),     &
                   grid%fnm, grid%fnp,               &
                   ids, ide, jds, jde, kds, kde,     &
                   ims, ime, jms, jme, kms, kme,     &
                   ips, ipe, jps, jpe, kps, kpe,     &
                   grid%i_start(ij), grid%i_end(ij), &
                   grid%j_start(ij), grid%j_end(ij), &
                   k_start, k_end                    )

      CALL proj_tuvw( rsfs_prec(ims,kms,jms,P_trecu),   &
                     rsfs_prec(ims,kms,jms,P_trecv),   &
                     rsfs_prec(ims,kms,jms,P_trecw),   &
                     rsfs_prec(ims,kms,jms,P_utrecu),  &
                     rsfs_prec(ims,kms,jms,P_vtrecv),  &
                     rsfs_prec(ims,kms,jms,P_wtrecw),  &
                     rsfs_rec(ims,kms,jms,P_urec),     &
                     rsfs_rec(ims,kms,jms,P_vrec),     &
                     rsfs_rec(ims,kms,jms,P_wrec),     &
                     rsfs_rec(ims,kms,jms,P_trec),     &
                     grid%fnm, grid%fnp,               &
                     ids, ide, jds, jde, kds, kde,     &
                     ims, ime, jms, jme, kms, kme,     &
                     ips, ipe, jps, jpe, kps, kpe,     &
                     grid%i_start(ij), grid%i_end(ij), &
                     grid%j_start(ij), grid%j_end(ij), &
                     k_start, k_end                    )


      CALL proj_quvw( rsfs_prec(ims,kms,jms,P_qrecu),   &
                     rsfs_prec(ims,kms,jms,P_qrecv),   &
                     rsfs_prec(ims,kms,jms,P_qrecw),   &
                     rsfs_prec(ims,kms,jms,P_uqrecu),  &
                     rsfs_prec(ims,kms,jms,P_vqrecv),  &
                     rsfs_prec(ims,kms,jms,P_wqrecw),  &
                     rsfs_rec(ims,kms,jms,P_urec),     &
                     rsfs_rec(ims,kms,jms,P_vrec),     &
                     rsfs_rec(ims,kms,jms,P_wrec),     &
                     rsfs_rec(ims,kms,jms,P_qrec),     &
                     grid%fnm, grid%fnp,               &
                     ids, ide, jds, jde, kds, kde,     &
                     ims, ime, jms, jme, kms, kme,     &
                     ips, ipe, jps, jpe, kps, kpe,     &
                     grid%i_start(ij), grid%i_end(ij), &
                     grid%j_start(ij), grid%j_end(ij), &
                     k_start, k_end                    )

      CALL proj_s( grid%usc, grid%vsc,                          &
                   grid%uusc, grid%vvsc, grid%wws,              &
                   grid%usd, grid%vsd, grid%uvsd,               &
                   grid%wse, grid%uwse,                         &
                   grid%wsf, grid%vwsf,                         &
                   grid%us, grid%vs, grid%ws,                   &
                   ids, ide, jds, jde,                          &
                   ims, ime, jms, jme,                          &
                   ips, ipe, jps, jpe,                          &
                   grid%i_start(ij), grid%i_end(ij),            &
                   grid%j_start(ij), grid%j_end(ij)             )

      CALL proj_s_tq( grid%tsu, grid%tsv,                          &
                      grid%utsu, grid%vtsv, grid%wtsw,             &
                      grid%qsu, grid%qsv,                          &
                      grid%uqsu, grid%vqsv, grid%wqsw,             &
                      grid%us, grid%vs, grid%ws, grid%ts, grid%qss,&
                      ids, ide, jds, jde,                          &
                      ims, ime, jms, jme,                          &
                      ips, ipe, jps, jpe,                          &
                      grid%i_start(ij), grid%i_end(ij),            &
                      grid%j_start(ij), grid%j_end(ij)             )












    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_RSFS_PREC_sub ( grid, &
  num_rsfs_prec, &
  rsfs_prec, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_PREC_sub ( grid, &
  config_flags, &
  num_rsfs_prec, &
  rsfs_prec, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_RSFS_PUVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_PUVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 
 
      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_urecc), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_uurecc), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )
 
      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vrecc), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vvrecc), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )
 
      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_wrecc), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_wwrecc), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_urecd), 'd', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vrecd), 'd', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_uvrecd), 'd', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_urece), 'e', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_wrece), 'e', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_uwrece), 'e', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vrecf), 'f', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_wrecf), 'f', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vwrecf), 'f', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )




      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_trecu), 'u',  &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_trecv), 'v',  &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_trecw), 'w',  &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_utrecu), 'u', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vtrecv), 'v', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_wtrecw), 'w', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )




      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_qrecu), 'u',  &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_qrecv), 'v',  &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_qrecw), 'w',  &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_uqrecu), 'u', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vqrecv), 'v', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_wqrecw), 'w', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )




      CALL set_physical_bc2d_m( grid%usc, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uusc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )
 
      CALL set_physical_bc2d_m( grid%vsc, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vvsc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wws, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%usd, 'd', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vsd, 'd', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uvsd, 'd', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wse, 'e', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uwse, 'e', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wsf, 'f', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vwsf, 'f', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )



      CALL set_physical_bc2d_m( grid%tsu, 'u', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%tsv, 'v', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%utsu, 'u', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vtsv, 'v', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wtsw, 'w', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )



      CALL set_physical_bc2d_m( grid%qsu, 'u', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%qsv, 'v', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uqsu, 'u', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vqsv, 'v', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wqsw, 'w', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

    ENDDO 
    !$OMP END PARALLEL DO






    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_urecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_urecc),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%usc, 'cc', iexp,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )
 
    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_uurecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_uurecc),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%uusc, 'cc', iexp,            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vrecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vrecc),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%vsc, 'cc', iexp,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vvrecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vvrecc),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%vvsc, 'cc', iexp,            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_wrecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_wrecc),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%ws, 'cw', iexp,              &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_wwrecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_wwrecc),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%wws, 'cw', iexp,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_urecd)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_urecd),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%usd, 'dd', iexp,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vrecd)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

     CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vrecd),   &
                       rsfs_filt(ims,kms,jms,P_temp),    &
                       grid%vsd, 'dd', iexp,             &
                       ids, ide, jds, jde, kds, kde,     &
                       ims, ime, jms, jme, kms, kme,     &
                       ips, ipe, jps, jpe, kps, kpe,     &
                       grid%i_start(ij), grid%i_end(ij), &
                       grid%j_start(ij), grid%j_end(ij), &
                       k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_uvrecd)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_uvrecd),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%uvsd, 'dd', iexp,            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_urece)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_urece),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%us, 'ee', iexp,              &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_wrece)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_wrece),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%wse, 'ew', iexp,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_uwrece)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_uwrece),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%uwse, 'ew', iexp,            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vrecf)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vrecf),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%vs, 'ff', iexp,              &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_wrecf)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_wrecf),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%wsf, 'fw', iexp,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vwrecf)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vwrecf),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%vwsf, 'fw', iexp,            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO




    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_rec(i,k,j,P_urec)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_rec(ims,kms,jms,P_urec),     &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%us, 'uu', iexp,                    &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_rec(i,k,j,P_vrec)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_rec(ims,kms,jms,P_vrec),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%vs, 'vv', iexp,                  &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_rec(i,k,j,P_wrec)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_rec(ims,kms,jms,P_wrec),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%ws, 'ww', iexp,                  &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO


    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_trecu)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_trecu),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%tsu, 'uu', iexp,                  &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_trecv)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_trecv),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%tsv, 'vv', iexp,                  &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_trecw)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_trecw),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%ts, 'ww', iexp,                  &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_utrecu)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_utrecu),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%utsu, 'uu', iexp,                  &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vtrecv)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vtrecv),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%vtsv, 'vv', iexp,                  &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_wtrecw)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_wtrecw),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%wtsw, 'ww', iexp,                  &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO






    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_qrecu)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_qrecu),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%qsu, 'uu',  iexp,                 &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_qrecv)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_qrecv),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%qsv, 'vv',  iexp,                 &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_qrecw)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_qrecw),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%qss, 'ww',  iexp,                 &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_uqrecu)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_uqrecu),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%uqsu, 'uu',  iexp,                 &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vqrecv)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vqrecv),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%vqsv, 'vv',  iexp,                 &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_wqrecw)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_wqrecw),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%wqsw, 'ww',  iexp,                 &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rmij( rsfs_rmij(ims,kms,jms,P_rm11),        &
                 rsfs_prec(ims,kms,jms,P_urecc),       &
                 rsfs_prec(ims,kms,jms,P_urecc),       &
                 rsfs_prec(ims,kms,jms,P_uurecc), 'c', &
                 ids, ide, jds, jde, kds, kde,         &
                 ims, ime, jms, jme, kms, kme,         &
                 ips, ipe, jps, jpe, kps, kpe,         &
                 grid%i_start(ij), grid%i_end(ij),     &
                 grid%j_start(ij), grid%j_end(ij),     &
                 k_start, k_end                        )
 
       CALL rmij( rsfs_rmij(ims,kms,jms,P_rm22),        &
                  rsfs_prec(ims,kms,jms,P_vrecc),       &
                  rsfs_prec(ims,kms,jms,P_vrecc),       &
                  rsfs_prec(ims,kms,jms,P_vvrecc), 'c', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )

       CALL rmij( rsfs_rmij(ims,kms,jms,P_rm33),        &
                  rsfs_prec(ims,kms,jms,P_wrecc),       &
                  rsfs_prec(ims,kms,jms,P_wrecc),       &
                  rsfs_prec(ims,kms,jms,P_wwrecc), 'c', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )
 
       CALL rmij( rsfs_rmij(ims,kms,jms,P_rm12),        &
                  rsfs_prec(ims,kms,jms,P_urecd),       &
                  rsfs_prec(ims,kms,jms,P_vrecd),       &
                  rsfs_prec(ims,kms,jms,P_uvrecd), 'd', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )

       CALL rmij( rsfs_rmij(ims,kms,jms,P_rm13),        &
                  rsfs_prec(ims,kms,jms,P_urece),       &
                  rsfs_prec(ims,kms,jms,P_wrece),       &
                  rsfs_prec(ims,kms,jms,P_uwrece), 'e', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )

       CALL rmij( rsfs_rmij(ims,kms,jms,P_rm23),        &
                  rsfs_prec(ims,kms,jms,P_vrecf),       &
                  rsfs_prec(ims,kms,jms,P_wrecf),       &
                  rsfs_prec(ims,kms,jms,P_vwrecf), 'f', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )




       CALL rmij( rsfs_rmij(ims,kms,jms,P_rh1),        &
                  rsfs_rec(ims,kms,jms,P_urec),       &
                  rsfs_prec(ims,kms,jms,P_trecu),       &
                  rsfs_prec(ims,kms,jms,P_utrecu), 'u', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )

       CALL rmij( rsfs_rmij(ims,kms,jms,P_rh2),        &
                  rsfs_rec(ims,kms,jms,P_vrec),       &
                  rsfs_prec(ims,kms,jms,P_trecv),       &
                  rsfs_prec(ims,kms,jms,P_vtrecv), 'v', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )

       CALL rmij( rsfs_rmij(ims,kms,jms,P_rh3),        &
                  rsfs_rec(ims,kms,jms,P_wrec),       &
                  rsfs_prec(ims,kms,jms,P_trecw),       &
                  rsfs_prec(ims,kms,jms,P_wtrecw), 'h', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )



       CALL rmij( rsfs_rmij(ims,kms,jms,P_rq1),        &
                  rsfs_rec(ims,kms,jms,P_urec),       &
                  rsfs_prec(ims,kms,jms,P_qrecu),       &
                  rsfs_prec(ims,kms,jms,P_uqrecu), 'u', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )

       CALL rmij( rsfs_rmij(ims,kms,jms,P_rq2),        &
                  rsfs_rec(ims,kms,jms,P_vrec),       &
                  rsfs_prec(ims,kms,jms,P_qrecv),       &
                  rsfs_prec(ims,kms,jms,P_vqrecv), 'v', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )

       CALL rmij( rsfs_rmij(ims,kms,jms,P_rq3),        &
                  rsfs_rec(ims,kms,jms,P_wrec),       &
                  rsfs_prec(ims,kms,jms,P_qrecw),       &
                  rsfs_prec(ims,kms,jms,P_wqrecw), 'h', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )

    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_RSFS_RMIJ_sub ( grid, &
  num_rsfs_rmij, &
  rsfs_rmij, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_RMIJ_sub ( grid, &
  config_flags, &
  num_rsfs_rmij, &
  rsfs_rmij, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

       CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rm11), 't', &
                               config_flags,                       &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               ips, ipe, jps, jpe, kps, kpe,       &
                               grid%i_start(ij), grid%i_end(ij),   &
                               grid%j_start(ij), grid%j_end(ij),   &
                               k_start, k_end                      )
 
       CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rm22), 't', &
                               config_flags,                       &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               ips, ipe, jps, jpe, kps, kpe,       &
                               grid%i_start(ij), grid%i_end(ij),   &
                               grid%j_start(ij), grid%j_end(ij),   &
                               k_start, k_end                      )
 
       CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rm33), 't', &
                               config_flags,                       &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               ips, ipe, jps, jpe, kps, kpe,       &
                               grid%i_start(ij), grid%i_end(ij),   &
                               grid%j_start(ij), grid%j_end(ij),   &
                               k_start, k_end                      )
 
       CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rm12), 'd', &
                               config_flags,                       &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               ips, ipe, jps, jpe, kps, kpe,       &
                               grid%i_start(ij), grid%i_end(ij),   &
                               grid%j_start(ij), grid%j_end(ij),   &
                               k_start, k_end                      )
 
       CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rm13), 'e', &
                               config_flags,                       &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               ips, ipe, jps, jpe, kps, kpe,       &
                               grid%i_start(ij), grid%i_end(ij),   &
                               grid%j_start(ij), grid%j_end(ij),   &
                               k_start, k_end                      )
 
       CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rm23), 'f', &
                               config_flags,                       &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               ips, ipe, jps, jpe, kps, kpe,       &
                               grid%i_start(ij), grid%i_end(ij),   &
                               grid%j_start(ij), grid%j_end(ij),   &
                               k_start, k_end                      )



       CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rh1), 'u', &
                               config_flags,                       &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               ips, ipe, jps, jpe, kps, kpe,       &
                               grid%i_start(ij), grid%i_end(ij),   &
                               grid%j_start(ij), grid%j_end(ij),   &
                               k_start, k_end                      )
 
       CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rh2), 'v', &
                               config_flags,                       &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               ips, ipe, jps, jpe, kps, kpe,       &
                               grid%i_start(ij), grid%i_end(ij),   &
                               grid%j_start(ij), grid%j_end(ij),   &
                               k_start, k_end                      )
 
       CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rh3), 'w', &
                               config_flags,                       &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               ips, ipe, jps, jpe, kps, kpe,       &
                               grid%i_start(ij), grid%i_end(ij),   &
                               grid%j_start(ij), grid%j_end(ij),   &
                               k_start, k_end                      )
 
 

       CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rq1), 'u', &
                               config_flags,                       &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               ips, ipe, jps, jpe, kps, kpe,       &
                               grid%i_start(ij), grid%i_end(ij),   &
                               grid%j_start(ij), grid%j_end(ij),   &
                               k_start, k_end                      )
 
       CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rq2), 'v', &
                               config_flags,                       &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               ips, ipe, jps, jpe, kps, kpe,       &
                               grid%i_start(ij), grid%i_end(ij),   &
                               grid%j_start(ij), grid%j_end(ij),   &
                               k_start, k_end                      )
 
       CALL set_physical_bc3d_m( rsfs_rmij(ims,kms,jms,P_rq3), 'w', &
                               config_flags,                       &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               ips, ipe, jps, jpe, kps, kpe,       &
                               grid%i_start(ij), grid%i_end(ij),   &
                               grid%j_start(ij), grid%j_end(ij),   &
                               k_start, k_end                      )
 
    ENDDO 
    !$OMP END PARALLEL DO

  ENDIF 

  IF ( config_flags%drm_opt .GE. 1 ) THEN




























































  IF ( config_flags%drm_opt .EQ. 1 ) THEN




















    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
              DO ij = 1 , grid%num_tiles 

                  CALL uvw_s( grid%us, grid%vs, grid%ws,        &
                              grid%u_2, grid%v_2,               &
                              grid%ht, grid%rdx, grid%rdy,      &
                              grid%cf1, grid%cf2, grid%cf3,     &
                              config_flags%rsfsvsbc_opt,        &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &  
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                    )

              ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_RSFS_UVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_UVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc2d_m( grid%us, 'u', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%vs, 'v', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%ws, 'w', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

        ENDDO 
        !$OMP END PARALLEL DO






   IF (config_flags%nested) THEN 

        DO i = ims, ime
        DO k = kms, kme
        DO j = jms, jme
           sijc_drm(ims,kms,jms,P_s11c)=grid%u_2(i,k,j)
           sijc_drm(ims,kms,jms,P_s22c)=grid%v_2(i,k,j)
           sijc_drm(ims,kms,jms,P_s33c)=grid%w_2(i,k,j)
        ENDDO
        ENDDO
        ENDDO







CALL HALO_EM_SIJC_DRM_sub ( grid, &
  num_sijc_drm, &
  sijc_drm, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_SIJC_DRM_sub ( grid, &
  config_flags, &
  num_sijc_drm, &
  sijc_drm, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( sijc_drm(ims,kms,jms,P_s11c), 'u', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 

          CALL set_physical_bc3d_m( sijc_drm(ims,kms,jms,P_s22c), 'v', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 


          CALL set_physical_bc3d_m( sijc_drm(ims,kms,jms,P_s33c), 'w', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 


        ENDDO 
        !$OMP END PARALLEL DO


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( tfu_drm(ims,kms,jms,P_tfu),       &
                            sijc_drm(ims,kms,jms,P_s11c),     &
                            grid%us, 'uu', iexp,              &    
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( tfu_drm(ims,kms,jms,P_tfv),       &
                            sijc_drm(ims,kms,jms,P_s22c),     &
                            grid%vs, 'vv', iexp,              &
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( tfu_drm(ims,kms,jms,P_tfw),       &
                            sijc_drm(ims,kms,jms,P_s33c),     &
                            grid%ws, 'ww', iexp,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 


        ENDDO 
        !$OMP END PARALLEL DO


   ELSE 

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

                CALL rsfs_filter( tfu_drm(ims,kms,jms,P_tfu),       &
                                  grid%u_2, grid%us, 'uu', itst,    &     
                                  ids, ide, jds, jde, kds, kde,     & 
                                  ims, ime, jms, jme, kms, kme,     & 
                                  ips, ipe, jps, jpe, kps, kpe,     & 
                                  grid%i_start(ij), grid%i_end(ij), & 
                                  grid%j_start(ij), grid%j_end(ij), & 
                                  k_start, k_end                    ) 

                CALL rsfs_filter( tfu_drm(ims,kms,jms,P_tfv),       &
                                  grid%v_2, grid%vs, 'vv', itst,    &     
                                  ids, ide, jds, jde, kds, kde,     & 
                                  ims, ime, jms, jme, kms, kme,     & 
                                  ips, ipe, jps, jpe, kps, kpe,     & 
                                  grid%i_start(ij), grid%i_end(ij), & 
                                  grid%j_start(ij), grid%j_end(ij), & 
                                  k_start, k_end                    ) 

                CALL rsfs_filter( tfu_drm(ims,kms,jms,P_tfw),       &
                                  grid%w_2, grid%ws, 'ww', itst,    & 
                                  ids, ide, jds, jde, kds, kde,     & 
                                  ims, ime, jms, jme, kms, kme,     & 
                                  ips, ipe, jps, jpe, kps, kpe,     & 
                                  grid%i_start(ij), grid%i_end(ij), & 
                                  grid%j_start(ij), grid%j_end(ij), & 
                                  k_start, k_end                    ) 

        ENDDO 
        !$OMP END PARALLEL DO

   ENDIF











  IF ( config_flags%rsfs_opt .EQ. 6 ) THEN
  print*,' must have rsfs_opt < 6 to use dwl_opt 1'
  STOP
  ENDIF






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rec_0( rsfs_rec(ims,kms,jms,P_urec),     &
                  rsfs_rec(ims,kms,jms,P_vrec),     &
                  rsfs_rec(ims,kms,jms,P_wrec),     &            
                  rcoef,                            & 
                  tfu_drm(ims,kms,jms,P_tfu),       &
                  tfu_drm(ims,kms,jms,P_tfv),       &
                  tfu_drm(ims,kms,jms,P_tfw),       &
                  config_flags%rsfs_opt,            &
                  ids, ide, jds, jde, kds, kde,     &
                  ims, ime, jms, jme, kms, kme,     &
                  ips, ipe, jps, jpe, kps, kpe,     &
                  grid%i_start(ij), grid%i_end(ij), &
                  grid%j_start(ij), grid%j_end(ij), &
                  k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO













    DO recn = 1, config_flags%rsfs_opt  



      IF (recn .EQ. 1 ) THEN 















CALL HALO_EM_TFU_DRM_sub ( grid, &
  num_tfu_drm, &
  tfu_drm, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_TFU_DRM_sub ( grid, &
  config_flags, &
  num_tfu_drm, &
  tfu_drm, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 
 
          CALL set_physical_bc3d_m( tfu_drm(ims,kms,jms,P_tfu),         &
                                  'u', config_flags,                  &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start, k_end                      )
 
          CALL set_physical_bc3d_m( tfu_drm(ims,kms,jms,P_tfv),         &
                                  'v', config_flags,                  &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start, k_end                      )

          CALL set_physical_bc3d_m( tfu_drm(ims,kms,jms,P_tfw),         &
                                  'w', config_flags,                  &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start, k_end                      )

        ENDDO 
        !$OMP END PARALLEL DO





    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
              DO ij = 1 , grid%num_tiles 

                  CALL uvw_s( grid%us, grid%vs, grid%ws,        &
                              tfu_drm(ims,kms,jms,P_tfu),       &
                              tfu_drm(ims,kms,jms,P_tfv),       &
                              grid%ht, grid%rdx, grid%rdy,      &
                              grid%cf1, grid%cf2, grid%cf3,     &
                              config_flags%rsfsvsbc_opt,        &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &  
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                    )

              ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_RSFS_UVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_UVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 
 
          CALL set_physical_bc2d_m( grid%us, 'u', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%vs, 'v', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%ws, 'w', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

        ENDDO 
        !$OMP END PARALLEL DO






       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_ufilt),   &
                            tfu_drm(ims,kms,jms,P_tfu),       &
                            grid%us, 'uu', iexp,              &    
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_vfilt),   &
                            tfu_drm(ims,kms,jms,P_tfv),       &
                            grid%vs, 'vv', iexp,              &
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_wfilt),   &
                            tfu_drm(ims,kms,jms,P_tfw),       &
                            grid%ws, 'ww', iexp,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

         ENDDO 
        !$OMP END PARALLEL DO







                  ELSE 













CALL HALO_EM_RSFS_FILT_sub ( grid, &
  num_rsfs_filt, &
  rsfs_filt, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_FILT_sub ( grid, &
  config_flags, &
  num_rsfs_filt, &
  rsfs_filt, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( rsfs_filt(ims,kms,jms,P_ufilt), 'u', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 

          CALL set_physical_bc3d_m( rsfs_filt(ims,kms,jms,P_vfilt), 'v', &
                                  config_flags,                        &  
                                  ids, ide, jds, jde, kds, kde,        &  
                                  ims, ime, jms, jme, kms, kme,        &  
                                  ips, ipe, jps, jpe, kps, kpe,        &  
                                  grid%i_start(ij), grid%i_end(ij),    &  
                                  grid%j_start(ij), grid%j_end(ij),    &  
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( rsfs_filt(ims,kms,jms,P_wfilt), 'w', &
                                  config_flags,                        &  
                                  ids, ide, jds, jde, kds, kde,        &  
                                  ims, ime, jms, jme, kms, kme,        &  
                                  ips, ipe, jps, jpe, kps, kpe,        & 
                                  grid%i_start(ij), grid%i_end(ij),    & 
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 

        ENDDO 
        !$OMP END PARALLEL DO






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
              DO ij = 1 , grid%num_tiles 

                  CALL uvw_s( grid%us, grid%vs, grid%ws,        &
                              rsfs_filt(ims,kms,jms,P_ufilt),   &
                              rsfs_filt(ims,kms,jms,P_vfilt),   &
                              grid%ht, grid%rdx, grid%rdy,      &
                              grid%cf1, grid%cf2, grid%cf3,     &
                              config_flags%rsfsvsbc_opt,        &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &  
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                    )

              ENDDO 
    !$OMP END PARALLEL DO













CALL HALO_EM_RSFS_UVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_UVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc2d_m( grid%us, 'u', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%vs, 'v', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%ws, 'w', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

        ENDDO 
        !$OMP END PARALLEL DO







        DO i = ims, ime
        DO k = kms, kme
        DO j = jms, jme
           rsfs_filt(i,k,j,P_temp)=rsfs_filt(i,k,j,P_ufilt)
        ENDDO
        ENDDO
        ENDDO

        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_ufilt),   &
                            rsfs_filt(ims,kms,jms,P_temp),    &
                            grid%us, 'uu', iexp,              &     
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    )
                  
         ENDDO 
        !$OMP END PARALLEL DO

        DO i = ims, ime
        DO k = kms, kme
        DO j = jms, jme
           rsfs_filt(i,k,j,P_temp)=rsfs_filt(i,k,j,P_vfilt)
        ENDDO
        ENDDO
        ENDDO

        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_vfilt),   &
                            rsfs_filt(ims,kms,jms,P_temp),    &
                            grid%vs, 'vv', iexp,              &     
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    )

         ENDDO 
        !$OMP END PARALLEL DO

        DO i = ims, ime
        DO k = kms, kme
        DO j = jms, jme
           rsfs_filt(i,k,j,P_temp)=rsfs_filt(i,k,j,P_wfilt)
        ENDDO
        ENDDO
        ENDDO

        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 


          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_wfilt),   &
                            rsfs_filt(ims,kms,jms,P_temp),    &
                            grid%ws, 'ww', iexp,              &     
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    )














         ENDDO 
        !$OMP END PARALLEL DO

    ENDIF 
















CALL HALO_EM_RSFS_FILT_sub ( grid, &
  num_rsfs_filt, &
  rsfs_filt, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_FILT_sub ( grid, &
  config_flags, &
  num_rsfs_filt, &
  rsfs_filt, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( rsfs_filt(ims,kms,jms,P_ufilt), 'u', &
                                  config_flags,                        &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  ips, ipe, jps, jpe, kps, kpe,        &
                                  grid%i_start(ij), grid%i_end(ij),    &
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 

          CALL set_physical_bc3d_m( rsfs_filt(ims,kms,jms,P_vfilt), 'v', &
                                  config_flags,                        &  
                                  ids, ide, jds, jde, kds, kde,        &  
                                  ims, ime, jms, jme, kms, kme,        &  
                                  ips, ipe, jps, jpe, kps, kpe,        &  
                                  grid%i_start(ij), grid%i_end(ij),    &  
                                  grid%j_start(ij), grid%j_end(ij),    &  
                                  k_start, k_end                       )

          CALL set_physical_bc3d_m( rsfs_filt(ims,kms,jms,P_wfilt), 'w', &
                                  config_flags,                        &  
                                  ids, ide, jds, jde, kds, kde,        &  
                                  ims, ime, jms, jme, kms, kme,        &  
                                  ips, ipe, jps, jpe, kps, kpe,        & 
                                  grid%i_start(ij), grid%i_end(ij),    & 
                                  grid%j_start(ij), grid%j_end(ij),    &
                                  k_start, k_end                       ) 

        ENDDO 
        !$OMP END PARALLEL DO






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
              DO ij = 1 , grid%num_tiles 

                  CALL uvw_s( grid%us, grid%vs, grid%ws,        &
                              rsfs_filt(ims,kms,jms,P_ufilt),   &
                              rsfs_filt(ims,kms,jms,P_vfilt),   &
                              grid%ht, grid%rdx, grid%rdy,      &
                              grid%cf1, grid%cf2, grid%cf3,     &
                              config_flags%rsfsvsbc_opt,        &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &  
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                    )

              ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_RSFS_UVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_UVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc2d_m( grid%us, 'u', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%vs, 'v', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

          CALL set_physical_bc2d_m( grid%ws, 'w', config_flags,       &
                                  ids, ide, jds, jde,               &
                                  ims, ime, jms, jme,               &
                                  ips, ipe, jps, jpe,               &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij)  )

        ENDDO 
        !$OMP END PARALLEL DO







        DO i = ims, ime
        DO k = kms, kme
        DO j = jms, jme
           rsfs_filt(i,k,j,P_temp)=rsfs_filt(i,k,j,P_ufilt)
        ENDDO
        ENDDO
        ENDDO

        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_ufilt),   &
                            rsfs_filt(ims,kms,jms,P_temp),    &
                            grid%us, 'uu', itst,              &     
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    )
                  
         ENDDO 
        !$OMP END PARALLEL DO

        DO i = ims, ime
        DO k = kms, kme
        DO j = jms, jme
           rsfs_filt(i,k,j,P_temp)=rsfs_filt(i,k,j,P_vfilt)
        ENDDO
        ENDDO
        ENDDO

        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_vfilt),   &
                            rsfs_filt(ims,kms,jms,P_temp),    &
                            grid%vs, 'vv', itst,              &     
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    )

         ENDDO 
        !$OMP END PARALLEL DO

        DO i = ims, ime
        DO k = kms, kme
        DO j = jms, jme
           rsfs_filt(i,k,j,P_temp)=rsfs_filt(i,k,j,P_wfilt)
        ENDDO
        ENDDO
        ENDDO

        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( rsfs_filt(ims,kms,jms,P_wfilt),   &
                            rsfs_filt(ims,kms,jms,P_temp),    &
                            grid%ws, 'ww', itst,              &     
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    )

         ENDDO 
        !$OMP END PARALLEL DO







      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )
      DO ij = 1 , grid%num_tiles 

        CALL rec( rsfs_rec(ims,kms,jms,P_urec), &
                  rsfs_rec(ims,kms,jms,P_vrec),     &
                  rsfs_rec(ims,kms,jms,P_wrec),     &
                  rsfs_filt(ims,kms,jms,P_ufilt),   &
                  rsfs_filt(ims,kms,jms,P_vfilt),   &
                  rsfs_filt(ims,kms,jms,P_wfilt),   &
                  rcoef(recn),                      &
                  ids, ide, jds, jde, kds, kde,     & 
                  ims, ime, jms, jme, kms, kme,     &
                  ips, ipe, jps, jpe, kps, kpe,     & 
                  grid%i_start(ij), grid%i_end(ij), & 
                  grid%j_start(ij), grid%j_end(ij), & 
                  k_start, k_end                    )

      ENDDO 
      !$OMP END PARALLEL DO









                ENDDO 






















CALL HALO_EM_RSFS_REC_sub ( grid, &
  num_rsfs_rec, &
  rsfs_rec, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_REC_sub ( grid, &
  config_flags, &
  num_rsfs_rec, &
  rsfs_rec, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

       CALL set_physical_bc3d_m( rsfs_rec(ims,kms,jms,P_urec),       &
                               'u', config_flags,                  &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               ips, ipe, jps, jpe, kps, kpe,       &
                               grid%i_start(ij), grid%i_end(ij),   &
                               grid%j_start(ij), grid%j_end(ij),   &
                               k_start, k_end                      )

       CALL set_physical_bc3d_m( rsfs_rec(ims,kms,jms,P_vrec),       &
                               'v', config_flags,                  &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               ips, ipe, jps, jpe, kps, kpe,       &
                               grid%i_start(ij), grid%i_end(ij),   &
                               grid%j_start(ij), grid%j_end(ij),   &
                               k_start, k_end                      )

       ENDDO 
       !$OMP END PARALLEL DO






    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
              DO ij = 1 , grid%num_tiles 

                  CALL uvw_s( grid%us, grid%vs, grid%ws,        &
                              rsfs_rec(ims,kms,jms,P_urec),     &
                              rsfs_rec(ims,kms,jms,P_vrec),     &
                              grid%ht, grid%rdx, grid%rdy,      &
                              grid%cf1, grid%cf2, grid%cf3,     &
                              config_flags%rsfsvsbc_opt,        &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &  
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                    )

              ENDDO 
    !$OMP END PARALLEL DO







    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

                  CALL update_w1( rsfs_rec(ims,kms,jms,P_wrec),     &
                                  grid%ws,                          &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                    )

     ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_RSFS_REC_sub ( grid, &
  num_rsfs_rec, &
  rsfs_rec, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_REC_sub ( grid, &
  config_flags, &
  num_rsfs_rec, &
  rsfs_rec, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_RSFS_UVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_UVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

       CALL set_physical_bc3d_m( rsfs_rec(ims,kms,jms,P_wrec),       &
                               'w', config_flags,                  &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               ips, ipe, jps, jpe, kps, kpe,       &
                               grid%i_start(ij), grid%i_end(ij),   &
                               grid%j_start(ij), grid%j_end(ij),   &
                               k_start, k_end                      )
 
       CALL set_physical_bc2d_m( grid%us, 'u', config_flags,       &
                               ids, ide, jds, jde,               &
                               ims, ime, jms, jme,               &
                               ips, ipe, jps, jpe,               &
                               grid%i_start(ij), grid%i_end(ij), &
                               grid%j_start(ij), grid%j_end(ij)  )

       CALL set_physical_bc2d_m( grid%vs, 'v', config_flags,       &
                               ids, ide, jds, jde,               &
                               ims, ime, jms, jme,               &
                               ips, ipe, jps, jpe,               &
                               grid%i_start(ij), grid%i_end(ij), &
                               grid%j_start(ij), grid%j_end(ij)  )

       CALL set_physical_bc2d_m( grid%ws, 'w', config_flags,       &
                               ids, ide, jds, jde,               &
                               ims, ime, jms, jme,               &
                               ips, ipe, jps, jpe,               &
                               grid%i_start(ij), grid%i_end(ij), &
                               grid%j_start(ij), grid%j_end(ij)  )

       ENDDO 
       !$OMP END PARALLEL DO








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL proj_c( rsfs_prec(ims,kms,jms,P_urecc),    &
                   rsfs_prec(ims,kms,jms,P_vrecc),    &
                   rsfs_prec(ims,kms,jms,P_wrecc),    &
                   rsfs_prec(ims,kms,jms,P_uurecc),   &
                   rsfs_prec(ims,kms,jms,P_vvrecc),   &
                   rsfs_prec(ims,kms,jms,P_wwrecc),   &
                   rsfs_rec(ims,kms,jms,P_urec),      &
                   rsfs_rec(ims,kms,jms,P_vrec),      &
                   rsfs_rec(ims,kms,jms,P_wrec), 'u', &
                   ids, ide, jds, jde, kds, kde,      &
                   ims, ime, jms, jme, kms, kme,      &
                   ips, ipe, jps, jpe, kps, kpe,      &
                   grid%i_start(ij), grid%i_end(ij),  &
                   grid%j_start(ij), grid%j_end(ij),  &
                   k_start, k_end                     )

      CALL proj_d( rsfs_prec(ims,kms,jms,P_urecd),   &
                   rsfs_prec(ims,kms,jms,P_vrecd),   &
                   rsfs_prec(ims,kms,jms,P_uvrecd),  &
                   rsfs_rec(ims,kms,jms,P_urec),     &
                   rsfs_rec(ims,kms,jms,P_vrec),     &
                   ids, ide, jds, jde, kds, kde,     &
                   ims, ime, jms, jme, kms, kme,     &
                   ips, ipe, jps, jpe, kps, kpe,     &
                   grid%i_start(ij), grid%i_end(ij), &
                   grid%j_start(ij), grid%j_end(ij), &
                   k_start, k_end                    )

      CALL proj_e( rsfs_prec(ims,kms,jms,P_urece),   &
                   rsfs_prec(ims,kms,jms,P_wrece),   &
                   rsfs_prec(ims,kms,jms,P_uwrece),  &
                   rsfs_rec(ims,kms,jms,P_urec),     &
                   rsfs_rec(ims,kms,jms,P_wrec),     &
                   grid%fnm, grid%fnp,               &
                   ids, ide, jds, jde, kds, kde,     &
                   ims, ime, jms, jme, kms, kme,     &
                   ips, ipe, jps, jpe, kps, kpe,     &
                   grid%i_start(ij), grid%i_end(ij), &
                   grid%j_start(ij), grid%j_end(ij), &
                   k_start, k_end                    )

      CALL proj_f( rsfs_prec(ims,kms,jms,P_vrecf),   &
                   rsfs_prec(ims,kms,jms,P_wrecf),   &
                   rsfs_prec(ims,kms,jms,P_vwrecf),  &
                   rsfs_rec(ims,kms,jms,P_vrec),     &
                   rsfs_rec(ims,kms,jms,P_wrec),     &
                   grid%fnm, grid%fnp,               &
                   ids, ide, jds, jde, kds, kde,     &
                   ims, ime, jms, jme, kms, kme,     &
                   ips, ipe, jps, jpe, kps, kpe,     &
                   grid%i_start(ij), grid%i_end(ij), &
                   grid%j_start(ij), grid%j_end(ij), &
                   k_start, k_end                    )

      CALL proj_s( grid%usc, grid%vsc,               &
                   grid%uusc, grid%vvsc, grid%wws,   &
                   grid%usd, grid%vsd, grid%uvsd,    &
                   grid%wse, grid%uwse,              &
                   grid%wsf, grid%vwsf,              &
                   grid%us, grid%vs, grid%ws,        &
                   ids, ide, jds, jde,               &
                   ims, ime, jms, jme,               &
                   ips, ipe, jps, jpe,               &
                   grid%i_start(ij), grid%i_end(ij), &
                   grid%j_start(ij), grid%j_end(ij)  )

    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_RSFS_PREC_sub ( grid, &
  num_rsfs_prec, &
  rsfs_prec, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_PREC_sub ( grid, &
  config_flags, &
  num_rsfs_prec, &
  rsfs_prec, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_RSFS_PUVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_PUVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 
 
      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_urecc), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_uurecc), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )
 
      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vrecc), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vvrecc), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )
 
      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_wrecc), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_wwrecc), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_urecd), 'd', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vrecd), 'd', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_uvrecd), 'd', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_urece), 'e', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_wrece), 'e', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_uwrece), 'e', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vrecf), 'f', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_wrecf), 'f', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vwrecf), 'f', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc2d_m( grid%usc, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uusc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )
 
      CALL set_physical_bc2d_m( grid%vsc, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vvsc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wws, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%usd, 'd', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vsd, 'd', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uvsd, 'd', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wse, 'e', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uwse, 'e', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wsf, 'f', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vwsf, 'f', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

    ENDDO 
    !$OMP END PARALLEL DO






    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_urecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_urecc),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%usc, 'cc', iexp,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )
 
    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_uurecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_uurecc),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%uusc, 'cc', iexp,            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vrecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vrecc),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%vsc, 'cc', iexp,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vvrecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vvrecc),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%vvsc, 'cc', iexp,            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_wrecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_wrecc),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%ws, 'cw', iexp,              &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_wwrecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_wwrecc),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%wws, 'cw', iexp,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_urecd)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_urecd),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%usd, 'dd', iexp,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vrecd)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

     CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vrecd),   &
                       rsfs_filt(ims,kms,jms,P_temp),    &
                       grid%vsd, 'dd', iexp,             &
                       ids, ide, jds, jde, kds, kde,     &
                       ims, ime, jms, jme, kms, kme,     &
                       ips, ipe, jps, jpe, kps, kpe,     &
                       grid%i_start(ij), grid%i_end(ij), &
                       grid%j_start(ij), grid%j_end(ij), &
                       k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_uvrecd)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_uvrecd),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%uvsd, 'dd', iexp,            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_urece)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_urece),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%us, 'ee', iexp,              &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_wrece)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_wrece),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%wse, 'ew', iexp,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_uwrece)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_uwrece),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%uwse, 'ew', iexp,            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vrecf)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vrecf),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%vs, 'ff', iexp,              &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_wrecf)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_wrecf),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%wsf, 'fw', iexp,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vwrecf)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vwrecf),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%vwsf, 'fw', iexp,            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO
















    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL uvw_s_c( grid%usc, grid%vsc, grid%ws,      &
                    grid%uusc, grid%vvsc, grid%wws,   &
                    grid%usd, grid%vsd, grid%uvsd,    &
                    grid%wse, grid%uwse,              &
                    grid%wsf, grid%vwsf,              &
                    rsfs_prec(ims,kms,jms,P_urecc),   &
                    rsfs_prec(ims,kms,jms,P_vrecc),   &
                    rsfs_prec(ims,kms,jms,P_wrecc),   &
                    rsfs_prec(ims,kms,jms,P_uurecc),  &
                    rsfs_prec(ims,kms,jms,P_vvrecc),  &
                    rsfs_prec(ims,kms,jms,P_wwrecc),  &
                    rsfs_prec(ims,kms,jms,P_urecd),   &
                    rsfs_prec(ims,kms,jms,P_vrecd),   &
                    rsfs_prec(ims,kms,jms,P_uvrecd),  &
                    rsfs_prec(ims,kms,jms,P_wrece),   &
                    rsfs_prec(ims,kms,jms,P_uwrece),  &
                    rsfs_prec(ims,kms,jms,P_wrecf),   &
                    rsfs_prec(ims,kms,jms,P_vwrecf),  &
                    grid%ht, grid%rdx, grid%rdy,      &
                    grid%cf1, grid%cf2, grid%cf3,     &
                    config_flags%rsfsvsbc_opt,        &
                    ids, ide, jds, jde, kds, kde,     &
                    ims, ime, jms, jme, kms, kme,     &
                    ips, ipe, jps, jpe, kps, kpe,     &
                    grid%i_start(ij), grid%i_end(ij), &
                    grid%j_start(ij), grid%j_end(ij), &
                    k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO













CALL HALO_EM_RSFS_PREC_sub ( grid, &
  num_rsfs_prec, &
  rsfs_prec, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_PREC_sub ( grid, &
  config_flags, &
  num_rsfs_prec, &
  rsfs_prec, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_RSFS_UVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_UVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_RSFS_PUVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_PUVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_urecc), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_uurecc), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )
 
      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vrecc), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vvrecc), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )
 
      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_wrecc), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_wwrecc), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_urecd), 'd', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vrecd), 'd', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_uvrecd), 'd', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_urece), 'e', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_wrece), 'e', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_uwrece), 'e', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vrecf), 'f', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_wrecf), 'f', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( rsfs_prec(ims,kms,jms,P_vwrecf), 'f', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc2d_m( grid%usc, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uusc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )
 
      CALL set_physical_bc2d_m( grid%vsc, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vvsc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%ws, 't', config_flags,       &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wws, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%usd, 'd', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vsd, 'd', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uvsd, 'd', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wse, 'e', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uwse, 'e', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wsf, 'f', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vwsf, 'f', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

    ENDDO 
    !$OMP END PARALLEL DO






    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_urecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_urecc),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%usc, 'cc', itst,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )
 
    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_uurecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_uurecc),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%uusc, 'cc', itst,            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vrecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vrecc),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%vsc, 'cc', itst,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vvrecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vvrecc),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%vvsc, 'cc', itst,            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_wrecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_wrecc),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%ws, 'cw', itst,              &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_wwrecc)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_wwrecc),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%wws, 'cw', itst,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_urecd)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_urecd),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%usd, 'dd', itst,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vrecd)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

     CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vrecd),   &
                       rsfs_filt(ims,kms,jms,P_temp),    &
                       grid%vsd, 'dd', itst,             &
                       ids, ide, jds, jde, kds, kde,     &
                       ims, ime, jms, jme, kms, kme,     &
                       ips, ipe, jps, jpe, kps, kpe,     &
                       grid%i_start(ij), grid%i_end(ij), &
                       grid%j_start(ij), grid%j_end(ij), &
                       k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_uvrecd)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_uvrecd),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%uvsd, 'dd', itst,            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_urece)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_urece),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%us, 'ee', itst,              &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_wrece)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_wrece),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%wse, 'ew', itst,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_uwrece)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_uwrece),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%uwse, 'ew', itst,            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vrecf)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vrecf),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%vs, 'ff', itst,              &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_wrecf)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_wrecf),   &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%wsf, 'fw', itst,             &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

    DO i = ims, ime
    DO k = kms, kme
    DO j = jms, jme
       rsfs_filt(i,k,j,P_temp)=rsfs_prec(i,k,j,P_vwrecf)
    ENDDO
    ENDDO
    ENDDO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rsfs_filter( rsfs_prec(ims,kms,jms,P_vwrecf),  &
                        rsfs_filt(ims,kms,jms,P_temp),    &
                        grid%vwsf, 'fw', itst,            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        ips, ipe, jps, jpe, kps, kpe,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO











    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL rmij( tgrmij_drm(ims,kms,jms,P_tgrm11),     &
                 rsfs_prec(ims,kms,jms,P_urecc),       &
                 rsfs_prec(ims,kms,jms,P_urecc),       &
                 rsfs_prec(ims,kms,jms,P_uurecc), 'c', &
                 ids, ide, jds, jde, kds, kde,         &
                 ims, ime, jms, jme, kms, kme,         &
                 ips, ipe, jps, jpe, kps, kpe,         &
                 grid%i_start(ij), grid%i_end(ij),     &
                 grid%j_start(ij), grid%j_end(ij),     &
                 k_start, k_end                        )
 
       CALL rmij( tgrmij_drm(ims,kms,jms,P_tgrm22),     &
                  rsfs_prec(ims,kms,jms,P_vrecc),       &
                  rsfs_prec(ims,kms,jms,P_vrecc),       &
                  rsfs_prec(ims,kms,jms,P_vvrecc), 'c', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )

       CALL rmij( tgrmij_drm(ims,kms,jms,P_tgrm33),     &
                  rsfs_prec(ims,kms,jms,P_wrecc),       &
                  rsfs_prec(ims,kms,jms,P_wrecc),       &
                  rsfs_prec(ims,kms,jms,P_wwrecc), 'c', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )
 
       CALL rmij( tgrmij_drm(ims,kms,jms,P_tgrm12),     &
                  rsfs_prec(ims,kms,jms,P_urecd),       &
                  rsfs_prec(ims,kms,jms,P_vrecd),       &
                  rsfs_prec(ims,kms,jms,P_uvrecd), 'd', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )

       CALL rmij( tgrmij_drm(ims,kms,jms,P_tgrm13),     &
                  rsfs_prec(ims,kms,jms,P_urece),       &
                  rsfs_prec(ims,kms,jms,P_wrece),       &
                  rsfs_prec(ims,kms,jms,P_uwrece), 'e', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )

       CALL rmij( tgrmij_drm(ims,kms,jms,P_tgrm23),     &
                  rsfs_prec(ims,kms,jms,P_vrecf),       &
                  rsfs_prec(ims,kms,jms,P_wrecf),       &
                  rsfs_prec(ims,kms,jms,P_vwrecf), 'f', &
                  ids, ide, jds, jde, kds, kde,         &
                  ims, ime, jms, jme, kms, kme,         &
                  ips, ipe, jps, jpe, kps, kpe,         &
                  grid%i_start(ij), grid%i_end(ij),     &
                  grid%j_start(ij), grid%j_end(ij),     &
                  k_start, k_end                        )

    ENDDO 
    !$OMP END PARALLEL DO






       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( tfrmij_drm(ims,kms,jms,P_tfrm11), &
                            rsfs_rmij(ims,kms,jms,P_rm11),    &
                            grid%us, 'tc', itst,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( tfrmij_drm(ims,kms,jms,P_tfrm22), &
                            rsfs_rmij(ims,kms,jms,P_rm22),    &
                            grid%us, 'tc', itst,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( tfrmij_drm(ims,kms,jms,P_tfrm33), &
                            rsfs_rmij(ims,kms,jms,P_rm33),    &
                            grid%us, 'tc', itst,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( tfrmij_drm(ims,kms,jms,P_tfrm12), &
                            rsfs_rmij(ims,kms,jms,P_rm12),    &
                            grid%us, 'td', itst,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( tfrmij_drm(ims,kms,jms,P_tfrm13), &
                            rsfs_rmij(ims,kms,jms,P_rm13),    &
                            grid%us, 'te', itst,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

          CALL rsfs_filter( tfrmij_drm(ims,kms,jms,P_tfrm23), &
                            rsfs_rmij(ims,kms,jms,P_rm23),    &
                            grid%us, 'tf', itst,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 

         ENDDO 
        !$OMP END PARALLEL DO






       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

                  CALL hij( hij_drm(ims,kms,jms,P_h11),       &
                            hij_drm(ims,kms,jms,P_h22),       &
                            hij_drm(ims,kms,jms,P_h33),       &
                            hij_drm(ims,kms,jms,P_h12),       &
                            hij_drm(ims,kms,jms,P_h13),       &
                            hij_drm(ims,kms,jms,P_h23),       &
                            tgrmij_drm(ims,kms,jms,P_tgrm11), &
                            tgrmij_drm(ims,kms,jms,P_tgrm22), &
                            tgrmij_drm(ims,kms,jms,P_tgrm33), &
                            tgrmij_drm(ims,kms,jms,P_tgrm12), &
                            tgrmij_drm(ims,kms,jms,P_tgrm13), &
                            tgrmij_drm(ims,kms,jms,P_tgrm23), &
                            tfrmij_drm(ims,kms,jms,P_tfrm11), &
                            tfrmij_drm(ims,kms,jms,P_tfrm22), &
                            tfrmij_drm(ims,kms,jms,P_tfrm33), &
                            tfrmij_drm(ims,kms,jms,P_tfrm12), &
                            tfrmij_drm(ims,kms,jms,P_tfrm13), &
                            tfrmij_drm(ims,kms,jms,P_tfrm23), &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            ips, ipe, jps, jpe, kps, kpe,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start, k_end                    )

         ENDDO 
        !$OMP END PARALLEL DO











CALL HALO_EM_HIJ_DRM_sub ( grid, &
  num_hij_drm, &
  hij_drm, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_HIJ_DRM_sub ( grid, &
  config_flags, &
  num_hij_drm, &
  hij_drm, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

      CALL set_physical_bc3d_m( hij_drm(ims,kms,jms,P_h11), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( hij_drm(ims,kms,jms,P_h22), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )
 
      CALL set_physical_bc3d_m( hij_drm(ims,kms,jms,P_h33), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( hij_drm(ims,kms,jms,P_h12), 'd', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )
 
      CALL set_physical_bc3d_m( hij_drm(ims,kms,jms,P_h13), 'e', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( hij_drm(ims,kms,jms,P_h23), 'f', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )














         ENDDO 
        !$OMP END PARALLEL DO

 ENDIF 












       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

                CALL proj_c( uc_drm(ims,kms,jms,P_uc),          &
                             uc_drm(ims,kms,jms,P_vc),          &
                             uc_drm(ims,kms,jms,P_wc),          &
                             uc_drm(ims,kms,jms,P_uuc),         &
                             uc_drm(ims,kms,jms,P_vvc),         &
                             uc_drm(ims,kms,jms,P_wwc),         &
                             grid%u_2, grid%v_2, grid%w_2, 'u', &
                             ids, ide, jds, jde, kds, kde,      &
                             ims, ime, jms, jme, kms, kme,      &
                             ips, ipe, jps, jpe, kps, kpe,      &
                             grid%i_start(ij), grid%i_end(ij),  &
                             grid%j_start(ij), grid%j_end(ij),  &
                             k_start, k_end                     )


                CALL proj_c( sijc_drm(ims,kms,jms,P_s12c),      &
                             sijc_drm(ims,kms,jms,P_s13c),      &
                             sijc_drm(ims,kms,jms,P_s23c),      &
                             tfu_drm(ims,kms,jms,P_tfu),        & 
                             tfu_drm(ims,kms,jms,P_tfv),        & 
                             tfu_drm(ims,kms,jms,P_tfw),        & 
                             grid%defor12, grid%defor13,        &
                             grid%defor23, 'd',                 &
                             ids, ide, jds, jde, kds, kde,      &
                             ims, ime, jms, jme, kms, kme,      &
                             ips, ipe, jps, jpe, kps, kpe,      &
                             grid%i_start(ij), grid%i_end(ij),  &
                             grid%j_start(ij), grid%j_end(ij),  &
                             k_start, k_end                     )




                CALL proj_c( sijc_drm(ims,kms,jms,P_s11c),      &
                             sijc_drm(ims,kms,jms,P_s22c),      &
                             sijc_drm(ims,kms,jms,P_s33c),      &
                             tfu_drm(ims,kms,jms,P_tfu),        & 
                             tfu_drm(ims,kms,jms,P_tfv),        & 
                             tfu_drm(ims,kms,jms,P_tfw),        & 
                             grid%defor11, grid%defor22,        &
                             grid%defor33, 'c',                 &
                             ids, ide, jds, jde, kds, kde,      &
                             ims, ime, jms, jme, kms, kme,      &
                             ips, ipe, jps, jpe, kps, kpe,      &
                             grid%i_start(ij), grid%i_end(ij),  &
                             grid%j_start(ij), grid%j_end(ij),  &
                             k_start, k_end                     )

                IF ( config_flags%drm_opt .EQ. 1 ) THEN  

                CALL proj_c( hijc_drm(ims,kms,jms,P_h12c),      &
                             hijc_drm(ims,kms,jms,P_h13c),      &
                             hijc_drm(ims,kms,jms,P_h23c),      &
                             tfu_drm(ims,kms,jms,P_tfu),        & 
                             tfu_drm(ims,kms,jms,P_tfv),        & 
                             tfu_drm(ims,kms,jms,P_tfw),        & 
                             hij_drm(ims,kms,jms,P_h12),        &
                             hij_drm(ims,kms,jms,P_h13),        &
                             hij_drm(ims,kms,jms,P_h23), 'h',   &
                             ids, ide, jds, jde, kds, kde,      &
                             ims, ime, jms, jme, kms, kme,      &
                             ips, ipe, jps, jpe, kps, kpe,      &
                             grid%i_start(ij), grid%i_end(ij),  &
                             grid%j_start(ij), grid%j_end(ij),  &
                             k_start, k_end                     )

                ENDIF 


        ENDDO 
        !$OMP END PARALLEL DO







       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
              DO ij = 1 , grid%num_tiles 

                CALL product( uc_drm(ims,kms,jms,P_uvc),        &
                              uc_drm(ims,kms,jms,P_uc),         &
                              uc_drm(ims,kms,jms,P_vc),         &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                    )

                CALL product( uc_drm(ims,kms,jms,P_uwc),        &
                              uc_drm(ims,kms,jms,P_uc),         &
                              uc_drm(ims,kms,jms,P_wc),         &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                    )

                CALL product( uc_drm(ims,kms,jms,P_vwc),        &
                              uc_drm(ims,kms,jms,P_vc),         &
                              uc_drm(ims,kms,jms,P_wc),         &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                    )


              ENDDO 
        !$OMP END PARALLEL DO








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL uvw_s_c( grid%usc, grid%vsc, grid%ws,      &
                    grid%uusc, grid%vvsc, grid%wws,   &
                    grid%usd, grid%vsd, grid%uvsd,    &
                    grid%wse, grid%uwse,              &
                    grid%wsf, grid%vwsf,              &
                    uc_drm(ims,kms,jms,P_uc),         &
                    uc_drm(ims,kms,jms,P_vc),         &
                    uc_drm(ims,kms,jms,P_wc),         &
                    uc_drm(ims,kms,jms,P_uuc),        &
                    uc_drm(ims,kms,jms,P_vvc),        &
                    uc_drm(ims,kms,jms,P_wwc),        &
                    uc_drm(ims,kms,jms,P_uc),         & 
                    uc_drm(ims,kms,jms,P_uc),         & 
                    uc_drm(ims,kms,jms,P_uvc),        &
                    uc_drm(ims,kms,jms,P_uc),         & 
                    uc_drm(ims,kms,jms,P_uwc),        &
                    uc_drm(ims,kms,jms,P_uc),         & 
                    uc_drm(ims,kms,jms,P_vwc),        &
                    grid%ht, grid%rdx, grid%rdy,      &
                    grid%cf1, grid%cf2, grid%cf3,     &
                    config_flags%rsfsvsbc_opt,        &
                    ids, ide, jds, jde, kds, kde,     &
                    ims, ime, jms, jme, kms, kme,     &
                    ips, ipe, jps, jpe, kps, kpe,     &
                    grid%i_start(ij), grid%i_end(ij), &
                    grid%j_start(ij), grid%j_end(ij), &
                    k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO













CALL HALO_EM_UC_DRM_sub ( grid, &
  num_uc_drm, &
  uc_drm, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_UC_DRM_sub ( grid, &
  config_flags, &
  num_uc_drm, &
  uc_drm, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_SIJC_DRM_sub ( grid, &
  num_sijc_drm, &
  sijc_drm, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_SIJC_DRM_sub ( grid, &
  config_flags, &
  num_sijc_drm, &
  sijc_drm, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_EM_RSFS_PUVWS_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_RSFS_PUVWS_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

      CALL set_physical_bc3d_m( uc_drm(ims,kms,jms,P_uc), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( uc_drm(ims,kms,jms,P_vc), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )
 
      CALL set_physical_bc3d_m( uc_drm(ims,kms,jms,P_wc), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( uc_drm(ims,kms,jms,P_uuc), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )
 
      CALL set_physical_bc3d_m( uc_drm(ims,kms,jms,P_vvc), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( uc_drm(ims,kms,jms,P_wwc), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( uc_drm(ims,kms,jms,P_uvc), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( uc_drm(ims,kms,jms,P_uwc), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( uc_drm(ims,kms,jms,P_vwc), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( sijc_drm(ims,kms,jms,P_s11c), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( sijc_drm(ims,kms,jms,P_s22c), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( sijc_drm(ims,kms,jms,P_s33c), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc3d_m( sijc_drm(ims,kms,jms,P_s12c), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( sijc_drm(ims,kms,jms,P_s13c), 't', &
                              config_flags,                        &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              ips, ipe, jps, jpe, kps, kpe,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )

      CALL set_physical_bc3d_m( sijc_drm(ims,kms,jms,P_s23c), 't', &
                              config_flags,                         &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              ips, ipe, jps, jpe, kps, kpe,         &
                              grid%i_start(ij), grid%i_end(ij),     &
                              grid%j_start(ij), grid%j_end(ij),     &
                              k_start, k_end                        )

      CALL set_physical_bc2d_m( grid%usc, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uusc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )
 
      CALL set_physical_bc2d_m( grid%vsc, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vvsc, 't', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%ws, 't', config_flags,       &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%wws, 't', config_flags,      &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uvsd, 'd', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%uwse, 'e', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

      CALL set_physical_bc2d_m( grid%vwsf, 'f', config_flags,     &
                              ids, ide, jds, jde,               &
                              ims, ime, jms, jme,               &
                              ips, ipe, jps, jpe,               &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij)  )

    ENDDO 
    !$OMP END PARALLEL DO








       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

          CALL drm_filter( tfuc_drm(ims,kms,jms,P_tfuc),     &
                           uc_drm(ims,kms,jms,P_uc),         &
                           grid%usc, 'cc', itst,             &    
                           ids, ide, jds, jde, kds, kde,     & 
                           ims, ime, jms, jme, kms, kme,     & 
                           ips, ipe, jps, jpe, kps, kpe,     & 
                           grid%i_start(ij), grid%i_end(ij), & 
                           grid%j_start(ij), grid%j_end(ij), & 
                           k_start, k_end                    ) 

          CALL drm_filter( tfuc_drm(ims,kms,jms,P_tfvc),     &
                           uc_drm(ims,kms,jms,P_vc),         &
                           grid%vsc, 'cc', itst,             &    
                           ids, ide, jds, jde, kds, kde,     & 
                           ims, ime, jms, jme, kms, kme,     & 
                           ips, ipe, jps, jpe, kps, kpe,     & 
                           grid%i_start(ij), grid%i_end(ij), & 
                           grid%j_start(ij), grid%j_end(ij), & 
                           k_start, k_end                    ) 

          CALL drm_filter( tfuc_drm(ims,kms,jms,P_tfwc),     &
                           uc_drm(ims,kms,jms,P_wc),         &
                           grid%ws, 'cw', itst,              &    
                           ids, ide, jds, jde, kds, kde,     & 
                           ims, ime, jms, jme, kms, kme,     & 
                           ips, ipe, jps, jpe, kps, kpe,     & 
                           grid%i_start(ij), grid%i_end(ij), & 
                           grid%j_start(ij), grid%j_end(ij), & 
                           k_start, k_end                    ) 

          CALL drm_filter( tfuc_drm(ims,kms,jms,P_tfuuc),    &
                           uc_drm(ims,kms,jms,P_uuc),        &
                           grid%uusc, 'cc', itst,            &    
                           ids, ide, jds, jde, kds, kde,     & 
                           ims, ime, jms, jme, kms, kme,     & 
                           ips, ipe, jps, jpe, kps, kpe,     & 
                           grid%i_start(ij), grid%i_end(ij), & 
                           grid%j_start(ij), grid%j_end(ij), & 
                           k_start, k_end                    ) 

          CALL drm_filter( tfuc_drm(ims,kms,jms,P_tfvvc),    &
                           uc_drm(ims,kms,jms,P_vvc),        &
                           grid%vvsc, 'cc', itst,            &    
                           ids, ide, jds, jde, kds, kde,     & 
                           ims, ime, jms, jme, kms, kme,     & 
                           ips, ipe, jps, jpe, kps, kpe,     & 
                           grid%i_start(ij), grid%i_end(ij), & 
                           grid%j_start(ij), grid%j_end(ij), & 
                           k_start, k_end                    ) 

          CALL drm_filter( tfuc_drm(ims,kms,jms,P_tfwwc),    &
                           uc_drm(ims,kms,jms,P_wwc),        &
                           grid%wws, 'cw', itst,             &    
                           ids, ide, jds, jde, kds, kde,     & 
                           ims, ime, jms, jme, kms, kme,     & 
                           ips, ipe, jps, jpe, kps, kpe,     & 
                           grid%i_start(ij), grid%i_end(ij), & 
                           grid%j_start(ij), grid%j_end(ij), & 
                           k_start, k_end                    ) 

          CALL drm_filter( tfuc_drm(ims,kms,jms,P_tfuvc),    &
                           uc_drm(ims,kms,jms,P_uvc),        &
                           grid%uvsd, 'cc', itst,            &    
                           ids, ide, jds, jde, kds, kde,     & 
                           ims, ime, jms, jme, kms, kme,     & 
                           ips, ipe, jps, jpe, kps, kpe,     & 
                           grid%i_start(ij), grid%i_end(ij), & 
                           grid%j_start(ij), grid%j_end(ij), & 
                           k_start, k_end                    ) 

          CALL drm_filter( tfuc_drm(ims,kms,jms,P_tfuwc),    &
                           uc_drm(ims,kms,jms,P_uwc),        &
                           grid%uwse, 'cw', itst,            &    
                           ids, ide, jds, jde, kds, kde,     & 
                           ims, ime, jms, jme, kms, kme,     & 
                           ips, ipe, jps, jpe, kps, kpe,     & 
                           grid%i_start(ij), grid%i_end(ij), & 
                           grid%j_start(ij), grid%j_end(ij), & 
                           k_start, k_end                    ) 

          CALL drm_filter( tfuc_drm(ims,kms,jms,P_tfvwc),    &
                           uc_drm(ims,kms,jms,P_vwc),        &
                           grid%vwsf, 'cw', itst,            &    
                           ids, ide, jds, jde, kds, kde,     & 
                           ims, ime, jms, jme, kms, kme,     & 
                           ips, ipe, jps, jpe, kps, kpe,     & 
                           grid%i_start(ij), grid%i_end(ij), & 
                           grid%j_start(ij), grid%j_end(ij), & 
                           k_start, k_end                    ) 

          CALL drm_filter( tfsijc_drm(ims,kms,jms,P_tfs11c), &
                           sijc_drm(ims,kms,jms,P_s11c),     &
                           grid%us, 'sh', itst,              & 
                           ids, ide, jds, jde, kds, kde,     & 
                           ims, ime, jms, jme, kms, kme,     & 
                           ips, ipe, jps, jpe, kps, kpe,     & 
                           grid%i_start(ij), grid%i_end(ij), & 
                           grid%j_start(ij), grid%j_end(ij), & 
                           k_start, k_end                    ) 

          CALL drm_filter( tfsijc_drm(ims,kms,jms,P_tfs22c), &
                           sijc_drm(ims,kms,jms,P_s22c),     &
                           grid%us, 'sh', itst,              & 
                           ids, ide, jds, jde, kds, kde,     & 
                           ims, ime, jms, jme, kms, kme,     & 
                           ips, ipe, jps, jpe, kps, kpe,     & 
                           grid%i_start(ij), grid%i_end(ij), & 
                           grid%j_start(ij), grid%j_end(ij), & 
                           k_start, k_end                    ) 

          CALL drm_filter( tfsijc_drm(ims,kms,jms,P_tfs33c), &
                           sijc_drm(ims,kms,jms,P_s33c),     &
                           grid%us, 'sh', itst,              & 
                           ids, ide, jds, jde, kds, kde,     & 
                           ims, ime, jms, jme, kms, kme,     & 
                           ips, ipe, jps, jpe, kps, kpe,     & 
                           grid%i_start(ij), grid%i_end(ij), & 
                           grid%j_start(ij), grid%j_end(ij), & 
                           k_start, k_end                    ) 

          CALL drm_filter( tfsijc_drm(ims,kms,jms,P_tfs12c), &
                           sijc_drm(ims,kms,jms,P_s12c),     &
                           grid%us, 'sh', itst,              & 
                           ids, ide, jds, jde, kds, kde,     & 
                           ims, ime, jms, jme, kms, kme,     & 
                           ips, ipe, jps, jpe, kps, kpe,     & 
                           grid%i_start(ij), grid%i_end(ij), & 
                           grid%j_start(ij), grid%j_end(ij), & 
                           k_start, k_end                    ) 

          CALL drm_filter( tfsijc_drm(ims,kms,jms,P_tfs13c), &
                           sijc_drm(ims,kms,jms,P_s13c),     &
                           grid%us, 'sh', itst,              & 
                           ids, ide, jds, jde, kds, kde,     & 
                           ims, ime, jms, jme, kms, kme,     & 
                           ips, ipe, jps, jpe, kps, kpe,     & 
                           grid%i_start(ij), grid%i_end(ij), & 
                           grid%j_start(ij), grid%j_end(ij), & 
                           k_start, k_end                    ) 

          CALL drm_filter( tfsijc_drm(ims,kms,jms,P_tfs23c), &
                           sijc_drm(ims,kms,jms,P_s23c),     &
                           grid%us, 'sh', itst,              & 
                           ids, ide, jds, jde, kds, kde,     & 
                           ims, ime, jms, jme, kms, kme,     & 
                           ips, ipe, jps, jpe, kps, kpe,     & 
                           grid%i_start(ij), grid%i_end(ij), & 
                           grid%j_start(ij), grid%j_end(ij), & 
                           k_start, k_end                    ) 


         ENDDO 
        !$OMP END PARALLEL DO






       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

                CALL drmnd( evc_drm(ims,kms,jms,P_drmn),      &
                            evc_drm(ims,kms,jms,P_drmd),      &
                            hij_drm(ims,kms,jms,P_h11),       &
                            hij_drm(ims,kms,jms,P_h22),       &
                            hij_drm(ims,kms,jms,P_h33),       &
                            hijc_drm(ims,kms,jms,P_h12c),     &
                            hijc_drm(ims,kms,jms,P_h13c),     &
                            hijc_drm(ims,kms,jms,P_h23c),     &
                            tfsijc_drm(ims,kms,jms,P_tfs11c), &
                            tfsijc_drm(ims,kms,jms,P_tfs22c), &
                            tfsijc_drm(ims,kms,jms,P_tfs33c), &
                            tfsijc_drm(ims,kms,jms,P_tfs12c), &
                            tfsijc_drm(ims,kms,jms,P_tfs13c), &
                            tfsijc_drm(ims,kms,jms,P_tfs23c), &
                            tfuc_drm(ims,kms,jms,P_tfuc),     &
                            tfuc_drm(ims,kms,jms,P_tfvc),     &
                            tfuc_drm(ims,kms,jms,P_tfwc),     &
                            tfuc_drm(ims,kms,jms,P_tfuuc),    &
                            tfuc_drm(ims,kms,jms,P_tfvvc),    &
                            tfuc_drm(ims,kms,jms,P_tfwwc),    &
                            tfuc_drm(ims,kms,jms,P_tfuvc),    &
                            tfuc_drm(ims,kms,jms,P_tfuwc),    &
                            tfuc_drm(ims,kms,jms,P_tfvwc),    &
                            config_flags%drm_opt,             &
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), &  
                            grid%j_start(ij), grid%j_end(ij), &  
                            k_start, k_end                    ) 

        ENDDO 
        !$OMP END PARALLEL DO












CALL HALO_EM_EVC_DRM_sub ( grid, &
  num_evc_drm, &
  evc_drm, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_EVC_DRM_sub ( grid, &
  config_flags, &
  num_evc_drm, &
  evc_drm, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

           CALL set_physical_bc3d_m( evc_drm(ims,kms,jms,P_drmn),      &   
                                   't', config_flags,                &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   ips, ipe, jps, jpe, kps, kpe,     &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end                    )

           CALL set_physical_bc3d_m( evc_drm(ims,kms,jms,P_drmd),      &
                                   't', config_flags,                &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   ips, ipe, jps, jpe, kps, kpe,     &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end                    )

        ENDDO 
        !$OMP END PARALLEL DO







       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( evc_drm(ims,kms,jms,P_fdrmn),     &
                            evc_drm(ims,kms,jms,P_drmn),      &
                            grid%us, 'kc', iexp,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 
       ENDDO 
       !$OMP END PARALLEL DO


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( evc_drm(ims,kms,jms,P_fdrmd),     &
                            evc_drm(ims,kms,jms,P_drmd),      &
                            grid%us, 'kc', iexp,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 
       ENDDO 
       !$OMP END PARALLEL DO












CALL HALO_EM_EVC_DRM_sub ( grid, &
  num_evc_drm, &
  evc_drm, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_EVC_DRM_sub ( grid, &
  config_flags, &
  num_evc_drm, &
  evc_drm, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

           CALL set_physical_bc3d_m( evc_drm(ims,kms,jms,P_fdrmn),     &
                                   't', config_flags,                &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   ips, ipe, jps, jpe, kps, kpe,     &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end                    )

           CALL set_physical_bc3d_m( evc_drm(ims,kms,jms,P_fdrmd),     &
                                   't',  config_flags,               &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   ips, ipe, jps, jpe, kps, kpe,     &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end                    )

        ENDDO 
        !$OMP END PARALLEL DO







       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( evc_drm(ims,kms,jms,P_drmn),      &
                            evc_drm(ims,kms,jms,P_fdrmn),     &
                            grid%us, 'kc', itst,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 
       ENDDO 
       !$OMP END PARALLEL DO

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( evc_drm(ims,kms,jms,P_drmd),      &
                            evc_drm(ims,kms,jms,P_fdrmd),     &
                            grid%us, 'kc', itst,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 
       ENDDO 
       !$OMP END PARALLEL DO


















CALL HALO_EM_EVC_DRM_sub ( grid, &
  num_evc_drm, &
  evc_drm, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_EVC_DRM_sub ( grid, &
  config_flags, &
  num_evc_drm, &
  evc_drm, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

           CALL set_physical_bc3d_m( evc_drm(ims,kms,jms,P_drmn),      &   
                                   't', config_flags,                &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   ips, ipe, jps, jpe, kps, kpe,     &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end                    )

           CALL set_physical_bc3d_m( evc_drm(ims,kms,jms,P_drmd),      &
                                   't', config_flags,                &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   ips, ipe, jps, jpe, kps, kpe,     &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end                    )

        ENDDO 
        !$OMP END PARALLEL DO







       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( evc_drm(ims,kms,jms,P_fdrmn),     &
                            evc_drm(ims,kms,jms,P_drmn),      &
                            grid%us, 'kc', iexp,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 
       ENDDO 
       !$OMP END PARALLEL DO


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( evc_drm(ims,kms,jms,P_fdrmd),     &
                            evc_drm(ims,kms,jms,P_drmd),      &
                            grid%us, 'kc', iexp,              &  
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 
       ENDDO 
       !$OMP END PARALLEL DO












CALL HALO_EM_EVC_DRM_sub ( grid, &
  num_evc_drm, &
  evc_drm, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_EVC_DRM_sub ( grid, &
  config_flags, &
  num_evc_drm, &
  evc_drm, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

           CALL set_physical_bc3d_m( evc_drm(ims,kms,jms,P_fdrmn),     &
                                   't', config_flags,                &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   ips, ipe, jps, jpe, kps, kpe,     &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end                    )

           CALL set_physical_bc3d_m( evc_drm(ims,kms,jms,P_fdrmd),     &
                                   't',  config_flags,               &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   ips, ipe, jps, jpe, kps, kpe,     &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end                    )

        ENDDO 
        !$OMP END PARALLEL DO







       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( evc_drm(ims,kms,jms,P_drmn),      &
                            evc_drm(ims,kms,jms,P_fdrmn),     &
                            grid%us, 'kc', itst,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 
       ENDDO 
       !$OMP END PARALLEL DO

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

          CALL rsfs_filter( evc_drm(ims,kms,jms,P_drmd),      &
                            evc_drm(ims,kms,jms,P_fdrmd),     &
                            grid%us, 'kc', itst,              & 
                            ids, ide, jds, jde, kds, kde,     & 
                            ims, ime, jms, jme, kms, kme,     & 
                            ips, ipe, jps, jpe, kps, kpe,     & 
                            grid%i_start(ij), grid%i_end(ij), & 
                            grid%j_start(ij), grid%j_end(ij), & 
                            k_start, k_end                    ) 
       ENDDO 
       !$OMP END PARALLEL DO






       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles 

               CALL drmevc( grid%xkmv, grid%xkhv, grid%xkmh, grid%xkhh, & 
                            evc_drm(ims,kms,jms,P_drmn),                &
                            evc_drm(ims,kms,jms,P_drmd),                &
                            ids, ide, jds, jde, kds, kde,               & 
                            ims, ime, jms, jme, kms, kme,               & 
                            ips, ipe, jps, jpe, kps, kpe,               & 
                            grid%i_start(ij), grid%i_end(ij),           &  
                            grid%j_start(ij), grid%j_end(ij),           &  
                            k_start, k_end                              ) 

        ENDDO 
       !$OMP END PARALLEL DO































CALL HALO_EM_EV_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_EV_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( grid%xkmh, 't', config_flags,     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                    )

          CALL set_physical_bc3d_m( grid%xkhh, 't', config_flags,     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                    )

          CALL set_physical_bc3d_m( grid%xkmv, 't', config_flags,     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                    )

          CALL set_physical_bc3d_m( grid%xkhv, 't', config_flags,     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

  IF ( config_flags%can_opt .EQ. 1 ) THEN








    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

          CALL drm_nw_canopy( grid%nwtau13, grid%nwtau23,       &
                              grid%u_2, grid%v_2,               &
                              grid%w_2, grid%rdz,               &
                              grid%rdzw, grid%dx,               &
                              grid%fnm, grid%fnp,               &
                              config_flags%canfact,             &
                              grid%ustm,                        & 
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO












CALL HALO_EM_NWTAU_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL PERIOD_EM_NWTAU_sub ( grid, &
  config_flags, &
  local_communicator_periodic, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles 

          CALL set_physical_bc3d_m( grid%nwtau13, 'e', config_flags,  &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                    )

          CALL set_physical_bc3d_m( grid%nwtau23, 'f', config_flags,  &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  ips, ipe, jps, jpe, kps, kpe,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                    )

    ENDDO 
    !$OMP END PARALLEL DO

  ENDIF







  ENDIF 


END SUBROUTINE sfs_driver

END MODULE module_sfs_driver
