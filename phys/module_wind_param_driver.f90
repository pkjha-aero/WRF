






















MODULE module_wind_param_driver

CONTAINS



SUBROUTINE wind_param_driver( grid, config_flags,           &
                              ru_tendf, rv_tendf, rw_tendf, &
                              u_2, v_2, w_2, rdzw,          &
                              muu, muv, mut,                &
                              dx, dy, dt,                   & 
                              x_turbine,                    &
                              y_turbine,                    &
                              hub_height,                   &
                              rotor_diameter,               &
                              blade_length,                 &
                              theta_turbine,                &
                              acc_yaw_err,                  & 
                              wp_acc_u, wp_acc_v, wp_acc_w, &
                              wp_ts,                        & 
                              thrust, torque,               &
                              rotrate, power,               &
                              n_turbines,                   &
                              n_timeseries,                 &
                              v0t, d0t, itimestep,          & 
                              ids,ide,jds,jde,kds,kde,      & 
                              ims,ime,jms,jme,kms,kme,      &
                              ips,ipe,jps,jpe,kps,kpe,      &
                              its,ite,jts,jte,kts,kte       )




  USE module_domain
  USE module_configure
  USE module_tiles
  USE module_dm
  USE module_machine
  USE module_state_description

  USE module_bc



  USE module_gen_act_disk

  IMPLICIT NONE



  TYPE(domain) , TARGET          :: grid

  TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme), INTENT( INOUT ) &
  ::  ru_tendf,  & 
      rv_tendf,  & 
      rw_tendf     

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme), INTENT( IN  ) &
  ::  u_2,       & 
      v_2,       & 
      w_2,       & 
      rdzw         

  REAL, DIMENSION( ims:ime, jms:jme) ,         INTENT( IN  ) &
  ::  muu,       & 
      muv,       & 
      mut          

 REAL, DIMENSION( n_turbines ),                 INTENT( INOUT ) &
  :: x_turbine       & 
   , y_turbine       & 
   , hub_height      & 
   , rotor_diameter  & 
   , blade_length    & 
   , theta_turbine    

 REAL, DIMENSION( n_turbines ),                 INTENT( INOUT ) &
  :: acc_yaw_err

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT( INOUT ) &
  :: wp_acc_u    & 
   , wp_acc_v    & 
   , wp_acc_w      

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT( INOUT ) & 
  :: wp_ts         


  REAL, DIMENSION( ims:ime, jms:jme ),           INTENT( OUT ) &
  :: thrust          & 
   , torque          & 
   , rotrate         & 
   , power             

  REAL, DIMENSION( n_timeseries, n_turbines ),   INTENT( INOUT ) &
  :: v0t         & 
   , d0t
   
  REAL,                                           INTENT( IN  ) &
  :: dx          & 
   , dy            

  REAL,                                           INTENT( IN  ) & 
  :: dt 

  INTEGER,                                        INTENT( IN  ) &
  :: n_turbines

  INTEGER,                                        INTENT( IN  ) &
  :: n_timeseries

  INTEGER, INTENT(IN) :: itimestep

  INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde,               &
                         ims,ime,jms,jme,kms,kme,               &
                         ips,ipe,jps,jpe,kps,kpe,               &
                         its,ite,jts,jte,kts,kte



  INTEGER :: ij, i, j, k

  IF ( config_flags%wp_opt .GE. 2 ) then


     CALL gen_act_disk ( ru_tendf, rv_tendf, rw_tendf,     & 
                         config_flags%wp_opt,              & 
                         n_turbines,                       &
                         n_timeseries,                     &
                         x_turbine,                        &
                         y_turbine,                        &
                         hub_height,                       &
                         rotor_diameter,                   &
                         blade_length,                     &
                         theta_turbine,                    &
                         acc_yaw_err,                      & 
                         wp_acc_u, wp_acc_v, wp_acc_w,     & 
                         wp_ts,                            & 
                         thrust, torque,                   &
                         rotrate, power,                   &
                         u_2, v_2, w_2,                    & 
                         muu, muv, mut,                    & 
                         rdzw, dx, dy, dt,                 & 
                         v0t, d0t, itimestep,              & 
                         ids, ide, jds, jde, kds, kde,     &
                         ims, ime, jms, jme, kms, kme,     &
                         ips, ipe, jps, jpe, kps, kpe,     &
                         its, ite, jts, jte, kts, kte      )

  ENDIF




RETURN

END SUBROUTINE wind_param_driver


END MODULE module_wind_param_driver
