      SUBROUTINE ana_lrflux (ng, tile, model)
!
!! svn $Id$
!!======================================================================
!! Copyright (c) 2002-2016 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine sets kinematic surface longwave radiation flux      !
!  "lrflx" (degC m/s) using an analytical expression.                  !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_forces
      USE mod_grid
      USE mod_ncparam
!
! Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model

#include "tile.h"
!
      CALL ana_lrflux_tile (ng, tile, model,                            &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      IminS, ImaxS, JminS, JmaxS,                 &
     &                      GRID(ng) % lonr,                            &
     &                      GRID(ng) % latr,                            &
#ifdef ALBEDO_CLOUD
     &                      FORCES(ng) % cloud,                         &
     &                      FORCES(ng) % Hair,                          &
     &                      FORCES(ng) % Tair,                          &
     &                      FORCES(ng) % Pair,                          &
#endif
     &                      FORCES(ng) % lrflx)
!
! Set analytical header file name used.
!
#ifdef DISTRIBUTE
      IF (Lanafile) THEN
#else
      IF (Lanafile.and.(tile.eq.0)) THEN
#endif
        ANANAME(27)=__FILE__
      END IF

      RETURN
      END SUBROUTINE ana_lrflux
!
!***********************************************************************
      SUBROUTINE ana_lrflux_tile (ng, tile, model,                      &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            IminS, ImaxS, JminS, JmaxS,           &
     &                            lonr, latr,                           &
#ifdef ALBEDO_CLOUD
     &                            cloud, Hair, Tair, Pair,              &
#endif
     &                            lrflx)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
      USE exchange_2d_mod, ONLY : exchange_r2d_tile
#ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
#endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!
#ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: lonr(LBi:,LBj:)
      real(r8), intent(in) :: latr(LBi:,LBj:)
# ifdef ALBEDO_CLOUD
      real(r8), intent(in) :: cloud(LBi:,LBj:)
      real(r8), intent(in) :: Hair(LBi:,LBj:)
      real(r8), intent(in) :: Tair(LBi:,LBj:)
      real(r8), intent(in) :: Pair(LBi:,LBj:)
# endif
      real(r8), intent(out) :: lrflx(LBi:,LBj:)
#else
      real(r8), intent(in) :: lonr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: latr(LBi:UBi,LBj:UBj)
# ifdef ALBEDO_CLOUD
      real(r8), intent(in) :: cloud(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Hair(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Tair(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Pair(LBi:UBi,LBj:UBj)
# endif
      real(r8), intent(out) :: lrflx(LBi:UBi,LBj:UBj)
#endif
!
!  Local variable declarations.
!
      integer :: i, j
#if defined ALBEDO_CLOUD || defined DIURNAL_SRFLUX
      integer :: iday, month, year
      real(r8) :: Dangle, Hangle, LatRad
      real(r8) :: cff1, cff2, hour, yday
# ifdef ALBEDO_CLOUD
      real(r8) :: Rsolar, e_sat, vap_p, zenith
# endif
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> TN: add
# if defined ANA_LONGWAVE_DOWN
      real(r8) :: TaK, Td, cff3, cff4
# endif

# if defined ANA_LONGWAVE_DOWN_TVA
      real(r8) :: TaK, Td, cff3, cff4
# endif

# if defined ANA_LONGWAVE_DOWN_HIRAGA
      real(r8) :: TaK, Td, cff3, cff4, Psat , Eva , Qa
# endif
# if defined ANA_LONGWAVE_DOWN_HIRAGA2
      real(r8) :: TaK, Td, cff3, cff4, Psat , Eva , Qa
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< TN: add
#endif
      real(r8) :: cff

#include "set_bounds.h"

!
!-----------------------------------------------------------------------
!  Set incoming longwave radiation (degC m/s).  Usually, the
!  longwave radiation from input files is Watts/m2 and then converted
!  to degC m/s by multiplying by conversion factor 1/(rho0*Cp) during
!  reading (Fscale). However, we are already inside ROMS kernel here
!  and all the fluxes are kinematic so longwave radiation units need
!  to be degC m/s.
!-----------------------------------------------------------------------
!
      cff=1.0_r8/(rho0*cp)
#if defined UPWELLING
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          lrflx(i,j)=cff*150.0_r8
        END DO
      END DO
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> TN: add
#elif defined ANA_LONGWAVE_DOWN
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          TaK = Tair(i,j) + 273.15_r8
          cff1 = 0.00000936_r8 * TaK*TaK              ! Swinbank (1963)
          cff2 = cff1*(1.0_r8+0.22_r8*cloud(i,j))     ! Ångström (1915)
          lrflx(i,j) = cff2 * StefBo*TaK*TaK*TaK*TaK
#elif defined ANA_LONGWAVE_DOWN_TVA
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          TaK = Tair(i,j) + 273.15_r8
          cff1 = 0.00000937_r8 * TaK*TaK*TaK*TaK*TaK*TaK              
          cff2 = cff1*(1.0_r8 + 0.17_r8 * cloud(i,j) * cloud(i,j))      
          lrflx(i,j) = cff2 * StefBo 
# if defined JMAMSM_FLUXES
          lrflx(i,j)=(lrflx(i,j)-16.7_r8)/0.977_r8  ! bias correction for JMAMSM
# elif defined JMAOBS_FLUXES
          lrflx(i,j)=(lrflx(i,j)+39.0_r8)/1.16_r8   ! bias correction for JMA weather station data
# endif
          lrflx(i,j)=cff*lrflx(i,j)
        END DO
      END DO
      
#elif defined ANA_LONGWAVE_DOWN_HIRAGA
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          TaK = Tair(i,j) + 273.15_r8
          Psat = 6.1078_r8 * 10.0_r8**(7.5_r8 * (TaK - 273.15_r8) / (TaK - 35.85_r8))
          Qa = Hair(i,j)
          Eva = Psat * Qa
          cff1 = 0.45_r8 + 0.066_r8 * sqrt(Eva)              ! brunt ()
          cff2 = cff1*(1.0_r8 - 0.7_r8 * cloud(i,j)) + 0.7_r8 * cloud(i,j)     ! hiraga
          lrflx(i,j) = cff2 * StefBo * TaK * TaK * TaK * TaK
# if defined JMAMSM_FLUXES
          lrflx(i,j)=(lrflx(i,j)-16.7_r8)/0.977_r8  ! bias correction for JMAMSM
# elif defined JMAOBS_FLUXES
          lrflx(i,j)=(lrflx(i,j)+39.0_r8)/1.16_r8   ! bias correction for JMA weather station data
# endif
          lrflx(i,j)=cff*lrflx(i,j)
        END DO
      END DO
#elif defined ANA_LONGWAVE_DOWN_HIRAGA2
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          TaK = Tair(i,j) + 273.15_r8
          Psat = 6.1078_r8 * 10.0_r8**(7.5_r8 * (TaK - 273.15_r8) / (TaK - 35.85_r8))
          Qa = Hair(i,j)
          Eva = Psat * Qa
          cff1 = 0.51_r8 + 0.066_r8 * sqrt(Eva)              ! brunt ()
          cff2 = cff1*(1.0_r8 - 0.84_r8 * cloud(i,j)) + 0.84_r8 * cloud(i,j)     ! hiraga
          lrflx(i,j) = cff2 * StefBo*TaK*TaK*TaK*TaK

# if defined JMAMSM_FLUXES
          lrflx(i,j)=(lrflx(i,j)-16.7_r8)/0.977_r8  ! bias correction for JMAMSM
# elif defined JMAOBS_FLUXES
          lrflx(i,j)=(lrflx(i,j)+39.0_r8)/1.16_r8   ! bias correction for JMA weather station data
# endif
          lrflx(i,j)=cff*lrflx(i,j)
        END DO
      END DO
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< TN: add
#else
      DO j=JstrT,JendT
        DO i=IstrT,IendT
          lrflx(i,j)=0.0_r8
        END DO
      END DO
#endif
!
!  Exchange boundary data.
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          lrflx)
      END IF

#ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, model, 1,                           &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    lrflx)
#endif

      RETURN
      END SUBROUTINE ana_lrflux_tile
