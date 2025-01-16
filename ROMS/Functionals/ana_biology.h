      SUBROUTINE ana_biology (ng, tile, model)
!
!! svn $Id: ana_biology.h 875 2017-11-03 01:10:02Z arango $
!!======================================================================
!! Copyright (c) 2002-2018 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets initial conditions for biological tracer fields   !
!  using analytical expressions.                                       !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_ncparam
      USE mod_ocean
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
#ifdef REEF_ECOSYS
      USE mod_grid
      USE mod_biology
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
#ifdef BEST_NPZ
# if defined CLIM_ICE_1D
      USE mod_clima
# endif
# ifdef  ICE_BIO
      USE mod_ice
# endif
#endif

#if defined BIO_GOANPZ || defined BEST_NPZ
      USE mod_grid
      USE mod_biology
#endif
!
! Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model

#include "tile.h"
!
      CALL ana_biology_tile (ng, tile, model,                           &
     &                       LBi, UBi, LBj, UBj,                        &
     &                       IminS, ImaxS, JminS, JmaxS,                &
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
#ifdef REEF_ECOSYS
     &                       OCEAN(ng) % HisBio2d,                      &
     &                       OCEAN(ng) % HisBio3d,                      &
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# ifdef SEDIMENT_ECOSYS
     &                       OCEAN(ng) % HisBiosed3d,                   &
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add 
     &                       GRID(ng)  % z_r,                           &
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
#ifdef BEST_NPZ
     &                       GRID(ng) % z_r,                            &
     &                       GRID(ng) % h,                              &
# ifdef BENTHIC
     &                       OCEAN(ng) % bt,                            &
# endif
# ifdef ICE_BIO
     &                       OCEAN(ng) % it,                            &
     &                       OCEAN(ng) % itL,                           &
#  ifdef CLIM_ICE_1D
     &                       CLIMA(ng) % tclmG,                         &
     &                       CLIMA(ng) % tclm,                          &
#  else
     &                       ICE(ng) % IcePhL,                          &
     &                       ICE(ng) % IceNO3,                          &
     &                       ICE(ng) % IceNH4,                          &
     &                       ICE(ng) % IceLog,                          &
     &                       ICE(ng) % ti,                              &
     &                       ICE(ng) % hi,                              &
     &                       ICE(ng) % ai,                              &
     &                       ICE(ng) % ageice,                          &
#  endif
# endif
# ifdef STATIONARY
     &                       OCEAN(ng) % st,                            &
     &                       NTS(ng),                                   &
# endif
# ifdef STATIONARY2
     &                       OCEAN(ng) % st2,                           &
     &                       NTS2(ng),                                  &
# endif
# ifdef PROD3R
     &                       OCEAN(ng) % pt3,                           &
     &                       NPT3(ng),                                  &
# endif
# ifdef PROD2R
     &                       OCEAN(ng) % pt2,                           &
     &                       NPT2(ng),                                  &
# endif
# ifdef BIOFLUX
     &                       OCEAN(ng) % bflx,                          &
# endif
#endif
# ifdef BIO_GOANPZ
     &                       GRID(ng) % z_r,                            &
# endif
     &                       OCEAN(ng) % t)
!
! Set analytical header file name used.
!
#ifdef DISTRIBUTE
      IF (Lanafile) THEN
#else
      IF (Lanafile.and.(tile.eq.0)) THEN
#endif
        ANANAME( 1)=__FILE__
      END IF

      RETURN
      END SUBROUTINE ana_biology
!
!***********************************************************************
      SUBROUTINE ana_biology_tile (ng, tile, model,                     &
     &                             LBi, UBi, LBj, UBj,                  &
     &                             IminS, ImaxS, JminS, JmaxS,          &
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
#if defined REEF_ECOSYS
     &                             HisBio2d,                            &
     &                             HisBio3d,                            &
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# ifdef SEDIMENT_ECOSYS
     &                             HisBiosed3d,                         &
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add
     &                             z_r,                                 &
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
#ifdef BEST_NPZ
     &                             z_r, h,                              &
# ifdef BENTHIC
     &                             bt,                                  &
# endif
# ifdef ICE_BIO
     &                             it, itL,                             &
#  ifdef CLIM_ICE_1D
     &                             tclmG,                               &
     &                             tclm,                                &
#  else
     &                             IcePhL, IceNO3, IceNH4, IceLog,      &
     &                             ti, hi, ai,                          &
     &                             ageice ,                             &
#  endif
# endif
# ifdef STATIONARY
     &                             st,                                  &
     &                             UBst ,                               &
# endif
# ifdef STATIONARY2
     &                             st2,                                 &
     &                             UBst2 ,                              &
# endif
# ifdef PROD3R
     &                             pt3,                                 &
     &                             UBpt3 ,                              &
# endif
# ifdef PROD2R
     &                             pt2,                                 &
     &                             UBpt2,                               &
# endif
# ifdef BIOFLUX
     &                             bflx,                                &
# endif
#endif
# ifdef BIO_GOANPZ
     &                             z_r,                                 &
# endif
     &                             t)
!***********************************************************************
!
      USE mod_param
      USE mod_parallel
      USE mod_biology
      USE mod_ncparam
      USE mod_iounits
      USE mod_scalars
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
#if defined REEF_ECOSYS
      USE mod_geochem
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
!
      USE stats_mod, ONLY : stats_3dfld
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!
#ifdef ASSUMED_SHAPE
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
# if defined REEF_ECOSYS
      real(r8), intent(inout) :: HisBio2d(LBi:,LBj:,:)
      real(r8), intent(inout) :: HisBio3d(LBi:,LBj:,:,:)
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# ifdef SEDIMENT_ECOSYS
      real(r8), intent(inout) :: HisBiosed3d(LBi:,LBj:,:,:)
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add
      real(r8), intent(inout) :: z_r(LBi:,LBj:,:)
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
# if defined BEST_NPZ
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: h(LBi:,LBj:)
#  ifdef BENTHIC
      real(r8), intent(inout) :: bt(LBi:,LBj:,:,:,:)
#  endif
#  ifdef STATIONARY
      real(r8), intent(inout) :: st(LBi:,LBj:,:,:,:)
      integer, intent(in) :: UBst
#  endif
#  ifdef STATIONARY2
      real(r8), intent(inout) :: st2(LBi:,LBj:,:,:)
      integer, intent(in) :: UBst2
#  endif
#  ifdef PROD3R
      real(r8), intent(inout) :: pt3(LBi:,LBj:,:,:,:)
      integer, intent(in) :: UBpt3
#  endif
#  ifdef PROD2R
      real(r8), intent(inout) :: pt2(LBi:,LBj:,:,:)
      integer, intent(in) :: UBpt2
#  endif

#  ifdef BIOFLUX
      real(r8), intent(inout) :: bflx(:,:)
#  endif

#  ifdef ICE_BIO
      real(r8), intent(inout) :: it(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: itL(LBi:,LBj:,:,:)
#    ifdef CLIM_ICE_1D
      real(r8), intent(inout) ::tclmG(LBi:,LBj:,:,:,:)
      real(r8), intent(inout) ::tclm(LBi:,LBj:,:,:)
#    else
      real(r8), intent(inout) :: IcePhL(LBi:,LBj:,:)
      real(r8), intent(inout) :: IceNO3(LBi:,LBj:,:)
      real(r8), intent(inout) :: IceNH4(LBi:,LBj:,:)
      integer, intent(inout)  :: IceLog(LBi:,LBj:,:)
      real(r8), intent(in) :: ti(LBi:,LBj:,:)
      real(r8), intent(in) :: hi(LBi:,LBj:,:)
      real(r8), intent(in) :: ai(LBi:,LBj:,:)
      real(r8), intent(in) :: ageice(LBi:,LBj:,:)
#    endif
#  endif
# endif
      real(r8), intent(inout) :: t(LBi:,LBj:,:,:,:)
#else
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
# if defined REEF_ECOSYS
      real(r8), intent(inout) :: HisBio2d(LBi:UBi,LBj:UBj,NHbio2d)
      real(r8), intent(inout) :: HisBio3d(LBi:UBi,LBj:UBj,UBk,NHbio3d)
!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# ifdef SEDIMENT_ECOSYS
      real(r8), intent(inout) :: HisBiosed3d(LBi:UBi,LBj:UBj,Nsed,NHbiosed3d)
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add
      real(r8), intent(inout) :: z_r(LBi:UBi,LBj:UBj,N(ng))
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add
# if defined BEST_NPZ
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
#   ifdef BENTHIC
      real(r8), intent(inout) :: bt(LBi:UBi,LBj:UBj,NBL(ng),3,NBeT(ng))
#   endif
#  ifdef STATIONARY
      real(r8), intent(inout) :: st(LBi:UBi,LBj:UBj,UBk,3,UBst)
      integer, intent(in) :: UBst
#  endif
#   ifdef STATIONARY2
      real(r8), intent(inout) :: st2(LBi:UBi,LBj:UBj,3,UBst2)
      integer, intent(in) :: UBst2
#   endif
#   ifdef PROD3R
      real(r8), intent(inout) :: pt3(LBi:UBi,LBj:UBj,UBk,3,UBpt3)
      integer, intent(in) :: UBpt3
#   endif
#   ifdef PROD2R
      real(r8), intent(inout) :: pt2(LBi:UBi,LBj:UBj,3,UBpt2)
      integer, intent(in) :: UBpt2
#   endif

#   ifdef BIOFLUX
      real(r8), intent(inout) :: bflx(NT(ng),NT(ng))
#   endif

#  ifdef ICE_BIO
      real(r8), intent(inout) :: it(LBi:UBi,LBj:UBj,3,NIceT(ng))
      real(r8), intent(inout) :: itL(LBi:UBi,LBj:UBj,3,NIceLog(ng))
#    ifdef CLIM_ICE_1D
      real(r8), intent(inout) ::tclmG(LBi:UBi,LBj:UBj,UBk,3,UBt+1)
      real(r8), intent(inout) ::tclm(LBi:UBi,LBj:UBj,UBk,UBt+1)
#    else
      real(r8), intent(inout) :: IcePhL(LBi:UBi,LBj:UBJ,2)
      real(r8), intent(inout) :: IceNO3(LBi:UBi,LBj:UBJ,2)
      real(r8), intent(inout) :: IceNH4(LBi:UBi,LBj:UBJ,2)
      integer, intent(inout)  :: IceLog(LBi:UBi,LBj:UBJ,2)
      real(r8), intent(in) :: ti(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: hi(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: ai(LBi:UBi,LBj:UBj,2)
      real(r8), intent(in) :: ageice(LBi:UBi,LBj:UBj,2)
#   endif
#  endif
# endif

# if defined BIO_GOANPZ
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
# endif
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
#endif
!
!  Local variable declarations.
!
      logical, save :: first = .TRUE.

      integer :: i, is, itrc, j, k, m

!!! yuta_edits_for_masa >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
#  if defined SEDIMENT_ECOSYS
      integer :: k_sed
#  endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
#if defined REEF_ECOSYS
      real(r8) :: TmpK, Salt, sspH, cCO3, cCO2aq, ssfCO2, ssCO2flux
      real(r8) :: TIC, TAlk, Oxyg, DOsatu, ssO2flux
#endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add

#if defined BIO_FENNEL || defined NEMURO || defined BIO_UMAINE
      real(r8) :: SiO4, cff1, cff2, temp
#elif defined ECOSIM
      real(r8) :: cff1, cff2, cff3, cff4, cff5, cff6, cff7, cff8, cff9
      real(r8) :: cff10, cff11, cff12, cff13, cff14, cff15
      real(r8) :: salt, sftm, temp
#elif defined BIO_GOANPZ || defined BEST_NPZ
      real(r8) :: var1, var2, var3, var4, var5, var6, var7
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,N(ng)) :: biod
      real(r8), dimension(NT(ng)) :: deepval
      real(r8), dimension(NT(ng)) :: loval
# ifdef IRON_LIMIT
      real(r8) :: FeSurf, FeDeep
# endif
      real(r8), parameter :: eps = 1.0E-20_r8
#endif
!
!   Maximum 80 biological tracers consider for field statistics.
!
      TYPE (T_STATS), save :: Stats(80)

#include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Initialize field statistics structure.
!-----------------------------------------------------------------------
!
      IF (first) THEN
        first=.FALSE.
        DO i=1,SIZE(Stats,1)
          Stats(i) % count=0.0_r8
          Stats(i) % min=Large
          Stats(i) % max=-Large
          Stats(i) % avg=0.0_r8
          Stats(i) % rms=0.0_r8
        END DO
      END IF

#if defined BIO_FENNEL
!
!-----------------------------------------------------------------------
!  Fennel et al. (2006), nitrogen-based biology model.
!-----------------------------------------------------------------------
!
      cff1=20.0_r8/3.0_r8
      cff2= 2.0_r8/3.0_r8
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            temp=t(i,j,k,1,itemp)
            IF (temp.lt.8.0_r8) THEN
              SiO4=30.0_r8
            ELSE IF ((temp.ge.8.0_r8).and.(temp.le.11.0_r8)) THEN
              SiO4=30.0_r8-((temp-8.0_r8)*cff1)
            ELSE IF ((temp.gt.11.0_r8).and.(temp.le.13.0_r8)) THEN
              SiO4=10.0_r8-((temp-11.0_r8)*4.0_r8)
            ELSE IF ((temp.gt.13.0_r8).and.(temp.le.16.0_r8)) THEN
              SiO4=2.0_r8-((temp-13.0_r8)*cff2)
            ELSE IF (temp.gt.16.0_r8) THEN
              SiO4=0.0_r8
            END IF
            t(i,j,k,1,iNO3_)=1.67_r8+0.5873_r8*SiO4+                    &
     &                               0.0144_r8*SiO4**2+                 &
     &                               0.0003099_r8*SiO4**3
            t(i,j,k,1,iPhyt)=0.08_r8
            t(i,j,k,1,iZoop)=0.06_r8
            t(i,j,k,1,iNH4_)=0.1_r8
            t(i,j,k,1,iLDeN)=0.02_r8
            t(i,j,k,1,iSDeN)=0.04_r8
            t(i,j,k,1,iChlo)=0.02_r8
            t(i,j,k,1,iOxyg)=0.00_r8
#ifdef CARBON
            t(i,j,k,1,iTIC_)=2100.0_r8
            t(i,j,k,1,iTAlk)=2350.0_r8
            t(i,j,k,1,iLDeC)=0.002_r8
            t(i,j,k,1,iSDeC)=0.06_r8
#endif
#ifdef OXYGEN
            t(i,j,k,1,iOxyg)=10.0_r8/0.02241_r8
#endif
          END DO
        END DO
      END DO

#elif defined BIO_UMAINE
!
!-----------------------------------------------------------------------
!  UMaine Carbon, Silicate, Nitrogen Ecosystem (CoSiNE) Model
!-----------------------------------------------------------------------
!
      cff1=20.0_r8/3.0_r8
      cff2= 2.0_r8/3.0_r8
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            temp=t(i,j,k,1,itemp)
            IF (temp.lt.8.0_r8) THEN
              SiO4=30.0_r8
            ELSE IF ((temp.ge.8.0_r8).and.(temp.le.11.0_r8)) THEN
              SiO4=30.0_r8-((temp-8.0_r8)*cff1)
            ELSE IF ((temp.gt.11.0_r8).and.(temp.le.13.0_r8)) THEN
              SiO4=10.0_r8-((temp-11.0_r8)*4.0_r8)
            ELSE IF ((temp.gt.13.0_r8).and.(temp.le.16.0_r8)) THEN
              SiO4=2.0_r8-((temp-13.0_r8)*cff2)
            ELSE IF (temp.gt.16.0_r8) THEN
              SiO4=0.0_r8
            END IF
            t(i,j,k,1,iNO3_)=1.67_r8+0.5873_r8*SiO4+                    &
     &                               0.0144_r8*SiO4**2+                 &
     &                               0.0003099_r8*SiO4**3
            t(i,j,k,1,iSiOH)=1.50_r8*t(i,j,k,1,iNO3_)
            t(i,j,k,1,iSphy)=0.08_r8
            t(i,j,k,1,iLphy)=0.08_r8
            t(i,j,k,1,iSzoo)=0.06_r8
            t(i,j,k,1,iLzoo)=0.06_r8
            t(i,j,k,1,iNH4_)=0.1_r8
            t(i,j,k,1,iSDet)=0.02_r8
            t(i,j,k,1,iopal)=0.04_r8
            t(i,j,k,1,iPO4_)=t(i,j,k,1,iNO3_)/16.0_r8
#ifdef OXYGEN
            t(i,j,k,1,iOxyg)=10.0_r8/0.02241_r8
#endif
#ifdef CARBON
            t(i,j,k,1,iTIC_)=2100.0_r8
            t(i,j,k,1,iTAlk)=2350.0_r8
#endif
          END DO
        END DO
      END DO

#elif defined NEMURO
!
!-----------------------------------------------------------------------
!  Nemuro lower trophic level ecosystem model.
!-----------------------------------------------------------------------
!
      cff1=20.0_r8/3.0_r8
      cff2= 2.0_r8/3.0_r8
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
#ifdef CGOA_ANABIO
            t(i,j,k,1,iNO3_)=15.0_r8
            t(i,j,k,1,iSphy)=0.1_r8
            t(i,j,k,1,iLphy)=0.1_r8
            t(i,j,k,1,iSzoo)=0.1_r8
            t(i,j,k,1,iLzoo)=0.1_r8
            t(i,j,k,1,iPzoo)=0.1_r8
            t(i,j,k,1,iNH4_)=0.0_r8
            t(i,j,k,1,iPON_)=0.0_r8
            t(i,j,k,1,iDON_)=0.0_r8
            t(i,j,k,1,iSiOH)=25.0_r8
            t(i,j,k,1,iopal)=0.0_r8
# ifdef IRON_LIMIT
            t(i,j,k,1,iFeD_)=0.2_r8
            t(i,j,k,1,iFeSp)=0.0_r8
            t(i,j,k,1,iFeLp)=0.0_r8
# endif
#else
            temp=t(i,j,k,1,itemp)
            IF (temp.lt.8.0_r8) THEN
              SiO4=30.0_r8
            ELSE IF ((temp.ge.8.0_r8).and.(temp.le.11.0_r8)) THEN
              SiO4=30.0_r8-((temp-8.0_r8)*cff1)
            ELSE IF ((temp.gt.11.0_r8).and.(temp.le.13.0_r8)) THEN
              SiO4=10.0_r8-((temp-11.0_r8)*4.0_r8)
            ELSE IF ((temp.gt.13.0_r8).and.(temp.le.16.0_r8)) THEN
              SiO4=2.0_r8-((temp-13.0_r8)*cff2)
            ELSE IF (temp.gt.16.0_r8) THEN
              SiO4=0.0_r8
            END IF
            t(i,j,k,1,iNO3_)=1.67_r8+0.5873_r8*SiO4+                    &
     &                               0.0144_r8*SiO4**2+                 &
     &                               0.0003099_r8*SiO4**3
            t(i,j,k,1,iSphy)=0.06_r8
            t(i,j,k,1,iLphy)=0.06_r8
            t(i,j,k,1,iSzoo)=0.05_r8
            t(i,j,k,1,iLzoo)=0.05_r8
            t(i,j,k,1,iPzoo)=0.05_r8
            t(i,j,k,1,iNH4_)=0.1_r8
            t(i,j,k,1,iPON_)=0.001_r8
            t(i,j,k,1,iDON_)=0.001_r8
            t(i,j,k,1,iSiOH)=SiO4
            t(i,j,k,1,iopal)=0.001_r8
# ifdef IRON_LIMIT
            t(i,j,k,1,iFeD_)=2.0_r8
            t(i,j,k,1,iFeSp)=0.001_r8
            t(i,j,k,1,iFeLp)=0.001_r8
# endif
#endif
          END DO
        END DO
      END DO

#elif defined NPZD_FRANKS || defined NPZD_POWELL
!
!-----------------------------------------------------------------------
!  NPZD biology model.
!-----------------------------------------------------------------------
!
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,iNO3_)=BioIni(iNO3_,ng)
            t(i,j,k,1,iPhyt)=BioIni(iPhyt,ng)
            t(i,j,k,1,iZoop)=BioIni(iZoop,ng)
            t(i,j,k,1,iSDet)=BioIni(iSDet,ng)
          END DO
        END DO
      END DO

#elif defined NPZD_IRON
!
!-----------------------------------------------------------------------
!  NPZD biology model with or without iron limitation on phytoplankton
!  growth.
!-----------------------------------------------------------------------
!
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
            t(i,j,k,1,iNO3_)=BioIni(iNO3_,ng)
            t(i,j,k,1,iPhyt)=BioIni(iPhyt,ng)
            t(i,j,k,1,iZoop)=BioIni(iZoop,ng)
            t(i,j,k,1,iSDet)=BioIni(iSDet,ng)
# ifdef IRON_LIMIT
            t(i,j,k,1,iFphy)=BioIni(iFphy,ng)
            t(i,j,k,1,iFdis)=BioIni(iFdis,ng)
# endif
          END DO
        END DO
      END DO

#elif defined ECOSIM
!
!---------------------------------------------------------------------
!  EcoSim initial fields.
!---------------------------------------------------------------------
!
! Assumed maximum temperature gradient.
!
      cff3=1.0_r8/14.0_r8
      cff4=1.0_r8/16.0_r8
      cff5=32.0_r8
      cff7=1.0_r8/0.0157_r8
      cff8=1.0_r8/6.625_r8
      cff9=1.0_r8/16.0_r8
      cff10=1.0_r8/15.0_r8
      cff11=1.0_r8/8.0_r8
      cff12=1.0_r8/128.0_r8
      cff13=1.0_r8/1000.0_r8
      cff14=1.0_r8/12.0_r8
      cff15=cff5*cff8*cff14                  ! mole N : gram Chl

      DO k=N(ng),1,-1
        DO j=JstrT,JendT
          DO i=IstrT,IendT
!
! Initialization of surface chlorophyll.
!
            sftm=t(i,j,N(ng),1,itemp)
            temp=t(i,j,k,1,itemp)
            salt=t(i,j,k,1,isalt)
            cff1=-0.0827_r8*sftm+2.6386_r8
            cff2=MAX(0.00001_r8,cff1*(1.0_r8-(sftm-temp)*cff3))
!
! Initialization of nutrients.
!
            t(i,j,k,1,iNH4_)=0.053_r8*temp+0.7990_r8
            t(i,j,k,1,iNO3_)=8.5_r8-cff2*cff15-t(i,j,k,1,iNH4_)
            t(i,j,k,1,iPO4_)=(t(i,j,k,1,iNH4_)+t(i,j,k,1,iNO3_))*cff4
            t(i,j,k,1,iFeO_)=1.0_r8
!
! Assuming diatoms are 75% of initialized chlorophyll.
!
            t(i,j,k,1,iSiO_)=5.5_r8-(cff2*0.75_r8)*cff15*1.20_r8
            t(i,j,k,1,iDIC_)=2000.0_r8
!
! Bacteria Initialization.
!
            DO is=1,Nbac
              t(i,j,k,1,iBacC(is))=0.85_r8
              t(i,j,k,1,iBacN(is))=t(i,j,k,1,iBacC(is))*N2cBAC(ng)
              t(i,j,k,1,iBacP(is))=t(i,j,k,1,iBacC(is))*P2cBAC(ng)
              t(i,j,k,1,iBacF(is))=t(i,j,k,1,iBacC(is))*Fe2cBAC(ng)
            END DO
!
! Initialize phytoplankton populations.
!
            t(i,j,k,1,iPhyC(1))=MAX(0.02_r8,                            &
     &                              0.75_r8*0.75_r8*cff5*cff2*cff14)
            t(i,j,k,1,iPhyC(2))=MAX(0.02_r8,                            &
     &                              0.75_r8*0.25_r8*cff5*cff2*cff14)
            t(i,j,k,1,iPhyC(3))=MAX(0.02_r8,                            &
     &                              0.125_r8*cff5*cff2*cff14)
            t(i,j,k,1,iPhyC(4))=t(i,j,k,1,iPhyC(3))
            DO is=1,Nphy
              t(i,j,k,1,iPhyN(is))=t(i,j,k,1,iPhyC(is))*cff8
              t(i,j,k,1,iPhyP(is))=t(i,j,k,1,iPhyN(is))*cff4
              t(i,j,k,1,iPhyF(is))=t(i,j,k,1,iPhyC(is))*cff13
              IF (iPhyS(is).gt.0) THEN
                t(i,j,k,1,iPhyS(is))=t(i,j,k,1,iPhyN(is))*1.20_r8
              END IF
!
!  Initialize Pigments in ugrams/liter (not umole/liter).
!  Chlorophyll-a
!
              cff6=12.0_r8/cff5
              t(i,j,k,1,iPigs(is,1))=cff6*t(i,j,k,1,iPhyC(is))
!
!  Chlorophyll-b.
!
              cff6=cff5-b_C2Cl(is,ng)
              IF (iPigs(is,2).gt.0) THEN
                 t(i,j,k,1,iPigs(is,2))=t(i,j,k,1,iPigs(is,1))*         &
     &                                  (b_ChlB(is,ng)+                 &
     &                                   mxChlB(is,ng)*cff6)
              END IF
!
!  Chlorophyll-c.
!
              IF (iPigs(is,3).gt.0) THEN
                 t(i,j,k,1,iPigs(is,3))=t(i,j,k,1,iPigs(is,1))*         &
     &                                  (b_ChlC(is,ng)+                 &
     &                                   mxChlC(is,ng)*cff6)
              END IF
!
!  Photosynthetic Carotenoids.
!
              IF (iPigs(is,4).gt.0) THEN
                 t(i,j,k,1,iPigs(is,4))=t(i,j,k,1,iPigs(is,1))*         &
     &                                  (b_PSC(is,ng)+                  &
     &                                   mxPSC(is,ng)*cff6)
              END IF
!
!  Photoprotective Carotenoids.
!
              IF (iPigs(is,5).gt.0) THEN
                 t(i,j,k,1,iPigs(is,5))=t(i,j,k,1,iPigs(is,1))*         &
     &                                  (b_PPC(is,ng)+                  &
     &                                   mxPPC(is,ng)*cff6)
              END IF
!
!  Low Urobilin Phycoeurythin Carotenoids.
!
              IF (iPigs(is,6).gt.0) THEN
                 t(i,j,k,1,iPigs(is,6))=t(i,j,k,1,iPigs(is,1))*         &
     &                                  (b_LPUb(is,ng)+                 &
     &                                   mxLPUb(is,ng)*cff6)
              END IF
!
!  High Urobilin Phycoeurythin Carotenoids.
!
              IF (iPigs(is,7).gt.0) THEN
                 t(i,j,k,1,iPigs(is,7))=t(i,j,k,1,iPigs(is,1))*         &
     &                                  (b_HPUb(is,ng)+                 &
     &                                   mxHPUb(is,ng)*cff6)
              END IF
            END DO
!
! DOC initialization.
!
            cff6=MAX(0.001_r8,-0.9833_r8*salt+33.411_r8)
            t(i,j,k,1,iDOMC(1))=0.1_r8
            t(i,j,k,1,iDOMN(1))=t(i,j,k,1,iDOMC(1))*cff8
            t(i,j,k,1,iDOMP(1))=t(i,j,k,1,iDOMN(1))*cff9
            t(i,j,k,1,iCDMC(1))=t(i,j,k,1,iDOMC(1))*cDOCfrac_c(1,ng)
            t(i,j,k,1,iDOMC(2))=15.254_r8*cff6+70.0_r8
            t(i,j,k,1,iDOMN(2))=t(i,j,k,1,iDOMC(2))*cff10
            t(i,j,k,1,iDOMP(2))=0.0_r8
            t(i,j,k,1,iCDMC(2))=(0.243_r8*cff6+0.055_r8)*cff7
!
! Fecal Initialization.
!
            DO is=1,Nfec
              t(i,j,k,1,iFecC(is))=0.002_r8
              t(i,j,k,1,iFecN(is))=t(i,j,k,1,iFecC(is))*cff11
              t(i,j,k,1,iFecP(is))=t(i,j,k,1,iFecC(is))*cff12
              t(i,j,k,1,iFecF(is))=t(i,j,k,1,iFecC(is))*cff13
              t(i,j,k,1,iFecS(is))=t(i,j,k,1,iFecC(is))*cff11
            END DO
          END DO
        END DO
      END DO
!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TN:Add
#elif defined REEF_ECOSYS
!
!-----------------------------------------------------------------------
!  Coral reef ecosystem model.
!-----------------------------------------------------------------------
!
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
          ! Initialize all tracer values to be zero
            t(i,j,k,1,iDIC (1)  :iDIC (N_Csp)     ) = 0.0_r8
            t(i,j,k,1,iNO3 (1)  :iNO3 (N_Nsp)     ) = 0.0_r8
            t(i,j,k,1,iNH4 (1)  :iNH4 (N_Nsp)     ) = 0.0_r8
            t(i,j,k,1,iPO4 (1)  :iPO4 (N_Psp)     ) = 0.0_r8
            t(i,j,k,1,iDOC (1,1):iDOC (N_Csp,Ndom)) = 0.0_r8     ! umolC L-1
            t(i,j,k,1,iPOC (1,1):iPOC (N_Csp,Npom)) = 0.0_r8     ! umolC L-1
            t(i,j,k,1,iDON (1,1):iDON (N_Nsp,Ndom)) = 0.0_r8     ! umolN L-1
            t(i,j,k,1,iPON (1,1):iPON (N_Nsp,Npom)) = 0.0_r8     ! umolN L-1
            t(i,j,k,1,iDOP (1,1):iDOP (N_Psp,Ndom)) = 0.0_r8     ! umolP L-1
            t(i,j,k,1,iPOP (1,1):iPOP (N_Psp,Npom)) = 0.0_r8     ! umolP L-1
            t(i,j,k,1,iPhyC(1,1):iPhyC(N_Csp,Nphy)) = 0.0_r8     ! umolC L-1
            t(i,j,k,1,iZooC(1,1):iZooC(N_Csp,Nzoo)) = 0.0_r8     ! umolC L-1
            t(i,j,k,1,iPhyN(1,1):iPhyN(N_Nsp,Nphy)) = 0.0_r8     ! umolN L-1
            t(i,j,k,1,iZooN(1,1):iZooN(N_Nsp,Nzoo)) = 0.0_r8     ! umolN L-1
            t(i,j,k,1,iPhyP(1,1):iPhyP(N_Psp,Nphy)) = 0.0_r8     ! umolP L-1
            t(i,j,k,1,iZooP(1,1):iZooP(N_Psp,Nzoo)) = 0.0_r8     ! umolP L-1
            t(i,j,k,1,iPIC (1,1):iPIC (N_Csp,Npim)) = 0.0_r8     ! umolC L-1

          ! TA  
            t(i,j,k,1,iTA)                              &
     &       = TA_Profile ( TA_0(ng),  t(i,j,k,1,iTemp))
          ! DIC  
            t(i,j,k,1,iDIC(iCt))                        &
     &       = DIC_Profile( DIC_0(ng), t(i,j,k,1,iTemp))
          END DO
        END DO
      END DO
      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
          ! DO  
            t(i,j,k,1,iDO) =                            &
     &        DO_Profile2( t(i,j,N(ng),1,iTemp)         &
     &                   , t(i,j,N(ng),1,iSalt)         &
     &                   , t(i,j,N(ng),1,iDIC(iCt))     &
     &                   , t(i,j,k    ,1,iDIC(iCt))     &
     &                   , t(i,j,N(ng),1,iTA)           &
     &                   , t(i,j,k    ,1,iTA)     )
          END DO
        END DO
      END DO

      DO k=1,N(ng)
        DO j=JstrT,JendT
          DO i=IstrT,IendT
          ! NO3  
            t(i,j,k,1,iNO3(iNt)) = NO3_Profile3( NO3_0(ng) &
     &                   , t(i,j,N(ng),1,iDO)              &
     &                   , t(i,j,k    ,1,iDO) )
          ! NH4   
            t(i,j,k,1,iNH4(iNt))=NH4_0(ng)     ! umol L-1
          ! PO4
            t(i,j,k,1,iPO4(iPt)) = PO4_Profile3( PO4_0(ng) &
     &                   , t(i,j,N(ng),1,iDO)              &
     &                   , t(i,j,k    ,1,iDO)  )
          ! DOC
            DO m=1,Ndom
              t(i,j,k,1,iDOC(iCt,m)) = DOC_0(m,ng)     ! umolC L-1
            END DO
          ! POC
            DO m=1,Npom
              t(i,j,k,1,iPOC(iCt,m)) = POC_0(m,ng)     ! umolC L-1
            END DO
          ! DON
            DO m=1,Ndom
              t(i,j,k,1,iDON(iNt,m)) = DON_0(m,ng)     ! umolC L-1
            END DO
          ! PON
            DO m=1,Npom
              t(i,j,k,1,iPON(iNt,m)) = PON_0(m,ng)     ! umolC L-1
            END DO
          ! DOP
            DO m=1,Ndom
              t(i,j,k,1,iDOP(iPt,m)) = DOP_0(m,ng)     ! umolC L-1
            END DO
          ! POP
            DO m=1,Npom
              t(i,j,k,1,iPOP(iPt,m)) = POP_0(m,ng)     ! umolC L-1
            END DO
          ! PhyC        
            DO m=1,Nphy
              t(i,j,k,1,iPhyC(iCt,m)) &
     &          = PHY_Profile2( m, PhyC_0(m,ng), z_r(i,j,k) )
            END DO
          ! ZooC
            DO m=1,Nzoo
              t(i,j,k,1,iZooC(iCt,m)) &
     &          = ZOO_Profile2( m, ZooC_0(m,ng), z_r(i,j,k) )
            END DO
            ! PhyN        
            DO m=1,Nphy
              t(i,j,k,1,iPhyN(iNt,m)) &
     &          = t(i,j,k,1,iPhyC(iCt,m))*rNCp(m)
            END DO
          ! ZooN
            DO m=1,Nzoo
              t(i,j,k,1,iZooN(iNt,m)) &
     &          = t(i,j,k,1,iZooC(iCt,m))*rNCz(m)
            END DO
          ! PhyP      
            DO m=1,Nphy
              t(i,j,k,1,iPhyP(iPt,m)) &
     &          = t(i,j,k,1,iPhyC(iCt,m))*rPCp(m)
            END DO
          ! ZooP
            DO m=1,Nzoo
              t(i,j,k,1,iZooP(iPt,m)) &
     &          = t(i,j,k,1,iZooC(iCt,m))*rPCz(m)
            END DO
          ! PIC
            t(i,j,k,1,iPIC(iCt,iLive)) &
     &         = t(i,j,k,1,iPhyC(iCt,iCcl))*rCaCp(iCcl) ! PIC_0(iLive,ng)     ! umolC L-1
            DO m=2,Npim
              t(i,j,k,1,iPIC(iCt,m)) = PIC_0(m,ng)     ! umolC L-1
            END DO

# if defined CARBON_ISOTOPE || defined CLUMPED_ISOTOPE
            t(i,j,k,1,iDIC(iC13))                                              &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iDIC(iCt)),   d13C_DIC_0(ng), R13C_VPDB )                    &
            DO m=1,Ndom
              t(i,j,k,1,iDOC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iDOC(iCt,m)), d13C_DOC_0(m,ng), R13C_VPDB )                    &
            END DO
            DO m=1,Npom
              t(i,j,k,1,iPOC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iPOC(iCt,m)), d13C_POC_0(m,ng), R13C_VPDB )                    &
            END DO
            DO m=1,Nphy
              t(i,j,k,1,iPhyC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iPhyC(iCt,m)), d13C_PhyC_0(m,ng), R13C_VPDB )                    &
            END DO
            DO m=1,Nzoo
              t(i,j,k,1,iZooC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iZooC(iCt,m)), d13C_ZooC_0(m,ng), R13C_VPDB )                    &
            END DO
            DO m=1,Npim
              t(i,j,k,1,iPIC(iC13,m))                                          &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iPIC(iCt,m)), d13C_PIC_0(m,ng), R13C_VPDB )                    &
            END DO
#  if defined CLUMPED_ISOTOPE
!**************** Under developpment *************************
            t(i,j,k,1,iDIC(iD47))                                              &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iDIC(iCt)),   D47_DIC_0(ng), R47D_???? )                    &
            DO m=1,Ndom
              t(i,j,k,1,iDOC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iDOC(iCt,m)), D47_DOC_0(m,ng), R47D_???? )                    &
            END DO
            DO m=1,Npom
              t(i,j,k,1,iPOC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iPOC(iCt,m)), D47_POC_0(m,ng), R47D_???? )                    &
            END DO
            DO m=1,Nphy
              t(i,j,k,1,iPhyC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iPhyC(iCt,m)), D47_PhyC_0(m,ng), R47D_???? )                    &
            END DO
            DO m=1,Nzoo
              t(i,j,k,1,iZooC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iZooC(iCt,m)), D47_ZooC_0(m,ng), R47D_???? )                    &
            END DO
            DO m=1,Npim
              t(i,j,k,1,iPIC(iD47,m))                                          &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iPIC(iCt,m)), D47_PIC_0(m,ng), R47D_???? )                    &
            END DO
#  endif
# endif
# if defined NITROGEN_ISOTOPE
            t(i,j,k,1,iNO3(iN15))                                              &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iNO3(iNt)),   d15N_NO3_0(ng), R15N_AIR )                    &
            t(i,j,k,1,iNH4(iN15))                                              &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iNH4(iNt)),   d15N_NH4_0(ng), R15N_AIR )                    &
            DO m=1,Ndom
              t(i,j,k,1,iDON(iN15,m))                                          &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iDON(iNt,m)), d15N_DON_0(m,ng), R15N_AIR )                    &
            END DO
            DO m=1,Npom
              t(i,j,k,1,iPON(iN15,m))                                          &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iPON(iNt,m)), d15N_PON_0(m,ng), R15N_AIR )                    &
            END DO
            DO m=1,Nphy
              t(i,j,k,1,iPhyN(iN15,m))                                          &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iPhyN(iNt,m)), d15N_PhyN_0(m,ng), R15N_AIR )                    &
            END DO
            DO m=1,Nzoo
              t(i,j,k,1,iZooN(iN15,m))                                          &
     &        = Ci_from_Ct_delta(t(i,j,k,1,iZooN(iNt,m)), d15N_ZooN_0(m,ng), R15N_AIR )                    &
            END DO
# endif
# if defined COT_STARFISH
            t(i,j,k,1,iCOTe)=COTe0(ng)     ! umolC L-1
            t(i,j,k,1,iCOTl)=COTl0(ng)     ! umolC L-1
# endif
  
          END DO
        END DO
      END DO
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<TN:Add

#elif defined BEST_NPZ
# include "ana_biology_BESTnpz.h"
#elif defined BIO_GOANPZ
# include "ana_biology_goanpz.h"
#endif
!
!  Report statistics.
!
      DO itrc=1,NBT
        i=idbio(itrc)
        CALL stats_3dfld (ng, tile, iNLM, u3dvar, Stats(itrc),          &
     &                    LBi, UBi, LBj, UBj, 1, N(ng), t(:,:,:,1,i))
        IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
          WRITE (stdout,10) TRIM(Vname(2,idTvar(i)))//': '//            &
     &                      TRIM(Vname(1,idTvar(i))),                   &
     &                      ng, Stats(itrc)%min, Stats(itrc)%max
        END IF
      END DO
!
  10  FORMAT (3x,' ANA_BIOLOGY - ',a,/,19x,                             &
     &        '(Grid = ',i2.2,', Min = ',1p,e15.8,0p,                   &
     &                         ' Max = ',1p,e15.8,0p,')')

      RETURN
      END SUBROUTINE ana_biology_tile
