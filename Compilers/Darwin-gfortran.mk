# svn $Id: Darwin-gfortran.mk 834 2017-01-25 18:49:17Z arango $
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Copyright (c) 2002-2018 The ROMS/TOMS Group                           :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Include file for GNU Fortran compiler on Linux
# -------------------------------------------------------------------------
#
# ARPACK_LIBDIR  ARPACK libary directory
# FC             Name of the fortran compiler to use
# FFLAGS         Flags to the fortran compiler
# CPP            Name of the C-preprocessor
# CPPFLAGS       Flags to the C-preprocessor
# CC             Name of the C compiler
# CFLAGS         Flags to the C compiler
# CXX            Name of the C++ compiler
# CXXFLAGS       Flags to the C++ compiler
# CLEAN          Name of cleaning executable after C-preprocessing
# NETCDF_INCDIR  NetCDF include directory
# NETCDF_LIBDIR  NetCDF libary directory
# LD             Program to load the objects into an executable
# LDFLAGS        Flags to the loader
# RANLIB         Name of ranlib command
# MDEPFLAGS      Flags for sfmakedepend  (-s if you keep .f files)
#
# First the defaults
#
               FC := gfortran
           FFLAGS := -frepack-arrays -w -fallow-argument-mismatch
              CPP := /usr/bin/cpp
         CPPFLAGS := -P -traditional
               CC := gcc
              CXX := g++
           CFLAGS :=
         CXXFLAGS :=
          LDFLAGS :=
               AR := ar
          ARFLAGS := -r
            MKDIR := mkdir -p
               RM := rm -f
           RANLIB := ranlib
             PERL := perl
             TEST := test

        MDEPFLAGS := --cpp --fext=f90 --file=- --objdir=$(SCRATCH_DIR)

#
# Library locations, can be overridden by environment variables.
#

ifdef USE_NETCDF4
        NF_CONFIG ?= nf-config
    NETCDF_INCDIR ?= $(shell $(NF_CONFIG) --prefix)/include
             LIBS := $(shell $(NF_CONFIG) --flibs)
else
    NETCDF_INCDIR ?= /usr/local/include
    NETCDF_LIBDIR ?= /usr/local/lib
             LIBS := -L$(NETCDF_LIBDIR) -lnetcdf -lnetcdff
endif

ifdef USE_ARPACK
 ifdef USE_MPI
   PARPACK_LIBDIR ?= /usr/local/lib
             LIBS += -L$(PARPACK_LIBDIR) -lparpack
 endif
    ARPACK_LIBDIR ?= /usr/local/lib
             LIBS += -L$(ARPACK_LIBDIR) -larpack
endif

ifdef USE_MPI
         CPPFLAGS += -DMPI
 ifdef USE_MPIF90
               FC := mpif90
 else
             LIBS += -lfmpi -lmpi
 endif
endif

ifdef USE_OpenMP
         CPPFLAGS += -D_OPENMP
           FFLAGS += -fopenmp
endif

ifdef USE_DEBUG
           FFLAGS += -g -fbounds-check
           CFLAGS += -g
         CXXFLAGS += -g
else
           FFLAGS += -O3
#          FFLAGS += -O3 -ffast-math
           CFLAGS += -O3
         CXXFLAGS += -O3
endif

ifdef USE_MPI
           FFLAGS += -I/usr/include
endif

ifdef USE_ESMF
          ESMF_OS ?= $(OS)
      ESMF_SUBDIR := $(ESMF_OS).$(ESMF_COMPILER).$(ESMF_ABI).$(ESMF_COMM).$(ESMF_SITE)
      ESMF_MK_DIR ?= $(ESMF_DIR)/lib/lib$(ESMF_BOPT)/$(ESMF_SUBDIR)
                     include $(ESMF_MK_DIR)/esmf.mk
           FFLAGS += $(ESMF_F90COMPILEPATHS)
             LIBS += $(ESMF_F90LINKPATHS) $(ESMF_F90ESMFLINKLIBS)
endif

ifdef USE_CXX
             LIBS += -lstdc++
endif

#
# Use full path of compiler.
#
               FC := $(shell which ${FC})
               LD := $(FC)

#
# Turn off bounds checking for function def_var, as "dimension(*)"
# declarations confuse Gnu Fortran 95 bounds-checking code.
#

$(SCRATCH_DIR)/def_var.o: FFLAGS += -fno-bounds-check

#
# Allow integer overflow in ran_state.F.  This is not allowed
# during -O3 optimization. This option should be applied only for
# Gfortran versions >= 4.2.
#

FC_TEST := $(findstring $(shell ${FC} --version | head -1 | cut -d " " -f 4 | \
                              cut -d "." -f 1-2),4.0 4.1)

ifeq "${FC_TEST}" ""
$(SCRATCH_DIR)/ran_state.o: FFLAGS += -fno-strict-overflow
endif

#
# Set free form format in source files to allow long string for
# local directory and compilation flags inside the code.
#

$(SCRATCH_DIR)/mod_ncparam.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/mod_strings.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/analytical.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/biology.o: FFLAGS += -ffree-form -ffree-line-length-none
ifdef USE_ADJOINT
$(SCRATCH_DIR)/ad_biology.o: FFLAGS += -ffree-form -ffree-line-length-none
endif
ifdef USE_REPRESENTER
$(SCRATCH_DIR)/rp_biology.o: FFLAGS += -ffree-form -ffree-line-length-none
endif
ifdef USE_TANGENT
$(SCRATCH_DIR)/tl_biology.o: FFLAGS += -ffree-form -ffree-line-length-none
endif

ifdef USE_WRF
             FFLAGS += -I$(WRF_DIR)/main -I$(WRF_DIR)/external/esmf_time_f90 -I$(WRF_DIR)/frame -I$(WRF_DIR)/share
             LIBS += WRF/main/module_wrf_top.o
             LIBS += WRF/main/libwrflib.a
             LIBS += WRF/external/fftpack/fftpack5/libfftpack.a
             LIBS += WRF/external/io_grib1/libio_grib1.a
             LIBS += WRF/external/io_grib_share/libio_grib_share.a
             LIBS += WRF/external/io_int/libwrfio_int.a
             LIBS += WRF/external/esmf_time_f90/libesmf_time.a
             LIBS += WRF/external/RSL_LITE/librsl_lite.a
             LIBS += WRF/frame/module_internal_header_util.o
             LIBS += WRF/frame/pack_utils.o
             LIBS += WRF/external/io_netcdf/libwrfio_nf.a
endif

ifdef USE_WW3
             FFLAGS += -I${COAWST_WW3_DIR}/mod_DIST/
             LIBS += WW3/obj/libWW3.a
endif

ifdef USE_MCT
       MCT_INCDIR ?= /usr/local/mct/include
       MCT_LIBDIR ?= /usr/local/mct/lib
           FFLAGS += -I$(MCT_INCDIR)
             LIBS += -L$(MCT_LIBDIR) -lmct -lmpeu
endif

#
# Supress free format in SWAN source files since there are comments
# beyond column 72.
#

ifdef USE_SWAN
$(SCRATCH_DIR)/ocpcre.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/ocpids.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/ocpmix.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swancom1.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swancom2.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swancom3.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swancom4.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swancom5.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swanmain.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swanout1.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swanout2.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swanparll.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swanpre1.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swanpre2.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swanser.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swmod1.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swmod2.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/SwanSpectPart.o:  FFLAGS += -ffixed-form
$(SCRATCH_DIR)/m_constants.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/m_fileio.o:    FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/mod_xnl4v5.o:  FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/serv_xnl4v5.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/nctablemd.o:   FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/agioncmd.o:    FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/swn_outnc.o:   FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SdsBabanin.o:  FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanBpntlist.o:  FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanCheckGrid.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanCompdata.o:  FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanCompUnstruc.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanComputeForce.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanConvAccur.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanConvStopc.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanCreateEdges.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanCrossObstacle.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanDiffPar.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanDispParm.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanFindObstacles.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanFindPoint.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanGridCell.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanGriddata.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanGridFace.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanGridobjects.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanGridTopology.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanGridVert.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanGSECorr.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanInitCompGrid.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanInterpolateAc.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanInterpolateOutput.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanInterpolatePoint.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanIntgratSpc.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanPointinMesh.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanPrepComp.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanPrintGridInfo.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanPropvelS.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanPropvelX.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanReadADCGrid.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanReadEasymeshGrid.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanReadGrid.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanReadTriangleGrid.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanSweepSel.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanThreadBounds.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanTranspAc.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanTranspX.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/SwanVertlist.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/waves_control.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/waves_coupler.o: FFLAGS += -ffree-form -ffree-line-length-none
endif
