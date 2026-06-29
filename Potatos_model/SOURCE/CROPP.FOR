*-------------------------------------------------------------------------*
* Copyright 2013. Wageningen University, Plant Production Systems group,  *
* P.O. Box 430, 6700 AK Wageningen, The Netherlands.                      *
* You may not use this work except in compliance with the Licence.        *
* You may obtain a copy of the Licence at:                                *
*                                                                         *
* http://models.pps.wur.nl/content/licence-agreement                      *
*                                                                         *
* Unless required by applicable law or agreed to in writing, software     *
* distributed under the Licence is distributed on an "AS IS" basis,       *
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.*
*-------------------------------------------------------------------------*
*------------------------------------------------------------------------- *
*  SUBROUTINE CROPP                                                         *
*  Author: Joost Wolf                                                      *
*  Date  : May 1996                                                        *
*  Purpose: This subroutine simulates the dry matter increase of a         *
*           potato crop as function of intercepted radiation, temperature, *
*           light use efficiency and water availability. Light use         *
*           efficiency is a function of the temperature sum since plant    *
*           emergence.                                                     *
*                                                                          *
*  FORMAL PARAMETERS:(I= input, O= output, C= control, IN= init., T-time)  *
*  name     meaning                                  units       class     *
*  ----     -------                                  -----       -----     *
*  ICROP    number of crop data set                   -           I        *
*  INITI    indicates initialization of run           -           I,C      *
*  IDAY     julian day number                         -           I        *
*  IDEM     date of emergence                         -           I        *
*  IDEMERG  date of emergence                         -           O        *
*  IDPL     date of planting                          -           I        *
*  IDTUBI   start date of tuber filling                           O        *
*  IDHALT   end date of crop growth                               O        *
*  PL       indicates planting as start of simulation -           I,C      *
*  TERMIN   indicates terminal section                -           I,C      *
*  TMIN     minimum air temperature                   C           I        *
*  TMAX     maximum air temperature                   C           I        *
*  AVRAD    daily total irradiation                   J m-2 d-1   I        *
*  CO       atmospheric CO2 concentration             ppmv        I        *
*  RDRY     reduction factor as function of drought   -           I        *   
*  DAYLP    photoperiodically active daylength        h           I        *
*  TAGB     total above-ground biomass                kg DM ha-1  O        *
*  WTU      weight of tubers                          kg DM ha-1  O        *
*  HI       harvest index                             -           O        *
*  TSULE1   temperature sum from emergence (foliage)  C d         O        *
*  TSUTE    temperature sum from emergence (tubers)   C d         O        *
*  TSULP    temperature sum from planting (foliage)   C d         O        *
*  FINT     fractional light interception             -           O        *
*  TPARINT  total intercepted radiation (PAR)         MJ m-2      O        *
*  TPAR     total photosynthetically active radiation MJ m-2      O        *
*                                                                          *
*  File usage: LINTER                                                      *
*------------------------------------------------------------------------
       SUBROUTINE CROPP (ICROP,INITI,IDAY,IDEM,IDEMERG,IDPL,IDTUBI,
     $          IDHALT,PL,TERMIN,TMIN,TMAX,AVRAD,CO,RDRY,DAYLP,TAGB,WTU,
     $          HI,TSULE1,TSUTE,TSULP,FINT,TPARINT,TPAR)


       IMPLICIT REAL (A-Z)
       INTEGER ICROP,IDAY,IDEM,IDEMERG,IDPL,IDTUBI,IDHALT,IDAYL,ILUE
       INTEGER ITMP,ICO

       LOGICAL TERMIN,INITI,PL,TUB,EMERG
       REAL LUETB (30)
       REAL TMPTB (30)
       REAL COTB (30)
       COMMON /CROPOUT/ TAGB1,WSO1,TPARINT1, HI1, RUE

*----------------------------------------------------
*      INITIALIZATION
*----------------------------------------------------

       IF (INITI) THEN
         TUB= .FALSE.
         EMERG= .FALSE.
       ELSE
         GOTO 200
       ENDIF

*---- Reading crop data from file CROPP.DAT
       
       IF (ICROP .EQ. 1) CALL RDINIT (14,0, 'CROPP1.DAT')
       IF (ICROP .EQ. 2) CALL RDINIT (14,0, 'CROPP2.DAT')
       IF (ICROP .EQ. 3) CALL RDINIT (14,0, 'CROPP3.DAT')
       IF (ICROP .EQ. 4) CALL RDINIT (14,0, 'CROPP4.DAT')
       IF (ICROP .EQ. 5) CALL RDINIT (14,0, 'CROPP5.DAT')
       IF (ICROP .EQ. 6) CALL RDINIT (14,0, 'CROPP6.DAT')

       CALL RDSREA ('TAGBI',TAGBI)
       CALL RDSREA ('FINT0',FINT0)
       CALL RDSREA ('R0',R0)
       CALL RDSREA ('DURE',DURE)
       CALL RDSREA ('TS50',TS50)
       CALL RDSREA ('HIM',HIM)
       CALL RDSREA ('HISLP',HISLP)
       CALL RDSREA ('STTUB',STTUB)
       CALL RDSREA ('DMSO',DMSO)
       CALL RDSREA ('TBASE',TBASE)
       CALL RDAREA ('LUETB',LUETB, 30,ILUE)
       CALL RDAREA ('TMPTB',TMPTB, 30,ITMP)
       CALL RDAREA ('COTB',COTB, 30,ICO)
       CALL RDSINT ('IDAYL', IDAYL)
       CALL RDSREA ('DAYMAX', DAYMAX)
       CALL RDSREA ('DAYMIN', DAYMIN)
       CALL RDSREA ('TSUMEM',TSUMEM)
       CALL RDSREA ('NPL',NPL)

       CLOSE (14, STATUS= 'DELETE')

*----- initialization of state variables

       TAGB= TAGBI
       WTU= 0.
       GRT= 0.

       TSULP= 0. 
       TSULE1= 0.
       TSULE2= 0.
       TSUTE= 0.
       DTSULP= 0. 
       DTSULE1= 0.
       DTSULE2= 0.
       DTSUTE= 0.
       
       CINT= 0.
       TPAR= 0.
       TPARINT= 0.
       PAR= 0.
       PARINT= 0.      

       DAYPL= REAL(IDPL)
       
       IF (.NOT. PL) THEN
          DAYEM= REAL(IDEM)
       ELSE
          DAYEM= 999.
       ENDIF


200     CONTINUE

        


        IF (TERMIN) GOTO 999


*---------------------------------------------------------
*      INTEGRATION
*---------------------------------------------------------

*----- Temperature sums after planting (P) and emergence (E; 1= without and 2=
*      with long day length effect) for development
*      of foliage (TSUL*) and tubers (TSUT*)

       TSULP= INTGRL(TSULP, DTSULP, 1.)
       TSULE1= INTGRL(TSULE1, DTSULE1, 1.)
       TSULE2= INTGRL(TSULE2, DTSULE2, 1.)
       TSUTE= INTGRL(TSUTE, DTSUTE, 1.)

*----- Harvest index
       HI= MAX(0., HIM * (1. - EXP(-HISLP * (TSUTE - STTUB) / HIM)))

*----- Start of tuber filling
       IF (.NOT. TUB .AND. TSUTE .GE. STTUB)  IDTUBI= IDAY
       IF (.NOT. TUB .AND. TSUTE .GE. STTUB)  TUB= .TRUE.

*---- Total photosynthetically active radiation (MJ/m2)
      TPAR= INTGRL(TPAR, PAR, 1.)

*---- Total intercepted radiation (MJ/m2)
      TPARINT= INTGRL(TPARINT, PARINT, 1.)

*---- Dry weights of total above-ground biomass and tubers (kg DM / ha)
      TAGB= INTGRL(TAGB, GRT,  1.)
      WTU= HI * TAGB


*-----------------------------------------------------------------------------
*      RATE CALCULATIONS
*-----------------------------------------------------------------------------


*------ Weather calculations

*------ Daily photosynthetically active radiation (PAR, MJ/m2)
        PAR= AVRAD/1.0E6 * 0.50

*------ Average daily temperature (C)
        TMPA= 0.5 *(TMIN + TMAX)
        DAY= REAL(IDAY)

*------ date of planting
        IF (PL) THEN
          PUSHPL= INSW(DAY - DAYPL, 0., 1.)
        ELSE
          PUSHPL= 0.
        ENDIF

        IF (PL .AND. (TSULP .GE. TSUMEM) .AND. (.NOT. EMERG)) THEN
           IDEMERG= IDAY
           DAYEM= REAL(IDEMERG)
           EMERG= .TRUE.
        ENDIF 

*-----  Reduction of development rate until tuber initiation by long day length

        FRDAY= (DAYLP - DAYMIN) / (DAYMAX - DAYMIN)

        IF (IDAYL .EQ. 1 .AND. .NOT. TUB) THEN
           RDAYL= 1. - 0.5 * LIMIT(0.,1., FRDAY)
        ELSE
           RDAYL= 1.
        ENDIF

        IF (.NOT. PL) IDEMERG= IDEM

*------ Temperature sums after planting for development of foliage (TSUL*) 
*       and tubers (TSUT*)
 
        PUSHEM= INSW(DAY - DAYEM, 0., 1.)
        DTSULL= LIMIT(0., 18., TMPA - TBASE)
        DTSUL= MAX(0., TMPA -TBASE) 
        TEFF=  INSW(TMPA-13., TMPA-TBASE, 29.-TMPA)
        DTSUT= LIMIT(0.,11., TEFF)

        DTSULP= DTSULL * PUSHPL
        DTSULE1= DTSUL * PUSHEM 
        DTSULE2= DTSUL * PUSHEM * RDAYL
        DTSUTE= DTSUT * PUSHEM * RDAYL


*------ Plant growth
        
*------ Light interception
        CALL LINTER (TSULE1,TSULE2, NPL, FINT0, R0, TS50, DURE, 
     $       FINT, FINTL, FINTS)

*------ Light use efficiency dependent on temperature sum
        LUE= LINT(LUETB,ILUE,TSULE1)
        
*------ Correction of light use efficiency for change in atmospheric CO2 concentration (-)
        RCO= LINT(COTB,ICO,CO)
        
*------ Reduction of light use efficiency outside optimum daily temperature range
        DTEMP= TMAX - 0.25*(TMAX-TMIN)
        RTMP= LINT(TMPTB,ITMP,DTEMP)
        
*------ Total growth rate (kg DM/ha/d) from intercepted PAR (MJ/m2/d)
        PARINT= FINT * PAR
        GRT= RTMP*RCO*LUE * PARINT * 10. * RDRY
 
        


999      CONTINUE

* ----- Data for summary output

        IF (TERMIN) THEN
        WRITE (*, '(//2A)') ' Crop development completed'

        TAGB1= TAGB
        WSO1= WTU
        TPARINT1= TPARINT
        HI1= WTU/TAGB
        IDHALT= IDAY

*----- radiation use efficiency in g above-ground D.M./radiation intercepted in MJ
        RUE= (TAGB/10.)/TPARINT
        END IF


*---- Finish conditions
      IF (FINT .LT. 0.005 .AND.  TAGB .GT. 200.) TERMIN= .TRUE.


         RETURN
         END




* ---------------------------------------------------------------------
* Subroutine LINTER
* Author: C.J.T. Spitters
* Date: 6 Dec. 1989
* Purpose: calculation of seasonal course of fractional light 
*          interception
* TSULE1   Temperature sum after plant emergence            C d       I
* TSULE2   Temperature sum inclusive long day length effect C d       I
* NPL      Plant density                                  plants/m2   I
*          Light interception: logistic increase (FINT0, R0):
* FINT0    Initial interception capacity per plant        m2/plant    I
* R0       Initial rel.growth rate of interception capacity C-1 d-1   I
*          Light interception: linear decrease (TS50, DURE):
* TS50     Time till 50% reduction of FINT                   C d      I
* DURE     Duration of phase of decreasing interception      C d      I
* FINT     Fractional light interception                      -       O
* FINTL    Fractional light interception (expansion phase)    -       O
* FINTS    Fractional light interception (senescence phase)   -       O
* ---------------------------------------------------------------------
      SUBROUTINE LINTER (TSULE1,TSULE2,NPL,FINT0,R0,TS50,DURE,
     $                   FINT,FINTL,FINTS)
      IMPLICIT REAL(A-Z)
      IF (TSULE1.GT.0.) THEN
*     Light interception during the phase of leaf area expansion
      FINTL = NPL * FINT0 * EXP(R0*TSULE1) /
     $    (NPL*FINT0*EXP(R0*TSULE1) + 1. - NPL*FINT0)
*     Light interception during the phase of senescence of the foliage
      FINTS = LIMIT (0., 1., 0.5 - (TSULE2 - TS50) / DURE)
      
*     Fractional light interception of the canopy
      FINT = MIN(FINTL,FINTS)
      ELSE
         FINT = 0.
      ENDIF
      RETURN
      END
