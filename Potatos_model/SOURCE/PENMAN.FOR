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

*---------------------------------------------------------------------------*
*  SUBROUTINE PENMAN                                                        *
*  Author : Daniel van Kraalingen                                           *
*           based on an earlier version written by: Kees van Diepen         *
*           and Gert-Jan Nooy                                               *
*  Date   : 9-JAN-1987                                                      *
*  Purpose: This subroutine calculates atmospheric transmission,            * 
*           potential evaporation from a free water surface (E0),           *
*           a bare soil surface (ES0) and the potential                     *
*           transpiration of a closed crop canopy (ET0)                     *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name    meaning                                     units  class         *
*  ----    -------                                     -----  -----         *
*  IDAY    Julian day number                             -      I,T         *
*  DAYL    Astronomical day length                       h      I           *
*  SINLD   Intermediate variable                         -      I           *
*  COSLD   Intermediate variable                         -      I           *
*  ALTI    Altitude                                      m      I           *
*  TMIN    Minimum temperature during day                C      I           *
*  TMAX    Maximum temperature during day                C      I           *
*  DTR     Daily total irradiation                     J m-2 d-1  I         *
*  VAP     Vapour pressure                             mbar     I           *
*  WIND    Average windspeed                           m s-1    I           *
*  CO      Atmospheric CO2 concentration               mbar     I           *
*  E0      Potential evaporation of open water         cm d-1   O           *
*  ES0     Potential evaporation of soil               cm d-1   O           *
*  ETC     Potential evapotranspiration of crop (CO2 corr.)  cm d-1   O     *
*  AVRAD   Daily total irradiation                     J m-2 d-1  O         *
*                                                                           *
*  FATAL ERROR CHECKS (execution terminated, message)                       *
*  condition                                                                *
*  ---------                                                                *
*  TMIN > TMAX                                                              *
*  WIND < 0                                                                 *
*  AVRAD < 0                                                                *
*  VAP < 0                                                                  *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE PENMAN (IDAY,DAYL,SINLD,COSLD,ALTI,TMIN,TMAX,
     $    DTR,WIND,VAP,CO,E0,ES0,ETC,AVRAD)

      IMPLICIT REAL (A-Z)
      INTEGER IDAY,IFPEN
      DIMENSION FPENMTB(10)
      LOGICAL INIT

      PARAMETER (PI=3.1415926)

      DATA INIT/.TRUE./
      DATA ATMTR/0.5/

*----- correction factor for Penman evapo-transpiration rate as a function
*       of atmospheric CO2 concentration (increase in LAI and light interception 
*       with CO2 increase taken into account in function values).
      DATA FPENMTB/40.,1.05,355.,1.00,710.,0.95,1000.,0.92,2000.,0.92/
      DATA IFPEN/10/

*---- initial section

      IF (.NOT.INIT) GOTO 100
      INIT = .FALSE.
      A = 0.20
      B = 0.56

*---- Albedo for water surface, soil surface and canopy

      REFCFW = 0.05
      REFCFS = 0.15
      REFCFC = 0.25

*---- Latent heat of evaporation of water (J kg-1=J mm-1 m-2) and 
*     Stefan Boltzmann constant (J m-2 d-1 K-1)) Psychrometric 
*     instrument constant (K-1) 

      LHVAP  = 2.45E6
      STBC   = 4.9E-3
      PSYCON = 0.000662

100   CONTINUE

*---- average daily temperature

      TMPA = (TMIN+TMAX)/2.

*---- error checks on some input variable ranges
      
      IF (TMIN.GT.TMAX) STOP 'ERR in PENMAN:TMIN > TMAX'
      IF (WIND.LT.0.)   STOP 'ERR in PENMAN:WIND < 0'
      IF (DTR.LT.0.)  STOP 'ERR in PENMAN:AVRAD < 0'
      IF (VAP.LT.0.)  STOP 'ERR in PENMAN:VAP < 0'

*---- Temperature difference (Celsius) 

      TDIF = TMAX-TMIN
 
*---- Coefficient Bu in wind function, dependent on temperature difference

      BU = 0.54+0.35*LIMIT(0.,1.,(TDIF-12.)/4.)

*---- Barometric pressure (mbar), Psychrometric constant (mbar K-1)

      PBAR = 1013.*EXP(-0.034*ALTI/(TMPA+273.))
      GAMMA = PSYCON*PBAR

*---- Saturated vapour pressure according to equation of Goudriaan (1977)

      SVAP = 6.11*EXP(17.4*TMPA/(TMPA+239.))

*---- Measured vapour pressure should not be greater than saturated
*     vapour pressure

      VAP  = AMIN1(SVAP,VAP)

*---- Derivative of SVAP with respect to temperature, i.e. slope of the 
*     SVAP-temperature curve (mbar K-1)

      DELTA = 239.*17.4*SVAP/(TMPA+239.)**2

*---- integral of SINB (DSINB) with SINB equal to sine of solar elevation

      AOB= LIMIT(-1.,+1.,SINLD/COSLD)
      DSINB= 3600.*(DAYL*SINLD+24.*COSLD*SQRT(1.-AOB*AOB)/PI) 

*---- solar constant (SC) and daily extraterrestrial radiation (ANGOT)

      SC= 1370.*(1.+0.033*COS(2.*PI*IDAY/365.))
      ANGOT= AMAX1(0.0001,SC*DSINB) 

*---- atmospheric transmission (ATMTR)
      
      AVRAD= AMIN1(0.80*ANGOT, DTR)
      ATMTR= AVRAD/ANGOT
      

*---- The expression n/N (RELSSD) from the Penman formula is estimated
*     from the Angstrom formula: RI=RA(A+B.n/N) -> n/N=(RI/RA-A)/B,
*     where AVRAD=RI and DSO=RA, the Angot radiation, obtained by a CALL 
*     to ASTRO and three statements from RADIAT:

      RELSSD = LIMIT (0.,1.,(ATMTR-A)/B)

*---- Terms of the Penman formula, for water surface, soil 
*     surface and canopy 
*     Net outgoing long-wave radiation (J m-2 d-1) according to Brunt (1932)

      RB  = STBC*(TMPA+273.)**4*(0.56-0.079*SQRT(VAP))*(0.1+0.9*RELSSD)  

*---- Net absorbed radiation

      RNW = AVRAD*(1.-REFCFW)-RB
      RNS = AVRAD*(1.-REFCFS)-RB
      RNC = AVRAD*(1.-REFCFC)-RB

*---- Evaporative demand of the atmosphere (mm d-1)

      EA  = 0.26*(SVAP-VAP)*(0.5+BU*WIND)
      EAC = 0.26*(SVAP-VAP)*(1.0+BU*WIND)

*---- Penman formula (1948), and conversion to cm d-1

      E0  = 0.1*(DELTA*(RNW/LHVAP)+GAMMA*EA)/(DELTA+GAMMA)
      ES0 = 0.1*(DELTA*(RNS/LHVAP)+GAMMA*EA)/(DELTA+GAMMA)
      ET0 = 0.1*(DELTA*(RNC/LHVAP)+GAMMA*EAC)/(DELTA+GAMMA)


*---- Evapo(transpi)ration is limited in order to prevent negative
*     values

      E0  = AMAX1(0.,E0)
      ES0 = AMAX1(0.,ES0)
      ET0 = AMAX1(0.,ET0)
  
*---- correction of potential evapo-transpiration for atmospheric 
*     CO2 concentration
      ETC = ET0 * LINT(FPENMTB,IFPEN,CO)
    
      RETURN
      END
