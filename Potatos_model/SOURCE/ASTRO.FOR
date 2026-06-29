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
*  SUBROUTINE ASTRO                                                        *
*  Author : Daniel van Kraalingen                                           *
*           documented by Rob Groot                                         * 
*  Date   : 15 march 1987                                                   *
*  Purpose: This subroutine calculates astronomical daylength (DAYL)        *
*           and photoperiodically active daylength (DAYLP), which           *
*           are used in the calculation of canopy assimilation and in the   *
*           calculation of the development rate.                            *
*           References to literature:                                       *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units  class         *
*  ----     -------                                    -----  -----         *
*  IDAY     Julian daynumber                                    I,T         *
*  LAT      latitude                                  degrees   I           *
*  DAYL     astronomical daylength                       h      O           *
*  DAYLP    photoperiodically active daylength           h      O           *
*  SINLD    intermediate variable                        =      O           *
*  COSLD    intermediate variable                        =      O           *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE ASTRO (IDAY,LAT,DAYL,DAYLP,SINLD,COSLD)                     
 
      IMPLICIT REAL (A-Z)
      INTEGER IDAY

      PARAMETER (PI=3.1415926)


*---- conversion factor from degrees to radians                         

      RAD=PI/180.


*---- declination of the sun as function of daynumber (IDAY)

      DEC=-ASIN(SIN(23.45*RAD)*COS(2.*PI*(IDAY+10.)/365.))


*---- SINLD, COSLD are intermediate variables

      SINLD=SIN(RAD*LAT)*SIN(DEC)
      COSLD=COS(RAD*LAT)*COS(DEC)


*---- daylength (DAYL) and photoperiodically active daylength (DAYLP)

      DAYL =12.0*(1.+2.*ASIN(LIMIT(-1.,+1.,SINLD/COSLD))/PI)
      DAYLP=12.0*(1.+2.*ASIN(LIMIT(-1.,+1.,
     $  (-SIN(-4.*RAD)+SINLD)/COSLD))/PI)

      RETURN                                                            
      END
