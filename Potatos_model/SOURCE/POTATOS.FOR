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

*---------------------------------------------------------------------*
*                                                                     *
*     PROGRAM POTATOs.FOR                                            *
*     Author : joost wolf                                             *
*     Date of last revision:  May 1996                                *
*                                                                     *
*     Purpose: This model simulates the growth of a potato crop as    *
*              function of intercepted radiation, temperature and     *
*              light use efficiency. Interception of radiation is a   *
*              function of the temperature sum since plant emergence. *
*              A water balance and the effects of water supply on     *
*              crop growth are also calculated.                       *  
*---------------------------------------------------------------------*

      PROGRAM POTATOS

      IMPLICIT REAL (A-Z)
      INTEGER ISYR,ISYEAR,IUWE,IIYR,INYEAR,IDPL,IDEM,IFINIT,IGAP,ISOIL
      INTEGER ICROP,IOPT,IOUT,IDAY,IYEAR,IWAR,IRRI,IDEMERG,IDTUBI
      INTEGER IDHALT

      CHARACTER RUNNAME*5,REMARK*80,STATR*5,CONTIN*1,WTRDIR*80
      COMMON /ITIME/ ISYR, ISYEAR

      LOGICAL TERMIN,INITI,TERMY,PL      
      
*-----iunit number for weather
      DATA IUWE/65/

*---- initialize program
*     LOOP for different weather stations

      
1      CALL INITP (IIYR,INYEAR,IDPL,IDEM,IFINIT,IGAP,REMARK,RUNNAME,
     $  STATR,CONTIN,SENSP,SENSV,SENSW,SENSR,SENST,CO,
     $  ISOIL,ICROP,IOPT,IRRI,IOUT,WTRDIR)



       TERMY= .FALSE.

*---- LOOP for a number of years

      IYEAR = IIYR

5     CONTINUE
            
      TERMIN = .FALSE.

      IF (IDPL .LE. 0) THEN
        IDAY= IDEM
        PL= .FALSE.
      ELSE 
        IDAY= IDPL
        PL= .TRUE.
      ENDIF
      
      ISYR = IYEAR 
      ISYEAR= IYEAR + 1900
      INITI = .TRUE.

*---- LOOP for one growth period (year)

        
10       CONTINUE

*------- read weather data
*         IF (IOPT .EQ. 1) THEN
         CALL WEATHR (IWAR,WTRDIR,STATR,IYEAR,IDAY,IGAP,LONGIE,LATIN,
     $                ALTI,TMIN,TMAX,DTR,RAIN,VAP,WIND)
*         ELSE
*         CALL DBMRD (WTRDIR,STATR,IUWE,4,IYEAR,IDAY,LATIN,ALTI,
*     $                TMIN,TMAX,DTR,VAP,WIND,RAIN)
*         ENDIF


* ----- to prevent temperature effect on relative humidity
          
          TMPA = (TMIN + TMAX)/2.
          SVAP1  = 6.10588 * EXP (17.32491*TMPA/(TMPA+238.102))
          VAP= AMIN1(VAP,SVAP1)
          RH= VAP/SVAP1

* ------  sensitivity analyses
        
*          TMIN= TMIN + SENST
*          TMAX= TMAX + SENST  
         
*          TMPA = (TMIN + TMAX)/2.
*          SVAP2  = 6.10588 * EXP (17.32491*TMPA/(TMPA+238.102))
*          VAP= RH*SVAP2
         
               
*          VAP= AMIN1(SVAP1, VAP * SENSV)
*          WIND= WIND * SENSW
*          DTR= DTR * SENSR
*          RAIN= RAIN * SENSP
          
*------- calculate daylength

         CALL ASTRO (IDAY,LATIN,DAYL,DAYLP,SINLD,COSLD)                     


*------- calculate potential soil evaporation and crop transpiration


         CALL PENMAN (IDAY,DAYL,SINLD,COSLD,ALTI,TMIN,TMAX,DTR,
     $       WIND,VAP,CO,E0,ES0,ETC,AVRAD)


*------- calculate soil water balance  

         CALL WATBALS (ISOIL,INITI,IRRI,TERMIN,ES0, ETC,RAIN,FINT,
     $        SMACT,TTRANS,TDRAIN,TRAIN,TESOIL,TRUNOF,TIRR,
     $        RDRY,RUNFR,WTOT,WAVT)



*------- when finish conditions are reached (TERMIN .TRUE.)


*-------- calculate crop growth
         
         IF (TERMIN) THEN
           CALL CROPP (ICROP,INITI,IDAY,IDEM,IDEMERG,IDPL,IDTUBI,IDHALT,
     $        PL,TERMIN,TMIN,TMAX,AVRAD,CO,RDRY,DAYLP,TAGB,WTU,HI,
     $        TSULE1,TSUTE,TSULP,FINT,TPARINT,TPAR)

     
*------- daily output
           CALL POTATOUT (IDAY,IOUT,IDEM,IDPL,IDEMERG,
     $        WTU,TAGB,TPARINT,TPAR,REMARK,
     $        RUNNAME,SMACT,TTRANS,TDRAIN,TRAIN,TESOIL,TRUNOF,TIRR,
     $        RDRY,TERMIN,STATR,ISOIL,ICROP,IRRI,RUNFR,CO,WTOT,WAVT)

*------- yearly output
           CALL STPOTAT (RUNNAME,REMARK,STATR,ISOIL,ICROP,TERMY,
     $         IDEMERG,IDPL,IDTUBI,IDHALT,CO)

         GOTO 20

         ENDIF


*-------- calculate crop growth

         CALL CROPP (ICROP,INITI,IDAY,IDEM,IDEMERG,IDPL,IDTUBI,IDHALT,
     $        PL,TERMIN,TMIN,TMAX,AVRAD,CO,RDRY,DAYLP,TAGB,WTU,HI,
     $        TSULE1,TSUTE,TSULP,FINT,TPARINT,TPAR)

   
*------- daily output
         CALL POTATOUT (IDAY,IOUT,IDEM,IDPL,IDEMERG,
     $        WTU,TAGB,TPARINT,TPAR,REMARK,
     $        RUNNAME,SMACT,TTRANS,TDRAIN,TRAIN,TESOIL,TRUNOF,TIRR,
     $        RDRY,TERMIN,STATR,ISOIL,ICROP,IRRI,RUNFR,CO,WTOT,WAVT)



         IF (TERMIN) GOTO 15   


*------- update daynumber and year

         CALL TIMER (IDAY,IYEAR,IFINIT,TERMIN,INITI)

         INITI = .FALSE.                
 
 
15      GOTO 10

20      CONTINUE
      
         IYEAR= IYEAR + 1         

         IF (IYEAR .NE. IIYR + INYEAR)  GO TO 5

*------- when finish conditions are reached for total number of years (TERMY= .TRUE.)

         TERMY= .TRUE.

*------- statistical analysis
         CALL STPOTAT (RUNNAME,REMARK,STATR,ISOIL,ICROP,TERMY,
     $         IDEMERG,IDPL,IDTUBI,IDHALT,CO)


*------- runs for new site or years ?

         IF (CONTIN .EQ. 'Y' .OR. CONTIN .EQ. 'y') GOTO 1
      END
