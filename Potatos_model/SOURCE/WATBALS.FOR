*------------------------------------------------------------------------ *
*  SUBROUTINE WATBALS                                                     *
*  Author: Joost Wolf                                                     *
*  Date  : May 1996                                                       *
*  Purpose: This subroutine simulates the soil water status in the        *
*  maximum effective rooted zone, of which the depth can be limited by    *
*  both crop and soil characteristics. The following processes are taken  *
*  into account:                                                          *
*    - soil surface evaporation                                           *
*    - crop transpiration                                                 *
*    - precipitation                                                      *
*    - surface runoff                                                     *
*    - irrigation                                                         *
*    - drainage to the sub-soil                                        *
*                                                                         *
*  FORMAL PARAMETERS:(I= input, O= output, C= control, IN= init., T-time) *
*  name     meaning                                  units       class    *
*  ----     -------                                  -----       -----    *
*   ISOIL   number of soil data set                   -            I      *  
*   INITI   indicates initialization of run           -            I,C    *
*   IRRI    irrigated (=1) or non irrigated(=0)       -            I,C    *
*   TERMIN  indicates terminal section                -            I,C    *
*   ES0     potential evaporation from soil surface   cm d-1       I      *
*   ETC     potential transpiration of crop           cm d-1       I      *   
*   RAIN    precipitation                             cm d-1       I      *
*   FINT    fractional light interception              -           I      *
*   SMACT   actual soil mosture content               cm3 cm-3     O      *
*   TTRANS  cumulative crop transpiration             cm           O      *
*   TDRAIN  cumulative drainage to the sub-soil       cm           O      *
*   TRAIN   cumulative precipitation                  cm           O      *
*   TESOIL  cumulative soil evaporation               cm           O      *
*   TRUNOF  cumulative surface runoff                 cm           O      *
*   TIRR    cumulative irrigation                     cm           O      *
*   RDRY    reduction as a result of drought          -            O      *
*   RUNFR   fraction of precipitation lost by runoff  -            O      *
*   WTOT    total water in maximum rooted zone        cm           O      *    
*   WAVT    total available water in maximum rooted zone cm        O      *
*  File usage: none                                                       *
*------------------------------------------------------------------------
       SUBROUTINE WATBALS (ISOIL,INITI,IRRI,TERMIN,ES0,ETC,RAIN,FINT, 
     $                 SMACT,TTRANS, TDRAIN,TRAIN,TESOIL,TRUNOF,TIRR,
     $                 RDRY,RUNFR,WTOT,WAVT)


       IMPLICIT REAL (A-Z)
       INTEGER ISOIL,IRRI,IRRI1
       LOGICAL TERMIN,INITI
       COMMON /WATBOUT/ TTRANS1, TDRAIN1, TRAIN1, TESOIL1, DWAVT
       COMMON /WATBOUT/ TRUNOF1, TIRR1, RUNFR1, IRRI1

*----------------------------------------------------
*      INITIALIZATION
*----------------------------------------------------

       IF (.NOT. INITI) GO TO 200
      
*---- Reading physical soil properties from data file SOILP.DAT
       
       IF (ISOIL .EQ. 1) CALL RDINIT (21,0, 'SOILP1.DAT')
       IF (ISOIL .EQ. 2) CALL RDINIT (21,0, 'SOILP2.DAT')
       IF (ISOIL .EQ. 3) CALL RDINIT (21,0, 'SOILP3.DAT')
       IF (ISOIL .EQ. 4) CALL RDINIT (21,0, 'SOILP4.DAT')

       CALL RDSREA ('SMW',SMW)
       CALL RDSREA ('SMFC',SMFC)
       CALL RDSREA ('SM0',SM0)
       CALL RDSREA ('SMI',SMI)
       CALL RDSREA ('RDMCR',RDMCR)
       CALL RDSREA ('RDMSO',RDMSO)
       CALL RDSREA ('RUNFR',RUNFR)
       CALL RDSREA ('CFET',CFET)
       CALL RDSREA ('CFEV',CFEV)

       CLOSE (21, STATUS= 'DELETE')

*----- initialization of water balance

       TDRAIN= 0.
       TRAIN= 0.
       DRAIN= 0.
      
       RAIN0= 0.

       TTRANS= 0.
       TESOIL= 0.
       TRA= 0.
       EVA= 0.

       TIRR= 0.
       DIRR= 0.     
       TRUNOF= 0.
       RIRR= 0.
       RUNOF= 0. 
       FINT = 0.

*----- Initialization of soil moisture content (SMACT, cm3/cm3), available amounts of water
*      (above wilting point) in maximum effective rooted zone, both actual (WAVT) 
*      and at field capacity (WAVFC), and total amount of water in maximum rooted zone (WTOT),
*      all in cm.

       SMACT= AMAX1(SMW, AMIN1(SMI,SMFC))
       RDM= AMIN1(RDMSO, RDMCR)
       WAVTI= RDM * (SMACT - SMW)
       WAVT= WAVTI
       WAVFC= RDM * (SMFC - SMW)
       WTOT= RDM * SMACT
       WTOTN= WTOT
       DWAT= 0.


        


       DSLR= 3.
200     CONTINUE

       

        IF (TERMIN) GOTO 999

        
*---------------------------------------------------------
*      INTEGRATION
*---------------------------------------------------------


*------- Integrals for drainage, rainfall, runoff, irrigation, transpiration, 
*        evaporation, total and available water

         TDRAIN= INTGRL(TDRAIN, DRAIN, 1.)
         TRAIN= INTGRL(TRAIN, RAIN0, 1.)
         TRUNOF= INTGRL(TRUNOF, RUNOF, 1.)
         TIRR= INTGRL(TIRR, RIRR, 1.)
         TESOIL= INTGRL(TESOIL, EVA, 1.)
         TTRANS= INTGRL(TTRANS, TRA, 1.)
         WTOT= INTGRL( WTOT, DWAT, 1.)
         WAVT= INTGRL( WAVT, DWAT, 1.)

*------- Actual soil moisture content (cm3/cm3)
         SMACT= WTOT/RDM

       
*-----------------------------------------------------------------------------
*      RATE CALCULATIONS
*-----------------------------------------------------------------------------


*------ Infiltration of precipitation (RAIN) and irrigation (RIRR)

         RIRR= LIMIT(0., 1., DIRR)
         PERC= (1.-RUNFR) * RAIN + RIRR
         
*------ Water loss by surface runoff

         RUNOF= RUNFR * RAIN


*------ Maximum transpiration rate as function of light interception (FINT) and 
*       crop-specific correction factor of potential transpiration (CFET)
     

         TRMAX= CFET * ETC * FINT

*------ Actual transpiration as function soil moisture content (SMACT)
 
         SWDEP= LIMIT( 0.10, 0.95, 1./(0.76 + 1.5*ETC) - 0.20)
         SMCR= (1. - SWDEP) * (SMFC - SMW) + SMW
         RDRY= LIMIT(0., 1., (SMACT - SMW)/(SMCR - SMW))
         TRA= RDRY * TRMAX


*------- Maximum soil evaporation rate as function of light interception (FINT)

         EVMAX= ES0 * (1. - FINT)

*------- Actual soil evaporation rate

         IF (PERC .GE. 1.) THEN
            EVA= EVMAX
            DSLR= 1.
         ELSE 
            DSLR= DSLR + 1.
            EVMAXT= EVMAX * LIMIT(0.,1.,(SQRT (DSLR) - SQRT(DSLR-1.))
     $               * CFEV)
            EVA= AMIN1(EVMAX, EVMAXT + PERC)
         END IF

*------ Effective percolation 
         PERC1= PERC - EVA - TRA 

*------ Water holding capacity of maximum rooted zone (cm)

         CAP= (SMFC - SMACT) * RDM
         
*------ Maximum rooted zone is filled up to field capacity and 
*        the excess amount is lost by drainage

         IF (CAP .LE. PERC1) THEN
            DRAIN= PERC1 - CAP
         ELSE
            DRAIN= 0.
         END IF

*------ Demand for irrigation water

        IF (IRRI .EQ. 1 .AND. (SMACT .LE. (SMCR + 0.02))) THEN 
            DIRR= 0.7 * (WAVFC - WAVT)
        ELSE 
            DIRR= 0.
        END IF

*----- precipitation during last day
        RAIN0= RAIN

*------ Change in total water and available water (DWAT)

        DWAT= PERC - EVA - TRA - DRAIN

*------ New amount of water in maximum rooted zone (after rate calculations)

        WTOTN= INTGRL(WTOTN,DWAT, 1.)

*------ Check on soil water balance: WBAL should be zero.

         WBAL= ABS(WTOT -TRA - EVA + RAIN + RIRR - RUNOF - DRAIN 
     $             - WTOTN)

         IF (WBAL .GE. 1.E-4) STOP
     $    ' soil moisture balance not 0, program aborted'


999      CONTINUE

* ----- Data for summary output

        IF (TERMIN) THEN

        TTRANS1= TTRANS
        TDRAIN1= TDRAIN
        TRAIN1= TRAIN
        TESOIL1= TESOIL
        TRUNOF1= TRUNOF
        TIRR1= TIRR
        IRRI1= IRRI
        RUNFR1= RUNFR
        DWAVT= WAVTI - WAVT

        END IF

         RETURN
         END