*---------------------------------------------------------------------------*
*  SUBROUTINE POTATOUT                                                      *
*  Author : Joost Wolf                                                      *
*  Date   : May 1996                                                        *
*  Purpose: This subroutine writes crop and soil water status to the        *
*	    output file Run/PTS.OUT                                        *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units     class      *
*  ----     -------                                    -----     -----      *
*  IDAY     julian daynumber                             =         I,T      *
*  IOUT     time interval for output                    d          C        *
*  IDEM     date of emergence                           -          I        *
*  IDPL     date of planting                            -          I        *
*  IDEMERG  date of emergence                           -          O        *
*  WTU      weight of tubers                           kg DM/ha    I        *
*  TAGB     total above-ground biomass                 kg DM/ha    I        *
*  TPARINT  total intercepted radiation (PAR)          MJ/m2       I        *          
*  TPAR     total photosynthetically active radiation  MJ/m2       I        * 
*  REMARK   output remarks                               =         I        *
*  RUNNAME  first 5 characters for name of output files            I        *
*  SMACT    actual soil moisture content               cm3/cm3     I        *
*  TTRANS   cumulative crop transpiration              cm          I        *
*  TDRAIN   cumulative drainage to the sub-soil        cm          I        *
*  TRAIN    cumulative precipitation                   cm          I        *
*  TESOIL   cumulative soil evaporation                cm          I        *
*  TRUNOF   cumulative surface runoff                  cm          I        *
*  TIRR     cumulative irrigation                      cm          I        *
*  RED      reduction factor for photosynthesis         -          I        *
*	    as a result of water stress                 -          I        *
*  TERMIN   indicates terminal section                  -          I,C      *
*  STATR    name of weather station (CHAR*5)            -          I        *
*  ISOIL    number of soil type                         -          I        *
*  ICROP    number of crop variety                      -          I        *
*  IRRI     0= irrigation; 1= irrigation                -          I        *
*  RUNFR    fraction of precipitation lost by runoff    -          I        *
*  CO       atmospheric CO2 concentration               ppmv       I        *
*  WTOT     total water in maximum rooted zone          cm         I        *
*  WAVT      total available water in maximum rooted zone cm       I        *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : crop status is written to output file                       *
*	    CRPOUT.DAT                                                      *
*---------------------------------------------------------------------------*

      SUBROUTINE POTATOUT (IDAY,IOUT,IDEM,IDPL,IDEMERG,
     $                   WTU,TAGB,TPARINT,TPAR,
     $                   REMARK,RUNNAME,
     $                   SMACT,TTRANS,TDRAIN,TRAIN,TESOIL,TRUNOF,TIRR,
     $                   RDRY,TERMIN,STATR,ISOIL,ICROP, IRRI,RUNFR,CO,
     $                   WTOT,WAVT)

      IMPLICIT REAL (A-Z)
      INTEGER IDAY,IOUT,IDEM,IDPL,IDEMERG,ISOIL,ICROP,IRRI,ISYR,ISYEAR
      INTEGER IDEM1,IDPL1,ICOUNT

      LOGICAL TERMIN, OPEN
      CHARACTER REMARK*80,RUNNAME*5,OUTFIL*12,STATR*5
      COMMON /ITIME/ ISYR, ISYEAR
      DATA OPEN /.TRUE./
      

      IF (OPEN) THEN

*---- open output file Run/PTS.OUT

        OUTFIL = RUNNAME//'PTS.OUT'
        OPEN(13,FILE=OUTFIL,STATUS='NEW')
      
        OPEN = .FALSE.

      ENDIF
      
      IF ((IDPL .LE. 0 .AND. IDAY .EQ. IDEM) .OR. 
     $     (IDPL .GT. 0 .AND. IDAY .EQ. IDPL)) THEN 
 
      IDEM1= IDEM
      IDPL1= IDPL
      IF(IDPL .LE. 0) IDPL1= 0
      IF (IDPL .GT. 0) IDEM1= 0
      
*---- write filename and start date on top of output-file

      WRITE (13,'(//A,A)') 'DATA-FILE  ', OUTFIL
      WRITE (13,'(//A,A)') 'Name of run: ', RUNNAME 
      WRITE (13,'(//A,A,A,I3,A,I3,A,I3,A,F4.2,A,F4.0)') 'Station=  ',
     $   STATR,', Soil=',ISOIL,', Crop=',ICROP,', Irr.=',IRRI,
     $   ', Runof=',RUNFR,', CO2=',CO 
      WRITE (13,'(/(A,I3,A,I4))')
     $      ' Planting on day ',IDPL1,', ',ISYEAR,
     $      ' Crop emergence on day ',IDEM1,', ',ISYEAR


*---- Write output remarks to file Run/PTS.OUT

      WRITE (13,'(1X,A)') REMARK


*---- writes header for to output file Run/PTS.OUT
      WRITE (13,'(/A)') ' POTATO PRODUCTION AND SOIL WATER STATUS'
      WRITE (13,'(A)')  ' ======================================='



         WRITE (13,'(2A)')
     $  ' DAY    WTUB    TAGB   RDRY   SMACT   TRAIN   TIRR ',
     $  ' TTRANS   TESOIL  TDRAIN  TRUNOF   WTOT   WAVT   PARAB    PAR'

      
            
      ICOUNT = -1
      
      ENDIF
 

      ICOUNT = ICOUNT+1

*----- Components of water balance in mm
       TRAIN1= 10.* TRAIN
       TIRR1= 10.* TIRR
       TTRANS1= 10.* TTRANS        
       TESOIL1= 10.* TESOIL
       TDRAIN1= 10.* TDRAIN
       TRUNOF1= 10.* TRUNOF
       WTOT1= 10.* WTOT
       WAVT1= 10.* WAVT
 
      IF (MOD(ICOUNT,IOUT) .EQ. 0 .OR. (IDAY .EQ. IDEMERG) 
     $      .OR. TERMIN) THEN

      WRITE (13,'(1X,I3,2F8.0,2F7.2,6F8.0,2F8.0,2F7.0))')
     $       IDAY,WTU,TAGB,RDRY,SMACT,TRAIN1,TIRR1,TTRANS1,TESOIL1,
     $       TDRAIN1,TRUNOF1,WTOT1,WAVT1,TPARINT,TPAR

      ENDIF
      RETURN
      END
