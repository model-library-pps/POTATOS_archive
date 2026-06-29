*-------------------------------------------------------------------------------------------------------*
*    STPOTAT :  statistics of potato production limited by radiation, temperature and                   *
*               availability of water.                                                                  *
*    Subroutine for statistical treatment of yield variables simulated in a series of                   *
*    runs with the NWHEAT model.                                                                        *
*    The yield data refer to one crop and a time series of daily weather data from one weather station. *
*-------------------------------------------------------------------------------------------------------*
      SUBROUTINE STPOTAT (RUNNAME,REMARK,STATR,ISOIL,ICROP,TERMY,
     $    IDEMERG,IDPL,IDTUBI,IDHALT,CO)

      IMPLICIT REAL (A-Z)
      INTEGER ISOIL,ICROP,IDEMERG,IDPL,IDTUBI,IDHALT,ISYR,ISYEAR,IWRUN
      INTEGER IRRI1,IHYEAR,IDPL1

      LOGICAL TERMY,OPEN,INIT
      CHARACTER RUNNAME*5,NWSTAT*30,STATR*5,REMARK*80

      COMMON /ITIME/ ISYR, ISYEAR
      COMMON /CROPOUT/ TAGB1,WTU1,TPARINT1,HI1,RUE
      COMMON /WATBOUT/ TTRANS1,TDRAIN1,TRAIN1,TESOIL1,DWAVT
      COMMON /WATBOUT/ TRUNOF1,TIRR1,RUNFR1,IRRI1

      DATA OPEN /.FALSE./
      DATA INIT /.TRUE./

      IF (.NOT. OPEN)  THEN
      OPEN= .TRUE.

*------ open output file
       NWSTAT = RUNNAME//'.STA'
       OPEN (80,FILE=NWSTAT,STATUS='NEW', ACCESS='SEQUENTIAL')
       ENDIF

        IF (.NOT. INIT) GOTO 100

*------ initialization number of runs and means
      
       IWRUN= 0
       MPL= 0.
       MEMER= 0.
       MTUBI= 0.
       MHALT= 0.
       MWTU= 0.
       MAGB= 0.
       MHI= 0.
       MPARA= 0.
       MRUE= 0.
       MDWA= 0.
       MRAIN= 0.
       MEV= 0.
       MTRA= 0.
       MDRAI= 0.
       MIRR= 0.
       MRUNO= 0.
       MWUE= 0.

*------ initialization sums
      
       SUMPL= 0.
       SUMEMER= 0.
       SUMTUBI= 0.
       SUMHALT= 0.
       SUMWTU= 0.
       SUMAGB= 0.
       SUMHI= 0.
       SUMPARA= 0.
       SUMRUE= 0.
       SUMDWA= 0.
       SUMRAIN= 0.
       SUMEV= 0.
       SUMTRA= 0.
       SUMDRAI= 0.
       SUMIRR= 0.
       SUMRUNO= 0.
       SUMWUE= 0.

*------ initialization square sums
       
       S2MPL= 0.
       S2MEMER= 0.
       S2MTUBI= 0.
       S2MHALT= 0.
       S2MWTU= 0.
       S2MAGB= 0.
       S2MHI= 0.
       S2MPARA= 0.
       S2MRUE= 0.
       S2MDWA= 0.
       S2MRAIN= 0.
       S2MEV= 0.
       S2MTRA= 0.
       S2MDRAI= 0.
       S2MIRR= 0.
       S2MRUNO= 0.
       S2MWUE= 0.

*------ initialization standard deviations
      
       SDPL= 0.
       SDEMER= 0.
       SDTUBI= 0.
       SDHALT= 0.
       SDWTU= 0.
       SDAGB= 0.
       SDHI= 0.
       SDPARA= 0.
       SDRUE= 0.
       SDDWA= 0.
       SDRAIN= 0.
       SDEV= 0.
       SDTRA= 0.
       SDDRAI= 0.
       SDIRR= 0.
       SDRUNO= 0.
       SDWUE= 0.


*------ initialization variation coefficients
     
       VCPL= 0.
       VCEMER= 0.
       VCTUBI= 0.
       VCHALT= 0.
       VCWTU= 0.
       VCAGB= 0.
       VCHI= 0.
       VCPARA= 0.
       VCRUE= 0.
       VCDWA= 0.
       VCRAIN= 0.
       VCEV= 0.
       VCTRA= 0.
       VCDRAI= 0.
       VCIRR= 0.
       VCRUNO= 0.
       VCWUE= 0.


*------ print headings
       
        WRITE (80,'(///,2A)') 
     $   '*** RESULTS OF CROP GROWTH MODEL POTATOS ***',
     $   ' SUMMARY OF OUTPUT FOR POTATO AND YIELD STATISTICS ***'

        WRITE (80,'(/,A,A,A,A,A,I3,A,I3,A,I3, A,F4.2,A,F4.0)') 
     $   'Name of run: ',
     $   RUNNAME,', Station= ', STATR,', Soil= ', ISOIL,', Crop= ',
     $   ICROP,', Irr.= ',IRRI1,', Runof= ',RUNFR1,', CO2= ',CO

        WRITE (80,'(1X,A)') REMARK

        WRITE (80,'(//,4A)')
     $  'YEAR    PL   EMERG TUBI  HALT    WTU ',   
     $  '  TAGB     HI  PARAB  RUE   DWAV RAIN  ESOIL   TRANS',
     $  ' DRAIN RUNOF   IRR  WUE ' 


100     CONTINUE

        INIT= .FALSE.
   

        IF (TERMY) GOTO 200 
     
*----harvest year
        IHYEAR= ISYEAR 

*----water use efficiency (g DM above-gr./kg water transpired)
*----components of water balance in mm
*----crop production in kg DM/ha
*----PAR intercepted in MJ/m2
*----radiation use efficiency in g D.M. above-gr./MJ PAR intercepted

        WUE= TAGB1/((TTRANS1 + TESOIL1) * 100.)
        DWAVT2= DWAVT*10.
        TRAIN2= TRAIN1*10.
        TESOIL2= TESOIL1*10.
        TTRANS2= TTRANS1*10.
        TDRAIN2= TDRAIN1*10.
        TRUNOF2= TRUNOF1*10.
        TIRR2=   TIRR1*10.

*----- no planting date used
        IF (IDPL .LE. 0) THEN
           IDPL1= 0
        ELSE
           IDPL1= IDPL
        ENDIF
 
*------ one line of output for each year

        WRITE (80,'(I4,4I6,2X,2F7.0,F5.2,F6.0,F6.2,3F6.0,1X,
     $   4F6.0,F6.2)')  
     $   IHYEAR, IDPL1,IDEMERG,IDTUBI,IDHALT,WTU1,
     $   TAGB1,HI1,TPARINT1,RUE,DWAVT2,TRAIN2,TESOIL2,TTRANS2,
     $   TDRAIN2,TRUNOF2,TIRR2,WUE
     

*-----counting the number of years and summing the variables

         IWRUN=   IWRUN+1
         SUMPL=  SUMPL + IDPL1
         SUMEMER= SUMEMER + IDEMERG
         SUMTUBI= SUMTUBI + IDTUBI
         SUMHALT= SUMHALT + IDHALT
         SUMWTU=  SUMWTU + WTU1
         SUMAGB=  SUMAGB + TAGB1
         SUMHI=   SUMHI + HI1
         SUMPARA= SUMPARA + TPARINT1
         SUMRUE=  SUMRUE + RUE
         SUMDWA=  SUMDWA + DWAVT2
         SUMRAIN= SUMRAIN + TRAIN2
         SUMEV=   SUMEV + TESOIL2
         SUMTRA=  SUMTRA + TTRANS2
         SUMDRAI= SUMDRAI + TDRAIN2
         SUMIRR=  SUMIRR + TIRR2
         SUMRUNO= SUMRUNO + TRUNOF2
         SUMWUE=  SUMWUE + WUE

*-----Square sums for the same variables
    
         S2MPL=  S2MPL +  IDPL1*IDPL1
         S2MEMER= S2MEMER + IDEMERG*IDEMERG
         S2MTUBI= S2MTUBI + IDTUBI*IDTUBI
         S2MHALT= S2MHALT + IDHALT*IDHALT
         S2MWTU=  S2MWTU +  WTU1*WTU1
         S2MAGB=  S2MAGB +  TAGB1*TAGB1
         S2MHI=   S2MHI +  HI1*HI1
         S2MPARA= S2MPARA + TPARINT1*TPARINT1
         S2MRUE=  S2MRUE + RUE*RUE
         S2MDWA=  S2MDWA + DWAVT2*DWAVT2
         S2MRAIN= S2MRAIN + TRAIN2*TRAIN2
         S2MEV=   S2MEV + TESOIL2*TESOIL2
         S2MTRA=  S2MTRA + TTRANS2*TTRANS2
         S2MDRAI= S2MDRAI + TDRAIN2*TDRAIN2
         S2MIRR=  S2MIRR + TIRR2*TIRR2
         S2MRUNO= S2MRUNO + TRUNOF2*TRUNOF2
         S2MWUE=  S2MWUE + WUE*WUE

         IF (.NOT. TERMY) RETURN

*------elaboration for statistical analysis

200    CONTINUE

*------Means     

       MPL=  SUMPL/IWRUN
       MEMER= SUMEMER/IWRUN
       MTUBI=  SUMTUBI/IWRUN
       MHALT= SUMHALT/IWRUN       
       MWTU=  SUMWTU/IWRUN
       MAGB=  SUMAGB/IWRUN
       MHI=   SUMHI/IWRUN
       MPARA= SUMPARA/IWRUN
       MRUE=  SUMRUE/IWRUN
       MDWA=  SUMDWA/IWRUN
       MRAIN= SUMRAIN/IWRUN
       MEV=   SUMEV/IWRUN
       MTRA=  SUMTRA/IWRUN
       MDRAI= SUMDRAI/IWRUN
       MIRR=  SUMIRR/IWRUN
       MRUNO= SUMRUNO/IWRUN
       MWUE=  SUMWUE/IWRUN

       IF (IWRUN .LT. 3) GOTO 300

*------standard deviation

       SDPL=  SQRT(AMAX1(0.,(S2MPL-(SUMPL*SUMPL)/IWRUN)/IWRUN))
       SDEMER= SQRT(AMAX1(0.,(S2MEMER-(SUMEMER*SUMEMER)/IWRUN)/IWRUN))
       SDTUBI=  SQRT(AMAX1(0.,(S2MTUBI-(SUMTUBI*SUMTUBI)/IWRUN)/IWRUN))
       SDHALT= SQRT(AMAX1(0.,(S2MHALT-(SUMHALT*SUMHALT)/IWRUN)/IWRUN))
       SDWTU=  SQRT(AMAX1(0.,(S2MWTU-(SUMWTU*SUMWTU)/IWRUN)/IWRUN))
       SDAGB=  SQRT(AMAX1(0.,(S2MAGB-(SUMAGB*SUMAGB)/IWRUN)/IWRUN))
       SDHI=   SQRT(AMAX1(0.,(S2MHI-(SUMHI*SUMHI)/IWRUN)/IWRUN))
       SDPARA= SQRT(AMAX1(0.,(S2MPARA-(SUMPARA*SUMPARA)/IWRUN)/IWRUN))
       SDRUE=  SQRT(AMAX1(0.,(S2MRUE-(SUMRUE*SUMRUE)/IWRUN)/IWRUN))
       SDDWA=  SQRT(AMAX1(0.,(S2MDWA-(SUMDWA*SUMDWA)/IWRUN)/IWRUN))
       SDRAIN= SQRT(AMAX1(0.,(S2MRAIN-(SUMRAIN*SUMRAIN)/IWRUN)/IWRUN))
       SDEV=   SQRT(AMAX1(0.,(S2MEV-(SUMEV*SUMEV)/IWRUN)/IWRUN))
       SDTRA=  SQRT(AMAX1(0.,(S2MTRA-(SUMTRA*SUMTRA)/IWRUN)/IWRUN))
       SDDRAI= SQRT(AMAX1(0.,(S2MDRAI-(SUMDRAI*SUMDRAI)/IWRUN)/IWRUN))
       SDIRR=  SQRT(AMAX1(0.,(S2MIRR-(SUMIRR*SUMIRR)/IWRUN)/IWRUN))
       SDRUNO= SQRT(AMAX1(0.,(S2MRUNO-(SUMRUNO*SUMRUNO)/IWRUN)/IWRUN))
       SDWUE=  SQRT(AMAX1(0.,(S2MWUE-(SUMWUE*SUMWUE)/IWRUN)/IWRUN))

*-----variation coefficient (%)

       VCPL= 100.*SDPL/AMAX1(0.01,ABS(MPL))
       VCEMER= 100.*SDEMER/AMAX1(0.01,ABS(MEMER))
       VCTUBI= 100.*SDTUBI/AMAX1(0.01,ABS(MTUBI))
       VCHALT= 100.*SDHALT/AMAX1(0.01,ABS(MHALT))
       VCWTU= 100.*SDWTU/AMAX1(0.01,ABS(MWTU))
       VCAGB= 100.*SDAGB/AMAX1(0.01,ABS(MAGB))
       VCHI= 100.*SDHI/AMAX1(0.01,ABS(MHI))
       VCPARA= 100.*SDPARA/AMAX1(0.01,ABS(MPARA))
       VCRUE= 100.*SDRUE/AMAX1(0.01,ABS(MRUE))
       VCDWA= 100.*SDDWA/AMAX1(0.01,ABS(MDWA))
       VCRAIN= 100.*SDRAIN/AMAX1(0.01,ABS(MRAIN))
       VCEV= 100.*SDEV/AMAX1(0.01,ABS(MEV))
       VCTRA= 100.*SDTRA/AMAX1(0.01,ABS(MTRA))
       VCDRAI= 100.*SDDRAI/AMAX1(0.01,ABS(MDRAI))
       VCIRR= 100.*SDIRR/AMAX1(0.01,ABS(MIRR))
       VCRUNO= 100.*SDRUNO/AMAX1(0.01,ABS(MRUNO))
       VCWUE= 100.*SDWUE/AMAX1(0.01,ABS(MWUE))

300     CONTINUE


*------statistical results on file

        WRITE (80, '(/A,I3,A)') ' MEANS over ',IWRUN,' years  :'
        WRITE (80,'(5X,4F6.0,1X,2F7.0,F5.2,F6.0,F6.2,3F6.0,1X,
     $   4F6.0,F6.2)')  
     $   MPL,MEMER,MTUBI,MHALT,MWTU,
     $   MAGB,MHI,MPARA,MRUE,MDWA,
     $   MRAIN,MEV,MTRA,MDRAI,MRUNO,MIRR,MWUE

        WRITE (80, '(A)')  ' STANDARD DEVIATIONS  :'
        WRITE (80,'(5X,4F6.0,1X,2F7.0,F5.2,F6.0,F6.2,3F6.0,1X,
     $   4F6.0,F6.2)')  
     $   SDPL,SDEMER,SDTUBI,SDHALT,SDWTU,
     $   SDAGB,SDHI,SDPARA,SDRUE,SDDWA,
     $   SDRAIN,SDEV,SDTRA,SDDRAI,SDRUNO,SDIRR,SDWUE

        WRITE (80, '(A)')  ' VARIATION COEFFICIENTS (%)  : '       
        WRITE (80,'(5X,4F6.1,1X,2F7.1,F5.1,F6.1,F6.1,3F6.1,1X,
     $   4F6.1,F6.1)')  
     $   VCPL,VCEMER,VCTUBI,VCHALT,VCWTU,
     $   VCAGB,VCHI,VCPARA,VCRUE,VCDWA,
     $   VCRAIN,VCEV,VCTRA,VCDRAI,VCRUNO,VCIRR,VCWUE

         INIT= .TRUE.
    

         END
