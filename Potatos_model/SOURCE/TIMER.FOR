*---------------------------------------------------------------------------*
*  SUBROUTINE TIMER                                                         *
*  Author : Rob Groot                                                       *
*  Date   : december 1986                                                   *
*  Purpose: This subroutine updates TIME with time step of 1 day            *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units  class         *
*  ----     -------                                    -----  -----         *
*  IFINIT   maximum number of days for single run        d      I,T         *
*  INITI    logical, indicates start of simulation run          I           *
*  IYEAR    running value for year                       -      O,T         *
*  IDAY     julian daynumber                             -      O,T         *
*  TERMIN   logical, simulation is finished when .TRUE.         O           *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE TIMER(IDAY,IYEAR,IFINIT,TERMIN,INITI)

      IMPLICIT REAL (A-Z)
      INTEGER IDAY,IYEAR,IFINIT,IRUN
      LOGICAL TERMIN,INITI


*---- write daynumber to screen

      CALL CLS      
      WRITE (*,*) IDAY
      IF (INITI) IRUN = 0

*---- update daynumber

      IDAY = IDAY+1

*---- running value for time
      IRUN = IRUN+1


*---- end of simulation is induced when finish time is reached          

      IF (IRUN.EQ.IFINIT) THEN
         TERMIN=.TRUE.
         WRITE (*,'(///A)') ' finish conditions reached'
      ENDIF


*---- January 1 is defined as DAY=1

      IF (IDAY.GT.365) THEN
         IDAY = 1
         IYEAR = IYEAR+1
      END IF


      RETURN 
      END


