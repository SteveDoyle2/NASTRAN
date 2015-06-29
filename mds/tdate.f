      SUBROUTINE TDATE (DATE)        

C     THIS ROUTINE OBTAINS THE MONTH, DAY AND YEAR, IN INTEGER FORMAT   
C        
      INTEGER DATE_TIME(8)
c     (year-4, month-int,day-int,UTC-minutes, hour, minutes,seconds,milliseconds)

      character*10 b(3)
      call date_and_time(b(1), b(2), b(3), date_time)        
      RETURN        
      END        
