      SUBROUTINE HELPER
C
C  user intervention during program execution
C
C  SYNTAX   Hlp  :HELPER  prompt='
C
C
C  typical application:
C  --------------------
C
C   ...... ...... ......
C   trap=-1 <SEMPER command>
C   if (rc=9) hlp pro 'Outside picture: Issue commands to correct this '
C   ...... ...... ......
C
C  or if repeated commands are required for corrective action
C
C          if(rc ~= 9) jump on
C          hlp pro 'Outside picture! '
C   again: ask 'more commands ? ' ny
C          if ~ny jump on
C          hlp pro '> '
C          jump again
C   on:........
C
C--------------------------------------------------
C
C  SEMPER routines
      LOGICAL KLINE,SEMKTX,VARSET,OBEYCL
      INTEGER IPACK,IVAL,NBLANK
C
      INCLUDE 'COMMON'
      INTEGER LENPRO,LENSTR,ITEXT(200),NPROMT,IRC
      CHARACTER*(200) PROMPT,STRING
      LOGICAL ISSUE
C
C  if prompt string supplied
C
      NPROMT=IPACK('PROMPT')
      IF(VARSET(NPROMT)) THEN
C
C  get prompt
C
         LENPRO=200
         IF(SEMKTX(NPROMT,' ',ITEXT,LENPRO,.FALSE.)) RETURN
C
C  convert to character form
C
         CALL SEMCHS(PROMPT,ITEXT,LENPRO)
C
      ELSE
C
C  supply default prompt containing return code from previous command
C
         IRC=IVAL(IPACK('RC'))
         PROMPT=' '
         WRITE(PROMPT,10) IRC
   10    FORMAT('Error ',I3,'>')
         LENPRO=NBLANK(PROMPT)
C
      ENDIF
C
C  prompt user for string to be obeyed
C
      LENSTR=0
      ISSUE=LENPRO .NE. 0
      IF(KLINE(PROMPT(1:LENPRO),ISSUE,STRING,LENSTR)) RETURN
C
C  issue command
C
      IF(OBEYCL(STRING)) RETURN
C
C  Done !
C
      RETURN
      END
