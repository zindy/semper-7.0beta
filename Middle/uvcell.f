C       SUBROUTINE UVCELL
C       -----------------
C
C       PARAMETERS:
C
C       None.
C
C       Implements the UIF CELL verb.  Allows the creation,
C       destruction, showing etc. of UIF cells.  Will set error
C       number correctly for Semper in case of UIF error.  Note that
C       the order of reading and acting on keys and options can be
C       important, so care must be taken if it is altered.
C
C----------------------------------------------------------------------
C
      SUBROUTINE UVCELL
C     =================
C
      INCLUDE 'COMMON'
      INCLUDE 'UIFCOM'
C
C     LOCAL VARIABLES:
C
C     Id of the panel cell resides on
C
      INTEGER PID
C
C     Id of cell we are to work on
C
      INTEGER EID
C
C     Id of menu to which cell is to be added
C
      INTEGER MID
C
C     Offset of the cell within a menu
C
      INTEGER XOFF, YOFF
C
C     Row and column of cell within a menu
C
      INTEGER ROW, COLUMN
C
C     Real value of a key
C
      REAL VALUE
C
C     Length of the text buffer
C
      INTEGER BUFLEN
      PARAMETER (BUFLEN=LNLINB)
C
C     Buffer for reading text/icon name into as integer
C
      INTEGER IBUFF(BUFLEN)
      EQUIVALENCE (IBUFF,RB6)
C
C     Character form of the text/icon name
C
      CHARACTER*(BUFLEN) PNAME
C
C     Length of the name
C
      INTEGER LENGTH
C
C     Number of cycles to performs
C
      INTEGER CYCLES
C
C     Picture number we are to use to make title cell
C
      INTEGER PICNO
C
C     Picture data for the picture
C
      INTEGER NCOL, NROW, NLAY, CLASS, FORM
C
C     Logical picture number we open picture on
C
      INTEGER LPN
C
C     Picture label data
C
      INTEGER LABEL(256)
      EQUIVALENCE (LABEL,RB1)
C
C     Hundreds, tens and units of the picture number
C
      INTEGER HUNDS, TENS, UNITS
C
C     Packed names for keys and options etc.
C
      INTEGER XICON,XTEXT,XCYCLE,XCHECK,XINVER,XTICK,XFLASH,XACTIV,XBOX
      INTEGER XADD,XCOLUM,XROW,XOFFS1,XOFFS2,XDROP,XTITLE
C
      PARAMETER (XICON=14535,XTEXT=-225,XCYCLE=5803)
      PARAMETER (XCHECK=5125,XINVER=14982,XTICK=-364,XFLASH=10081)
      PARAMETER (XACTIV=30612,XBOX=3824)
      PARAMETER (XADD=1764,XCOLUM=5412,XROW=29423,XOFFS1=24246)
      PARAMETER (XOFFS2=24272,XDROP=7135,XTITLE=-381)
C
C     CALLED FUNCTIONS:
C
C     Reads and acts on the common element keys and options
C
      LOGICAL UVELE1, UVELE2
C
C     Checks to see if an option is set
C
      LOGICAL OPT
C
C     Checks to see is a variable/key is set
C
      LOGICAL VARSET
C
C     Checks to see is a variable/key is set and returns value
C
      LOGICAL SEMLU
C
C     Extracts a string from the input buffer
C
      LOGICAL SEMTEX
C
C     Sets a cells highlighting type
C
      LOGICAL UIFCHI
C
C     Sets a cell to have text contents
C
      LOGICAL UIFCTE
C
C     Sets a cell to have iconic contents
C
C!      LOGICAL UIFCIN
C
C     Cycles a cell through its states
C
      LOGICAL UIFCCY
C
C     Adds a cell to the menu
C
      LOGICAL UIFCAD
C
C     Sets the cell to be a DROP cell
C
      LOGICAL UIFCDO
C
C     Sets a cell's offset in the menu
C
      LOGICAL UIFCOF
C
C     Sets a cell's column position in the menu
C
      LOGICAL UIFCCO
C
C     Sets a cell's row position in the menu
C
      LOGICAL UIFCRO
C
C     Sets a cell to have a box around
C
      LOGICAL UIFCBO
C
C     Gets a value as a picture number
C
      INTEGER IVALPN
C
C     Opend a picture
C
      LOGICAL SEMOPN
C
C     Reads a picture label
C
      LOGICAL SEMLAB
C
C     Closes a picture
C
      LOGICAL SEMCLS
C
C     Get the identifier of the cell to work on: may involve
C     creating a new one
C
      IF ( UVELE1 ( CELL, PID, EID ) ) GOTO 10
C
C     If we are to add to a menu, do it now so colours work properly
C
      IF ( SEMLU ( -1, XADD, VALUE ) ) THEN
C
C        Add cell to the menu....
C
         MID = INT ( VALUE )
         IF ( UIFCAD ( MID, EID ) ) GOTO 20
      ENDIF
      IF ( UVELE2 ( CELL, PID, EID ) ) GOTO 10
C
C     Get the rest of the keys and options, and act on them
C
      IF ( OPT ( XCHECK ) ) THEN
C
C        Check highlighting...
C
         IF ( UIFCHI ( EID, CHECK ) ) GOTO 20
      ENDIF
      IF ( OPT ( XTICK ) ) THEN
C
C        Check highlighting....
C
         IF ( UIFCHI ( EID, TICK ) ) GOTO 20
      ENDIF
      IF ( OPT ( XFLASH ) ) THEN
C
C        Flash highlighting....
C
         IF ( UIFCHI ( EID, FLASH ) ) GOTO 20
      ENDIF
      IF ( OPT ( XINVER ) ) THEN
C
C        Invert highlighting....
C
         IF ( UIFCHI ( EID, INVERT ) ) GOTO 20
      ENDIF
C!      IF ( OPT ( XACTIV ) ) THEN
C!C
C!C        Select the cell....
C!C
C!      ENDIF
      IF ( OPT ( XBOX ) ) THEN
C
C        Set the cell to have a box around....
C
         IF ( UIFCBO ( EID ) ) GOTO 20
      ENDIF
      IF ( OPT ( XDROP ) ) THEN
C
C        Mark cell as being a DROP cell....
C
         IF ( UIFCDO ( EID ) ) GOTO 20
      ENDIF
      IF ( SEMLU ( -1, XCYCLE, VALUE ) ) THEN
C
C        Size....
C
         CYCLES = INT ( VALUE )
         IF ( UIFCCY ( EID, CYCLES ) ) GOTO 20
      ENDIF
      IF ( VARSET ( XTEXT ) ) THEN
C
C        Text contents....
C
         LENGTH = BUFLEN
         IF ( SEMTEX ( XTEXT, IBUFF, LENGTH ) ) GOTO 10
C
C           Unpack the name into a character variable
C
         CALL SEMCHS ( PNAME, IBUFF, LENGTH )
         IF ( UIFCTE ( EID, PNAME, LENGTH ) ) GOTO 20
      ENDIF
C!      IF ( VARSET ( XICON ) ) THEN
C!C
C!C        Icon contents....
C!C
C!         LENGTH = BUFLEN
C!         IF ( SEMTEX ( XICON, IBUFF, LENGTH ) ) GOTO 10
C!C
C!C           Unpack the name into a character variable
C!C
C!         CALL SEMCHS ( PNAME, IBUFF, LENGTH )
C!         IF ( UIFCIN ( EID, PNAME, LENGTH ) ) GOTO 20
C!      ENDIF
      IF ( SEMLU ( -1, XTITLE, VALUE ) ) THEN
C
C        Picture title to go into cell...
C
C        Open the picture number required so we can get title info.
C
         PICNO = IVALPN ( XTITLE )
         IF ( SEMOPN (1, PICNO, NCOL, NROW, NLAY, CLASS, FORM, LPN) )
     +        GOTO 10
C
C        Read picture lable
C
         IF ( SEMLAB ( 1, LABEL, LPN ) ) GOTO 10
C
C        Now see if the picture has a title.  If not, make one up
C
         IF ( LABEL(LBNCTT) .GT. 0 ) THEN
C
C           Convert the title into a character string.
C
            LENGTH = LABEL(LBNCTT)
            CALL SEMCHS ( PNAME, LABEL(LBTT1), LENGTH )
            IF ( LENGTH .GT. 20 ) LENGTH = 20
            IF ( SEMCLS ( LPN ) ) GOTO 10
         ELSE
C
C           Make up a title from the picture number (without the
C           device number added)
C
            PNAME(1:11) = 'Picture    '
            LENGTH = 11
C
            PICNO = MOD ( PICNO, 1000 )
            HUNDS = PICNO / 100
            TENS  = ( PICNO - HUNDS * 100 ) / 10
            UNITS = MOD ( PICNO, 10 )
C
            IF ( HUNDS .NE. 0 ) THEN
               PNAME(9:9) = CHAR ( HUNDS + 48 )
            ENDIF
            IF ( TENS .NE. 0 .OR. HUNDS .NE. 0 ) THEN
               PNAME(10:10) = CHAR ( TENS + 48 )
            ENDIF
            PNAME(11:11) = CHAR ( UNITS + 48 )
         ENDIF
C
C        Now create a cell with the text in
C
         IF ( UIFCTE ( EID, PNAME, LENGTH ) ) GOTO 20
      ENDIF
      IF ( SEMLU ( -1, XCOLUM, VALUE ) ) THEN
C
C        Set cell to a column on the menu....
C
         COLUMN = INT ( VALUE )
         IF ( UIFCCO ( EID, COLUMN ) ) GOTO 20
      ENDIF
      IF ( SEMLU ( -1, XROW, VALUE ) ) THEN
C
C        Set cell to a row on the menu....
C
         ROW = INT ( VALUE )
         IF ( UIFCRO ( EID, ROW ) ) GOTO 20
      ENDIF
      IF ( SEMLU ( -1, XOFFS1, VALUE ) ) THEN
C
C        Offset....
C
         XOFF = INT ( VALUE )
         IF ( SEMLU ( -1, XOFFS2, VALUE ) ) THEN
            YOFF = INT ( VALUE )
         ELSE
            YOFF = 0
         ENDIF
         IF ( UIFCOF ( EID, XOFF, YOFF ) ) GOTO 20
      ENDIF
C
   10 CONTINUE
C
C     All done.
C
      RETURN
C
   20 CALL UIFAPE
      GOTO 10
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
