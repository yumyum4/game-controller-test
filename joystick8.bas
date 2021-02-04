' ################################################################################################################################################################
' Multi gamepad test

' BASED ON CODE BY SMcNeill FROM:
' Simple Joystick Detection and Interaction  (Read 316 times)
' https://www.qb64.org/forum/index.php?topic=2160.msg129051#msg129051
' ################################################################################################################################################################

' =============================================================================
' GLOBAL DECLARATIONS a$=string, i%=integer, L&=long, s!=single, d#=double

' boolean constants
CONST FALSE = 0
CONST TRUE = NOT FALSE

' =============================================================================
' UDTs

' UDT TO HOLD THE INFO FOR A PLAYER
TYPE PlayerType
    x AS INTEGER ' player x position
    y AS INTEGER ' player y position
    c AS INTEGER ' character to display on screen
    xOld AS INTEGER
    yOld AS INTEGER
    buttonCount AS INTEGER
    axisCount AS INTEGER
END TYPE ' PlayerType

' =============================================================================
' GLOBAL VARIABLES
DIM ProgramPath$
DIM ProgramName$

' =============================================================================
' INITIALIZE
ProgramName$ = MID$(COMMAND$(0), _INSTRREV(COMMAND$(0), "\") + 1)
ProgramPath$ = LEFT$(COMMAND$(0), _INSTRREV(COMMAND$(0), "\"))

'DIM a$ ' string
'DIM i% ' integer
'DIM L& ' long integer
'DIM s! ' single precision
'DIM d# ' double precision
'DIM bValue% ' boolean is an integer

' =============================================================================
' TRY THE MOUSE
main ProgramName$

' =============================================================================
' FINISH
SYSTEM ' return control to the operating system
PRINT ProgramName$ + " finished."
END

' /////////////////////////////////////////////////////////////////////////////

SUB main (ProgName$)
    DIM RoutineName AS STRING:: RoutineName = "main"
    DIM in$
    DIM iDeviceCount AS INTEGER

    iDeviceCount = _DEVICES ' Find the number of devices on someone's system

    ' 1 is the keyboard
    ' 2 is the mouse
    ' 3 is the joystick
    ' unless someone has a strange setup with multiple mice/keyboards/ect...
    ' In that case, you can use _DEVICE$(i) to look for "KEYBOARD", "MOUSE", "JOYSTICK", if necessary.
    ' I've never actually found it necessary, but I figure it's worth mentioning, just in case...

    IF iDeviceCount > 2 THEN
        TestJoysticks
    ELSE
        PRINT "No joysticks found."
    END IF

    INPUT "PRESS <ENTER> TO CONTINUE", in$
END SUB ' main

' /////////////////////////////////////////////////////////////////////////////

SUB TestJoysticks ()
    DIM RoutineName AS STRING:: RoutineName = "TestJoysticks"

    CONST cMaxPlayers = 8
    CONST cMaxButtons = 2
    CONST cMaxAxis = 2

    DIM in$
    DIM iDeviceCount AS INTEGER
    DIM iDevice AS INTEGER

    'DIM a$ ' string
    'DIM i% ' integer
    'DIM L& ' long integer
    'DIM s! ' single precision
    'DIM d# ' double precision
    'DIM bValue% ' boolean is an integer

    ' WE'RE PREPARED TO SUPPORT UPTO 8 JOYSTICKS, WITH UPTO 2 BUTTONS AND 2 AXES EACH
    ' (THIS IS FOR ATARI 2600 JOYSTICKS)
    DIM arrButton(8, 2) ' number of buttons on the joystick
    DIM arrButtonMin(8, 2) ' stores the minimum value read
    DIM arrButtonMax(8, 2) ' stores the maximum value read
    DIM arrAxis(8, 3) ' number of axis on the joystick
    DIM arrAxisMin(8, 3) ' stores the minimum value read
    DIM arrAxisMax(8, 3) ' stores the maximum value read
    DIM arrButtonNew(8, 2) ' tracks when to initialize values
    DIM arrAxisNew(8, 3) ' tracks when to initialize values

    'DIM Button(_LASTBUTTON(3) ) ' number of buttons on the joystick
    'DIM Axis(_LASTAXIS(3) ) ' number of axis on the joystick

    DIM arrPlayer(8) AS PlayerType ' holds info for each player
    DIM iNumPlayers AS INTEGER
    DIM iPlayerLoop AS INTEGER
    DIM iNextY AS INTEGER
    DIM iNextX AS INTEGER
    DIM iNextC AS INTEGER
    DIM iLoop AS INTEGER
    DIM iDigits AS INTEGER ' # digits to display (values are truncated to this length)
    DIM strValue AS STRING

    ' INITIALIZE
    iDigits = 11

    ' COUNT # OF JOYSTICKS
    iDeviceCount = _DEVICES ' Find the number of devices on someone's system
    IF iDeviceCount < 3 THEN
        CLS
        PRINT "NO JOYSTICKS FOUND, EXITING..."
        INPUT "PRESS <ENTER>", in$
        EXIT SUB
    END IF

    ' BASE # OF PLAYERS ON HOW MANY CONTROLLERS FOUND
    iNumPlayers = iDeviceCount - 2 ' TODO: find out the right way to count joysticks
    IF iNumPlayers > cMaxPlayers THEN
        iNumPlayers = cMaxPlayers
    END IF

    ' INITIALIZE PLAYER COORDINATES AND SCREEN CHARACTERS
    iNextY = 1
    iNextX = -3
    iNextC = 64
    FOR iPlayerLoop = 1 TO iNumPlayers
        iNextX = iNextX + 4
        IF iNextX > 80 THEN
            iNextX = 1
            iNextY = iNextY + 4
        END IF
        iNextC = iNextC + 1
        arrPlayer(iPlayerLoop).x = iNextX
        arrPlayer(iPlayerLoop).y = iNextY
        arrPlayer(iPlayerLoop).c = iNextC
        arrPlayer(iPlayerLoop).xOld = iNextX
        arrPlayer(iPlayerLoop).yOld = iNextY
        arrPlayer(iPlayerLoop).buttonCount = cMaxButtons
        arrPlayer(iPlayerLoop).axisCount = cMaxAxis

        FOR iLoop = 1 TO cMaxButtons
            arrButtonNew(iPlayerLoop, iLoop) = TRUE
        NEXT iLoop
        FOR iLoop = 1 TO cMaxAxis
            arrAxisNew(iPlayerLoop, iLoop) = TRUE
        NEXT iLoop
    NEXT iPlayerLoop

    ' CLEAR THE SCREEN
    DO
        FOR iPlayerLoop = 1 TO iNumPlayers
            iDevice = iPlayerLoop + 2

            WHILE _DEVICEINPUT(iDevice) ' clear and update the device buffer
                'IF _DEVICEINPUT = 3 THEN ' this says we only care about joystick input values

                ' check all the buttons
                FOR iLoop = 1 TO _LASTBUTTON(iDevice)
                    IF (iLoop > cMaxButtons) THEN
                        EXIT FOR
                    END IF
                    arrPlayer(iPlayerLoop).buttonCount = iLoop

                    ' update button array to indicate if a button is up or down currently.
                    IF _BUTTONCHANGE(iLoop) THEN
                        ' _BUTTON(number) returns -1 when a button is pressed and 0 when released.
                        'arrButton(iLoop) = NOT arrButton(iLoop)
                        arrButton(iPlayerLoop, iLoop) = _BUTTON(iLoop)
                    END IF

                    ' SAVE MINIMUM VALUE
                    IF arrButton(iPlayerLoop, iLoop) < arrButtonMin(iPlayerLoop, iLoop) THEN
                        arrButtonMin(iPlayerLoop, iLoop) = arrButton(iPlayerLoop, iLoop)

                        ' INITIALIZE THE MAX TO THE MINIMUM VALUE
                        IF arrButtonNew(iPlayerLoop, iLoop) = TRUE THEN
                            arrButtonMax(iPlayerLoop, iLoop) = arrButtonMin(iPlayerLoop, iLoop)
                            arrButtonNew(iPlayerLoop, iLoop) = FALSE
                        END IF

                    END IF

                    ' SAVE MAXIMUM VALUE
                    IF arrButton(iPlayerLoop, iLoop) > arrButtonMax(iPlayerLoop, iLoop) THEN
                        arrButtonMax(iPlayerLoop, iLoop) = arrButton(iPlayerLoop, iLoop)
                    END IF

                NEXT iLoop

                FOR iLoop = 1 TO _LASTAXIS(iDevice) ' this loop checks all my axis
                    IF (iLoop > cMaxAxis) THEN
                        EXIT FOR
                    END IF
                    arrPlayer(iPlayerLoop).axisCount = iLoop

                    ' I like to give a little "jiggle" resistance to my controls, as I have an old joystick
                    ' which is prone to always give minute values and never really center on true 0.
                    ' A value of 1 means my axis is pushed fully in one direction.
                    ' A value greater than 0.1 means it's been partially pushed in a direction (such as at a 45 degree diagional angle).
                    ' A value of less than 0.1 means we count it as being centered. (As if it was 0.)
                    'IF ABS(_AXIS(iLoop)) <= 1 AND ABS(_AXIS(iLoop)) >= .1 THEN
                    IF ABS(_AXIS(iLoop)) <= 1 AND ABS(_AXIS(iLoop)) >= .001 THEN
                        arrAxis(iPlayerLoop, iLoop) = _AXIS(iLoop)
                    ELSE
                        arrAxis(iPlayerLoop, iLoop) = 0
                    END IF

                    ' SAVE MINIMUM VALUE
                    IF arrAxis(iPlayerLoop, iLoop) < arrAxisMin(iPlayerLoop, iLoop) THEN
                        arrAxisMin(iPlayerLoop, iLoop) = arrAxis(iPlayerLoop, iLoop)

                        ' INITIALIZE THE MAX TO THE MINIMUM VALUE
                        IF arrAxisNew(iPlayerLoop, iLoop) = TRUE THEN
                            arrAxisMax(iPlayerLoop, iLoop) = arrAxisMin(iPlayerLoop, iLoop)
                            arrAxisNew(iPlayerLoop, iLoop) = FALSE
                        END IF

                    END IF

                    ' SAVE MAXIMUM VALUE
                    IF arrAxis(iPlayerLoop, iLoop) > arrAxisMax(iPlayerLoop, iLoop) THEN
                        arrAxisMax(iPlayerLoop, iLoop) = arrAxis(iPlayerLoop, iLoop)
                    END IF

                NEXT iLoop
            WEND ' clear and update the device buffer

        NEXT iPlayerLoop

        ' And below here is just the simple display routine which displays our values.
        ' If this was for a game, I'd choose something like arrAxis(1) = -1 for a left arrow style input,
        ' arrAxis(1) = 1 for a right arrow style input, rather than just using _KEYHIT or INKEY$.
        CLS
        PRINT "Game controller test program."
        PRINT "This program is free to use and distribute per GNU GPLv3 license."
        PRINT "Tests up to 4 controllers with 2 axes / 2 buttons each."
        PRINT "Plug in controllers and move them & press buttons."
        PRINT "-------------------------------------------------------------------------------"
        FOR iPlayerLoop = 1 TO iNumPlayers
            FOR iLoop = 1 TO arrPlayer(iPlayerLoop).axisCount ' A loop for each axis
                ' display their status to the screen
                PRINT "Player " + cstr$(iPlayerLoop);

                strValue = FormatNumber$(arrAxis(iPlayerLoop, iLoop), iDigits)
                PRINT ",   Axis " + strValue;

                strValue = FormatNumber$(arrAxisMin(iPlayerLoop, iLoop), iDigits)
                PRINT ", Min=" + strValue;

                strValue = FormatNumber$(arrAxisMax(iPlayerLoop, iLoop), iDigits)
                PRINT ", Max=" + strValue;

                PRINT
            NEXT iLoop
            FOR iLoop = 1 TO arrPlayer(iPlayerLoop).buttonCount ' A loop for each button
                ' display their status to the screen
                PRINT "Player " + cstr$(iPlayerLoop);

                strValue = FormatNumber$(arrButton(iPlayerLoop, iLoop), iDigits)
                PRINT ", Button " + strValue;

                strValue = FormatNumber$(arrButtonMin(iPlayerLoop, iLoop), iDigits)
                PRINT ", Min=" + strValue;

                strValue = FormatNumber$(arrButtonMax(iPlayerLoop, iLoop), iDigits)
                PRINT ", Max=" + strValue;

                PRINT
            NEXT iLoop
        NEXT iPlayerLoop
        PRINT "-------------------------------------------------------------------------------"
        PRINT "PRESS <ESC> TO EXIT"

        _LIMIT 30
    LOOP UNTIL _KEYHIT = 27 ' ESCAPE to quit
END SUB ' TestJoysticks

' ################################################################################################################################################################
' BEGIN GENERAL ROUTINES
' ################################################################################################################################################################

' /////////////////////////////////////////////////////////////////////////////

FUNCTION FormatNumber$ (myValue, iDigits AS INTEGER)
    DIM strValue AS STRING
    strValue = StrNum$(myValue) + STRING$(iDigits, " ")
    IF myValue < 1 THEN
        IF myValue < 0 THEN
            strValue = Replace$(strValue, "-.", "-0.")
        ELSEIF myValue > 0 THEN
            strValue = "0" + strValue
        END IF
    END IF
    FormatNumber$ = LEFT$(strValue, iDigits)
END FUNCTION ' FormatNumber$

' /////////////////////////////////////////////////////////////////////////////

FUNCTION cstr$ (myValue)
    'cstr$ = LTRIM$(RTRIM$(STR$(myValue)))
    cstr$ = _TRIM$(STR$(myValue))
END FUNCTION ' cstr$

' /////////////////////////////////////////////////////////////////////////////

FUNCTION cstrl$ (myValue AS LONG)
    cstrl$ = _TRIM$(STR$(myValue))
END FUNCTION ' cstrl$

' /////////////////////////////////////////////////////////////////////////////

FUNCTION IIF (Condition, IfTrue, IfFalse)
    IF Condition THEN IIF = IfTrue ELSE IIF = IfFalse
END FUNCTION

' /////////////////////////////////////////////////////////////////////////////

FUNCTION IIFSTR$ (Condition, IfTrue$, IfFalse$)
    IF Condition THEN IIFSTR$ = IfTrue$ ELSE IIFSTR$ = IfFalse$
END FUNCTION

' /////////////////////////////////////////////////////////////////////////////
' FROM: String Manipulation
' http://www.qb64.net/forum/index_topic_5964-0/
'
'SUMMARY:
'   Purpose:  A library of custom functions that transform strings.
'   Author:   Dustinian Camburides (dustinian@gmail.com)
'   Platform: QB64 (www.qb64.net)
'   Revision: 1.6
'   Updated:  5/28/2012

'SUMMARY:
'[Replace$] replaces all instances of the [Find] sub-string with the [Add] sub-string within the [Text] string.
'INPUT:
'Text: The input string; the text that's being manipulated.
'Find: The specified sub-string; the string sought within the [Text] string.
'Add: The sub-string that's being added to the [Text] string.

FUNCTION Replace$ (Text1 AS STRING, Find1 AS STRING, Add1 AS STRING)
    ' VARIABLES:
    DIM Text2 AS STRING
    DIM Find2 AS STRING
    DIM Add2 AS STRING
    DIM lngLocation AS LONG ' The address of the [Find] substring within the [Text] string.
    DIM strBefore AS STRING ' The characters before the string to be replaced.
    DIM strAfter AS STRING ' The characters after the string to be replaced.

    ' INITIALIZE:
    ' MAKE COPIESSO THE ORIGINAL IS NOT MODIFIED (LIKE ByVal IN VBA)
    Text2 = Text1
    Find2 = Find1
    Add2 = Add1

    lngLocation = INSTR(1, Text2, Find2)

    ' PROCESSING:
    ' While [Find2] appears in [Text2]...
    WHILE lngLocation
        ' Extract all Text2 before the [Find2] substring:
        strBefore = LEFT$(Text2, lngLocation - 1)

        ' Extract all text after the [Find2] substring:
        strAfter = RIGHT$(Text2, ((LEN(Text2) - (lngLocation + LEN(Find2) - 1))))

        ' Return the substring:
        Text2 = strBefore + Add2 + strAfter

        ' Locate the next instance of [Find2]:
        lngLocation = INSTR(1, Text2, Find2)

        ' Next instance of [Find2]...
    WEND

    ' OUTPUT:
    Replace$ = Text2
END FUNCTION ' Replace$

' /////////////////////////////////////////////////////////////////////////////
' Scientific notation - QB64 Wiki
' https://www.qb64.org/wiki/Scientific_notation

' Example: A string function that displays extremely small or large exponential decimal values.

FUNCTION StrNum$ (n#)
    value$ = UCASE$(LTRIM$(STR$(n#)))
    Xpos% = INSTR(value$, "D") + INSTR(value$, "E") 'only D or E can be present
    IF Xpos% THEN
        expo% = VAL(MID$(value$, Xpos% + 1))
        IF VAL(value$) < 0 THEN
            sign$ = "-": valu$ = MID$(value$, 2, Xpos% - 2)
        ELSE valu$ = MID$(value$, 1, Xpos% - 1)
        END IF
        dot% = INSTR(valu$, "."): L% = LEN(valu$)
        IF expo% > 0 THEN add$ = STRING$(expo% - (L% - dot%), "0")
        IF expo% < 0 THEN min$ = STRING$(ABS(expo%) - (dot% - 1), "0"): DP$ = "."
        FOR n = 1 TO L%
            IF MID$(valu$, n, 1) <> "." THEN num$ = num$ + MID$(valu$, n, 1)
        NEXT
    ELSE StrNum$ = value$: EXIT FUNCTION
    END IF
    StrNum$ = _TRIM$(sign$ + DP$ + min$ + num$ + add$)
END FUNCTION ' StrNum$

' ################################################################################################################################################################
' END GENERAL ROUTINES
' ################################################################################################################################################################

