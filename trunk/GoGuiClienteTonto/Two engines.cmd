REM Windows CMD Script to use the GoGUI graphical GTP interface to allow two separate GTP engines to play against each other.
REM In this case, the fuego 1.0 engine is being used, and it is competing against itself.  One instance is using a custom configuration files and one instance is
REM using the defaults for everything.

REM Define the parameters used to open the twogtp program within the gogui program.
set JAVA=C:Program Files (x86)Javajre6binjavaw.exe
set DESKTOP_PATH=C:UsersEro-SenninDesktop
set GOGUI=%DESKTOP_PATH%Go_Programminggogui-1.2.3libgogui.jar
set TWOGTP_JAR=%DESKTOP_PATH%Go_Programminggogui-1.2.3libgogui-twogtp.jar
set SAVED_GAME_FILE_PREFIX=fuego-vs-conffuego
set GAMES=10
set TIME_MINUTES=5
set BOARDSIZE=9
set KOMI=6.5
REM fuego.exe configuration file argument --config fuego_config.ini will affect BOTH copies of the engine even though they are completely separate EXE files.
set BLACK="%DESKTOP_PATH%fuego-1.0mingwfuegomainfuego.exe"
set WHITE="""%DESKTOP_PATH%fuegoreleasefuego-vs2010.exe --config fuego_config.ini"""
set TWOGTP="%JAVA% -jar %TWOGTP_JAR% -komi %KOMI% -size %BOARDSIZE% -black %BLACK% -white %WHITE% -time %TIME_MINUTES% -verbose -alternate -games %GAMES% -sgffile %SAVED_GAME_FILE_PREFIX%"

REM Open the program using Java Runtime Environment for Windows (javaw.exe).  Note that this command actually passes a javaw.exe instance argument to ITSELF,
REM because both gogui and gogui-twogtp are independent Java programs.
"%JAVA%" -jar "%GOGUI%" -komi %KOMI% -time %TIME_MINUTES% -computer-both -auto -program %TWOGTP%

echo %errorlevel%

REM Analyze/Process the saved game files and generate an HTML summary file.
"%JAVA%" -jar %TWOGTP_JAR% -analyze %SAVED_GAME_FILE_PREFIX%.dat