set TONTO="dist/GoGuiCliente.exe"
set DUMMY="dist/gogui-dummy.exe"
set GNU="""dist/gnugo.exe --mode gtp"""
set BLACK=%DUMMY%
set WHITE=%TONTO%
set BOARDSIZE=5
set TWOGTP="gogui-twogtp.exe  -black %BLACK% -white %WHITE% -games 2 -size %BOARDSIZE% -alternate -sgffile pruebadat1"
gogui.exe -size %BOARDSIZE% -program %TWOGTP% -computer-both -auto 
