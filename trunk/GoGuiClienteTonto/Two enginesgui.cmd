set TONTO="misc/GoGuiCliente.exe"
set DUMMY="misc/gogui-dummy.exe"
set GNU="""misc/gnugo.exe --mode gtp"""
set BLACK=%DUMMY%
set WHITE=%TONTO%
set BOARDSIZE=5
set TWOGTP="gogui-twogtp.exe  -black %BLACK% -white %WHITE% -games 2 -size %BOARDSIZE% -alternate -force -xml -sgffile pruebadat1"
gogui.exe -size %BOARDSIZE% -program %TWOGTP% -computer-both -auto
