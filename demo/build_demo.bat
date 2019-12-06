@del ..\output\*.o
@del ..\output\*.nes
@del ..\output\*.dbg
@del ..\output\*.map

cc65\bin\ca65 -o ..\output\demo.o -g demo.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ca65 -o ..\output\music.o -g ..\music.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o ..\output\demo.nes -m ..\output\demo.map --dbgfile ..\output\demo.dbg -C demo.cfg ..\output\demo.o ..\output\music.o
@IF ERRORLEVEL 1 GOTO error

@echo.
@echo.
@echo Build successful!
@pause
@GOTO end
:error
@echo.
@echo.
@echo Build error!
@pause
:end