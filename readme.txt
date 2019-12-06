Lizard Music Engine
===================

This is an NES music engine derived from the game Lizard.
http://lizardnes.com

This code is released under the Creative Commons Attribution License (CC BY 4.0)
For more details, and for more Lizard source code, see:
https://github.com/bbbradsmith/lizard_src_demo

The music contained in the music/ folder is the exclusive right of its author,
and may not be reused or redistributed in any modified form.
It is provided here for example only.
Projects that use this engine must provide their own new music to replace it.

A summary of the CC BY 4.0 license and more information is available at:
https://creativecommons.org/licenses/by/4.0/

This project may be supported on Patreon:
https://www.patreon.com/rainwarrior


Prerequisites:
- Famitracker 0.4.6: http://famitracker.com/
- Python 3: https://www.python.org/
- CC65: https://cc65.github.io/


To build the music data:

1. Place FamiTracker.exe (0.4.6) in this folder
2. Plase music FTM files in the music/ folder.
   The lowest file alphabetically should be a silent FTM, and will play on startup.
3. Plase sound effect FTM files in the sfx/ folder.
4. Run export_music.py to build the exported music data.

Music can use only the following effects:
   Bxx (looping)
   D00 (variable pattern length)
   F0x (speed change)
Volume column is supported.
Music must use tempo 150. Only the speed setting may change.
Hi-Pitch macros are not supported.
Note release macros are not supported.
DPCM is not supported.
Using pitch and arpeggio macros simultaneously may have a different result than Famitracker.

The maximum number of empty rows between events in a pattern is 127.
Empty patterns longer than 127 rows may need to be broken up with extra events
to avoid a "too many skipped rows" error.

Sound effects must be created with speed 1.
Can only use one channel, either the first square, or the noise channel.
Will end when a note cut is reached.
Volume column is allowed.
Vxx is allowed, but no other effects are.

A playing sound effect will cancel any current music note on the same channel,
and play instead of music for its duration of effect.
The music will resume on that channel at the next note.

This default behaviour can be changed by assembling music.s with: -D SFX_NO_HALT
The no-halt version instead will not cancel any playing note on the channel,
and immediately returns to the in-progress note whenever the sound effect finishes.


To use the music engine:

1. Export music data with music_export.py
2. Build music.s with ca65 and include its object in your link.
3. Titles from the FTM files will become MUSIC_[TITLE] and SOUND_[TITLE] enums in output\data_music_enums.inc
4. Include music.inc in your project's assembly files that need to interface with the music engine.
5. Build and link output\data_music.s into your project.
6. Set player_pal to 1 if this is a 50Hz system.
7. Call music_init at startup.
8. Call music_tick at end of NMI.

To play music:
	lda #MUSIC_[TITLE]
	sta player_music_next

To play a sound (immediate):
	; this macro will clobber A
	PLAY_SOUND SOUND_[TITLE]

Write 1 to player_pause to temporarily pause music. 0 to resume.

Four SEGMENTs are required in your cc65 linker config:
* ZEROPAGE for zero page variables
* RAM for other variables
* CODE to place the music player code
* MUSIC to place the exported music data

These can be renamed in music.s if other segment usage is preferred.


To test the music engine with its demo:

First download CC65 and unzip it into demo/cc65/

1. Export music data with music_export.py
2. Run demo/build_demo.bat to rebuild output/demo.nes
3. Run output/demo.nes to use the demo.

Direction pad will select music/sound or pause.
Pressing A will begin the selected music/sound, or pause/unpause.
Holding SELECT will use a grey line to indicate visually how long music_tick took that frame.


Notes:

2152 bytes of code and note/volume tables
22 bytes of zeropage
105 bytes of other RAM
~1830 cycles per most frames
~2500 cycles peak
~4000 cycles if a new music is loaded

(Optionally could place all variables on ZP, saves about 300 bytes of code and ~100 cycles per frame.)

Music and SFX enumerations will be created in alphabetical order.
Music begins counting at 0, SFX at 1. (A sound called "NONE" is automatically SFX 0.)
To manually control the order, prefixing the filenames with a number may help.

An additional output/data_music_strings.inc is generated with the names, which is used for the demo,
but may have other diagnostic utility.

To play a sound with a runtime SFX index rather than an immediate constant,
you may need to generate a small table to resolve whether each SFX is a square or noise sound.
See "sfx_type" in demo.s for an example of how to do this.


Version 1
