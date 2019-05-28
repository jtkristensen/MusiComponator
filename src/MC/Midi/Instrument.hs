{-|
  Module      : MC.Midi.Instrument
  Description : The General MIDI (GM) tone-bank settings.
  Copyright   : (c) Joachim Tilsted Kristensen, 2019
  License     : BSD3
  Maintainer  : tilsted@di.ku.dk
  Stability   : experimental
  Portability : POSIX
-}

module MC.Midi.Instrument where

import MC.Midi.Core

-- ----------------------------------------------------------
-- Piano
-- ----------------------------------------------------------
acousticGrandPiano  = Piano 0x00
brightAcousticPiano = Piano 0x01
electricGrandPiano  = Piano 0x02
honkyTonkPiano      = Piano 0x03
electricPiano1      = Piano 0x04
electricPiano2      = Piano 0x05
harpsichord         = Piano 0x06
clavinet            = Piano 0x07

-- ----------------------------------------------------------
-- Chromatic percussion.
-- ----------------------------------------------------------
celesta             = Piano 0x08
glockenspiel        = Piano 0x09
musicBox            = Piano 0x0a
vibraphone          = Piano 0x0b
marimba             = Piano 0x0c
xylophone           = Piano 0x0d
tubularBells        = Piano 0x0e
dulcimer            = Piano 0x0f

-- ----------------------------------------------------------
-- Organ.
-- ----------------------------------------------------------
drawbarOrgan        = Piano 0x10
percussiveOrgan     = Piano 0x11
rockOrgan           = Piano 0x12
churchOrgan         = Piano 0x13
reedOrgan           = Piano 0x14
accordion           = Piano 0x15
harmonica           = Piano 0x16
tangoAccordion      = Piano 0x17

-- ----------------------------------------------------------
-- Guitar.
-- ----------------------------------------------------------
acousticGuitarNylon = Piano 0x18
acousticGuitarSteel = Piano 0x19
electricGuitarJazz  = Piano 0x1a
electricGuitarClean = Piano 0x1b
electricGuitarMuted = Piano 0x1c
overdrivenGuitar    = Piano 0x1d
distortionGuitar    = Piano 0x1e
guitarHarmonics     = Piano 0x1f

-- ----------------------------------------------------------
-- Bass.
-- ----------------------------------------------------------
acousticBass        = Piano 0x20
electricBassFinger  = Piano 0x21
electricBassPick    = Piano 0x22
fretlessBass        = Piano 0x23
slapBass1           = Piano 0x24
slapBass2           = Piano 0x25
synthBass1          = Piano 0x26
synthBass2          = Piano 0x27

-- ----------------------------------------------------------
-- Strings.
-- ----------------------------------------------------------
violin              = Piano 0x28
viola               = Piano 0x29
cello               = Piano 0x2a
contrabass          = Piano 0x2b
tremoloStrings      = Piano 0x2c
pizzicatoStrings    = Piano 0x2d
orchestralHarp      = Piano 0x2e
timpani             = Piano 0x2f

-- ----------------------------------------------------------
-- Ensemble.
-- ----------------------------------------------------------

stringEnsemble1     = Piano 0x30
stringEnsemble2     = Piano 0x31
synthStrings1       = Piano 0x32
synthStrings2       = Piano 0x33
choirAahs           = Piano 0x34
voiceOohs           = Piano 0x35
synthChoir          = Piano 0x36
orchestraHit        = Piano 0x37

-- ----------------------------------------------------------
-- Brass
-- ----------------------------------------------------------

trumpet             = Piano 0x38
trombone            = Piano 0x39
tuba                = Piano 0x3a
mutedTrumpet        = Piano 0x3b
frenchHorn          = Piano 0x3c
brassSection        = Piano 0x3d
synthBrass1         = Piano 0x3e
synthBrass2         = Piano 0x3f

-- ----------------------------------------------------------
-- Reed
-- ----------------------------------------------------------

sopranoSax          = Piano 0x40
altoSax             = Piano 0x41
tenorSax            = Piano 0x42
baritoneSax         = Piano 0x43
oboe                = Piano 0x44
englishHorn         = Piano 0x45
bassoon             = Piano 0x46
clarinet            = Piano 0x47

-- ----------------------------------------------------------
-- Pipe
-- ----------------------------------------------------------

piccolo             = Piano 0x48
flute               = Piano 0x49
recorder            = Piano 0x4a
panFlute            = Piano 0x4b
blownBottle         = Piano 0x4c
shakuhachi          = Piano 0x4d
whistle             = Piano 0x4e
ocarina             = Piano 0x4f

-- ----------------------------------------------------------
-- Synth Lead
-- ----------------------------------------------------------

leadSquare          = Piano 0x50
leadSawtooth        = Piano 0x51
leadCalliope        = Piano 0x52
leadChiff           = Piano 0x53
leadCharang         = Piano 0x54
leadVoice           = Piano 0x55
leadFifths          = Piano 0x56
leadBassLead        = Piano 0x57

-- ----------------------------------------------------------
-- Synth Pad
-- ----------------------------------------------------------

padNewAge           = Piano 0x58
padWarm             = Piano 0x59
padPolysynth        = Piano 0x5a
padChoir            = Piano 0x5b
padBowed            = Piano 0x5c
padMetallic         = Piano 0x5d
padHalo             = Piano 0x5e
padSweep            = Piano 0x5f

-- ----------------------------------------------------------
-- Synth Effects
-- ----------------------------------------------------------

fxRain              = Piano 0x60
fxSoundtrack        = Piano 0x61
fxCrystal           = Piano 0x62
fxAtmosphere        = Piano 0x63
fxBrightness        = Piano 0x64
fxGoblins           = Piano 0x65
fxEchoes            = Piano 0x66
fxSciF              = Piano 0x67

-- ----------------------------------------------------------
-- Ethnic
-- ----------------------------------------------------------

sitar               = Piano 0x68
banjo               = Piano 0x69
shamisen            = Piano 0x6a
koto                = Piano 0x6b
kalimba             = Piano 0x6c
bagpipe             = Piano 0x6d
fiddle              = Piano 0x6e
shanai              = Piano 0x6f

-- ----------------------------------------------------------
-- Percussive
-- ----------------------------------------------------------

tinkleBell          = Piano 0x70
agogo               = Piano 0x71
steelDrums          = Piano 0x72
woodblock           = Piano 0x73
taikoDrum           = Piano 0x74
melodicTom          = Piano 0x75
synthDrum           = Piano 0x76
reverseCymbal       = Piano 0x77

-- ----------------------------------------------------------
-- Sound effects
-- ----------------------------------------------------------

guitarFretNoise     = Piano 0x78
breathNoise         = Piano 0x79
seashore            = Piano 0x7a
birdTweet           = Piano 0x7b
telephoneRing       = Piano 0x7c
helicopter          = Piano 0x7d
applause            = Piano 0x7e
gunshot             = Piano 0x7f

-- ----------------------------------------------------------
-- Percussion
-- ----------------------------------------------------------

acousticBassDrum  = flip Percussion $ 0x23
bassDrum          = flip Percussion $ 0x24
sideStickRimshot  = flip Percussion $ 0x25
acousticSnare     = flip Percussion $ 0x26
handClap          = flip Percussion $ 0x27
electricSnare     = flip Percussion $ 0x28
lowFloorTom       = flip Percussion $ 0x29
closedHihat       = flip Percussion $ 0x2a
highFloorTom      = flip Percussion $ 0x2b
pedalHihat        = flip Percussion $ 0x2c
lowTom            = flip Percussion $ 0x2d
openHihat         = flip Percussion $ 0x2e
lowMidTom         = flip Percussion $ 0x2f
hiMidTom          = flip Percussion $ 0x30
crashCymbal1      = flip Percussion $ 0x31
highTom           = flip Percussion $ 0x33
rideCymbal1       = flip Percussion $ 0x34
chineseCymbal     = flip Percussion $ 0x35
rideBell          = flip Percussion $ 0x36
-- todo .. (add more percussion instruments).
