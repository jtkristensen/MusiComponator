MC-0.1.0.0
==========

## Welcome to the MC GitHub page.

MC is a domain specific programming language for music composition.
This repository contains an embeded implementation in the Haskell programming
language.

## How do I get started?

Assuming that you are on a Debian-based linux distro, you can follow the
list below:

 1. Check that your machine has a group for playing audio by typing:
    > `grep audio /etc/group`
    look for something that looks like:
    > `audio:x:29:pulse`
    Add yourself to the group by typing :
    > `sudo gpasswd -a` **yourUserName** `audio`
 2. Install a music-synthesis or sequencer program (I suggest fluidsynth).
    > `sudo apt-get install fluidsynth`
    > `sudo apt-get install fluid-soundfont-gm`
    > `sudo apt-get install jackd2`
    > `sudo apt-get install alsa`
 3. Install a Haskell and Stack.
    > `sudo apt-get install ghc
    > `sudo apt-get install stack
 4. Compile the project, and produce the documentation, by standing in this folder
    and typing:
    > stack build; stack haddock
 5. Now you are now ready to use the library, for examples go to the subdirectory
    src/MC/Songbook/. To run one of the programs, do
    > stack runhaskell -- path/to/the/program.hs
 6. Running a program will produce one or more midi files in the folder
    in which you are standing.
    If you followed the instructions above, you will have a MIDI sound font
    installed, go a head and check that the file is present at
    > `/usr/share/sounds/sf2/FluidR3_GM.sf2`
    To play a midi-file with fluid-synth, type
    > `fluidsynth --verbose --audio-driver=alsa -o audio.alsa.device=hw:0 /path/to/sound/font.sf2 ./path/to/midi/file.mid
 7. For more help, find the documentation in the subdirectory
    > `.stack-work/dist/x86_64-linux/Cabal-v.e.r.s.i.o.n/doc
 8. Enjoy.