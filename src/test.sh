
rm -f test.mid
stack runhaskell -- ./MusiCompoNator/Songbook/Twinkle.hs
fluidsynth --verbose --audio-driver=alsa -o audio.alsa.device=hw:0 /usr/share/sounds/sf2/FluidR3_GM.sf2 test.mid
