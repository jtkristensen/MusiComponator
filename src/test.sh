# usage : ./test.sh ./Samples/songname.mid

stack build

rm -f ./Samples/*.mid
stack runhaskell -- ./MC/Songbook/Twinkle.hs
stack runhaskell -- ./MC/Songbook/PopCorn.hs
stack runhaskell -- ./MC/Songbook/AllTheThingsYouAre.hs
stack runhaskell -- ./MC/Songbook/DillaFeel101.hs
mv *.mid ./Samples

fluidsynth --verbose --audio-driver=alsa -o audio.alsa.device=hw:0 /usr/share/sounds/sf2/FluidR3_GM.sf2 $1
