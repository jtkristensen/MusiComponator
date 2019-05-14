
module MusiCompoNator.Songbook.Twinkle where

import MusiCompoNator.Composition
import MusiCompoNator.Core
import MusiCompoNator.GeneralMidi
import ZMidi.Core

melody :: Voice ()
melody = singleV $ a <> b <> c <> c <> a <> b
  where a = line [i,   i,    v,   v,  vi,  vi,  v] :<: r -- Twinkle twinkle ..
        b = line [iv, iv,  iii, iii,  ii,  ii,  i] :<: r -- How I wonder ..
        c = line [ v,  v,   iv,  iv, iii, iii, ii] :<: r -- Up above the ..
        r = qns 6 <> hns 1

composition :: MidiComposition ()
composition = do
  midiVoice (Piano 0x00) melody
  closeMC

main :: IO ()
main = do
  (f, ()) <- return $ runMidiComposition (ionian c) composition
  writeMidi "./test.mid" f
