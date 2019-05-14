
import MusiCompoNator.Core
import MusiCompoNator.Composition
import MusiCompoNator.GeneralMidi
import System.Process


main :: IO ()
main =
  do putStrLn "\nhello !"

-- writeMidi "./test/out/main.mid" file
-- system "fluidsynth --verbose --audio-driver=alsa -o audio.alsa.device=hw:0 /usr/share/sounds/sf2/FluidR3_GM.sf2 ./test/out/main.mid"
-- return ()
