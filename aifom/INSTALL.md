## SETUP ##

On Mac, you only need to include the src/ subfolder of the
distribution into your `$PATH`, as it already contains the Mac executable
"aifomega" and the script `prow`.

Otherwise, you need the Haskell platform (see haskell.org ) for
compiling AIF-omega from source. 

To compile using the Makefile, go to the src/ subfolder and run
`make`. Please add the `src/` folder to your `$PATH`. 

Alternatively, one can compile with Cabal (included in the Haskell
platform): run `cabal build` and then `cabal install`. (Make sure that
your local cabal binary folder (usually `$HOME/.cabal/bin`) is in the
`$PATH` variable.)

The tool currently generates output only for ProVerif, which you
should also install, see:
   `http://prosecco.gforge.inria.fr/personal/bblanche/proverif/`

## USAGE ##

You can run the tool directly by calling `aifomega model.aifom` where
`model.aifom` is your AIF-omega specification, producing the ProVerif
translation on standard out.

Normally you can work with the script `prow` (in the `src/`
folder). This script assumes `aifomega` is available as well as
ProVerif under the command `proverif`. Running `prow model.aifom` on
an AIF-omega specification `model.aifom` will first run the AIF-omega
translator, store the result in `tmp.prv`, then run ProVerif, store
the transcript in `tmp.log`, and extract the result from it (either
that attack is reachable or unreachable).

You can also run a small testsuite in the folder `examples/` with the
command `make`.

Please contact us for questions, bug reports, feedback:
- Alessandro Bruni brun@itu.dk
- Sebastian Moedersheim samo@dtu.dk
