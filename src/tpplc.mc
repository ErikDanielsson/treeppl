/-
TreePPL Compiler command line
-/

-- (vsenderov, 2923-06-16 I don't remeber what are all the includes for any more;
-- perhaps we should test and see if we need all of them)
include "sys.mc"

include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type-check.mc"
include "mexpr/generate-json-serializers.mc"

include "treeppl-to-coreppl/compile.mc"

include "coreppl::dppl-arg.mc" -- inherit cmd-line opts from cppl
include "coreppl::parser.mc"




mexpr

use TreePPLThings in

let fancyInference : OptParser (ModelOptionsTemp, Type -> Loader -> (Loader, InferMethod)) =
  let mk = lam. lam thinPeriod. lam incrementalPrinting. lam globalProb. lam iterations. lam outputType. lam loader.
    match includeFileExn "." "stdlib::json.mc" loader with (jsonEnv, loader) in
    match serializationPairsFor [outputType] loader with (loader, [outputSer]) in
    let keepSample = if incrementalPrinting
      then ulam_ "" false_
      else if eqi 1 thinPeriod
        then ulam_ "" true_
        else ulam_ "idx" (eqi_ (int_ 0) (modi_ (var_ "idx") (int_ thinPeriod))) in
    let continue =
      let ret = utuple_ [addi_ (var_ "idx") (int_ 1), neqi_ (var_ "idx") (int_ iterations)] in
      let ret =
        if incrementalPrinting then
          let print = app_ (nvar_ (_getVarExn "printJsonLn" jsonEnv)) (app_ outputSer.serializer (var_ "sample")) in
          let print = if eqi thinPeriod 1
            then print
            else if_ (eqi_ (int_ 0) (modi_ (var_ "idx") (int_ thinPeriod))) print unit_ in
          semi_ print ret
        else ret in
      utuple_ [int_ 0, ulam_ "idx" (ulam_ "sample" ret)] in
    (loader, LightweightMCMC {keepSample = keepSample, continue = continue, globalProb = float_ globalProb}) in
  let m = _methodFlag false "mcmc-lightweight-temp" in
  let thinPeriod =
    let default = 1 in
    let opt = optArg
      { optArgDefInt with long = "thin-period"
      , description = concat "If N above 1, keep only every Nth sample. Defaults to " (int2string default)
      , arg = "N"
      } in
    optOr opt (optPure default) in
  let incrementalPrinting = optFlag
    { optFlagDef with long = "incremental-printing"
    , description = "Print samples as they are produced, one json value per line"
    } in
  optMap2 (lam a. lam b. (b, a)) (optMap5 mk m thinPeriod incrementalPrinting _mcmcLightweightGlobalProb _particles) mcmcLightweightOptions in

let useDefault : OptParser (Type -> Loader -> (Loader, InferMethod)) =
  optPure (lam. lam loader. (loader, Default {runs = int_ 5000})) in

let inference : OptParser (ModelOptionsTemp, Type -> Loader -> (Loader, InferMethod)) =
  let default = optMap2 (lam a. lam b. (a, b)) inferenceMethodOptions useDefault in
  optOr fancyInference default in

let options : OptParser (String, Options, Type -> Loader -> (Loader, InferMethod)) =
  let mk = lam frontend. lam transformations. lam pair.
    ( { frontend = frontend
      , cpplFiles = {dpplTypeCheck = false, printSamples = false, printAcceptanceRate = false}
      , transformations = {transformations with defaultMethod = pair.0}
      }
    , pair.1
    ) in
  let separated = optMap3 mk frontendOptions (transformationOptions (optPure _modelOptionsTempDefault)) inference in
  let oldStyle = optMap
    (lam x. match mkBackcompatOptions x.0 with (filename, options) in (filename, options, x.1))
    separated in
  oldStyle in

match optParseWithHelp "tpplc" "" options (tail argv)
  with (filename, options, mkInferenceMethod) in
compileTpplToExecutable filename options mkInferenceMethod
