{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Language.Hakaru.Syntax.Prelude
import Language.Hakaru.CodeGen.Wrapper
import Language.Hakaru.CodeGen.CodeGenMonad
import Language.Hakaru.CodeGen.AST
import Language.Hakaru.CodeGen.Pretty
import Language.Hakaru.Syntax.AST
import Language.Hakaru.Syntax.ABT
import Language.Hakaru.Evaluation.ConstantPropagation
import Language.Hakaru.Syntax.AST.Transforms (expandTransformations, optimizations)
import Language.Hakaru.Syntax.TypeOf (typeOf)
import Language.Hakaru.Syntax.TypeCheck
import Language.Hakaru.Types.DataKind
import Language.Hakaru.Summary
import Text.PrettyPrint (render)
import Data.Text hiding (foldr)
import qualified Data.Text.IO as IO


main :: IO ()
main = compileHakaru bucketFanout2 "foo.c"

bucketAdd :: TrivialABT Term '[] 'HNat
bucketAdd = triv $ bucket (nat_ 0) (nat_ 10) (r_add (const (nat_ 1)))

bucketNoOp :: TrivialABT Term '[] HUnit
bucketNoOp = triv $ bucket (nat_ 0) (nat_ 10) r_nop

bucketFanout :: TrivialABT Term '[] (HPair 'HNat 'HNat)
bucketFanout = triv $ bucket (nat_ 0) (nat_ 10)
  (r_fanout (r_add (const (nat_ 1)))
            (r_add (const (nat_ 2))))

bucketFanout2 :: TrivialABT Term '[] (HPair 'HNat (HPair 'HNat 'HNat))
bucketFanout2 = triv $ bucket (nat_ 0) (nat_ 10)
  (r_fanout (r_add (const (nat_ 1)))
            (r_fanout (r_add (const (nat_ 2)))
                      (r_add (const (nat_ 3)))))

bucketSplit :: TrivialABT Term '[] (HPair 'HNat 'HNat)
bucketSplit = triv $ bucket (nat_ 0) (nat_ 10)
  (r_split (const true)
           (r_add (const (nat_ 1)))
           (r_add (const (nat_ 2))))


compileHakaru
  :: TrivialABT Term '[] a
  -> FilePath
  -> IO ()
compileHakaru abt outFile = do
  let ast' = TypedAST (typeOf abt) $ foldr id abt abtPasses
      codeGen = wrapProgram ast' Nothing (PrintConfig True True)
      codeGenConfig = emptyCG
      cast = CAST $ runCodeGenWith codeGen codeGenConfig
      output  = pack . render . pretty $ cast
  IO.writeFile "foo.c" output
  where abtPasses = [ expandTransformations
                    , constantPropagation
                    -- , optimizations
                    ]
