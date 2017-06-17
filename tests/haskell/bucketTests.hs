{-# LANGUAGE DataKinds,
             FlexibleContexts,
             TypeOperators #-}

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
main = do
  compileHakaru bucketAdd "bucketAdd.c"
  compileHakaru bucketNoOp "bucketNoOp.c"
  compileHakaru bucketFanout "bucketFanout.c"
  compileHakaru bucketFanout2 "bucketFanout2.c"
  compileHakaru bucketFanout3 "bucketFanout3.c"
  compileHakaru bucketFanout4 "bucketFanout4.c"
  compileHakaru bucketSplit "bucketSplit.c"
  compileHakaru bucketIndex "bucketIndex.c"
  compileHakaru bucketIndex2 "bucketIndex2.c"

bucketAdd :: TrivialABT Term '[] ('HNat ':-> 'HNat)
bucketAdd = triv $
  lam $ \x -> bucket (nat_ 0) x (r_add (const (nat_ 1)))

bucketNoOp :: TrivialABT Term '[] ('HNat ':-> HUnit)
bucketNoOp = triv $
  lam $ \x -> bucket (nat_ 0) x r_nop

bucketFanout :: TrivialABT Term '[] ('HNat ':-> (HPair 'HNat 'HNat))
bucketFanout = triv $
  lam $ \x -> bucket (nat_ 0) x
  (r_fanout (r_add (const (nat_ 1)))
            (r_add (const (nat_ 2))))

bucketFanout2 :: TrivialABT Term '[] ('HNat ':-> (HPair 'HNat (HPair 'HNat 'HNat)))
bucketFanout2 = triv $ lam $ \x -> bucket (nat_ 0) x
  (r_fanout (r_add (const (nat_ 1)))
            (r_fanout (r_add (const (nat_ 2)))
                      (r_add (const (nat_ 3)))))

bucketFanout3 :: TrivialABT Term '[] ('HNat ':-> (HPair 'HNat HUnit))
bucketFanout3 = triv $ lam $ \x -> bucket (nat_ 0) x
  (r_fanout (r_add (const (nat_ 1)))
            r_nop)

bucketFanout4 :: TrivialABT Term '[] ('HNat ':-> (HPair (HPair 'HNat 'HNat) 'HNat))
bucketFanout4 = triv $ lam $ \x -> bucket (nat_ 0) x
  (r_fanout (r_fanout (r_add (const (nat_ 2)))
                      (r_add (const (nat_ 3))))
            (r_add (const (nat_ 1))))


bucketSplit :: TrivialABT Term '[] ('HNat ':-> (HPair 'HNat 'HNat))
bucketSplit = triv $ lam $ \x -> bucket (nat_ 0) x
  (r_split (const true)
           (r_add (const (nat_ 1)))
           (r_add (const (nat_ 2))))

bucketIndex :: TrivialABT Term '[] ('HNat ':-> ('HArray 'HNat))
bucketIndex = triv $ lam $ \x -> bucket (nat_ 0) x
  (r_index (const (nat_ 10))
           (const (nat_ 5))
           (r_add (const (nat_ 42))))

bucketIndex2 :: TrivialABT Term '[] ('HNat ':-> ('HArray (HPair 'HNat 'HNat)))
bucketIndex2 = triv $ lam $ \x -> bucket (nat_ 0) x
  (r_index (const (nat_ 10))
           (const (nat_ 5))
           (r_fanout (r_add (const (nat_ 1)))
                     (r_add (const (nat_ 2)))))


compileHakaru
  :: TrivialABT Term '[] a
  -> FilePath
  -> IO ()
compileHakaru abt outFile = do
  let ast' = TypedAST (typeOf abt) $ foldr id abt abtPasses
      codeGen = wrapProgram ast' Nothing (PrintConfig True True)
      codeGenConfig = emptyCG { sharedMem = True }
      cast = CAST $ runCodeGenWith codeGen codeGenConfig
      output  = pack . render . pretty $ cast
  IO.writeFile outFile output
  where abtPasses = [ expandTransformations
                    , constantPropagation
                    , optimizations
                    ]
