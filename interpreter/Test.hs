import qualified Lucretia.Test(tests)
import qualified Lucretia.TypeChecker.Test(tests)
import Test.HUnit(runTestTT, Test(TestList))

main = runTestTT $ TestList $
  --Lucretia.Test.tests ++
  Lucretia.TypeChecker.Test.tests

