module SprintfSpec

import Specdris.Spec
import Sprintf

%access export
%default total

specSuite : IO ()
specSuite = spec $ do
  describe "Fixed tests" $ do
    it "Should accept no arg when no fmt" $ do
      sprintf "" `shouldBe` ""
      sprintf "I reject my humanity, Jojo!" `shouldBe` "I reject my humanity, Jojo!"
    it "Should format integers" $ do
      sprintf "%d" 233 `shouldBe` "233"
      sprintf "%d %d" 233 666 `shouldBe` "233 666"
      sprintf "%d,%d" 233 666 `shouldBe` "233,666"
    it "Should format char/float/%" $ do
      sprintf "%%%d%f" 233 233.3 `shouldBe` "%233233.3"
      sprintf "%d %f" 233 666.6 `shouldBe` "233 666.6"
      sprintf "%c%f%d" '1' 23.4 5 `shouldBe` "123.45"
    it "Should dram" $ do
      sprintf "%% %% (%d + %d = %d) (%d / %d = %f)" 8 7 3 2 5 4.5 `shouldBe` "% % (8 + 7 = 3) (2 / 5 = 4.5)"


