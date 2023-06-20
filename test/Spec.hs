{-# LANGUAGE QuasiQuotes #-}

import Brainfuck (bf)
import Data.ByteString.Internal (c2w)
import Data.Word (Word8)
import Test.Hspec

main :: IO ()
main = hspec $ do
  it "interprets a `hello world` brainfuck program, no matter what indentation style is used" $
    let helloWorld =
          [bf|
            anything that is not brainfuck should be ignored
            >++++++++
              [<+++++++++>-]
            <.>++++[<+++++++>-]
            <+.+++++++..+++.>>++++++[
              <+++++++>- # this comment should be ignored too during parsing
            ]
            <++.------------.>++++++
              [<+++++++++>-]
            <+.<.+++.------.--------.>>>++++
              [<++++++++>-]
            <+.
          |]
     in helloWorld (fromString "") `shouldBe` fromString "Hello, World!"
  it "reads input and does things with it" $
    let doubleFirstThreeNumbers =
          [bf|
            +++[>,>[-]<[>++<-]>.<<-]
          |]
     in doubleFirstThreeNumbers [0, 10, 35, 100] `shouldBe` [0, 20, 70]

fromString :: String -> [Word8]
fromString = fmap c2w
