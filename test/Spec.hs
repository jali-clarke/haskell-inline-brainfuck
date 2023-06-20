{-# LANGUAGE QuasiQuotes #-}

import Data.ByteString.Internal (c2w)
import Data.Word (Word8)
import Test.Hspec

import Brainfuck (bf)

main :: IO ()
main = hspec $ do
  it "should interpret a `hello world` brainfuck program, no matter what indentation style is used" $
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

fromString :: String -> [Word8]
fromString = fmap c2w
