module Network.Control.LRUCacheSpec (spec) where

import Data.Maybe
import Network.Control
import qualified Network.Control as LRU
import Test.Hspec

spec :: Spec
spec = do
    describe "LRUCache" $ do
        it "can keep entry if looked up" $ do
            let cache = insert 'b' "bar" $ insert 'a' "foo" $ empty 2
                (v, cache') = fromJust $ LRU.lookup' 'a' cache
            v `shouldBe` "foo"
            let cache'' = insert 'c' "baz" cache'
            fst <$> (LRU.lookup' 'a' cache'') `shouldBe` Just "foo"
            fst <$> (LRU.lookup' 'b' cache'') `shouldBe` Nothing
            fst <$> (LRU.lookup' 'c' cache'') `shouldBe` Just "baz"
