import           Database.HDBC.Record  (runQuery)
import           Database.HDBC.Session (handleSqlError', withConnectionIO)
import           Database.Relational   (relationalQuery)
import           Test.Hspec

import qualified DataSource            as DS
import qualified DataSource.Pure       as DSP
import qualified Relation.Person       as Person
import qualified Relation.Pure.Person  as PersonPure

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
  it "run" $ do
    handleSqlError' $ withConnectionIO DS.connect $ \conn -> withConnectionIO DSP.connect $ \connPure -> do
      persons <- ((\(Person.Person id name) -> (id, name)) <$>) <$> runQuery conn (relationalQuery Person.person) ()
      personsPure <- ((\(PersonPure.Person id name) -> (id, name)) <$>) <$> runQuery connPure (relationalQuery PersonPure.person) ()
      personsPure `shouldBe` persons
