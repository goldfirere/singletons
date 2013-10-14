{- GradingClient.hs

(c) Richard Eisenberg 2012
eir@cis.upenn.edu

This file accesses the database described in Database.hs and performs
some basic queries on it.

-}

{-# LANGUAGE TemplateHaskell, DataKinds #-}

module Test.GradingClient where

import Data.Singletons
import Test.Database

$(singletons [d|
  lastName = [CL, CA, CS, CT]
  firstName = [CF, CI, CR, CS, CT]
  yearName = [CY, CE, CA, CR]
  gradeName = [CG, CR, CA, CD, CE]
  majorName = [CM, CA, CJ, CO, CR]

  gradingSchema = Sch [Attr lastName STRING,
                       Attr firstName STRING,
                       Attr yearName NAT,
                       Attr gradeName NAT,
                       Attr majorName BOOL]

  names = Sch [Attr firstName STRING,
               Attr lastName STRING]
  |])

main :: IO ()
main = do
  h <- connect "Test/data/grades" sGradingSchema
  let ra = Read h

  allStudents <- query $ Project sNames ra
  putStrLn $ "Names of all students: " ++ (show allStudents) ++ "\n"

  majors <- query $ Select (Element sGradingSchema sMajorName) ra
  putStrLn $ "Students in major: " ++ (show majors) ++ "\n"

  b_students <-
    query $ Project sNames $
            Select (LessThan (Element sGradingSchema sGradeName) (LiteralNat 90)) ra
  putStrLn $ "Names of students with grade < 90: " ++ (show b_students) ++ "\n"
