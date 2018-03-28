{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Test.MixML.EL.Parser where

import           Data.Either      (isRight)
import           Data.Extensible
import           MixML.EL.Parser  (parse)
import           MixML.EL.Syntax
import           Test.Data
import           Test.Tasty
import           Test.Tasty.HUnit

test_parse :: [TestTree]
test_parse =
  [ testCase "Test.Data.s1" $ parse s1 @?= Right c1
  , testCase "Test.Data.s2" $ parse s2 @?= Right c2
  , testCase "Test.Data.s3" $ parse s3 @?= Right c3
  , testCase "Test.Data.s4" $ parse s4 @?= Right c4
  , testCase "Test.Data.s5" $ parse s5 @?= Right c5
  , testCase "Test.Data.s6" $ parse s6 @?= Right c6
  , testCase "Test.Data.s7" $ parse s7 @?= Right c7
  ]

test_parse_right :: [TestTree]
test_parse_right =
  [ testCase "Test.Data.rmc1"  $ isRight (parse rmc1)  @?= True
  , testCase "Test.Data.rmc2"  $ isRight (parse rmc2)  @?= True
  , testCase "Test.Data.rmc3"  $ isRight (parse rmc3)  @?= True
  , testCase "Test.Data.rmc3'" $ isRight (parse rmc3') @?= True
  , testCase "Test.Data.rmc4"  $ isRight (parse rmc4)  @?= True
  , testCase "Test.Data.rmc4'" $ isRight (parse rmc4') @?= True
  ]

c1,c2,c3,c4,c5,c6,c7 :: Prog
c1 =
  #it @= StructM "it"
    (#it @= LinkM "_self%3"
      (#it @= StructM "n"
        (#it @= ValM
          (#it @= IntE 10 <: #region @= (#l @= (1,10) <: #r @= (1,12) <: nil) <: nil)
        <: #region @= (#l @= (1,2) <: #r @= (1,12) <: nil)
        <: nil)
      <: #region @= (#l @= (1,2) <: #r @= (1,12) <: nil)
      <: nil)
      (#it @= LinkM "_self%4"
        (#it @= StructM "m"
          (#it @= ValM
            (#it @= PlusE
              (#it @= ModE
                (#it @= DotM
                  (#it @= VarM "_self%3" <: #region @= (#l @= (1,22) <: #r @= (1,23) <: nil) <: nil) "n"
                <: #region @= (#l @= (1,22) <: #r @= (1,23) <: nil)
                <: nil)
              <: #region @= (#l @= (1,22) <: #r @= (1,23) <: nil)
              <: nil)
              (#it @= IntE 3 <: #region @= (#l @= (1,24) <: #r @= (1,25) <: nil) <: nil)
            <: #region @= (#l @= (1,22) <: #r @= (1,25) <: nil)
            <: nil)
          <: #region @= (#l @= (1,14) <: #r @= (1,25) <: nil)
          <: nil)
        <: #region @= (#l @= (1,14) <: #r @= (1,25) <: nil)
        <: nil)
        (#it @= StructM "_do%1"
          (#it @= ValM
            (#it @= PrintE
              (#it @= ModE
                (#it @= DotM
                  (#it @= VarM "_self%4" <: #region @= (#l @= (1,36) <: #r @= (1,37) <: nil) <: nil) "m"
                <: #region @= (#l @= (1,36) <: #r @= (1,37) <: nil)
                <: nil)
              <: #region @= (#l @= (1,36) <: #r @= (1,37) <: nil)
              <: nil)
            <: #region @= (#l @= (1,30) <: #r @= (1,37) <: nil)
            <: nil)
          <: #region @= (#l @= (1,27) <: #r @= (1,37) <: nil)
          <: nil)
        <: #region @= (#l @= (1,27) <: #r @= (1,37) <: nil)
        <: nil)
      <: #region @= (#l @= (1,1) <: #r @= (1,37) <: nil)
      <: nil)
    <: #region @= (#l @= (1,1) <: #r @= (1,37) <: nil)
    <: nil)
  <: #region @= (#l @= (1,1) <: #r @= (1,37) <: nil)
  <: nil
c2 =
  #it @= StructM "U2"
    (#it @= UnitM
      (#it @= LinkM "_link%2"
        (#it @= StructM "t"
          (#it @= AbsTypM
            (#it @= StarK <: #region @= (#l @= (1,17) <: #r @= (1,18) <: nil) <: nil)
          <: #region @= (#l @= (1,12) <: #r @= (1,18) <: nil)
          <: nil)
        <: #region @= (#l @= (1,12) <: #r @= (1,18) <: nil)
        <: nil)
        (#it @= StructM "t"
          (#it @= AbsTypM
            (#it @= StarK <: #region @= (#l @= (1,31) <: #r @= (1,32) <: nil) <: nil)
          <: #region @= (#l @= (1,26) <: #r @= (1,32) <: nil)
          <: nil)
        <: #region @= (#l @= (1,26) <: #r @= (1,32) <: nil)
        <: nil)
      <: #region @= (#l @= (1,11) <: #r @= (1,33) <: nil)
      <: nil)
    <: #region @= (#l @= (1,1) <: #r @= (1,33) <: nil)
    <: nil)
  <: #region @= (#l @= (1,1) <: #r @= (1,33) <: nil)
  <: nil
c3 =
  #it @= StructM "U3"
    (#it @= UnitM
      (#it @= LinkM "_link%2"
        (#it @= StructM "t"
          (#it @= AbsTypM
            (#it @= StarK <: #region @= (#l @= (1,17) <: #r @= (1,18) <: nil) <: nil)
          <: #region @= (#l @= (1,12) <: #r @= (1,18) <: nil)
          <: nil)
        <: #region @= (#l @= (1,12) <: #r @= (1,18) <: nil)
        <: nil)
        (#it @= StructM "t"
          (#it @= TypM
            (#it @= IntT <: #region @= (#l @= (1,35) <: #r @= (1,38) <: nil) <: nil)
          <: #region @= (#l @= (1,26) <: #r @= (1,38) <: nil)
          <: nil)
        <: #region @= (#l @= (1,26) <: #r @= (1,38) <: nil)
        <: nil)
      <: #region @= (#l @= (1,11) <: #r @= (1,39) <: nil)
      <: nil)
    <: #region @= (#l @= (1,1) <: #r @= (1,39) <: nil)
    <: nil)
  <: #region @= (#l @= (1,1) <: #r @= (1,39) <: nil)
  <: nil
c4 =
  #it @= StructM "U4"
    (#it @= UnitM
      (#it @= LinkM "_link%2"
        (#it @= StructM "t"
          (#it @= TypM
            (#it @= IntT <: #region @= (#l @= (1,21) <: #r @= (1,24) <: nil) <: nil)
          <: #region @= (#l @= (1,12) <: #r @= (1,24) <: nil)
          <: nil)
        <: #region @= (#l @= (1,12) <: #r @= (1,24) <: nil)
        <: nil)
        (#it @= StructM "t"
          (#it @= AbsTypM
            (#it @= StarK <: #region @= (#l @= (1,37) <: #r @= (1,38) <: nil) <: nil)
          <: #region @= (#l @= (1,32) <: #r @= (1,38) <: nil)
          <: nil)
        <: #region @= (#l @= (1,32) <: #r @= (1,38) <: nil)
        <: nil)
      <: #region @= (#l @= (1,11) <: #r @= (1,39) <: nil)
      <: nil)
    <: #region @= (#l @= (1,1) <: #r @= (1,39) <: nil)
    <: nil)
  <: #region @= (#l @= (1,1) <: #r @= (1,39) <: nil)
  <: nil
c5 =
  #it @= StructM "U5"
    (#it @= UnitM
      (#it @= LinkM "_link%2"
        (#it @= StructM "t"
          (#it @= TypM
            (#it @= IntT <: #region @= (#l @= (1,21) <: #r @= (1,24) <: nil) <: nil)
          <: #region @= (#l @= (1,12) <: #r @= (1,24) <: nil)
          <: nil)
        <: #region @= (#l @= (1,12) <: #r @= (1,24) <: nil)
        <: nil)
        (#it @= StructM "t"
          (#it @= TypM
            (#it @= IntT <: #region @= (#l @= (1,41) <: #r @= (1,44) <: nil) <: nil)
          <: #region @= (#l @= (1,32) <: #r @= (1,44) <: nil)
          <: nil)
        <: #region @= (#l @= (1,32) <: #r @= (1,44) <: nil)
        <: nil)
      <: #region @= (#l @= (1,11) <: #r @= (1,45) <: nil)
      <: nil)
    <: #region @= (#l @= (1,1) <: #r @= (1,45) <: nil)
    <: nil)
  <: #region @= (#l @= (1,1) <: #r @= (1,45) <: nil)
  <: nil
c6 =
  #it @= StructM "U6"
    (#it @= UnitM
      (#it @= LinkM "X%1"
        (#it @= StructM "t"
          (#it @= AbsTypM
            (#it @= StarK <: #region @= (#l @= (1,26) <: #r @= (1,27) <: nil) <: nil)
          <: #region @= (#l @= (1,21) <: #r @= (1,27) <: nil)
          <: nil)
        <: #region @= (#l @= (1,21) <: #r @= (1,27) <: nil)
        <: nil)
        (#it @= StructM "t"
          (#it @= TypM
            (#it @= ModT
              (#it @= DotM
                (#it @= VarM "X%1" <: #region @= (#l @= (1,44) <: #r @= (1,47) <: nil) <: nil) "t"
              <: #region @= (#l @= (1,44) <: #r @= (1,47) <: nil)
              <: nil)
            <: #region @= (#l @= (1,44) <: #r @= (1,47) <: nil)
            <: nil)
          <: #region @= (#l @= (1,35) <: #r @= (1,47) <: nil)
          <: nil)
        <: #region @= (#l @= (1,35) <: #r @= (1,47) <: nil)
        <: nil)
      <: #region @= (#l @= (1,11) <: #r @= (1,48) <: nil)
      <: nil)
    <: #region @= (#l @= (1,1) <: #r @= (1,48) <: nil)
    <: nil)
  <: #region @= (#l @= (1,1) <: #r @= (1,48) <: nil)
  <: nil
c7 =
  #it @= StructM "U7"
    (#it @= UnitM
      (#it @= LinkM "X%1"
        (#it @= StructM "t"
          (#it @= TypM
            (#it @= IntT <: #region @= (#l @= (1,30) <: #r @= (1,33) <: nil) <: nil)
          <: #region @= (#l @= (1,21) <: #r @= (1,33) <: nil)
          <: nil)
        <: #region @= (#l @= (1,21) <: #r @= (1,33) <: nil)
        <: nil)
        (#it @= StructM "t"
          (#it @= TypM
            (#it @= ModT
              (#it @= DotM
                (#it @= VarM "X%1" <: #region @= (#l @= (1,50) <: #r @= (1,53) <: nil) <: nil) "t"
              <: #region @= (#l @= (1,50) <: #r @= (1,53) <: nil)
              <: nil)
            <: #region @= (#l @= (1,50) <: #r @= (1,53) <: nil)
            <: nil)
          <: #region @= (#l @= (1,41) <: #r @= (1,53) <: nil)
          <: nil)
        <: #region @= (#l @= (1,41) <: #r @= (1,53) <: nil)
        <: nil)
      <: #region @= (#l @= (1,11) <: #r @= (1,54) <: nil)
      <: nil)
    <: #region @= (#l @= (1,1) <: #r @= (1,54) <: nil)
    <: nil)
  <: #region @= (#l @= (1,1) <: #r @= (1,54) <: nil)
  <: nil
