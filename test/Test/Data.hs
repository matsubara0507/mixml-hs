{-# LANGUAGE QuasiQuotes #-}

module Test.Data where

import           Text.Heredoc

s1,s2,s3,s4,s5,s6,s7 :: String
s1 = "{val n = 10, val m = n+3, do print m}"
s2 = "unit U2 = {type t} with {type t}"
s3 = "unit U3 = {type t} with {type t = int}"
s4 = "unit U4 = {type t = int} with {type t}"
s5 = "unit U5 = {type t = int} with {type t = int}"
s6 = "unit U6 = link X = {type t} with {type t = X.t}" -- ill-typed!
s7 = "unit U7 = link X = {type t = int} with {type t = X.t}"

rmc1 :: String
rmc1 =
  [str|unit RMC1 =
      |  link X = {module A = {type t}, module B = {type u}} with
      |  {
      |    module A = {type t = int type u = X.B.u}
      |    module B = {type u = int type t = X.A.t}
      |  }
      |]

rmc2 :: String
rmc2 =
  [str|unit RMC2 =
      |  link X = {module A = {type t}, module B = {type u}} with
      |  {
      |    module A = {type t = int type u = X.B.u} :> {type t, type u = X.B.u}
      |    module B = {type u = int type t = X.A.t} :> {type u, type t = X.A.t}
      |  }
      |]

rmc3 :: String
rmc3 =
  [str|unit RMC3 =
      |  link X =
      |  {
      |    module A = {type t}
      |    module B = {type u, type t, val g : t -> (u, t)}
      |  }
      |  with
      |  {
      |    module A =
      |    {
      |      type t = int
      |      type u = X.B.u
      |      val x = 666
      |      val f(x : t) = let p = X.B.g(x + 3) in (p#1, p#2 + 5)
      |    } :> {type t, type u = X.B.u, val x : t, val f : t -> (u, t)}
      |    module B =
      |    {
      |      type u = int
      |      type t = X.A.t
      |      val y = A.x (*(A.f A.x)#2*)
      |      val g(x : t) = (0, x)
      |    } :> {type u, type t = X.A.t, val y : t, val g : t -> (u, t)}
      |  }
      |]

rmc3' :: String
rmc3' = "link R = !RMC3 with {do print (R.A.f R.B.y); print \"\\n\"}"

rmc4 :: String
rmc4 =
  [str|unit RMC4 =
      |  link X =
      |  {
      |    module A = {type t}
      |    module B = {type u, type t, val g : t -> (u, t)}
      |  }
      |  with
      |  {
      |    module A =
      |    {type t, type u = X.B.u, val x : t, val f : t -> (u, t)} seals
      |    {
      |      type t = int
      |      type u = X.B.u
      |      val x = 666
      |      val f(x : t) = let p = X.B.g(x + 3) in (p#1, p#2 + 5)
      |    }
      |    module B =
      |    {type u, type t = X.A.t, val y : t, val g : t -> (u, t)} seals
      |    {
      |      type u = int
      |      type t = X.A.t
      |      val y = A.x (*(A.f A.x)#2*)
      |      val g(x : t) = (0, x)
      |    }
      |  }
      |]

rmc4' :: String
rmc4' = "link R = !RMC4 with {do print (R.A.f R.B.y); print \"\\n\"}"
