/-
  HTTP Client Tests — Request Building and Response Parsing
-/

import LeanConsensus.Network.HttpClient

namespace Test.Network.HttpClient

open LeanConsensus.Network.HttpClient

def hasSubstr (s : String) (sub : String) : Bool :=
  (s.splitOn sub).length > 1

def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

def runTests : IO (Nat × Nat) := do
  IO.println "\n── HTTP Client ──"
  let mut total := 0
  let mut failures := 0

  -- Test 1: HttpResponse.getHeader case-insensitive lookup
  do
    let resp : HttpResponse := {
      statusCode := 200
      headers := #[("Content-Type", "application/json"), ("Content-Length", "42")]
      body := ByteArray.empty
    }
    let ct := resp.getHeader "content-type"
    let (t, f) ← check "getHeader case-insensitive" (ct == some "application/json")
    total := total + t; failures := failures + f

  -- Test 2: HttpResponse.getHeader returns none for missing header
  do
    let resp : HttpResponse := {
      statusCode := 200
      headers := #[("Content-Type", "text/html")]
      body := ByteArray.empty
    }
    let missing := resp.getHeader "X-Custom"
    let (t, f) ← check "getHeader missing returns none" (missing.isNone)
    total := total + t; failures := failures + f

  -- Test 3: HttpResponse toString
  do
    let resp : HttpResponse := {
      statusCode := 404
      headers := #[]
      body := "not found".toUTF8
    }
    let s := toString resp
    let (t, f) ← check "HttpResponse toString" (hasSubstr s "404")
    total := total + t; failures := failures + f

  -- Test 4: SseEvent toString
  do
    let event : SseEvent := {
      event := "block"
      data := "{\"slot\":\"42\"}"
    }
    let s := toString event
    let (t, f) ← check "SseEvent toString" (hasSubstr s "block")
    total := total + t; failures := failures + f

  -- Test 5: Multiple headers with same key
  do
    let resp : HttpResponse := {
      statusCode := 200
      headers := #[("Set-Cookie", "a=1"), ("Set-Cookie", "b=2"), ("Content-Type", "text/plain")]
      body := ByteArray.empty
    }
    -- getHeader returns first match
    let cookie := resp.getHeader "Set-Cookie"
    let (t, f) ← check "getHeader returns first match" (cookie == some "a=1")
    total := total + t; failures := failures + f

  -- Test 6: Empty body response
  do
    let resp : HttpResponse := {
      statusCode := 204
      headers := #[]
      body := ByteArray.empty
    }
    let (t, f) ← check "empty body response" (resp.body.size == 0 && resp.statusCode == 204)
    total := total + t; failures := failures + f

  return (total, failures)

end Test.Network.HttpClient
