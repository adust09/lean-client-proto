/-
  Actor Framework Tests
-/

import LeanConsensus.Actor

namespace Test.Actor

open LeanConsensus.Actor

def check (name : String) (condition : Bool) : IO (Nat × Nat) := do
  if condition then
    IO.println s!"  ✓ {name}"
    return (1, 0)
  else
    IO.println s!"  ✗ {name}"
    return (1, 1)

def runTests : IO (Nat × Nat) := do
  IO.println "\n── Actor Framework ──"
  let mut total := 0
  let mut failures := 0

  -- Test 1: Spawn actor, send messages, verify receipt via IO.Ref counter
  do
    let counter ← IO.mkRef (0 : Nat)
    let actor ← spawnActor (msg := Nat) fun _msg => do
      counter.modify (· + 1)
      return true
    send actor 1
    send actor 2
    send actor 3
    -- Give actor time to process
    IO.sleep 100
    shutdown actor
    let count ← counter.get
    let (t, f) ← check "spawn and send 3 messages" (count == 3)
    total := total + t; failures := failures + f

  -- Test 2: Two actors exchanging messages (ping-pong)
  do
    let pingCount ← IO.mkRef (0 : Nat)
    let pongCount ← IO.mkRef (0 : Nat)

    -- Create pong actor first (will be set up to receive from ping)
    let pongActor ← spawnActor (msg := String) fun msg => do
      pongCount.modify (· + 1)
      -- Pong just counts, doesn't send back to avoid infinite loop
      return msg != "stop"

    -- Create ping actor that forwards to pong
    let pingActor ← spawnActor (msg := String) fun msg => do
      pingCount.modify (· + 1)
      if msg != "stop" then
        send pongActor msg
      return msg != "stop"

    send pingActor "ping1"
    send pingActor "ping2"
    IO.sleep 100
    send pingActor "stop"
    send pongActor "stop"
    IO.sleep 100
    shutdown pingActor
    shutdown pongActor

    let pings ← pingCount.get
    let pongs ← pongCount.get
    let (t, f) ← check "ping-pong: ping received 3" (pings == 3)
    total := total + t; failures := failures + f
    let (t, f) ← check "ping-pong: pong received 3" (pongs == 3)
    total := total + t; failures := failures + f

  -- Test 3: Shutdown terminates actor loop
  do
    let processed ← IO.mkRef (0 : Nat)
    let actor ← spawnActor (msg := Nat) fun _msg => do
      processed.modify (· + 1)
      return true
    send actor 42
    IO.sleep 50
    shutdown actor
    -- After shutdown, sending should not be possible (channel closed)
    let closed ← actor.channel.isClosed
    let (t, f) ← check "shutdown closes channel" closed
    total := total + t; failures := failures + f
    let count ← processed.get
    let (t, f) ← check "shutdown: message was processed" (count == 1)
    total := total + t; failures := failures + f

  -- Test 4: Self-termination via handler returning false
  do
    let counter ← IO.mkRef (0 : Nat)
    let actor ← spawnActor (msg := Nat) fun msg => do
      counter.modify (· + 1)
      return msg != 0  -- terminate on 0
    send actor 1
    send actor 2
    send actor 0  -- trigger self-termination
    IO.sleep 100
    let count ← counter.get
    let (t, f) ← check "self-termination on handler false" (count == 3)
    total := total + t; failures := failures + f
    shutdown actor

  return (total, failures)

end Test.Actor
