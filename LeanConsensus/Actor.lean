/-
  Actor Framework — CloseableChannel-based Actor System

  Implements a lightweight actor model using Lean 4.28.0's Std.CloseableChannel:
  - ActorHandle: holds a channel + background task + cancellation token
  - spawnActor: create an actor with a message handler
  - send: deliver a message to an actor
  - shutdown: graceful shutdown via CancellationToken + channel close + task await
  - SlotTick: slot timer message type
  - spawnSlotTimer: periodic slot tick producer with cancellation support
-/

import Std.Sync.Channel
import Std.Sync.CancellationToken

namespace LeanConsensus.Actor

open Std

/-- Handle to a running actor. Holds the communication channel, background task,
    and cancellation token for graceful shutdown. -/
structure ActorHandle (msg : Type) where
  channel : CloseableChannel.Sync msg
  task    : Task (Except IO.Error Unit)
  token   : CancellationToken

/-- Spawn an actor that processes messages with the given handler.
    The handler returns `true` to continue processing, `false` to self-terminate.
    The actor loop exits when the cancellation token is set, the channel is closed
    (recv returns `none`), or the handler returns `false`. -/
partial def spawnActor {msg : Type} (handler : msg → IO Bool)
    (capacity : Option Nat := none) : IO (ActorHandle msg) := do
  let ch ← CloseableChannel.Sync.new (α := msg) capacity
  let token ← CancellationToken.new
  let task ← IO.asTask (prio := .default) do
    while true do
      let cancelled ← token.isCancelled
      if cancelled then break
      let optMsg ← ch.recv
      match optMsg with
      | none => break
      | some m =>
        let continue_ ← handler m
        if !continue_ then break
  return { channel := ch, task := task, token := token }

/-- Send a message to an actor. Blocks until the message is accepted. -/
def send {msg : Type} (actor : ActorHandle msg) (m : msg) : IO Unit := do
  actor.channel.send m

/-- Gracefully shut down an actor by cancelling its token, closing its channel,
    and awaiting task completion. -/
def shutdown {msg : Type} (actor : ActorHandle msg) : IO Unit := do
  actor.token.cancel
  try
    actor.channel.close
  catch
    | _ => pure ()  -- already closed
  let _ ← IO.wait actor.task

/-- Message type for slot timer ticks. -/
structure SlotTick where
  slot : UInt64

/-- Spawn a slot timer that sends SlotTick messages to a target actor.
    Sends ticks from `startSlot` with `intervalMs` millisecond interval.
    Stops when the target actor's cancellation token is set or channel is closed. -/
partial def spawnSlotTimer (target : ActorHandle SlotTick) (startSlot : UInt64)
    (intervalMs : UInt32 := 4000) : IO (Task (Except IO.Error Unit)) := do
  IO.asTask (prio := .default) do
    let mut slot := startSlot
    while true do
      let cancelled ← target.token.isCancelled
      if cancelled then break
      IO.sleep intervalMs
      let success ← target.channel.trySend { slot := slot }
      if !success then break
      slot := slot + 1

end LeanConsensus.Actor
