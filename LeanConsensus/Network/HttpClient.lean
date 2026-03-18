/-
  HttpClient — Minimal HTTP/1.1 Client over Raw TCP

  Provides basic HTTP GET/POST and SSE (Server-Sent Events) support
  for communicating with a localhost Beacon Node sidecar.
  Uses curl subprocess for HTTP requests and SSE streaming.

  Design decisions:
  - Connection: close (no keep-alive management)
  - No TLS (sidecar is localhost)
  - Content-Length based body reading
-/

import Std.Sync.CancellationToken

namespace LeanConsensus.Network.HttpClient

-- ════════════════════════════════════════════════════════════════
-- HTTP Response
-- ════════════════════════════════════════════════════════════════

/-- Parsed HTTP response. -/
structure HttpResponse where
  statusCode : UInt32
  headers    : Array (String × String)
  body       : ByteArray

instance : ToString HttpResponse where
  toString r := s!"HttpResponse(status={r.statusCode}, bodySize={r.body.size})"

/-- Get a header value by name (case-insensitive). -/
def HttpResponse.getHeader (resp : HttpResponse) (name : String) : Option String :=
  resp.headers.findSome? fun (k, v) =>
    if k.toLower == name.toLower then some v else none

-- ════════════════════════════════════════════════════════════════
-- SSE (Server-Sent Events)
-- ════════════════════════════════════════════════════════════════

/-- A single Server-Sent Event. -/
structure SseEvent where
  event : String
  data  : String
  id    : Option String := none

instance : ToString SseEvent where
  toString e := s!"SseEvent(event={e.event}, data={e.data})"

/-- Handle for reading SSE events from a long-lived connection. -/
structure SseStream where
  buffer : IO.Ref ByteArray
  reader : IO (Option ByteArray)
  closed : IO.Ref Bool

-- ════════════════════════════════════════════════════════════════
-- Internal Parsing
-- ════════════════════════════════════════════════════════════════

/-- Parse status line like "HTTP/1.1 200 OK". -/
private def parseStatusLine (line : String) : Option UInt32 :=
  let parts := line.splitOn " "
  if parts.length < 2 then none
  else parts[1]!.toNat?.map (·.toUInt32)

/-- Parse a header line like "Content-Length: 42". -/
private def parseHeaderLine (line : String) : Option (String × String) :=
  match line.splitOn ": " with
  | name :: rest => some (name, ": ".intercalate rest)
  | _ =>
    match line.splitOn ":" with
    | name :: rest =>
      some (name.trimAscii.toString, (":".intercalate rest).trimAscii.toString)
    | _ => none

/-- Split headers from body at the \r\n\r\n boundary.
    Returns (headerPart, remainingBody). -/
private def splitHeaderBody (data : ByteArray) : Option (String × ByteArray) := Id.run do
  if data.size < 4 then return none
  for i in [:data.size - 3] do
    if data.get! i == 0x0D && data.get! (i + 1) == 0x0A &&
       data.get! (i + 2) == 0x0D && data.get! (i + 3) == 0x0A then
      let headerBytes := data.extract 0 i
      let bodyBytes := data.extract (i + 4) data.size
      return some (String.fromUTF8! headerBytes, bodyBytes)
  return none

-- ════════════════════════════════════════════════════════════════
-- HTTP via curl
-- ════════════════════════════════════════════════════════════════

/-- Parse HTTP headers string into status code and header array. -/
private def parseResponseHeaders (headerStr : String) :
    Option (UInt32 × Array (String × String)) := do
  let lines := headerStr.splitOn "\r\n"
  match lines with
  | [] => none
  | statusLine :: headerLines =>
    let statusCode ← parseStatusLine statusLine
    let headers := headerLines.filterMap parseHeaderLine
    some (statusCode, headers.toArray)

-- ════════════════════════════════════════════════════════════════
-- Public API
-- ════════════════════════════════════════════════════════════════

/-- Send an HTTP GET request and return the response.
    Uses curl for reliable HTTP communication. -/
def httpGet (host : String) (port : UInt16) (path : String)
    (extraHeaders : Array (String × String) := #[]) : IO HttpResponse := do
  let mut args : Array String := #[
    "-s", "-S",                         -- silent but show errors
    "-D", "-",                          -- dump headers to stdout
    "-o", "/dev/null",                  -- we handle body separately
    "--max-time", "10",
    s!"http://{host}:{port}{path}"
  ]
  for (k, v) in extraHeaders do
    args := args ++ #["-H", s!"{k}: {v}"]
  -- First get headers
  let headerProc ← IO.Process.output {
    cmd := "curl"
    args := args
  }
  -- Now get body
  let mut bodyArgs : Array String := #[
    "-s", "-S",
    "--max-time", "10",
    s!"http://{host}:{port}{path}"
  ]
  for (k, v) in extraHeaders do
    bodyArgs := bodyArgs ++ #["-H", s!"{k}: {v}"]
  let bodyProc ← IO.Process.output {
    cmd := "curl"
    args := bodyArgs
  }
  let bodyData := bodyProc.stdout.toUTF8
  -- Parse the headers
  match parseResponseHeaders headerProc.stdout with
  | some (statusCode, headers) =>
    return { statusCode, headers, body := bodyData }
  | none =>
    if headerProc.exitCode != 0 then
      throw (IO.userError s!"HTTP GET failed: {headerProc.stderr}")
    return { statusCode := 0, headers := #[], body := bodyData }

/-- Send an HTTP POST request with a body and return the response. -/
def httpPost (host : String) (port : UInt16) (path : String) (body : ByteArray)
    (contentType : String := "application/json")
    (extraHeaders : Array (String × String) := #[]) : IO HttpResponse := do
  -- Write body to a temp file for binary safety
  let tmpPath := "/tmp/lean-http-body-" ++ toString (← IO.monoNanosNow)
  IO.FS.writeBinFile tmpPath body
  let mut args : Array String := #[
    "-s", "-S", "-i",                   -- include headers in output
    "-X", "POST",
    "-H", s!"Content-Type: {contentType}",
    "--data-binary", s!"@{tmpPath}",
    "--max-time", "10",
    s!"http://{host}:{port}{path}"
  ]
  for (k, v) in extraHeaders do
    args := args ++ #["-H", s!"{k}: {v}"]
  let proc ← IO.Process.output {
    cmd := "curl"
    args := args
  }
  -- Clean up temp file
  try IO.FS.removeFile tmpPath catch _ => pure ()
  let rawData := proc.stdout.toUTF8
  match splitHeaderBody rawData with
  | some (headerStr, bodyData) =>
    match parseResponseHeaders headerStr with
    | some (statusCode, headers) =>
      return { statusCode, headers, body := bodyData }
    | none =>
      return { statusCode := 0, headers := #[], body := bodyData }
  | none =>
    if proc.exitCode != 0 then
      throw (IO.userError s!"HTTP POST failed: {proc.stderr}")
    return { statusCode := 0, headers := #[], body := rawData }

-- ════════════════════════════════════════════════════════════════
-- SSE Stream
-- ════════════════════════════════════════════════════════════════

/-- Strip a given number of characters and trim whitespace. Returns a String. -/
private def dropAndTrim (line : String) (n : Nat) : String :=
  (line.drop n).trimAscii.toString

/-- Parse SSE lines into an SseEvent.
    SSE format: "event: <type>\ndata: <json>\nid: <id>\n\n" -/
private def parseSseLines (lines : Array String) : Option SseEvent := Id.run do
  let mut event := ""
  let mut dataLines : Array String := #[]
  let mut id : Option String := none
  for line in lines do
    if line.startsWith "event:" then
      event := dropAndTrim line 6
    else if line.startsWith "data:" then
      dataLines := dataLines.push (dropAndTrim line 5)
    else if line.startsWith "id:" then
      id := some (dropAndTrim line 3)
  if dataLines.isEmpty && event.isEmpty then return none
  let data := "\n".intercalate dataLines.toList
  return some { event, data, id }

/-- Connect to an SSE endpoint. Returns an SseStream for reading events.
    Uses curl with streaming mode. -/
def sseConnect (host : String) (port : UInt16) (path : String)
    (extraHeaders : Array (String × String) := #[]) : IO SseStream := do
  let bufRef ← IO.mkRef ByteArray.empty
  let closedRef ← IO.mkRef false
  let mut args : Array String := #[
    "-s", "-N", "--no-buffer",
    "-H", "Accept: text/event-stream",
    s!"http://{host}:{port}{path}"
  ]
  for (k, v) in extraHeaders do
    args := args ++ #["-H", s!"{k}: {v}"]
  let proc ← IO.Process.spawn {
    cmd := "curl"
    args := args
    stdout := .piped
    stderr := .null
    stdin := .null
  }
  let stdout := proc.stdout
  let reader : IO (Option ByteArray) := do
    let closed ← closedRef.get
    if closed then return none
    let chunk ← stdout.read 4096
    if chunk.isEmpty then
      closedRef.set true
      return none
    else
      return some chunk
  return { buffer := bufRef, reader, closed := closedRef }

/-- Read the next SSE event from a stream.
    Returns `none` if the connection is closed. -/
partial def SseStream.next (stream : SseStream) : IO (Option SseEvent) := do
  let closed ← stream.closed.get
  if closed then return none
  let mut buf ← stream.buffer.get
  -- Read until we have a complete event (double newline)
  for _ in [:1000] do  -- bounded loop for safety
    let bufStr := String.fromUTF8! buf
    -- SSE events are separated by blank lines
    match bufStr.splitOn "\n\n" with
    | eventStr :: rest =>
      if rest.length > 0 then
        -- We have a complete event
        let remaining := "\n\n".intercalate rest
        stream.buffer.set remaining.toUTF8
        let lines := eventStr.splitOn "\n"
        match parseSseLines lines.toArray with
        | some event => return some event
        | none =>
          buf ← stream.buffer.get
          continue
      else
        -- Need more data
        match ← stream.reader with
        | none =>
          stream.closed.set true
          let lines := bufStr.splitOn "\n"
          return parseSseLines lines.toArray
        | some chunk =>
          buf := buf ++ chunk
          stream.buffer.set buf
    | [] =>
      match ← stream.reader with
      | none =>
        stream.closed.set true
        return none
      | some chunk =>
        buf := buf ++ chunk
        stream.buffer.set buf
  return none

/-- Close an SSE stream. -/
def SseStream.close (stream : SseStream) : IO Unit := do
  stream.closed.set true

end LeanConsensus.Network.HttpClient
