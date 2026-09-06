# Aibo API

The default server is `http://127.0.0.1:5000`. It exposes JSON under `/api` and
a live event stream at `/api/events`. Read `/openapi.json` for the full request
and response schemas.

- `GET /api/locations` lists location UUIDs, names, and current paths.
- `POST /api/locations` creates a unique location with `name` and `path`.
- `PATCH /api/locations/{id}` updates its `path`, interrupting active chats using
  that location and recording their new working directory.
- `GET /api/projects` lists active projects; add `?archived=true` for archived ones.
- `POST /api/projects` creates a project with `name` and `description` and creates
  its README if needed. Edit the README directly for durable instructions.
- `PATCH /api/projects/{id}` updates its description or archived state.
- `GET /api/chats` lists visible manager/bot chats in activity order.
- `GET /api/chats/by-bot/{number}` resolves the newest matching `b#`.
- `GET /api/chats/{id}` returns a chat and its complete message/event history.
  `notice` describes the current interruption, cancellation, failure, missing
  prerequisite, or location move. `notice_message_id` identifies its source
  event when one exists; clients can display it once while keeping the history.
- `POST /api/chats` creates a manager, bot, or shadow chat. For bot delegation,
  use `source_message_id` from an original manager user message plus `m_context`
  (empty when unnecessary). The server copies the source's verbatim user text
  and every attachment in order into the new bot's stored user message.
  Alternatively, supply `user_message` and `attachments` (absolute image paths)
  with `m_context`. Do not mix these with `source_message_id`. Image-only
  messages are supported; unreadable or invalid images reject creation.
- `POST /api/chats/{id}/messages` appends a message.
- `PATCH /api/chats/{id}/location` moves a chat and interrupts active work.
- `PATCH /api/chats/{id}` updates its title or project assignment.
- `PATCH /api/chats/{id}/status` records a lifecycle transition. Exceptional
  states are `interrupted`, `cancelled`, `error`, and `blocked`; an optional
  `reason` supplies a concise notification and persisted event.
- `POST /api/chats/{id}/submit` sends `text` and optional local `attachments`.
  Missing working directories return 422 and record `blocked` before consuming
  the user message. Fix the location and retry. Each turn uses the location's
  current cwd, including when resuming an existing thread after a move.
  To send the initial delegated user message already returned by chat creation,
  submit `{"message_id":"<bot-user-message-uuid>"}`. This sends that stored text
  and all stored attachments without appending it again. For delegation to an
  existing bot, submit `source_message_id` from the original manager user message
  with `m_context`, or supply verbatim `text`, all `attachments`, and `m_context`.
  A `source_message_id` cannot be mixed with `text`, `attachments`, or `message_id`.
  The source must be a manager user message; missing sources return 404 and
  invalid attachments return 422 without appending a partial handoff.
  A `message_id` is local to the destination chat and cannot be combined with
  new content. Manager context never substitutes for real image attachments.
- `GET /api/notifications` returns active, unread, and recent read sections.
  Unread/read sections sort by notification creation time, newest first;
  later activity in the source chat does not reorder an existing notification.
  `account_usage.remaining_percent` is the lowest remaining percentage across
  the Codex bucket's primary and secondary windows (rounded down).
  `account_usage.resets` is the authoritative available earned-reset count,
  not a reset timestamp or the number of returned credit details. Either can
  be null when unavailable. Account limits are cached for up to 30 seconds;
  Codex account/rate-limit events invalidate the cache and refresh both clients.
- `POST /api/notifications/{id}/read` marks one notification read.

Shadow chats are intentionally absent from every visible listing and notification
response. Chat UUIDs are authoritative; bot numbers wrap and can repeat.

Project descriptions, README paths and contents, and current location are sent
as workspace context before each turn's literal user input. Changed context is
recorded as an internal system message; unchanged context does not add history.
README edits and project reassignment therefore apply when an existing thread
continues. Role instructions are supplied when the thread is first created.

Conversations default to goal mode; internal shadow chats do not. Chat creation
accepts `goal: false` to opt out. Submission also accepts `goal`; omitting it keeps
the current setting, except a new delegation with `m_context` defaults to enabled.
The literal user request supplies a worker's objective, separately from manager
context. Manager goals scope that request to coordination: an accepted handoff
finishes their work unless the user explicitly requests monitoring.
`PUT /api/chats/{id}/goal` with `{"enabled":true}` sets the goal to the last stored
user message; `{"enabled":false}` clears it. Enabling requires a text message.
Chat responses include `goal_enabled`, native `goal` (objective and status),
`tokens_used` (cumulative thread tokens), `elapsed_seconds` (completed running
time), and `running_since` (current running interval, or null). Clients add the
current interval for a live elapsed display; idle time is excluded.

Normal intermediate turns in active/paused goals do not create completion
notifications. The final turn at goal completion or blockage does. Failed or
interrupted turns still notify; chats without a native goal notify per completed
turn. Eligibility is persisted with the completion event, so repair/replay does
not turn intermediate history into new notifications after a goal ends.

## Compact transcript reads (Emacs)

`GET /api/chats/{id}/history` returns a chat with the latest 60 user/final-assistant/error
messages. Assistant `commentary`, tools, events and system context become consecutive
range summaries (`data.hidden_count`, `first_id`, `last_id`); their bodies are excluded
by the SQL projection, not just hidden after download. Partial streaming items remain
excluded, as in full transcript reads. Legacy assistant items without a phase remain
visible. `older_before` is a message UUID cursor for the next history page (`?before=…`).
`history_version` prevents delayed snapshots replacing newer state in Emacs.

`GET /api/chats/{id}/details?first=…&last=…` loads at most 30 stored messages in that
inclusive range; `next_after` supplies the exclusive `after` cursor for more. Cursors
are scoped to the chat. These reads never alter the durable transcript. The original
`GET /api/chats/{id}` remains the full transcript API.

`POST /api/chats/{id}/submit?compact=true` returns the compact chat acknowledgement.
`/api/events?compact=true` omits unused protocol/message bodies and nested chat
transcripts; clients reconcile via history reads. The ordinary event stream remains
available for other consumers. No real model turn is needed to test these readers.
