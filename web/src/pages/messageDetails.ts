import type { Message } from "../types";

function record(value: unknown): Record<string, unknown> {
  return value !== null && typeof value === "object" && !Array.isArray(value)
    ? (value as Record<string, unknown>)
    : {};
}

function text(value: unknown): string | undefined {
  return typeof value === "string" ? value : undefined;
}

export interface FileChange {
  path: string;
  moved?: string;
  kind: string;
  diff?: string;
}

export interface EventDetails {
  label: string;
  body?: string;
  output?: string;
  status?: string;
  changes?: FileChange[];
  language?: string;
  mcp?: {
    name: string;
    arguments?: Payload;
    results: Payload[];
    error?: string;
  };
}

export interface Payload {
  text: string;
  language?: string;
}

function payload(value: unknown): Payload {
  if (typeof value === "string") {
    try {
      const parsed: unknown = JSON.parse(value);
      if (parsed !== null && typeof parsed === "object")
        return { text: JSON.stringify(parsed, null, 2), language: "json" };
    } catch {
      /* Ordinary tool output stays literal. */
    }
    return { text: value };
  }
  return { text: JSON.stringify(value, null, 2), language: "json" };
}

function fileDiff(diff: string, kind: string): string {
  if (kind !== "add" && kind !== "delete") return diff;
  const lines = diff === "" ? [] : diff.replace(/\n$/, "").split("\n");
  const adding = kind === "add";
  return [
    adding ? `@@ -0,0 +1,${lines.length} @@` : `@@ -1,${lines.length} +0,0 @@`,
    ...lines.map((line) => `${adding ? "+" : "-"}${line}`),
    ...(diff && !diff.endsWith("\n") ? ["\\ No newline at end of file"] : []),
  ].join("\n");
}

export function eventDetails(message: Message): EventDetails | undefined {
  const item = record(message.data.item);
  switch (item.type ?? message.content) {
    case "commandExecution":
      return {
        label: "exec",
        language: "zsh",
        body: text(item.command) ?? "Command unavailable",
        output: text(item.aggregatedOutput),
        status: [
          text(item.status),
          typeof item.exitCode === "number"
            ? `exit ${item.exitCode}`
            : undefined,
        ]
          .filter(Boolean)
          .join(" · "),
      };
    case "reasoning":
      return { label: "reasoning" };
    case "mcpToolCall":
      return { label: "mcp", status: text(item.status), mcp: mcpDetails(item) };
    case "fileChange":
      return {
        label: "file change",
        status: text(item.status),
        changes: (Array.isArray(item.changes) ? item.changes : []).map(
          (value) => {
            const change = record(value);
            const kind =
              text(change.kind) ?? text(record(change.kind).type) ?? "update";
            const moved = text(record(change.kind).move_path);
            let diff = text(change.diff);
            const suffix = moved ? `\n\nMoved to: ${moved}` : "";
            if (diff && suffix && diff.endsWith(suffix))
              diff = diff.slice(0, -suffix.length);
            return {
              path: text(change.path) ?? "Unknown file",
              kind,
              moved,
              diff: diff === undefined ? undefined : fileDiff(diff, kind),
            };
          },
        ),
      };
    default:
      return undefined;
  }
}

export function messageId(message: Message): string {
  return text(record(message.data.item).id) ?? message.id;
}

export function groupExpanded(
  messages: Message[],
  expanded: Set<string>,
): boolean {
  return messages.some((message) => expanded.has(messageId(message)));
}

export function expandGroup(
  previous: Set<string>,
  messages: Message[],
  open: boolean,
): Set<string> {
  if (messages.every((message) => previous.has(messageId(message)) === open))
    return previous;
  const next = new Set(previous);
  for (const message of messages) {
    if (open) next.add(messageId(message));
    else next.delete(messageId(message));
  }
  return next;
}

function mcpDetails(
  item: Record<string, unknown>,
): NonNullable<EventDetails["mcp"]> {
  const result = record(item.result);
  const results: Payload[] = [];
  if (Array.isArray(result.content)) {
    for (const value of result.content) {
      const part = record(value);
      const resource = record(part.resource);
      results.push(
        payload(
          text(part.text) ??
            text(resource.text) ??
            text(part.uri) ??
            text(resource.uri) ??
            `[${text(part.type) ?? "content"}]`,
        ),
      );
    }
  }
  if (result.structuredContent != null) {
    const structured = payload(result.structuredContent);
    if (!results.some((part) => part.text === structured.text))
      results.push(structured);
  }
  return {
    name: [text(item.server), text(item.tool)].filter(Boolean).join("/"),
    arguments: item.arguments != null ? payload(item.arguments) : undefined,
    results,
    error:
      item.error != null
        ? (text(record(item.error).message) ??
          text(item.error) ??
          "Tool call failed")
        : result.isError === true
          ? "Tool returned an error"
          : undefined,
  };
}

export function eventText(details: EventDetails): string {
  return (
    [
      details.body,
      details.status,
      details.output,
      ...(details.changes ?? []).map(
        (change) =>
          `${change.kind} ${change.path}${change.moved ? ` → ${change.moved}` : ""}\n${change.diff ?? ""}`,
      ),
      details.mcp?.name,
      details.mcp?.arguments && `arguments\n${details.mcp.arguments.text}`,
      ...(details.mcp?.results ?? []).map((part) => part.text),
      details.mcp?.error,
    ]
      .filter((part) => part !== undefined)
      .join("\n\n") || details.label
  );
}
