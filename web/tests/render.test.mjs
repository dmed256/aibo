import { test, expect } from "bun:test";
import { createElement as h } from "react";
import { renderToStaticMarkup as render } from "react-dom/server";
import { compactTokens } from "../src/utils";
import { ChatMetadata } from "../src/components/ChatMetadata";
import { Composer } from "../src/components/Composer";
import { Sidebar } from "../src/components/Sidebar";
import { ChatView } from "../src/pages/ChatView";
import { Home } from "../src/pages/Home";
import { Locations } from "../src/pages/Locations";
import { Projects } from "../src/pages/Projects";
import { CodeBlock } from "../src/components/CodeBlock";
import { eventDetails, eventText } from "../src/pages/messageDetails";

const chat = (kind, index) => ({
  id: `${kind}-${index}`,
  label: `${kind === "bot" ? "b" : "m"}${index}`,
  kind,
  title: `Task ${index}`,
  active: index === 0,
  status: index === 0 ? "running" : "completed",
  location: null,
  project: null,
  activity_at: "2026-09-05T00:00:00Z",
  last_active_at: null,
  created_at: "2026-09-05T00:00:00Z",
  messages: [],
});
const noop = () => {};

test("MCP formats encoded JSON once and preserves non-JSON output", () => {
  const result = { ok: false, empty: [], value: null };
  const details = eventDetails({
    kind: "event",
    data: {
      item: {
        type: "mcpToolCall",
        server: "files",
        tool: "read",
        arguments: '{"path":"/tmp/file"}',
        result: {
          content: [
            { type: "text", text: JSON.stringify(result) },
            { type: "text", text: "{incomplete" },
          ],
          structuredContent: result,
          isError: true,
        },
      },
    },
  });
  expect(details.mcp.arguments).toEqual({
    text: '{\n  "path": "/tmp/file"\n}',
    language: "json",
  });
  expect(details.mcp.results).toEqual([
    { text: JSON.stringify(result, null, 2), language: "json" },
    { text: "{incomplete" },
  ]);
  expect(eventText(details)).toContain("Tool returned an error");
  expect(eventText(details)).not.toContain("mcpToolCall");
});

test("highlighting preserves literal shell text and escapes HTML", () => {
  const source = 'echo "<script>alert(1)</script>" $HOME # literal\n';
  const html = render(h(CodeBlock, { text: source, language: "zsh" }));
  expect(html).toContain('aria-label="copy code"');
  expect(html).toContain('class="hljs-string"');
  expect(html).not.toContain("<script>");
  expect(
    html
      .replace(/<[^>]+>/g, "")
      .replaceAll("&quot;", '"')
      .replaceAll("&lt;", "<")
      .replaceAll("&gt;", ">"),
  ).toBe(source);
});

test("sidebar shows remaining usage and only positive resets", () => {
  for (const [remaining_percent, resets] of [
    [75, 2],
    [0, 0],
    [null, null],
  ]) {
    const html = render(
      h(Sidebar, {
        data: {
          active: [],
          unread: [],
          read: [],
          account_usage: { remaining_percent, resets },
        },
        currentId: null,
        connected: true,
        onOpen: noop,
      }),
    );
    expect(html).toContain(
      `<span>usage</span><span class="metadata-value">${remaining_percent == null ? "—" : `${remaining_percent}%`}</span>`,
    );
    expect(html.includes("<span>resets</span>")).toBe(resets > 0);
    if (resets > 0)
      expect(html).toContain('<span class="metadata-value">2</span>');
    expect(html).toContain(
      '<div id="notifications" class="sidebar-content" tabindex="-1"><div class="account-usage"',
    );
  }
});

test("composer keeps status dots before shortcuts without tab badges", () => {
  const statuses = [
    "queued",
    "running",
    "error",
    "blocked",
    "interrupted",
    "cancelled",
    "idle",
    "completed",
  ];
  const chats = [
    ...Array.from({ length: 12 }, (_, i) => chat("bot", i)),
    ...Array.from({ length: 12 }, (_, i) => chat("manager", i)),
  ].map((item, index) => ({
    ...item,
    status: statuses[index % 12] ?? "completed",
    active: index % 12 < 2,
  }));
  const html = render(
    h(Composer, {
      chats,
      target: null,
      text: "",
      attachments: [],
      sending: false,
      onTarget: noop,
      onNew: noop,
      onText: noop,
      onDeleteAttachment: noop,
      onPaste: noop,
    }),
  );
  const [bots, managers] = html
    .split('aria-label="Bot chats"')[1]
    .split('aria-label="Manager chats"');
  expect(bots).toContain("<kbd>M-1</kbd>");
  expect(bots).toContain("<kbd>M-9</kbd>");
  expect(bots).not.toContain("<kbd>M-10</kbd>");
  expect(html).toContain("<kbd>M-0</kbd> new");
  expect(managers).toContain("<kbd>cb0</kbd>");
  expect(managers).toContain("<kbd>cb9</kbd>");
  expect(html).not.toContain("New manager");
  expect(html).not.toContain("M-RET");
  expect(html).toContain('badge-identity">m</span>');
  expect(html).toContain('rows="1"');
  expect(html).not.toContain("Send message");
  expect(html).not.toContain("file-button");
  expect(html.match(/class="new-slot"/g)).toHaveLength(1);
  const tabs = html
    .split('class="shortcut-bars"')[1]
    .split('class="attachments"')[0];
  expect(tabs).not.toContain("chat-badge");
  for (const kind of ["bot", "manager"]) {
    for (const [index, status] of statuses.entries()) {
      const tab = tabs.match(
        new RegExp(`<a href="/chats/${kind}-${index}"[^>]*>(.*?)</a>`),
      )[1];
      const key = kind === "bot" ? `M-${index + 1}` : `cb${index}`;
      if (["idle", "completed"].includes(status)) {
        expect(tab).not.toContain("status-dot");
      } else {
        const color = index < 2 ? "active" : "failed";
        expect(tab).toStartWith(
          `<span class="status-dot ${color}" aria-label="${status}">●</span><kbd>${key}</kbd>`,
        );
      }
      expect(tab).not.toContain("badge-identity");
    }
  }
  expect(bots).toContain('class="recent-target bot"');
  expect(managers).toContain('class="recent-target manager"');
  expect(tabs).toContain('class="tab-title">Task 0</span>');
});

test("reference pages are read-only and empty home contains only its message", () => {
  expect(
    render(
      h(Home, {
        chats: [],
        projects: [],
        archivedProjects: [],
        groups: {},
        onOpen: noop,
      }),
    ),
  ).toBe('<div class="empty">No conversations yet</div>');
  const locations = render(
    h(Locations, {
      locations: [
        { id: "a", name: "aibo", path: "/tmp/aibo" },
        { id: "n", name: "notes", path: "/tmp/notes" },
      ],
    }),
  );
  expect(locations).toContain("</tr><tr>");
  const projects = render(
    h(Projects, {
      projects: [{ id: "a", name: "aibo", description: "Project context" }],
      archived: [{ id: "o", name: "old" }],
    }),
  );
  expect(projects).toContain("Project context");
  expect(projects).toContain(">Archived</h2>");
  expect(locations + projects).not.toMatch(/<(input|form|select|button)/);
});

test("internal messages have type badges and omit turn markers", () => {
  const message = (id, kind, content) => ({
    id,
    kind,
    content,
    created_at: new Date().toISOString(),
    data: {},
  });
  const html = render(
    h(ChatView, {
      chat: {
        ...chat("bot", 0),
        notice: "Blocked on location",
        notice_message_id: "notice",
        messages: [
          message("sys", "system", "Instructions"),
          message("tool", "tool", "Read a file"),
          message("u", "user", "Request"),
          message("start", "event", "Codex turn started"),
          message("end", "event", "Codex turn completed"),
          message("notice", "event", "Blocked on location"),
          message("a", "assistant", "Response"),
        ],
      },
    }),
  );
  expect(html).toContain("2 hidden messages");
  expect(html).toContain("<svg");
  expect(html).not.toContain("[2 hidden messages]");
  expect(html).not.toMatch(/[▸▾]/);
  expect(html.match(/class="message bot"/g)).toHaveLength(1);
  expect(html.match(/class="message internal"/g)).toHaveLength(2);
  expect(html).toContain('badge internal">system</span>');
  expect(html).not.toContain("Codex turn started");
  expect(html).not.toContain("Codex turn completed");
  expect(html).toContain('badge user">user</span>');
  expect(html.match(/Blocked on location/g)).toHaveLength(1);
  expect(html).not.toContain("<pre>{");
});

test("sidebar keeps entries beyond shortcut range and uses total read count", () => {
  const active = Array.from({ length: 65 }, (_, i) => chat("bot", i));
  const html = render(
    h(Sidebar, {
      data: { active, unread: [], read: [], read_count: 12 },
      currentId: null,
      connected: true,
      onOpen: noop,
    }),
  );
  expect(html.match(/class="notification active-notification"/g)).toHaveLength(
    65,
  );
  expect(html).toContain('READ<span class="section-count">12</span>');
  expect(html).toContain('notification-key">z');
  expect(html).toContain('notification-key">A');
  expect(html).toContain('notification-key">Z');
  expect(html).not.toContain('notification-key">10');
  expect(
    render(h(ChatView, { chat: { ...chat("bot", 0), kind: "shadow" } })),
  ).toBe("");
});

test("initial app and search render with preserved input chrome", async () => {
  const { default: App } = await import("../src/App");
  const { default: SearchPage } = await import("../src/pages/SearchPage");
  const html = render(h(App));
  expect(html).toContain("Loading workspace…");
  expect(html).toContain('aria-label="Message Aibo"');
  expect(html).toContain("M-0");
  expect(html).not.toContain("NEW LOCATION");
  expect(html).toContain('aria-label="Breadcrumb"');
  expect(html).not.toContain("Workspace pages");
  expect(html).not.toContain("brand-name");
  const search = render(h(SearchPage, { open: noop }));
  expect(search).toContain('class="search-query"');
  expect(search).toContain('aria-label="Search chats"');
  expect(search).toContain("Searching…");
  expect(search).not.toContain("Fuzzy label");
});

test("tutorial has seven static examples and a home link", async () => {
  const { default: Tutorial, Terminal } = await import(
    "../src/tutorial/Tutorial"
  );
  const { default: data } = await import("../src/tutorial/frames.json");
  const html = render(h(Tutorial, { onHome: noop }));
  expect(html).toContain("Write a request");
  expect(html.match(/<button/g)).toHaveLength(7);
  expect(html).toContain('href="/"');
  expect(html).toContain('class="sidebar tutorial-sidebar"');
  expect(html).toContain("input-cursor");
  expect(html).not.toContain("Try M-/");
  expect(html).not.toContain("Previous");
  expect(html).not.toContain("Next →");
  expect(html).not.toContain("Demo only");
  expect(html).not.toContain("lesson-practice");
  expect(html).not.toContain('tabindex="0"');
  expect(html).not.toContain("<textarea");
  expect(html).toContain("Tutorial chapters");
  expect(html).not.toContain("<iframe");
  expect(html).toContain("<main");
  expect(html).not.toContain("A request,");
  expect(html).not.toContain("A short walkthrough");
  for (const [name, frame] of Object.entries(data.frames)) {
    expect(frame.lines).toHaveLength(40);
    expect(frame.runs).toHaveLength(40);
    const terminal = render(h(Terminal, { name }));
    expect(terminal.match(/class="terminal-line"/g)).toHaveLength(40);
    expect(terminal).toContain("120 columns by 40 rows");
  }
});

test("goal control and usage reflect persisted state", () => {
  for (const [enabled, status] of [
    [true, "blocked"],
    [false, "off"],
  ]) {
    const html = render(
      h(ChatMetadata, {
        chat: {
          ...chat("bot", 0),
          project: { name: "aibo" },
          goal_enabled: enabled,
          goal: enabled ? { objective: "Fix the UI", status } : null,
          tokens_used: 234567,
          elapsed_seconds: 126,
        },
        onGoal: noop,
      }),
    );
    expect(html).toContain(`aria-pressed="${enabled}"`);
    expect(html).toContain(
      `goal</span><span class="metadata-value">${status}</span>`,
    );
    expect(html).toContain("234k");
    expect(html).toContain("2m");
    expect(html).toContain(
      '<span>project</span><span class="metadata-value">aibo</span>',
    );
  }
});

test("errors stay visible across recovery without duplicate notices", () => {
  const failure = {
    id: "failure",
    kind: "error",
    content: "Lost connection",
    data: {},
  };
  for (const status of ["error", "running"]) {
    const html = render(
      h(ChatView, {
        chat: {
          ...chat("bot", 0),
          status,
          messages: [failure],
          notice: "Lost connection",
          notice_message_id: "failure",
        },
      }),
    );
    expect(html).toContain('badge error">error</span>');
    expect(html).not.toContain("hidden message");
    expect(html.match(/Lost connection/g)).toHaveLength(1);
  }
});

test("read entries fill the remaining sidebar slots", () => {
  const active = [chat("bot", 0)];
  const notice = (i) => ({
    id: `notice-${i}`,
    chat: chat("bot", i),
    body: `Notice ${i}`,
    created_at: new Date().toISOString(),
  });
  const read = Array.from({ length: 60 }, (_, i) => notice(i));
  const output = (unread) =>
    render(
      h(Sidebar, {
        data: { active, unread, read },
        currentId: null,
        connected: true,
        onOpen: noop,
      }),
    );
  expect(output([notice(99)]).match(/class="notification"/g)).toHaveLength(49);
  expect(output([]).match(/class="notification"/g)).toHaveLength(49);
  const overflow = output(
    Array.from({ length: 52 }, (_, i) => notice(i + 100)),
  );
  expect(overflow.match(/class="notification"/g)).toHaveLength(52);
  expect(overflow).not.toContain("Notice 0</span>");
});

test("token units change at a million", () => {
  expect(
    [123, 1000, 234567, 999999, 1000000, 2693000].map(compactTokens),
  ).toEqual(["123", "1k", "234k", "999k", "1m", "2.7m"]);
});

test("MCP badges expose readable payloads, not protocol or binary wrappers", () => {
  const html = render(
    h(ChatView, {
      chat: {
        ...chat("bot", 0),
        messages: [
          {
            id: "call",
            kind: "event",
            content: "mcpToolCall",
            data: {
              item: {
                type: "mcpToolCall",
                server: "files",
                tool: "read",
                status: "failed",
                arguments: { path: "`literal`.el" },
                result: {
                  content: [
                    { type: "text", text: "# Raw output" },
                    { type: "image", data: "binary-not-text" },
                  ],
                  structuredContent: { count: 0 },
                },
                error: { message: "File unavailable" },
              },
            },
          },
        ],
      },
    }),
  );
  expect(html).toContain('badge internal">mcp</span>');
  for (const part of [
    "files/read",
    "failed",
    "arguments",
    "`literal`.el",
    "# Raw output",
    "[image]",
    "error: File unavailable",
  ])
    expect(html).toContain(part);
  expect(html).not.toContain("mcpToolCall");
  expect(html).not.toContain("binary-not-text");
  expect(html).not.toContain("<h1>");
});

test("expansion survives sampled row replacement and regrouping", async () => {
  const { expandGroup, groupExpanded } = await import(
    "../src/pages/messageDetails"
  );
  const sample = {
    id: "sample-row",
    kind: "event",
    content: "mcpToolCall",
    data: { item: { id: "call-1" } },
  };
  const earlier = {
    id: "earlier",
    kind: "system",
    content: "Context",
    data: {},
  };
  const other = {
    id: "other",
    kind: "system",
    content: "Other group",
    data: {},
  };
  let expanded = expandGroup(new Set(), [sample, other], true);
  const stored = { ...sample, id: "stored-row" };
  expect(groupExpanded([earlier, stored], expanded)).toBe(true);
  expanded = expandGroup(expanded, [earlier, stored], true);
  expect(groupExpanded([earlier], expanded)).toBe(true);
  expect(expandGroup(expanded, [earlier, stored], true)).toBe(expanded);
  expanded = expandGroup(expanded, [earlier, stored], false);
  expect(groupExpanded([stored], expanded)).toBe(false);
  expect(groupExpanded([other], expanded)).toBe(true);
  expect(groupExpanded([stored], new Set())).toBe(false);
});

test("events show literal commands and patches with compact reasoning", () => {
  const items = [
    {
      type: "commandExecution",
      command: "echo `literal` # [not a link](path)",
      aggregatedOutput: "<script>literal output</script>",
      status: "completed",
      exitCode: 0,
    },
    { type: "reasoning", text: "Private reasoning body" },
    {
      type: "fileChange",
      status: "completed",
      changes: [
        { path: "/tmp/new.txt", kind: { type: "add" }, diff: "hello\nworld" },
        { path: "/tmp/old.txt", kind: { type: "delete" }, diff: "old\n" },
        {
          path: "/tmp/before.txt",
          kind: { type: "update", move_path: "/tmp/after.txt" },
          diff: "@@ -1 +1 @@\n-old\n+new\n\nMoved to: /tmp/after.txt",
        },
      ],
    },
  ];
  const html = render(
    h(ChatView, {
      chat: {
        ...chat("bot", 0),
        messages: items.map((item, i) => ({
          id: String(i),
          kind: "event",
          content: item.type,
          data: { item },
        })),
      },
    }),
  );
  expect(html).toContain('badge internal">exec</span>');
  expect(html.replace(/<[^>]+>/g, "")).toContain(
    "echo `literal` # [not a link](path)",
  );
  expect(html).toContain("hljs-built_in");
  expect(html).toContain("hljs-comment");
  expect(html).toContain("&lt;script&gt;literal output&lt;/script&gt;");
  expect(html).toContain("completed · exit 0");
  expect(html).toContain('badge internal">reasoning</span>');
  expect(html).not.toContain("Private reasoning body");
  expect(html).toContain('badge internal">file change</span>');
  expect(html).toContain("@@ -0,0 +1,2 @@");
  expect(html).toContain("+hello");
  expect(html).toContain("@@ -1,1 +0,0 @@");
  expect(html).toContain("\\ No newline at end of file");
  expect(html).toContain('class="diff-line added"');
  expect(html).toContain('class="diff-line removed"');
  expect(html).toContain("/tmp/after.txt");
  expect(html).not.toContain("Moved to:");
  expect(html).not.toContain("<a ");
});

test("turn bookkeeping uses metadata without hiding outcomes or literal text", () => {
  const message = (id, kind, content, data = {}) => ({
    id,
    kind,
    content,
    data,
  });
  const html = render(
    h(ChatView, {
      chat: {
        ...chat("bot", 1),
        messages: [
          message("s", "event", "Starting", { event: "turn/started" }),
          message("d", "event", "Done", {
            event: "turn/completed",
            turn: { status: "completed" },
          }),
          message("f", "event", "Codex turn failed", {
            event: "turn/completed",
            turn: { status: "failed" },
          }),
          message("u", "user", "Codex turn started"),
          message("a", "assistant", "Codex turn completed"),
        ],
      },
    }),
  );
  expect(html).not.toContain("Starting");
  expect(html).not.toContain("Done");
  expect(html).toContain("1 hidden message");
  expect(html).toContain("Codex turn failed");
  expect(html).toContain("Codex turn started");
  expect(html).toContain("Codex turn completed");
});

test("Markdown chat links keep labels and never expose the custom scheme to browser handlers", () => {
  const id = "12345678-1234-1234-1234-123456789abc";
  const html = render(
    h(ChatView, {
      chat: {
        ...chat("bot", 0),
        messages: [
          {
            id: "links",
            kind: "assistant",
            content: `[this chat](aibo://chat/${id}) [b123](aibo://bot/b123) [bad](aibo://bot/b256) [web](https://example.com) [file](/tmp/report.md) [unsafe](javascript:alert) ![image](aibo://bot/b123)`,
          },
        ],
      },
    }),
  );
  expect(html).toContain(`href="/chats/${id}"`);
  expect(html).toContain('href="#" title="aibo://bot/b123">b123</a>');
  expect(html).toContain(
    'aria-disabled="true" title="Invalid Aibo chat link">bad</a>',
  );
  expect(html).toContain('href="https://example.com"');
  expect(html).toContain('href="/tmp/report.md"');
  expect(html).not.toMatch(/(?:href|src)="(?:aibo|javascript):/);
  expect(html).toContain(">this chat</a>");
});

test("progress stays inline before the final answer", () => {
  const html = render(
    h(ChatView, {
      chat: {
        ...chat("bot", 0),
        messages: [
          { id: "request", kind: "user", content: "Request", data: {} },
          {
            id: "progress",
            kind: "assistant",
            content: "Checking the layout.",
            data: { item: { type: "agentMessage", phase: "commentary" } },
          },
          { id: "tool", kind: "tool", content: "Tool payload", data: {} },
        ],
      },
    }),
  );
  expect(html).toContain("Checking the layout.");
  expect(html).toContain('class="hidden-messages"');
  expect(html.indexOf("Checking the layout.")).toBeLessThan(
    html.indexOf('class="hidden-messages"'),
  );
});
