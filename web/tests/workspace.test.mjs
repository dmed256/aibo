import { test, expect, mock } from "bun:test";
import { api } from "../src/api";
import { Workspace, recent } from "../src/state/workspace";
import { Search } from "../src/state/search";

const chat = (index, kind = "bot") => ({
  id: `${kind}-${index}`,
  kind,
  label: `${kind === "bot" ? "b" : "m"}${index}`,
  title: `Task ${index}`,
  bot_number: kind === "bot" ? index : null,
  status: "idle",
  active: false,
  location: { id: "location", name: "aibo", path: "/tmp/aibo" },
  project: null,
  activity_at: "2026-09-05T00:00:00Z",
  last_active_at: null,
  created_at: "2026-09-05T00:00:00Z",
  messages: [],
});
const attachment = {
  path: "/tmp/image.png",
  url: "/api/attachments/image.png",
  name: "image.png",
  size: 32,
};
const deferred = () => {
  let resolve, reject;
  const promise = new Promise((yes, no) => {
    resolve = yes;
    reject = no;
  });
  return { promise, resolve, reject };
};
const client = (overrides = {}) => ({
  chats: mock(async () => []),
  chat: mock(async (id) => ({ ...chat(0), id })),
  projects: mock(async () => []),
  locations: mock(async () => [chat(0).location]),
  sidebar: mock(async () => ({
    active: [],
    unread: [],
    read: [],
    read_count: 0,
  })),
  createChat: mock(async () => chat(0, "manager")),
  submit: mock(async () => ({})),
  readNotification: mock(async () => ({})),
  readChatNotifications: mock(async () => ({})),
  ...overrides,
});
const flush = async () => {
  for (let i = 0; i < 20; i++) await Promise.resolve();
};

test("shortcuts prioritize active chats, then latest message time, before limiting", () => {
  const chats = ["bot", "manager"].flatMap((kind) => {
    const item = (index, status, last_active_at) => ({
      ...chat(index, kind),
      status,
      active: ["queued", "running"].includes(status),
      last_active_at,
    });
    return [
      ...Array.from({ length: 12 }, (_, i) =>
        item(i, "completed", "2026-09-05T01:00:00Z"),
      ),
      item(12, "running", "2026-09-05T02:00:00Z"),
      item(13, "queued", "2026-09-05T03:00:00Z"),
      {
        ...item(14, "error", "2026-09-05T05:00:00Z"),
        activity_at: "2026-09-10T00:00:00Z",
      },
      item(15, "completed", "2026-09-05T01:00:00-07:00"),
      { ...item(16, "idle", null), created_at: "2026-09-05T04:00:00Z" },
    ];
  });
  const before = [...chats];
  for (const kind of ["bot", "manager"]) {
    const sorted = recent(chats, kind);
    expect(sorted.map((chat) => chat.id)).toEqual(
      [
        13,
        12,
        15,
        14,
        16,
        ...Array.from({ length: kind === "bot" ? 4 : 5 }, (_, i) => i),
      ].map((index) => `${kind}-${index}`),
    );
  }
  expect(chats).toEqual(before);
});

test("shortcuts retain older active chats and reorder on completion", async () => {
  const finished = {
    ...chat(1),
    status: "completed",
    last_active_at: "2026-09-05T05:00:00Z",
  };
  const running = {
    ...chat(500),
    active: true,
    status: "running",
    last_active_at: "2026-09-05T01:00:00Z",
  };
  const source = client({
    chats: mock(async () => [finished]),
    sidebar: mock(async () => ({ active: [running], unread: [], read: [] })),
  });
  const workspace = new Workspace(source);
  await workspace.refresh();
  expect(
    recent(workspace.getSnapshot().chats, "bot").map((chat) => chat.id),
  ).toEqual([running.id, finished.id]);
  const completed = {
    ...running,
    active: false,
    status: "completed",
    last_active_at: "2026-09-05T06:00:00Z",
  };
  const resumed = {
    ...finished,
    active: true,
    status: "queued",
    last_active_at: "2026-09-05T05:30:00Z",
  };
  source.chats = mock(async () => [completed, resumed]);
  source.sidebar = mock(async () => ({
    active: [resumed],
    unread: [],
    read: [],
  }));
  await workspace.refresh();
  expect(
    recent(workspace.getSnapshot().chats, "bot").map((chat) => chat.id),
  ).toEqual([resumed.id, completed.id]);
});

test("API sends paging and independent home filters", async () => {
  const fetch = globalThis.fetch;
  const requests = [];
  globalThis.fetch = mock(async (url) => {
    requests.push(new URL(url, "http://local"));
    return Response.json([]);
  });
  try {
    await api.chats({ query: "1 scroll", limit: 51, offset: 50 });
    await api.chats({ projectId: "older-project", limit: 10 });
    await api.chats({ unassigned: true, limit: 10 });
    expect(Object.fromEntries(requests[0].searchParams)).toEqual({
      query: "1 scroll",
      limit: "51",
      offset: "50",
    });
    expect(requests[1].searchParams.get("project_id")).toBe("older-project");
    expect(requests[2].searchParams.get("unassigned")).toBe("true");
  } finally {
    globalThis.fetch = fetch;
  }
});

test("search pages 50/50/final and bounds selection", async () => {
  const all = Array.from({ length: 123 }, (_, index) => chat(index));
  const source = client({
    chats: mock(async ({ offset, limit }) => all.slice(offset, offset + limit)),
  });
  const search = new Search(source);
  await search.fetch();
  expect(search.getSnapshot().results).toHaveLength(50);
  search.select(500);
  expect(search.getSnapshot().selection).toBe(50);
  expect(await search.activate()).toBeNull();
  expect(search.getSnapshot().results).toHaveLength(100);
  expect(search.getSnapshot().selection).toBe(50);
  search.select(100);
  await search.activate();
  expect(search.getSnapshot().results).toHaveLength(123);
  expect(search.getSnapshot().more).toBe(false);
  search.select(500);
  expect(await search.activate()).toBe("bot-122");
  search.select(-500);
  expect(search.getSnapshot().selection).toBe(-1);
  expect(source.chats.mock.calls.map(([params]) => params.offset)).toEqual([
    0, 50, 100,
  ]);
});

test("query edits invalidate old replies before debounce", async () => {
  const old = deferred(),
    latest = deferred();
  const source = client({
    chats: mock(({ query }) =>
      query === "new" ? latest.promise : old.promise,
    ),
  });
  const search = new Search(source);
  const first = search.fetch();
  search.change("new");
  old.resolve([chat(0)]);
  await first;
  expect(search.getSnapshot().results).toEqual([]);
  const second = search.fetch();
  latest.resolve([]);
  await second;
  search.select(100);
  expect(search.getSnapshot().selection).toBe(-1);
  expect(await search.activate()).toBeNull();
});

test("failed continuation retains results and retries once while loading", async () => {
  const next = deferred();
  const source = client({
    chats: mock(async ({ offset }) =>
      offset ? next.promise : Array.from({ length: 51 }, (_, i) => chat(i)),
    ),
  });
  const search = new Search(source);
  await search.fetch();
  search.select(50);
  const first = search.activate();
  await search.activate();
  expect(source.chats).toHaveBeenCalledTimes(2);
  next.reject(new Error("offline"));
  await first;
  expect(search.getSnapshot().results).toHaveLength(50);
  expect(search.getSnapshot().selection).toBe(50);
  source.chats = mock(async () => [chat(50)]);
  await search.activate();
  expect(search.getSnapshot().results).toHaveLength(51);
  expect(search.getSnapshot().more).toBe(false);
});

test("home groups go beyond recent cache and filter shadow records", async () => {
  const shadow = { ...chat(9), kind: "shadow" };
  const source = client({
    chats: mock(async (params) =>
      params?.projectId
        ? [chat(500)]
        : params?.unassigned
          ? [chat(501)]
          : [shadow, ...Array.from({ length: 100 }, (_, i) => chat(i))],
    ),
    projects: mock(async (archived) =>
      archived
        ? []
        : [{ id: "old-project", name: "old", description: "older work" }],
    ),
    sidebar: mock(async () => ({
      active: [shadow, chat(2)],
      unread: [{ id: "private", chat: shadow }],
      read: [],
      read_count: 0,
    })),
  });
  const workspace = new Workspace(source);
  workspace.edit("  keep this draft\n");
  workspace.attach([attachment]);
  await workspace.refresh();
  const state = workspace.getSnapshot();
  expect(state.groups["old-project"][0].id).toBe("bot-500");
  expect(state.groups[""][0].id).toBe("bot-501");
  expect(state.chats).toHaveLength(100);
  expect(state.sidebar.active).toEqual([chat(2)]);
  expect(state.sidebar.unread).toEqual([]);
  expect(state.text).toBe("  keep this draft\n");
  expect(state.attachments).toEqual([attachment]);
  expect(() => workspace.target(shadow)).toThrow("Internal");
  expect(
    recent([shadow, chat(0, "manager"), ...state.chats], "bot"),
  ).toHaveLength(9);
});

test("refresh coalesces bursts without starving its first snapshot", async () => {
  const gate = deferred();
  const source = client({
    chats: mock(async (options) => (options ? [] : gate.promise)),
  });
  const workspace = new Workspace(source);
  const first = workspace.refresh();
  for (let i = 0; i < 100; i++) expect(workspace.refresh()).toBe(first);
  gate.resolve([chat(1)]);
  await first;
  expect(workspace.getSnapshot().chats).toEqual([chat(1)]);
  await flush();
  expect(source.chats.mock.calls.filter(([options]) => !options)).toHaveLength(
    2,
  );
});

test("old chat loads and refreshes cannot undo newer navigation", async () => {
  const gate = deferred();
  const source = client({
    chat: mock((id) =>
      id === "old" ? gate.promise : Promise.resolve({ ...chat(2), id }),
    ),
  });
  const workspace = new Workspace(source);
  const opening = workspace.open("old");
  await workspace.open("new");
  gate.resolve({ ...chat(1), id: "old" });
  await opening;
  expect(workspace.getSnapshot().current.id).toBe("new");
  await flush();
  const refreshGate = deferred();
  source.chat = mock(() => refreshGate.promise);
  const refreshing = workspace.refresh();
  workspace.page("locations");
  refreshGate.resolve({ ...chat(2), id: "new" });
  await refreshing;
  expect(workspace.getSnapshot().page).toBe("locations");
  expect(workspace.getSnapshot().current).toBeNull();
});

test("submit preserves literal text, rejects duplicates and keeps a newer draft", async () => {
  const gate = deferred();
  const source = client({ submit: mock(() => gate.promise) });
  const workspace = new Workspace(source);
  workspace.target(chat(1));
  workspace.edit("  literal request\n\n");
  workspace.attach([attachment]);
  const send = workspace.submit();
  expect(await workspace.submit()).toBeNull();
  workspace.edit("new draft");
  workspace.target(chat(2));
  gate.resolve({});
  expect(await send).toEqual({ label: "b1", cleared: false });
  expect(source.submit.mock.calls).toEqual([
    ["bot-1", "  literal request\n\n", ["/tmp/image.png"]],
  ]);
  expect(workspace.getSnapshot().text).toBe("new draft");
  expect(workspace.getSnapshot().attachments).toEqual([attachment]);
  expect(workspace.getSnapshot().target.id).toBe("bot-2");
});

test("failed image-only submission retries the same created manager", async () => {
  const source = client({
    submit: mock(async () => {
      throw new Error("offline");
    }),
  });
  const workspace = new Workspace(source);
  workspace.attach([attachment]);
  await expect(workspace.submit()).rejects.toThrow("offline");
  expect(workspace.getSnapshot().attachments).toEqual([attachment]);
  expect(workspace.getSnapshot().target.id).toBe("manager-0");
  source.submit = mock(async () => ({}));
  expect(await workspace.submit()).toEqual({ label: "m0", cleared: true });
  expect(source.createChat).toHaveBeenCalledTimes(1);
  expect(source.submit.mock.calls).toEqual([
    ["manager-0", "", ["/tmp/image.png"]],
  ]);
  expect(workspace.getSnapshot().attachments).toEqual([]);
});

test("new manager is a draft until send", async () => {
  const gate = deferred();
  const source = client({ createChat: mock(() => gate.promise) });
  const workspace = new Workspace(source);
  workspace.target(chat(0, "manager"));
  workspace.edit("retained");
  workspace.newManager();
  workspace.newManager();
  expect(source.createChat.mock.calls).toEqual([]);
  expect(workspace.getSnapshot().target).toBeNull();
  const pending = workspace.submit();
  workspace.page("projects");
  gate.resolve(chat(1, "manager"));
  await pending;
  expect(source.createChat).toHaveBeenCalledTimes(1);
  expect(workspace.getSnapshot().page).toBe("projects");
  expect(source.submit.mock.calls[0][1]).toBe("retained");
});

test("title events update caches without fetching or replacing messages", async () => {
  const current = chat(0, "manager");
  const source = client({
    chats: mock(async () => [current]),
    chat: mock(async () => current),
    sidebar: mock(async () => ({
      active: [current],
      unread: [{ chat: current }],
      read: [{ chat: current }],
    })),
  });
  const workspace = new Workspace(source);
  await workspace.open(current.id);
  await workspace.refresh();
  workspace.edit("Keep typing");
  const messages = workspace.getSnapshot().current.messages;
  const requests = source.chats.mock.calls.length;
  expect(
    workspace.titleEvent({
      kind: "chat_title_updated",
      chat_id: current.id,
      title: "New title #aibo #ui",
    }),
  ).toBe(true);
  const state = workspace.getSnapshot();
  expect(state.current.messages).toBe(messages);
  expect(state.text).toBe("Keep typing");
  for (const item of [
    state.current,
    state.target,
    state.chats[0],
    state.groups[""][0],
    state.sidebar.active[0],
    state.sidebar.unread[0].chat,
  ]) {
    expect(item.title).toBe("New title #aibo #ui");
  }
  expect(source.chats.mock.calls.length).toBe(requests);
  // An older response in flight must not restore the previous title.
  await workspace.refresh();
  expect(workspace.getSnapshot().current.title).toBe("New title #aibo #ui");
  expect(workspace.titleEvent(null)).toBe(false);
});

test("pending attachments cannot reappear after clear or be omitted by early submit", async () => {
  const source = client();
  const workspace = new Workspace(source);
  workspace.target(chat(1));
  workspace.edit("request");
  const gate = deferred();
  const importing = workspace.importAttachment(() => gate.promise);
  expect(workspace.getSnapshot().uploading).toBe(1);
  expect(await workspace.submit()).toBeNull();
  workspace.clear();
  gate.resolve(attachment);
  await importing;
  expect(workspace.getSnapshot().attachments).toEqual([]);
  expect(workspace.getSnapshot().uploading).toBe(0);
  expect(source.submit).not.toHaveBeenCalled();
  const next = deferred();
  const adding = workspace.importAttachment(() => next.promise);
  workspace.edit("typing while uploading");
  next.resolve(attachment);
  await adding;
  expect(workspace.getSnapshot().attachments).toEqual([attachment]);
});

test("failed refresh preserves loaded data and retry recovers the initial error", async () => {
  const source = client({
    chats: mock(async () => {
      throw new Error("offline");
    }),
  });
  const workspace = new Workspace(source);
  workspace.edit("retained");
  await workspace.refresh();
  expect(workspace.getSnapshot().loaded).toBe(false);
  expect(workspace.getSnapshot().error).toBe("offline");
  source.chats = mock(async () => [chat(1)]);
  await workspace.refresh();
  expect(workspace.getSnapshot().loaded).toBe(true);
  expect(workspace.getSnapshot().error).toBeNull();
  source.chats = mock(async () => {
    throw new Error("offline again");
  });
  await workspace.refresh();
  expect(workspace.getSnapshot().chats).toEqual([chat(1)]);
  expect(workspace.getSnapshot().text).toBe("retained");
});

test("cleared upload failures stay quiet while a new upload can proceed", async () => {
  const workspace = new Workspace(client());
  const old = deferred(),
    next = deferred();
  const cancelled = workspace.importAttachment(() => old.promise);
  workspace.clear();
  expect(workspace.getSnapshot().uploading).toBe(0);
  const current = workspace.importAttachment(() => next.promise);
  old.reject(new Error("old clipboard failure"));
  await cancelled;
  expect(workspace.getSnapshot().uploading).toBe(1);
  next.resolve(attachment);
  await current;
  expect(workspace.getSnapshot().uploading).toBe(0);
  expect(workspace.getSnapshot().attachments).toEqual([attachment]);
});

test("recipient swap remembers each role without changing the draft or view", async () => {
  const bot = chat(2),
    manager = chat(3, "manager");
  const workspace = new Workspace(
    client({
      chats: mock(async () => [bot, manager]),
      chat: mock(async () => bot),
    }),
  );
  await workspace.refresh();
  await workspace.open(bot.id);
  workspace.edit("Keep this draft");
  workspace.swapTarget();
  expect(workspace.getSnapshot().target.id).toBe(manager.id);
  workspace.swapTarget();
  expect(workspace.getSnapshot().target.id).toBe(bot.id);
  workspace.target(chat(9, "manager"));
  workspace.swapTarget();
  expect(workspace.getSnapshot().target.id).toBe(bot.id);
  workspace.swapTarget();
  expect(workspace.getSnapshot().target.id).toBe("manager-9");
  expect(workspace.getSnapshot().text).toBe("Keep this draft");
  expect(workspace.getSnapshot().current.id).toBe(bot.id);
  const managersOnly = new Workspace(client());
  managersOnly.target(manager);
  managersOnly.swapTarget();
  expect(managersOnly.getSnapshot().target.id).toBe(manager.id);
});

test("project search keeps its filter through paging", async () => {
  const source = client({
    chats: mock(async () => Array.from({ length: 51 }, (_, i) => chat(i))),
  });
  const search = new Search(source, "project-1");
  search.change("sidebar");
  await search.fetch();
  await search.fetch(true);
  expect(source.chats.mock.calls.map(([request]) => request)).toEqual([
    { query: "sidebar", projectId: "project-1", offset: 0, limit: 51 },
    { query: "sidebar", projectId: "project-1", offset: 50, limit: 51 },
  ]);
});

test("notification entry reads its whole chat, including active entries", async () => {
  const source = client();
  const workspace = new Workspace(source);
  await workspace.open("bot-0", true);
  expect(source.readChatNotifications.mock.calls).toEqual([["bot-0"]]);
  expect(source.readNotification).not.toHaveBeenCalled();
  await workspace.open("bot-1");
  expect(source.readChatNotifications).toHaveBeenCalledTimes(1);
});

test("deep links restore chats, archived projects, and search queries", async () => {
  const project = {
    id: "older-project",
    name: "Archived",
    archived_at: "2026-01-01",
  };
  const source = client({
    projects: mock(async (archived) => (archived ? [project] : [])),
  });
  const workspace = new Workspace(source);
  workspace.edit("Keep the draft");
  await workspace.navigate("/chats/bot-9");
  expect(workspace.getSnapshot().current.id).toBe("bot-9");
  expect(workspace.getSnapshot().routePath).toBe("/chats/bot-9");
  await workspace.navigate("/projects/older-project?q=scroll+state");
  expect(workspace.getSnapshot().currentProject).toEqual(project);
  expect(workspace.getSnapshot().searchQuery).toBe("scroll state");
  expect(workspace.getSnapshot().text).toBe("Keep the draft");
  await workspace.navigate("/chats?q=layout");
  expect(workspace.getSnapshot().page).toBe("search");
  expect(workspace.getSnapshot().searchQuery).toBe("layout");
  await workspace.navigate("/unknown");
  expect(workspace.getSnapshot().routeError).toBe("Page not found");
  expect(workspace.getSnapshot().routePath).toBe("/unknown");
});

test("tutorial navigation preserves the live draft and recipient", async () => {
  const workspace = new Workspace(client());
  const recipient = chat(2);
  workspace.target(recipient);
  workspace.edit("Keep this draft");
  workspace.attach([attachment]);
  await workspace.navigate("/tutorial");
  expect(workspace.getSnapshot().page).toBe("tutorial");
  expect(workspace.getSnapshot().routePath).toBe("/tutorial");
  expect(workspace.getSnapshot().routeError).toBeNull();
  await workspace.navigate("/");
  expect(workspace.getSnapshot().target).toEqual(recipient);
  expect(workspace.getSnapshot().text).toBe("Keep this draft");
  expect(workspace.getSnapshot().attachments).toEqual([attachment]);
});

test("a stale failed route cannot replace newer navigation", async () => {
  const gate = deferred();
  const workspace = new Workspace(client({ chat: () => gate.promise }));
  const opening = workspace.open("missing");
  expect(workspace.getSnapshot().routePath).toBe("/chats/missing");
  workspace.page("projects");
  gate.reject(new Error("Chat not found"));
  await opening;
  expect(workspace.getSnapshot().routePath).toBe("/projects");
  expect(workspace.getSnapshot().routeError).toBeNull();
});

const exactId = "12345678-1234-1234-1234-123456789abc";
const newerId = "87654321-1234-1234-1234-123456789abc";

test("Aibo links use UUIDs and resolve repeated bot labels on each click", async () => {
  const latestBot = mock(async () => ({ ...chat(123), id: newerId }));
  const backend = client({ latestBot });
  const workspace = new Workspace(backend);
  workspace.edit("Keep this draft");
  workspace.attach([attachment]);
  await workspace.openAiboLink(`aibo://chat/${exactId}`);
  expect(backend.chat).toHaveBeenLastCalledWith(exactId);
  await workspace.openAiboLink("aibo://bot/b123");
  expect(workspace.getSnapshot().current.id).toBe(newerId);
  latestBot.mockImplementation(async () => ({ ...chat(123), id: exactId }));
  await workspace.openAiboLink("aibo://bot/b123");
  expect(workspace.getSnapshot().current.id).toBe(exactId);
  expect(latestBot.mock.calls).toEqual([[123], [123]]);
  expect(workspace.getSnapshot().text).toBe("Keep this draft");
  expect(workspace.getSnapshot().attachments).toEqual([attachment]);
});

test("bad, missing, and hidden Aibo targets fail without clearing drafts", async () => {
  const backend = client({
    latestBot: mock(async () => {
      throw new Error("Not found");
    }),
  });
  const workspace = new Workspace(backend);
  workspace.edit("Unsent");
  for (const url of [
    "aibo://chat/nope",
    "aibo://bot/b256",
    "aibo://bot/b-1",
    "aibo://bot/b01",
    "aibo://bot/b1?x",
    "aibo://else/b1",
    `aibo://chat/${exactId}/extra`,
  ])
    await expect(workspace.openAiboLink(url)).rejects.toThrow("Invalid");
  expect(backend.chat).not.toHaveBeenCalled();
  expect(backend.latestBot).not.toHaveBeenCalled();
  await expect(workspace.openAiboLink("aibo://bot/b123")).rejects.toThrow(
    "Not found",
  );
  backend.chat.mockImplementation(async () => ({
    ...chat(0, "shadow"),
    id: exactId,
  }));
  await expect(
    workspace.openAiboLink(`aibo://chat/${exactId}`),
  ).rejects.toThrow("Internal");
  expect(workspace.getSnapshot().current).toBeNull();
  backend.chat.mockImplementation(async () => {
    throw new Error("Not found");
  });
  await expect(
    workspace.openAiboLink(`aibo://chat/${exactId}`),
  ).rejects.toThrow("Not found");
  expect(workspace.getSnapshot().text).toBe("Unsent");
});

test("slow bot resolution cannot override later navigation", async () => {
  const pending = deferred();
  const workspace = new Workspace(client({ latestBot: () => pending.promise }));
  const opening = workspace.openAiboLink("aibo://bot/b123");
  await workspace.openAiboLink(`aibo://chat/${exactId}`);
  pending.resolve({ ...chat(123), id: newerId });
  await opening;
  expect(workspace.getSnapshot().current.id).toBe(exactId);
});
