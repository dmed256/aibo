import { Store } from "./store";
import { api } from "../api";
import {
  chatPath,
  pagePaths,
  parseRoute,
  parseAiboLink,
  projectPath,
  searchPath,
} from "../routing";
import type {
  Attachment,
  Chat,
  ChatSummary,
  Location,
  Page,
  Project,
  SidebarData,
} from "../types";

export function publicChat(chat: ChatSummary): boolean {
  return chat.kind === "manager" || chat.kind === "bot";
}

export function recent(
  chats: ChatSummary[],
  kind: "manager" | "bot",
): ChatSummary[] {
  return chats
    .filter((chat) => chat.kind === kind)
    .sort(
      (a, b) =>
        Number(b.active) - Number(a.active) ||
        Date.parse(b.last_active_at ?? b.created_at) -
          Date.parse(a.last_active_at ?? a.created_at),
    )
    .slice(0, kind === "bot" ? 9 : 10);
}

export interface WorkspaceState {
  chats: ChatSummary[];
  projects: Project[];
  archived: Project[];
  locations: Location[];
  groups: Record<string, ChatSummary[]>;
  sidebar: SidebarData;
  current: Chat | null;
  currentProject: Project | null;
  target: ChatSummary | null;
  page: Page;
  routePath: string;
  routeLoading: boolean;
  routeError: string | null;
  searchQuery: string;
  text: string;
  attachments: Attachment[];
  uploading: number;
  sending: boolean;
  loading: boolean;
  loaded: boolean;
  error: string | null;
}

export class Workspace extends Store<WorkspaceState> {
  private navigation = 0;
  private draftVersion = 0;
  private clearVersion = 0;
  private targetVersion = 0;
  private refreshFlight: Promise<void> | null = null;
  private refreshAgain = false;
  private titles = new Map<string, string>();
  private lastTargets = new Map<"manager" | "bot", ChatSummary>();

  constructor(private client = api) {
    super({
      chats: [],
      projects: [],
      archived: [],
      locations: [],
      groups: {},
      sidebar: { active: [], unread: [], read: [], read_count: 0 },
      current: null,
      target: null,
      currentProject: null,
      page: "home",
      routePath: "/",
      routeLoading: false,
      routeError: null,
      searchQuery: "",
      text: "",
      attachments: [],
      uploading: 0,
      sending: false,
      loading: true,
      loaded: false,
      error: null,
    });
  }

  edit(text: string): void {
    this.draftVersion++;
    this.update({ text });
  }

  attach(attachments: Attachment[]): void {
    this.draftVersion++;
    this.update({ attachments });
  }

  clear(): void {
    this.draftVersion++;
    this.clearVersion++;
    this.update({ text: "", attachments: [], uploading: 0 });
  }

  async importAttachment(load: () => Promise<Attachment>): Promise<void> {
    const version = this.clearVersion;
    this.update({ uploading: this.state.uploading + 1 });
    try {
      const attachment = await load();
      if (version === this.clearVersion)
        this.attach([...this.state.attachments, attachment]);
    } catch (error) {
      if (version === this.clearVersion) throw error;
    } finally {
      if (version === this.clearVersion)
        this.update({ uploading: this.state.uploading - 1 });
    }
  }

  target(chat: ChatSummary | null): void {
    if (chat && !publicChat(chat))
      throw new Error("Internal chats are not visible");
    if (chat) this.lastTargets.set(chat.kind, chat);
    this.targetVersion++;
    this.update({ target: chat });
  }

  page(page: Page): void {
    this.navigation++;
    this.update({
      page,
      current: null,
      currentProject: null,
      routePath: pagePaths[page],
      routeLoading: false,
      routeError: null,
      searchQuery: "",
    });
  }

  project(project: Project): void {
    this.navigation++;
    this.update({
      page: "project",
      current: null,
      currentProject: project,
      routePath: projectPath(project.id),
      routeLoading: false,
      routeError: null,
      searchQuery: "",
    });
  }

  swapTarget(): void {
    const kind = this.state.target?.kind === "bot" ? "manager" : "bot";
    const previous = this.lastTargets.get(kind);
    const target =
      this.state.chats.find((chat) => chat.id === previous?.id) ??
      previous ??
      recent(this.state.chats, kind)[0] ??
      null;
    if (target || kind === "manager") this.target(target);
  }

  async open(id: string, fromNotification = false): Promise<void> {
    const navigation = ++this.navigation;
    this.update({
      page: "chat",
      routePath: chatPath(id),
      routeLoading: true,
      routeError: null,
      current: this.state.current?.id === id ? this.state.current : null,
      currentProject: null,
    });
    try {
      if (fromNotification) await this.client.readChatNotifications(id);
      const chat = this.withTitle(await this.client.chat(id));
      if (!publicChat(chat)) throw new Error("Internal chats are not visible");
      if (navigation !== this.navigation) return;
      this.target(chat);
      this.update({ current: chat, routeLoading: false });
      void this.refresh();
    } catch (error) {
      if (navigation !== this.navigation) return;
      this.update({
        routeLoading: false,
        routeError: error instanceof Error ? error.message : String(error),
      });
      throw error;
    }
  }

  async openAiboLink(url: string): Promise<void> {
    const target = parseAiboLink(url);
    if (!target) throw new Error("Invalid Aibo chat link");
    if (target.kind === "chat") return this.open(target.id);
    const navigation = ++this.navigation;
    const chat = await this.client.latestBot(target.number);
    if (navigation !== this.navigation) return;
    if (chat.kind !== "bot") throw new Error("Bot chat not found");
    return this.open(chat.id);
  }

  searchQuery(query: string): void {
    const path =
      this.state.page === "project" && this.state.currentProject
        ? projectPath(this.state.currentProject.id)
        : pagePaths.search;
    this.update({ searchQuery: query, routePath: searchPath(path, query) });
  }

  async navigate(path: string): Promise<void> {
    const route = parseRoute(path);
    if (!route) {
      this.navigation++;
      this.update({
        routePath: path,
        page: "home",
        current: null,
        currentProject: null,
        routeLoading: false,
        routeError: "Page not found",
      });
      return;
    }
    if (route.page === "chat") return this.open(route.id!);
    if (route.page === "project") {
      const navigation = ++this.navigation;
      this.update({
        page: "project",
        current: null,
        currentProject: null,
        routePath: searchPath(projectPath(route.id!), route.query),
        routeLoading: true,
        routeError: null,
        searchQuery: route.query,
      });
      try {
        const projects = [...this.state.projects, ...this.state.archived];
        let project = projects.find((project) => project.id === route.id);
        if (!project)
          project = (
            await Promise.all([
              this.client.projects(),
              this.client.projects(true),
            ])
          )
            .flat()
            .find((project) => project.id === route.id);
        if (navigation !== this.navigation) return;
        if (!project) throw new Error("Project not found");
        this.update({ currentProject: project, routeLoading: false });
      } catch (error) {
        if (navigation !== this.navigation) return;
        this.update({
          routeLoading: false,
          routeError: error instanceof Error ? error.message : String(error),
        });
      }
      return;
    }
    this.page(route.page);
    if (route.page === "search") this.searchQuery(route.query);
  }

  connectHistory(browser: Window): () => void {
    const locationPath = () =>
      browser.location.pathname + browser.location.search;
    const restore = () => {
      void this.navigate(locationPath()).catch(() => {});
    };
    restore();
    const unsubscribe = this.subscribe(() => {
      const path = this.state.routePath;
      if (path === locationPath()) return;
      if (path.split("?")[0] === browser.location.pathname)
        browser.history.replaceState(null, "", path);
      else browser.history.pushState(null, "", path);
    });
    browser.addEventListener("popstate", restore);
    return () => {
      unsubscribe();
      browser.removeEventListener("popstate", restore);
    };
  }

  newManager(): void {
    this.target(null);
  }

  async setGoal(chat: ChatSummary, enabled: boolean): Promise<void> {
    const updated = await this.client.setGoal(chat.id, enabled);
    this.update({
      chats: this.state.chats.map((item) =>
        item.id === chat.id ? updated : item,
      ),
      ...(this.state.current?.id === chat.id ? { current: updated } : {}),
      ...(this.state.target?.id === chat.id ? { target: updated } : {}),
    });
    void this.refresh();
  }

  private withTitle = <T extends ChatSummary>(chat: T): T => {
    const title = this.titles.get(chat.id);
    return title !== undefined && title !== chat.title
      ? { ...chat, title }
      : chat;
  };

  titleEvent(event: unknown): boolean {
    if (
      !event ||
      typeof event !== "object" ||
      !("kind" in event) ||
      !("chat_id" in event) ||
      !("title" in event)
    )
      return false;
    if (
      event.kind !== "chat_title_updated" ||
      typeof event.chat_id !== "string" ||
      typeof event.title !== "string"
    )
      return false;
    this.titles.set(event.chat_id, event.title);
    const { chats, groups, sidebar, current, target } = this.state;
    this.update({
      chats: chats.map(this.withTitle),
      groups: Object.fromEntries(
        Object.entries(groups).map(([id, chats]) => [
          id,
          chats.map(this.withTitle),
        ]),
      ),
      sidebar: {
        ...sidebar,
        active: sidebar.active.map(this.withTitle),
        unread: sidebar.unread.map((item) => ({
          ...item,
          chat: this.withTitle(item.chat),
        })),
        read: sidebar.read.map((item) => ({
          ...item,
          chat: this.withTitle(item.chat),
        })),
      },
      current: current ? this.withTitle(current) : null,
      target: target ? this.withTitle(target) : null,
    });
    return true;
  }

  reconnect(): Promise<void> {
    this.titles.clear();
    return this.refresh();
  }

  refresh(): Promise<void> {
    if (this.refreshFlight) {
      this.refreshAgain = true;
      return this.refreshFlight;
    }
    this.refreshFlight = this.load()
      .catch((error: unknown) => {
        this.update({
          loading: false,
          error: error instanceof Error ? error.message : String(error),
        });
      })
      .finally(() => {
        this.refreshFlight = null;
        if (this.refreshAgain) {
          this.refreshAgain = false;
          void this.refresh();
        }
      });
    return this.refreshFlight;
  }

  private async load(): Promise<void> {
    const navigation = this.navigation;
    const currentId = this.state.current?.id;
    const [chats, sidebar, projects, archived, locations, current] =
      await Promise.all([
        this.client.chats(),
        this.client.sidebar(),
        this.client.projects(),
        this.client.projects(true),
        this.client.locations(),
        currentId ? this.client.chat(currentId) : null,
      ]);
    const groups = Object.fromEntries(
      await Promise.all([
        this.client
          .chats({ limit: 10, unassigned: true })
          .then((chats) => ["", chats.filter(publicChat)] as const),
        ...projects.map(
          async (project) =>
            [
              project.id,
              (
                await this.client.chats({ limit: 10, projectId: project.id })
              ).filter(publicChat),
            ] as const,
        ),
      ]),
    );
    // Long-running chats can fall outside the activity-ordered recent cache.
    const cachedIds = new Set(chats.map((chat) => chat.id));
    const visible = [
      ...chats,
      ...sidebar.active.filter((chat) => !cachedIds.has(chat.id)),
    ]
      .filter(publicChat)
      .map(this.withTitle);
    const next: Partial<WorkspaceState> = {
      chats: visible,
      projects,
      archived,
      locations,
      groups: Object.fromEntries(
        Object.entries(groups).map(([id, chats]) => [
          id,
          chats.map(this.withTitle),
        ]),
      ),
      loading: false,
      loaded: true,
      error: null,
      sidebar: {
        ...sidebar,
        active: sidebar.active.filter(publicChat).map(this.withTitle),
        unread: sidebar.unread
          .filter((item) => publicChat(item.chat))
          .map((item) => ({ ...item, chat: this.withTitle(item.chat) })),
        read: sidebar.read
          .filter((item) => publicChat(item.chat))
          .map((item) => ({ ...item, chat: this.withTitle(item.chat) })),
      },
    };
    if (
      current &&
      publicChat(current) &&
      navigation === this.navigation &&
      currentId === this.state.current?.id
    ) {
      next.current = this.withTitle(current);
    }
    if (this.state.target) {
      const target =
        next.current?.id === this.state.target.id
          ? next.current
          : visible.find((chat) => chat.id === this.state.target?.id);
      if (target) next.target = target;
    }
    this.update(next);
  }

  async submit(): Promise<{ label: string; cleared: boolean } | null> {
    if (
      this.state.sending ||
      this.state.uploading ||
      (!this.state.text.trim() && !this.state.attachments.length)
    )
      return null;
    const { text, attachments } = this.state;
    const draftVersion = this.draftVersion;
    const navigation = this.navigation;
    let targetVersion = this.targetVersion;
    let destination = this.state.target;
    this.update({ sending: true });
    try {
      if (!destination) {
        const created = await this.client.createChat({
          kind: "manager",
          title: "New chat",
        });
        destination = created;
        // Remember a created recipient even if submit fails, so retry cannot create another chat.
        if (targetVersion === this.targetVersion) {
          this.target(destination);
          targetVersion = this.targetVersion;
          if (navigation === this.navigation)
            this.update({
              current: created,
              page: "chat",
              routePath: chatPath(created.id),
              routeLoading: false,
              routeError: null,
            });
        }
      }
      await this.client.submit(
        destination.id,
        text,
        attachments.map((attachment) => attachment.path),
      );
      const cleared =
        this.draftVersion === draftVersion &&
        this.targetVersion === targetVersion;
      if (cleared) this.clear();
      void this.refresh();
      return { label: destination.label, cleared };
    } finally {
      this.update({ sending: false });
    }
  }
}
