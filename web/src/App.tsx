import {
  lazy,
  Suspense,
  useCallback,
  useEffect,
  useLayoutEffect,
  useRef,
  useState,
  useSyncExternalStore,
} from "react";
import { AiboLinkContext } from "./markdownLinks";
import { Badge } from "./components/Badge";
import { ChatMetadata } from "./components/ChatMetadata";
import CommandPalette from "./components/CommandPalette";
import { Composer } from "./components/Composer";
import { Sidebar } from "./components/Sidebar";
import { useAttachments } from "./hooks/useAttachments";
import { useReadingPosition } from "./hooks/useReadingPosition";
import { useShortcuts } from "./hooks/useShortcuts";
import { useWorkspaceEvents } from "./hooks/useWorkspaceEvents";
import { ChatView } from "./pages/ChatView";
import Customization from "./pages/Customization";
import { Help } from "./pages/Help";
import { Home } from "./pages/Home";
import { Locations } from "./pages/Locations";
import { Projects } from "./pages/Projects";
import SearchPage from "./pages/SearchPage";
import { chatPath, followLink, pagePaths, projectPath } from "./routing";
import { Workspace } from "./state/workspace";
import type { ChatSummary, Page, Project, SidebarItem } from "./types";

const Tutorial = lazy(() => import("./tutorial/Tutorial"));

export default function App() {
  const [workspace] = useState(() => new Workspace());
  const state = useSyncExternalStore(
    workspace.subscribe,
    workspace.getSnapshot,
    workspace.getSnapshot,
  );
  const chat = state.current;
  const [paletteOpen, setPaletteOpen] = useState(false);
  const [status, setStatus] = useState<{
    message: string;
    error: boolean;
  } | null>(null);
  const [goalBusy, setGoalBusy] = useState(false);
  const goalFlight = useRef(false);
  const statusTimer = useRef<number | null>(null);
  const content = useRef<HTMLElement>(null);
  const view = state.routePath;
  const viewedChatId = state.page === "chat" ? state.current?.id : null;

  useEffect(() => workspace.connectHistory(window), [workspace]);

  const focusInput = useCallback(() => {
    if (!document.querySelector("dialog[open]")) {
      document
        .querySelector<HTMLTextAreaElement>("#input")
        ?.focus({ preventScroll: true });
    }
  }, []);

  useLayoutEffect(() => {
    if (
      !["search", "project", "customization", "tutorial"].includes(state.page)
    )
      focusInput();
  }, [view, state.page, state.target?.id, focusInput]);

  useEffect(() => {
    const editable = (element: Element | null) =>
      element?.closest(
        "input,textarea,select,[contenteditable=true],dialog,.model-field,.aibo-dropdown-content,.tutorial",
      );
    const refocus = () => {
      if (!editable(document.activeElement)) focusInput();
    };
    const clicked = (event: MouseEvent) => {
      if (
        event.button !== 0 ||
        !(event.target instanceof Element) ||
        event.target.closest(
          "a,input,textarea,select,[contenteditable=true],dialog,.model-field,.aibo-dropdown-content,.tutorial",
        ) ||
        window.getSelection()?.toString() ||
        editable(document.activeElement)
      )
        return;
      focusInput();
    };
    window.addEventListener("focus", refocus);
    document.addEventListener("click", clicked);
    return () => {
      window.removeEventListener("focus", refocus);
      document.removeEventListener("click", clicked);
    };
  }, [focusInput]);

  const notify = useCallback((message: string, error = false) => {
    setStatus({ message, error });
    if (statusTimer.current) window.clearTimeout(statusTimer.current);
    statusTimer.current = window.setTimeout(() => setStatus(null), 4000);
  }, []);
  useEffect(
    () => () => {
      if (statusTimer.current !== null)
        window.clearTimeout(statusTimer.current);
    },
    [],
  );
  const report = useCallback(
    (error: unknown) => {
      notify(error instanceof Error ? error.message : String(error), true);
    },
    [notify],
  );
  const setGoal = useCallback(
    (chat: ChatSummary, enabled: boolean) => {
      if (goalFlight.current) return;
      goalFlight.current = true;
      setGoalBusy(true);
      void workspace
        .setGoal(chat, enabled)
        .catch(report)
        .finally(() => {
          goalFlight.current = false;
          setGoalBusy(false);
        });
    },
    [workspace, report],
  );
  const open = useCallback(
    (id: string) => {
      void workspace.open(id).catch(report);
    },
    [workspace, report],
  );
  const page = useCallback((page: Page) => workspace.page(page), [workspace]);
  const project = useCallback(
    (project: Project) => workspace.project(project),
    [workspace],
  );
  const sidebarOpen = useCallback(
    (item: SidebarItem) => {
      void workspace
        .open("chat" in item ? item.chat.id : item.id, true)
        .catch(report);
    },
    [workspace, report],
  );

  const connected = useWorkspaceEvents(workspace, viewedChatId, report);

  useReadingPosition(
    content,
    view,
    state.page === "chat",
    !state.routeLoading &&
      !state.routeError &&
      (state.page !== "chat" || Boolean(chat)),
  );

  const newManager = useCallback(() => {
    workspace.newManager();
    document.querySelector<HTMLTextAreaElement>("#input")?.focus();
  }, [workspace]);
  const swapTarget = useCallback(() => {
    workspace.swapTarget();
    document
      .querySelector<HTMLTextAreaElement>("#input")
      ?.focus({ preventScroll: true });
  }, [workspace]);
  const submit = useCallback(() => {
    void workspace
      .submit()
      .then((result) => {
        if (result)
          notify(
            result.cleared
              ? `Message sent to ${result.label}`
              : `Previous message sent to ${result.label} · current draft preserved`,
          );
      })
      .catch(report);
  }, [workspace, notify, report]);

  const { attachClipboard, paste } = useAttachments(workspace, report);

  useShortcuts({
    workspace,
    content,
    paletteOpen,
    setPaletteOpen,
    attachClipboard,
    open,
    sidebarOpen,
    page,
    newManager,
    submit,
    swapTarget,
  });

  if (state.page === "tutorial") {
    return (
      <Suspense fallback={<p className="empty">Loading tutorial…</p>}>
        <Tutorial onHome={() => page("home")} />
      </Suspense>
    );
  }

  const crumbs = [
    { label: "home", href: "/", action: () => page("home") },
    ...(state.page === "chat"
      ? [
          {
            label: "chats",
            href: pagePaths.search,
            action: () => page("search"),
          },
          {
            label: chat?.title ?? "Conversation",
            href: chat ? chatPath(chat.id) : state.routePath,
            action: null,
          },
        ]
      : state.page === "project"
        ? [
            {
              label: "projects",
              href: pagePaths.projects,
              action: () => page("projects"),
            },
            {
              label: state.currentProject?.name ?? "Project",
              href: state.currentProject
                ? projectPath(state.currentProject.id)
                : state.routePath,
              action: null,
            },
          ]
        : state.page !== "home"
          ? [
              {
                label: state.page === "search" ? "chats" : state.page,
                href: pagePaths[state.page],
                action: null,
              },
            ]
          : []),
  ];
  return (
    <>
      <div className="shell">
        <Sidebar
          data={state.sidebar}
          currentId={state.current?.id ?? null}
          connected={connected}
          onOpen={sidebarOpen}
        />
        <main className="workspace">
          <header className="workspace-header">
            <div className="toolbar">
              <nav className="breadcrumbs" aria-label="Breadcrumb">
                <ol>
                  {crumbs.map((crumb, index) => (
                    <li key={index}>
                      {index > 0 && (
                        <span
                          className="breadcrumb-separator"
                          aria-hidden="true"
                        >
                          /
                        </span>
                      )}
                      {state.page === "chat" &&
                        chat &&
                        index === crumbs.length - 1 && <Badge chat={chat} />}
                      <a
                        className="text-link"
                        href={crumb.href}
                        aria-current={
                          index === crumbs.length - 1 ? "page" : undefined
                        }
                        onClick={(event) =>
                          followLink(event, () => {
                            if (crumb.action) crumb.action();
                            else content.current?.scrollTo({ top: 0 });
                          })
                        }
                        title={crumb.label}
                      >
                        {crumb.label}
                      </a>
                    </li>
                  ))}
                </ol>
              </nav>
              <kbd
                className="palette-hint"
                title="Search with Command K or Control K"
              >
                ⌘ K
              </kbd>
            </div>
            {state.page === "chat" && chat && (
              <ChatMetadata
                chat={chat}
                onProject={project}
                onGoal={setGoal}
                goalBusy={goalBusy}
              />
            )}
          </header>
          <section
            ref={content}
            id="content"
            className={`content${["search", "project"].includes(state.page) ? " search-content" : ""}`}
            tabIndex={-1}
          >
            {state.error && (
              <div className="load-error" role="status">
                Could not refresh the workspace.{" "}
                <button onClick={() => void workspace.refresh()}>Retry</button>
              </div>
            )}
            {state.loading &&
              ["home", "locations", "projects"].includes(state.page) && (
                <div className="empty">Loading workspace…</div>
              )}
            {state.routeLoading && <p className="empty">Loading…</p>}
            {state.routeError && (
              <div className="load-error" role="alert">
                {state.routeError}{" "}
                <button
                  onClick={() =>
                    void workspace.navigate(state.routePath).catch(report)
                  }
                >
                  Retry
                </button>
              </div>
            )}
            {!state.routeLoading && !state.routeError && (
              <>
                {state.page === "home" && state.loaded && (
                  <Home
                    chats={state.chats}
                    projects={state.projects}
                    archivedProjects={state.archived}
                    groups={state.groups}
                    onOpen={open}
                    onProject={project}
                  />
                )}
                {state.page === "chat" && state.current && (
                  <AiboLinkContext.Provider
                    value={(url) => {
                      void workspace.openAiboLink(url).catch(report);
                    }}
                  >
                    <ChatView key={state.current.id} chat={state.current} />
                  </AiboLinkContext.Provider>
                )}
                {state.page === "help" && <Help />}
                {state.page === "locations" && state.loaded && (
                  <Locations locations={state.locations} />
                )}
                {state.page === "projects" && state.loaded && (
                  <Projects
                    projects={state.projects}
                    archived={state.archived}
                    onProject={project}
                  />
                )}
                {state.page === "search" && (
                  <SearchPage
                    key="all-chats"
                    open={open}
                    query={state.searchQuery}
                    onQuery={(query) => workspace.searchQuery(query)}
                  />
                )}
                {state.page === "project" && state.currentProject && (
                  <SearchPage
                    key={state.currentProject.id}
                    open={open}
                    project={state.currentProject}
                    query={state.searchQuery}
                    onQuery={(query) => workspace.searchQuery(query)}
                  />
                )}
                {state.page === "customization" && <Customization />}
              </>
            )}
          </section>
        </main>
        <Composer
          chats={state.chats}
          target={state.target}
          text={state.text}
          attachments={state.attachments}
          sending={state.sending}
          uploading={state.uploading}
          onSwap={swapTarget}
          onTarget={(chat) => open(chat.id)}
          onNew={newManager}
          onText={(text) => workspace.edit(text)}
          onDeleteAttachment={(index) =>
            workspace.attach(
              state.attachments.filter((_, item) => item !== index),
            )
          }
          onPaste={paste}
        />
      </div>
      {paletteOpen && (
        <CommandPalette
          chats={state.chats}
          projects={[...state.projects, ...state.archived]}
          onPage={page}
          onChat={open}
          onProject={project}
          onNew={newManager}
          onClose={() => setPaletteOpen(false)}
        />
      )}
      {status && (
        <div
          className={`status visible${status.error ? " error" : ""}`}
          role="status"
        >
          {status.message}
        </div>
      )}
    </>
  );
}
