import { useEffect, useId, useLayoutEffect, useRef, useState } from "react";
import {
  FiArrowUpRight,
  FiFolder,
  FiHelpCircle,
  FiHome,
  FiMapPin,
  FiMessageSquare,
  FiPlus,
  FiSearch,
  FiSettings,
} from "react-icons/fi";
import { api } from "../api";
import { Badge } from "../components/Badge";
import type { ChatSummary, Page, Project } from "../types";
import { publicChat } from "../state/workspace";

const pages: {
  page: Page;
  label: string;
  keywords: string;
  icon: typeof FiHome;
}[] = [
  {
    page: "home",
    label: "Home",
    keywords: "page recent workspace",
    icon: FiHome,
  },
  {
    page: "search",
    label: "Chats",
    keywords: "page search conversations messages",
    icon: FiMessageSquare,
  },
  {
    page: "projects",
    label: "Projects",
    keywords: "page folders",
    icon: FiFolder,
  },
  {
    page: "locations",
    label: "Locations",
    keywords: "page directories",
    icon: FiMapPin,
  },
  {
    page: "customization",
    label: "Customization",
    keywords: "page settings models",
    icon: FiSettings,
  },
  {
    page: "help",
    label: "Help",
    keywords: "page shortcuts keyboard",
    icon: FiHelpCircle,
  },
];

export default function CommandPalette({
  chats,
  projects,
  onPage,
  onChat,
  onProject,
  onNew,
  onClose,
}: {
  chats: ChatSummary[];
  projects: Project[];
  onPage: (page: Page) => void;
  onChat: (id: string) => void;
  onProject: (project: Project) => void;
  onNew: () => void;
  onClose: () => void;
}) {
  const dialog = useRef<HTMLDialogElement>(null);
  const list = useRef<HTMLDivElement>(null);
  const listId = useId();
  const [query, setQuery] = useState("");
  const [selection, setSelection] = useState(0);
  const [remote, setRemote] = useState<{
    query: string;
    chats: ChatSummary[];
    error?: string;
  } | null>(null);
  const term = query.trim();
  const words = term.toLowerCase().split(/\s+/);
  const matches = (text: string) =>
    words.every((word) => text.toLowerCase().includes(word));

  useLayoutEffect(() => {
    const element = dialog.current!;
    const previous =
      document.activeElement instanceof HTMLElement
        ? document.activeElement
        : null;
    element.showModal();
    return () => {
      const restore = element.open;
      element.close();
      if (restore && previous?.isConnected)
        previous.focus({ preventScroll: true });
    };
  }, []);
  useEffect(() => {
    if (!term) return;
    let cancelled = false;
    const timer = window.setTimeout(() => {
      void api
        .chats({ query: term, limit: 20 })
        .then((chats) => {
          if (!cancelled)
            setRemote({ query: term, chats: chats.filter(publicChat) });
        })
        .catch(() => {
          if (!cancelled)
            setRemote({
              query: term,
              chats: [],
              error: "Chat search is unavailable. Try again.",
            });
        });
    }, 150);
    return () => {
      cancelled = true;
      window.clearTimeout(timer);
    };
  }, [term]);

  const results = [
    ...pages
      .filter((item) => matches(`${item.label} ${item.keywords}`))
      .map((item) => ({
        id: item.page,
        group: "Pages",
        label: item.label,
        detail: "Page",
        visual: <item.icon />,
        run: () => onPage(item.page),
      })),
    ...(matches("New conversation manager chat")
      ? [
          {
            id: "new",
            group: "Actions",
            label: "New conversation",
            detail: "M-0",
            visual: <FiPlus />,
            run: onNew,
          },
        ]
      : []),
    ...(matches("Emacs tutorial shortcuts")
      ? [
          {
            id: "tutorial",
            group: "Actions",
            label: "Emacs tutorial",
            detail: "Guide",
            visual: <FiHelpCircle />,
            run: () => onPage("tutorial"),
          },
        ]
      : []),
    ...projects
      .filter((project) =>
        matches(`project ${project.name} ${project.description}`),
      )
      .slice(0, 6)
      .map((project) => ({
        id: `project:${project.id}`,
        group: "Projects",
        label: project.name,
        detail: project.archived_at ? "Archived project" : "Project",
        visual: <FiFolder />,
        run: () => onProject(project),
      })),
    ...(term && remote?.query === term && !remote.error
      ? remote.chats
      : chats.filter(
          (chat) => publicChat(chat) && matches(`${chat.label} ${chat.title}`),
        )
    )
      .slice(0, term ? 20 : 6)
      .map((chat) => ({
        id: `chat:${chat.id}`,
        group: term ? "Conversations" : "Recent conversations",
        label: chat.title,
        detail: chat.project?.name ?? "Chat",
        visual: <Badge chat={chat} />,
        run: () => onChat(chat.id),
      })),
  ];
  const selected = Math.min(selection, Math.max(0, results.length - 1));
  const choose = (index: number) => {
    const result = results[index];
    if (!result) return;
    dialog.current?.close();
    onClose();
    result.run();
  };
  useLayoutEffect(() => {
    list.current
      ?.querySelector('[aria-selected="true"]')
      ?.scrollIntoView({ block: "nearest" });
  }, [selected, term, remote]);

  return (
    <dialog
      ref={dialog}
      className="command-palette"
      aria-label="Command palette"
      onCancel={(event) => {
        event.preventDefault();
        onClose();
      }}
      onClick={(event) => {
        if (event.target === event.currentTarget) onClose();
      }}
    >
      <div
        className="palette-surface"
        onKeyDown={(event) => {
          if (event.nativeEvent.isComposing) return;
          if (event.key === "Tab") {
            event.preventDefault();
            event.currentTarget.querySelector("input")?.focus();
          } else if (event.key === "ArrowDown" || event.key === "ArrowUp") {
            event.preventDefault();
            setSelection(
              Math.max(
                0,
                Math.min(
                  results.length - 1,
                  selected + (event.key === "ArrowDown" ? 1 : -1),
                ),
              ),
            );
          } else if (event.key === "Enter") {
            event.preventDefault();
            choose(selected);
          }
        }}
      >
        <div className="palette-search">
          <FiSearch aria-hidden="true" />
          <input
            aria-label="Search pages, projects, and chats"
            role="combobox"
            aria-autocomplete="list"
            aria-expanded="true"
            aria-controls={listId}
            aria-activedescendant={
              results.length ? `${listId}-${selected}` : undefined
            }
            placeholder="Search pages, projects, conversations…"
            value={query}
            autoComplete="off"
            spellCheck={false}
            onChange={(event) => {
              setQuery(event.target.value);
              setSelection(0);
            }}
          />
          <kbd>esc</kbd>
        </div>
        <div
          ref={list}
          id={listId}
          className="palette-results"
          role="listbox"
          aria-label="Search results"
        >
          {results.map((result, index) => (
            <div key={result.id}>
              {results[index - 1]?.group !== result.group && (
                <div className="palette-group" role="presentation">
                  {result.group}
                </div>
              )}
              <button
                id={`${listId}-${index}`}
                role="option"
                aria-selected={selected === index}
                tabIndex={-1}
                onMouseDown={(event) => event.preventDefault()}
                onClick={() => choose(index)}
                onPointerMove={() => setSelection(index)}
              >
                <span className="palette-icon" aria-hidden="true">
                  {result.visual}
                </span>
                <span className="palette-label">{result.label}</span>
                <span className="palette-detail">{result.detail}</span>
                <FiArrowUpRight className="palette-open" aria-hidden="true" />
              </button>
            </div>
          ))}
        </div>
        <div className="palette-feedback" role="status">
          {term && remote?.query !== term
            ? "Searching conversations…"
            : term && remote?.query === term && remote.error
              ? remote.error
              : !results.length
                ? "No results found."
                : ""}
        </div>
        <footer>
          <span>
            <kbd>↑</kbd> <kbd>↓</kbd> navigate
          </span>
          <span>
            <kbd>↵</kbd> open
          </span>
          <span className="palette-shortcut">⌘ K</span>
        </footer>
      </div>
    </dialog>
  );
}
