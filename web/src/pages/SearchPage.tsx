import {
  useEffect,
  useLayoutEffect,
  useRef,
  useState,
  useSyncExternalStore,
} from "react";
import { Badge } from "../components/Badge";
import { Search } from "../state/search";
import { api } from "../api";
import type { Project } from "../types";
import { chatPath, followLink } from "../routing";

export default function SearchPage({
  open,
  project,
  query = "",
  onQuery,
}: {
  open: (id: string) => void;
  project?: Project;
  query?: string;
  onQuery?: (query: string) => void;
}) {
  const [search] = useState(() => new Search(api, project?.id, query));
  const state = useSyncExternalStore(
    search.subscribe,
    search.getSnapshot,
    search.getSnapshot,
  );
  const input = useRef<HTMLInputElement>(null);
  const results = useRef<HTMLDivElement>(null);
  const root = useRef<HTMLDivElement>(null);
  const focused = useRef(true);
  const caret = useRef(0);
  useEffect(() => {
    if (query !== search.getSnapshot().query) search.change(query);
  }, [query, search]);
  useEffect(() => {
    const timer = window.setTimeout(() => void search.fetch(), 100);
    return () => window.clearTimeout(timer);
  }, [search, state.query]);
  useLayoutEffect(() => {
    if (!focused.current) return;
    if (state.selection < 0) {
      if (document.activeElement !== input.current) {
        input.current?.focus({ preventScroll: true });
        input.current?.setSelectionRange(caret.current, caret.current);
      }
    } else {
      const selected = results.current?.querySelector<HTMLElement>(
        `[data-index="${state.selection}"]`,
      );
      selected?.focus({ preventScroll: true });
      selected?.scrollIntoView({ block: "nearest" });
    }
  }, [state.selection, state.results]);
  const activate = () => {
    void search.activate().then((id) => {
      if (id) open(id);
    });
  };
  return (
    <div
      ref={root}
      className="search-page"
      onFocusCapture={() => {
        focused.current = true;
      }}
      onBlurCapture={(event) => {
        if (event.relatedTarget)
          focused.current = Boolean(
            root.current?.contains(event.relatedTarget),
          );
      }}
      onKeyDown={(event) => {
        if (event.altKey || event.metaKey) return;
        if (
          event.key === "ArrowDown" ||
          (event.ctrlKey && event.key === "n") ||
          (event.key === "Tab" && !event.shiftKey)
        ) {
          event.preventDefault();
          search.select(state.selection + 1);
        } else if (
          event.key === "ArrowUp" ||
          (event.ctrlKey && event.key === "p") ||
          (event.key === "Tab" && event.shiftKey)
        ) {
          event.preventDefault();
          search.select(state.selection - 1);
        } else if (event.key === "Enter") {
          event.preventDefault();
          activate();
        }
      }}
    >
      {project && (
        <header className="project-heading">
          <h1>{project.name}</h1>
          <p>{project.description}</p>
        </header>
      )}
      <input
        ref={input}
        className="search-query"
        value={state.query}
        placeholder={
          project ? `Search ${project.name} chats…` : "Search conversations…"
        }
        aria-label="Search chats"
        aria-controls="search-results"
        onBlur={(event) => {
          caret.current = event.currentTarget.selectionStart ?? 0;
        }}
        onFocus={() => {
          if (state.selection !== -1) search.select(-1);
        }}
        onChange={(event) => {
          caret.current = event.currentTarget.selectionStart ?? 0;
          search.change(event.target.value);
          onQuery?.(event.target.value);
        }}
      />
      <div
        ref={results}
        id="search-results"
        className="search-results"
        role="listbox"
        aria-label="Matching chats"
      >
        {state.results.map((chat, index) => (
          <a
            href={chatPath(chat.id)}
            key={chat.id}
            data-index={index}
            className={`search-result${state.selection === index ? " selected" : ""}`}
            role="option"
            aria-selected={state.selection === index}
            tabIndex={-1}
            onFocus={() => {
              if (state.selection !== index) search.select(index);
            }}
            onClick={(event) =>
              followLink(event, () => {
                search.select(index);
                activate();
              })
            }
          >
            <Badge chat={chat} />
            <span>{chat.title}</span>
          </a>
        ))}
        {state.more && (
          <button
            data-index={state.results.length}
            className={`search-result${state.selection === state.results.length ? " selected" : ""}`}
            role="option"
            aria-selected={state.selection === state.results.length}
            tabIndex={-1}
            onFocus={() => search.select(state.results.length)}
            onClick={() => {
              search.select(state.results.length);
              activate();
            }}
          >
            [Show 50 more]
          </button>
        )}
      </div>
      {state.error ? (
        <p className="empty">Search failed. RET to retry.</p>
      ) : (
        !state.results.length && (
          <p className="empty">
            {state.loading ? "Searching…" : "No matching chats."}
          </p>
        )
      )}
    </div>
  );
}
