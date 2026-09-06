import { type RefObject, useEffect } from "react";
import type { Page, SidebarItem } from "../types";
import { recent, type Workspace } from "../state/workspace";

import { notificationKeys, visibleRead } from "../notifications";

interface Shortcuts {
  workspace: Workspace;
  content: RefObject<HTMLElement | null>;
  paletteOpen: boolean;
  setPaletteOpen: (update: (open: boolean) => boolean) => void;
  attachClipboard: () => Promise<void>;
  open: (id: string) => void;
  sidebarOpen: (item: SidebarItem) => void;
  page: (page: Page) => void;
  newManager: () => void;
  submit: () => void;
  swapTarget: () => void;
}

export function useShortcuts({
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
}: Shortcuts) {
  useEffect(() => {
    let chord = "";
    const reset = () => {
      chord = "";
    };
    const keydown = (event: KeyboardEvent) => {
      // The tutorial is a static showcase, not a hidden composer.
      if (workspace.getSnapshot().page === "tutorial") {
        reset();
        return;
      }
      const key = event.key.toLowerCase();
      if (
        (event.metaKey || event.ctrlKey) &&
        key === "k" &&
        !event.altKey &&
        !event.isComposing
      ) {
        if (document.querySelector("dialog[open]") && !paletteOpen) return;
        event.preventDefault();
        reset();
        setPaletteOpen((open) => !open);
        return;
      }
      if (
        event.defaultPrevented ||
        event.isComposing ||
        document.querySelector("dialog[open]")
      ) {
        reset();
        return;
      }
      // Modifiers are separate keydown events, not steps in an Emacs chord.
      if (["Control", "Shift", "Alt", "Meta", "AltGraph"].includes(event.key))
        return;
      if (chord && event.repeat) {
        event.preventDefault();
        return;
      }
      const state = workspace.getSnapshot();
      if (chord === "C-c") {
        event.preventDefault();
        if (["b", "n", "p"].includes(key)) chord = `C-c ${key}`;
        else {
          reset();
          if (event.ctrlKey && key === "i") void attachClipboard();
        }
        return;
      }
      if (chord) {
        event.preventDefault();
        const command = chord;
        reset();
        if (command === "C-c b" && /^[0-9]$/.test(key)) {
          const chat = recent(state.chats, "manager")[Number(key)];
          if (chat) open(chat.id);
        } else if (command === "C-c n" && /^[0-9a-zA-Z]$/.test(event.key)) {
          const item = [
            ...state.sidebar.active,
            ...state.sidebar.unread,
            ...visibleRead(state.sidebar),
          ][notificationKeys.indexOf(event.key)];
          if (item) sidebarOpen(item);
        } else if (command === "C-c p") {
          const pages: Record<string, Page> = {
            h: "help",
            s: "search",
            l: "locations",
            p: "projects",
            c: "customization",
          };
          if (pages[key]) page(pages[key]);
          if (key === "n")
            document.querySelector<HTMLElement>("#notifications")?.focus();
        }
        return;
      }
      const meta = event.altKey || event.metaKey;
      const input = document.querySelector<HTMLTextAreaElement>("#input");
      const editing =
        event.target instanceof HTMLInputElement ||
        event.target instanceof HTMLTextAreaElement;
      const selection = editing
        ? event.target.selectionStart !== event.target.selectionEnd
        : Boolean(window.getSelection()?.toString());
      if (event.ctrlKey && key === "c" && !selection) {
        event.preventDefault();
        chord = "C-c";
      } else if (event.ctrlKey && meta && key === "h") {
        event.preventDefault();
        page("home");
      } else if (event.ctrlKey && key === "o") {
        event.preventDefault();
        const main = ["search", "project"].includes(state.page)
          ? document.querySelector<HTMLInputElement>(".search-query")
          : content.current;
        (document.activeElement === input ? main : input)?.focus({
          preventScroll: true,
        });
      } else if (meta && key === "/") {
        event.preventDefault();
        swapTarget();
      } else if (event.altKey && !event.isComposing && key === "enter") {
        event.preventDefault();
        submit();
      } else if (meta && key === "0") {
        event.preventDefault();
        newManager();
      } else if (meta && /^[1-9]$/.test(key)) {
        event.preventDefault();
        const chat = recent(state.chats, "bot")[Number(key) - 1];
        if (chat) open(chat.id);
      } else if ((event.ctrlKey && key === "g") || key === "escape") {
        event.preventDefault();
        workspace.clear();
      }
    };
    document.addEventListener("keydown", keydown);
    document.addEventListener("pointerdown", reset);
    window.addEventListener("blur", reset);
    return () => {
      document.removeEventListener("keydown", keydown);
      document.removeEventListener("pointerdown", reset);
      window.removeEventListener("blur", reset);
    };
  }, [
    workspace,
    attachClipboard,
    open,
    sidebarOpen,
    page,
    newManager,
    submit,
    swapTarget,
    paletteOpen,
    content,
    setPaletteOpen,
  ]);
}
