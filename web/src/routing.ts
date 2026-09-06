import type { MouseEvent } from "react";
import type { Page } from "./types";

export const pagePaths: Record<Page, string> = {
  home: "/",
  chat: "/chats",
  search: "/chats",
  project: "/projects",
  projects: "/projects",
  locations: "/locations",
  help: "/help",
  tutorial: "/tutorial",
  customization: "/customization",
};
export const chatPath = (id: string) => `/chats/${encodeURIComponent(id)}`;
export const projectPath = (id: string) =>
  `/projects/${encodeURIComponent(id)}`;
export const searchPath = (path: string, query: string) =>
  query ? `${path}?${new URLSearchParams({ q: query })}` : path;

export function parseRoute(
  path: string,
): { page: Page; id?: string; query: string } | null {
  const url = new URL(path, "http://aibo.local");
  const pathname = url.pathname.replace(/\/+$/, "") || "/";
  const query = url.searchParams.get("q") ?? "";
  const page = Object.entries(pagePaths).find(
    ([page, path]) => !["chat", "project"].includes(page) && path === pathname,
  )?.[0] as Page | undefined;
  if (page) return { page, query };
  const match = pathname.match(/^\/(chats|projects)\/([^/]+)$/);
  if (!match) return null;
  try {
    return {
      page: match[1] === "chats" ? "chat" : "project",
      id: decodeURIComponent(match[2]!),
      query,
    };
  } catch {
    return null;
  }
}

export function followLink(
  event: MouseEvent<HTMLAnchorElement>,
  navigate: () => void,
): void {
  if (
    event.button !== 0 ||
    event.metaKey ||
    event.ctrlKey ||
    event.shiftKey ||
    event.altKey
  )
    return;
  event.preventDefault();
  navigate();
}

export type AiboTarget =
  | { kind: "chat"; id: string }
  | { kind: "bot"; number: number };

export function parseAiboLink(url: string): AiboTarget | null {
  const chat = url.match(
    /^aibo:\/\/chat\/([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})$/i,
  );
  if (chat) return { kind: "chat", id: chat[1]!.toLowerCase() };
  const bot = url.match(/^aibo:\/\/bot\/b(0|[1-9][0-9]{0,2})$/i);
  if (bot && Number(bot[1]) <= 255)
    return { kind: "bot", number: Number(bot[1]) };
  return null;
}
