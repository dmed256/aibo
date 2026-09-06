import { useEffect, useState } from "react";
import { api } from "../api";
import type { Workspace } from "../state/workspace";

export function useWorkspaceEvents(
  workspace: Workspace,
  viewedChatId: string | null | undefined,
  report: (error: unknown) => void,
) {
  const [connected, setConnected] = useState(false);
  useEffect(() => {
    const id = viewedChatId;
    if (!id) return;
    let timer: number | undefined;
    const schedule = () => {
      window.clearInterval(timer);
      if (document.visibilityState !== "visible" || !document.hasFocus())
        return;
      timer = window.setInterval(() => {
        void api
          .readChatNotifications(id)
          .then(() => workspace.refresh())
          .catch(report);
      }, 15_000);
    };
    schedule();
    document.addEventListener("visibilitychange", schedule);
    window.addEventListener("focus", schedule);
    window.addEventListener("blur", schedule);
    return () => {
      window.clearInterval(timer);
      document.removeEventListener("visibilitychange", schedule);
      window.removeEventListener("focus", schedule);
      window.removeEventListener("blur", schedule);
    };
  }, [viewedChatId, workspace, report]);

  useEffect(() => {
    void workspace.refresh();
    let socket: WebSocket | null = null;
    let reconnect: number | null = null;
    let refresh: number | null = null;
    let stopped = false;
    const connect = () => {
      const next = new WebSocket(
        `${location.protocol === "https:" ? "wss:" : "ws:"}//${location.host}/api/events`,
      );
      socket = next;
      const current = () => !stopped && socket === next;
      next.addEventListener("open", () => {
        if (current()) {
          setConnected(true);
          void workspace.reconnect();
        }
      });
      next.addEventListener("message", (message) => {
        if (!current()) return;
        try {
          if (workspace.titleEvent(JSON.parse(String(message.data)))) return;
        } catch {
          /* Reconcile malformed events. */
        }
        if (current() && refresh === null)
          refresh = window.setTimeout(() => {
            refresh = null;
            if (current()) void workspace.refresh();
          }, 80);
      });
      next.addEventListener("close", () => {
        if (current()) {
          setConnected(false);
          reconnect = window.setTimeout(connect, 1000);
        }
      });
      next.addEventListener("error", () => {
        if (current()) next.close();
      });
    };
    connect();
    return () => {
      stopped = true;
      socket?.close();
      if (reconnect !== null) window.clearTimeout(reconnect);
      if (refresh !== null) window.clearTimeout(refresh);
    };
  }, [workspace]);

  return connected;
}
