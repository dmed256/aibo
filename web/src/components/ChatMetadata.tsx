import { useEffect, useState } from "react";
import { FiTarget } from "react-icons/fi";
import type { ChatSummary, Project } from "../types";
import { compactTokens, elapsedTime } from "../utils";
import { followLink, projectPath } from "../routing";

export function GoalButton({
  chat,
  onGoal,
  busy = false,
}: {
  chat: ChatSummary;
  onGoal?: (chat: ChatSummary, enabled: boolean) => void;
  busy?: boolean;
}) {
  const enabled = chat.goal_enabled ?? true;
  const status = enabled ? (chat.goal?.status ?? "ready") : "off";
  return (
    <button
      className="metadata-badge goal-button"
      aria-label={
        enabled ? "Disable goal mode" : "Set goal to last sent message"
      }
      title={
        enabled
          ? `Goal: ${chat.goal?.status ?? "ready"}. Click to disable`
          : "Set goal to last sent message"
      }
      aria-pressed={enabled}
      disabled={busy || !onGoal}
      onClick={() => onGoal?.(chat, !enabled)}
    >
      <span className="metadata-label">
        <FiTarget aria-hidden="true" />
        goal
      </span>
      <span className="metadata-value">{status}</span>
    </button>
  );
}

export function ChatMetadata({
  chat,
  onProject,
  onGoal,
  goalBusy,
}: {
  chat: ChatSummary;
  onProject?: (project: Project) => void;
  onGoal?: (chat: ChatSummary, enabled: boolean) => void;
  goalBusy?: boolean;
}) {
  const [now, setNow] = useState(() => Date.now());
  useEffect(() => {
    if (!chat.running_since) return;
    const timer = window.setInterval(() => setNow(Date.now()), 1000);
    return () => window.clearInterval(timer);
  }, [chat.running_since]);
  const running = chat.running_since
    ? Math.max(0, (now - new Date(chat.running_since).getTime()) / 1000)
    : 0;
  return (
    <div className="chat-metadata" aria-label="Conversation status">
      {chat.project && onProject ? (
        <a
          href={projectPath(chat.project.id)}
          className="metadata-badge"
          onClick={(event) => followLink(event, () => onProject(chat.project!))}
        >
          <span>project</span>
          <span className="metadata-value">{chat.project.name}</span>
        </a>
      ) : (
        <span className="metadata-badge">
          <span>project</span>
          <span className="metadata-value">{chat.project?.name ?? "—"}</span>
        </span>
      )}
      <GoalButton chat={chat} onGoal={onGoal} busy={goalBusy} />
      <span
        className="metadata-badge"
        title={`${chat.tokens_used ?? 0} tokens`}
      >
        <span>tokens</span>
        <span className="metadata-value">
          {compactTokens(chat.tokens_used ?? 0)}
        </span>
      </span>
      <span className="metadata-badge">
        <span>elapsed</span>
        <span className="metadata-value">
          {elapsedTime((chat.elapsed_seconds ?? 0) + running)}
        </span>
      </span>
    </div>
  );
}
