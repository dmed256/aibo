import type { ChatSummary } from "../types";

export function StatusDot({ chat }: { chat: ChatSummary }) {
  if (
    !chat.active &&
    !["error", "interrupted", "cancelled", "blocked"].includes(chat.status)
  )
    return null;
  return (
    <span
      className={`status-dot ${chat.active ? "active" : "failed"}`}
      aria-label={chat.status}
    >
      ●
    </span>
  );
}

export function Badge({
  chat,
  onSwap,
}: {
  chat: ChatSummary | null;
  onSwap?: () => void;
}) {
  const className = `badge chat-badge ${chat?.kind ?? "manager"}`;
  const contents = (
    <>
      <span className="badge-identity">
        {chat && <StatusDot chat={chat} />}
        {chat?.label ?? "m"}
      </span>
      {chat?.kind === "bot" && chat.location && (
        <span className="badge-location">{chat.location.name}</span>
      )}
    </>
  );
  return onSwap ? (
    <button
      className={`${className} badge-switch`}
      onClick={onSwap}
      aria-label={`Switch to ${chat?.kind === "bot" ? "manager" : "bot"}`}
      title="Switch recipient · M-/"
    >
      {contents}
    </button>
  ) : (
    <span className={className}>{contents}</span>
  );
}
