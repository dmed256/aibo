import type { ChatSummary } from "../types";
import { publicChat } from "../state/workspace";
import { chatPath, followLink } from "../routing";
import { Badge } from "./Badge";

export function ChatRow({
  chat,
  onOpen,
}: {
  chat: ChatSummary;
  onOpen: (id: string) => void;
}) {
  if (!publicChat(chat)) return null;
  return (
    <a
      href={chatPath(chat.id)}
      className={`chat-row ${chat.kind}-row${chat.active ? " active" : ""}`}
      onClick={(event) => followLink(event, () => onOpen(chat.id))}
    >
      <Badge chat={chat} />
      <span className="chat-row-title">{chat.title}</span>
    </a>
  );
}
