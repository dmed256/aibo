import { isValidElement, memo, useState } from "react";
import ReactMarkdown from "react-markdown";
import { FiChevronRight } from "react-icons/fi";
import ImageGallery from "../components/ImageGallery";
import { Badge } from "../components/Badge";
import { MessageEvent } from "../components/MessageEvent";
import { CodeBlock } from "../components/CodeBlock";
import { ChatLink } from "../components/ChatLink";
import { markdownUrl } from "../markdownLinks";
import { CopyButton } from "../components/CopyButton";
import type { Chat, ChatSummary, Message } from "../types";
import { relativeTime } from "../utils";
import { publicChat } from "../state/workspace";
import {
  expandGroup,
  groupExpanded,
  eventDetails,
  eventText,
  messageId,
} from "./messageDetails";

function routineTurn(message: Message) {
  const turn = message.data?.turn as { status?: string } | undefined;
  return (
    message.kind === "event" &&
    (["Codex turn started", "Codex turn completed"].includes(message.content) ||
      ["turn_started", "turn/started"].includes(String(message.data?.event)) ||
      (message.data?.event === "turn/completed" &&
        turn?.status === "completed"))
  );
}

function MessageView({
  message,
  chat,
}: {
  message: Message;
  chat: ChatSummary;
}) {
  const internal = !["user", "assistant", "error"].includes(message.kind);
  const details = internal ? eventDetails(message) : undefined;
  const role = internal
    ? "internal"
    : message.kind === "assistant"
      ? chat.kind
      : message.kind;
  return (
    <article className={`message ${role}`}>
      <div className="message-label">
        {internal ? (
          <span className="badge internal">
            {(details?.label ?? message.kind).toLowerCase()}
          </span>
        ) : ["user", "error"].includes(message.kind) ? (
          <span className={`badge ${message.kind}`}>{message.kind}</span>
        ) : (
          <Badge chat={chat} />
        )}
        {message.created_at && (
          <time className="message-time" dateTime={message.created_at}>
            {relativeTime(message.created_at)}
          </time>
        )}
        <CopyButton
          text={details ? eventText(details) : message.content}
          label="copy message"
        />
      </div>
      <div className="message-content">
        {details ? (
          <MessageEvent details={details} />
        ) : (
          <ReactMarkdown
            urlTransform={markdownUrl}
            components={{
              pre: ({ children }) => {
                if (
                  !isValidElement<{ children?: string; className?: string }>(
                    children,
                  )
                )
                  return <pre>{children}</pre>;
                return (
                  <CodeBlock
                    text={String(children.props.children ?? "")}
                    language={children.props.className?.replace(
                      /^language-/,
                      "",
                    )}
                  />
                );
              },
              a: ChatLink,
              img: ({ src, alt }) =>
                typeof src === "string" && src ? (
                  <ImageGallery images={[{ url: src, name: alt || "Image" }]} />
                ) : null,
            }}
          >
            {message.content}
          </ReactMarkdown>
        )}
        <ImageGallery images={message.attachments ?? []} />
      </div>
    </article>
  );
}

export function ChatView({ chat }: { chat: Chat }) {
  return publicChat(chat) ? <ChatMessages key={chat.id} chat={chat} /> : null;
}

const ChatMessages = memo(
  function ChatMessages({ chat }: { chat: Chat }) {
    // Keep expansion by item, not a group's changing position/first message.
    const [expanded, setExpanded] = useState<Set<string>>(() => new Set());
    const groups: Message[][] = [];
    const errorNotice = chat.messages.some(
      (message) =>
        message.kind === "error" && message.id === chat.notice_message_id,
    );
    for (const message of chat.messages.filter(
      (message) =>
        message.kind === "error" || message.id !== chat.notice_message_id,
    )) {
      if (routineTurn(message)) continue;
      const previous = groups.at(-1);
      const hidden = !["user", "assistant", "error"].includes(message.kind);
      if (
        hidden &&
        previous &&
        !["user", "assistant", "error"].includes(previous[0]!.kind)
      )
        previous.push(message);
      else groups.push([message]);
    }
    return (
      <div className="messages">
        {groups.map((messages) => {
          const first = messages[0]!;
          if (["user", "assistant", "error"].includes(first.kind))
            return <MessageView key={first.id} message={first} chat={chat} />;
          return (
            <details
              className="hidden-messages"
              key={messageId(first)}
              open={groupExpanded(messages, expanded)}
              onToggle={(event) => {
                const open = event.currentTarget.open;
                setExpanded((previous) =>
                  expandGroup(previous, messages, open),
                );
              }}
            >
              <summary onMouseDown={(event) => event.preventDefault()}>
                <FiChevronRight aria-hidden="true" />
                {messages.length} hidden message
                {messages.length === 1 ? "" : "s"}
              </summary>
              <div className="hidden-message-body">
                {messages.map((message) => (
                  <MessageView key={message.id} message={message} chat={chat} />
                ))}
              </div>
            </details>
          );
        })}
        {chat.notice && !errorNotice && (
          <p
            className={`notice ${["error", "blocked"].includes(chat.status) ? "failure" : ""}`}
          >
            {chat.notice}
          </p>
        )}
        {!chat.messages.length && (
          <div className="empty">
            No messages yet. Write below to start this conversation.
          </div>
        )}
      </div>
    );
  },
  (previous, next) =>
    previous.chat.messages === next.chat.messages &&
    previous.chat.notice === next.chat.notice &&
    previous.chat.notice_message_id === next.chat.notice_message_id &&
    previous.chat.status === next.chat.status &&
    previous.chat.active === next.chat.active &&
    previous.chat.kind === next.chat.kind &&
    previous.chat.label === next.chat.label &&
    previous.chat.location === next.chat.location,
);
