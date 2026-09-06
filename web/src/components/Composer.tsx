import { useLayoutEffect, useRef } from "react";
import ImageGallery from "./ImageGallery";
import { Badge, StatusDot } from "./Badge";
import type { Attachment, ChatSummary } from "../types";
import { recent } from "../state/workspace";
import { chatPath, followLink } from "../routing";

export function Composer({
  chats,
  target,
  text,
  attachments,
  sending,
  uploading = 0,
  onTarget,
  onNew,
  onText,
  onDeleteAttachment,
  onPaste,
  onSwap,
}: {
  chats: ChatSummary[];
  target: ChatSummary | null;
  text: string;
  attachments: Attachment[];
  sending: boolean;
  uploading?: number;
  onTarget: (chat: ChatSummary) => void;
  onNew: () => void;
  onText: (text: string) => void;
  onDeleteAttachment: (index: number) => void;
  onPaste: (event: React.ClipboardEvent<HTMLTextAreaElement>) => void;
  onSwap?: () => void;
}) {
  const inputRef = useRef<HTMLTextAreaElement>(null);
  const targetsRef = useRef<HTMLElement>(null);
  useLayoutEffect(() => {
    const selected = targetsRef.current?.querySelector<HTMLElement>(
      ".recent-target.selected",
    );
    const bar = selected?.parentElement;
    if (!selected || !bar) return;
    const left =
      selected.getBoundingClientRect().left - bar.getBoundingClientRect().left;
    if (left < 0) bar.scrollLeft += left;
    else if (left + selected.offsetWidth > bar.clientWidth)
      bar.scrollLeft += left + selected.offsetWidth - bar.clientWidth;
  }, [target?.id]);
  useLayoutEffect(() => {
    const input = inputRef.current;
    if (!input) return;
    const resize = () => {
      const top = input.scrollTop;
      const following = input.scrollHeight - top - input.clientHeight < 3;
      input.style.height = "auto";
      const style = getComputedStyle(input);
      const padding =
        parseFloat(style.paddingTop) + parseFloat(style.paddingBottom);
      const lineHeight = parseFloat(style.lineHeight);
      const height = input.scrollHeight;
      input.dataset.singleRow = String(height <= lineHeight + padding + 1);
      input.style.height = `${Math.min(height, lineHeight * 15 + padding)}px`;
      input.scrollTop = following ? input.scrollHeight : top;
    };
    resize();
    let width = input.getBoundingClientRect().width;
    const observer = new ResizeObserver(() => {
      const nextWidth = input.getBoundingClientRect().width;
      if (nextWidth !== width) {
        width = nextWidth;
        resize();
      }
    });
    observer.observe(input);
    return () => observer.disconnect();
  }, [text]);
  const tab = (chat: ChatSummary, key: string) => (
    <a
      href={chatPath(chat.id)}
      className={`recent-target ${chat.kind}${target?.id === chat.id ? " selected" : ""}`}
      key={chat.id}
      aria-current={target?.id === chat.id ? "page" : undefined}
      title={`${key}: ${chat.label} ${chat.title}`}
      onClick={(event) => followLink(event, () => onTarget(chat))}
    >
      <StatusDot chat={chat} />
      <kbd>{key}</kbd>
      <span className="tab-title">{chat.title}</span>
    </a>
  );
  return (
    <section
      ref={targetsRef}
      className={`composer ${target?.kind ?? "manager"}`}
      aria-label="Global input"
      aria-busy={sending || uploading > 0}
    >
      <div className="shortcut-bars">
        <div className="recent-targets bot-bar" aria-label="Bot chats">
          {recent(chats, "bot").map((chat, index) =>
            tab(chat, `M-${index + 1}`),
          )}
        </div>
        <button className="new-slot" onClick={onNew}>
          <kbd>M-0</kbd> new
        </button>
        <div className="recent-targets manager-bar" aria-label="Manager chats">
          {recent(chats, "manager").map((chat, index) =>
            tab(chat, `cb${index}`),
          )}
        </div>
      </div>
      {uploading > 0 && <div className="empty">Adding attachment…</div>}
      <div className="attachments">
        <ImageGallery images={attachments} onRemove={onDeleteAttachment} />
      </div>
      <div className="compose-row">
        <div className="target">
          <Badge chat={target} onSwap={onSwap} />
        </div>
        <textarea
          ref={inputRef}
          autoFocus
          id="input"
          rows={1}
          value={text}
          placeholder="Message…"
          onChange={(event) => onText(event.target.value)}
          onPaste={onPaste}
          aria-label="Message Aibo"
          aria-keyshortcuts="Alt+Enter"
          title="Enter for a newline · Alt/Option-Enter to send"
        />
      </div>
    </section>
  );
}
