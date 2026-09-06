import type { Notification, SidebarData, SidebarItem } from "../types";
import { relativeTime } from "../utils";
import { publicChat } from "../state/workspace";
import { chatPath, followLink } from "../routing";
import { notificationKeys, visibleRead } from "../notifications";
import { Badge } from "./Badge";

function isNotification(item: SidebarItem): item is Notification {
  return "chat" in item;
}

export function Sidebar({
  data,
  currentId,
  connected,
  onOpen,
}: {
  data: SidebarData;
  currentId: string | null;
  connected: boolean;
  onOpen: (item: SidebarItem) => void;
}) {
  let index = 0;
  const section = (
    title: string,
    items: SidebarItem[],
    count = items.length,
  ) => (
    <section
      className={`sidebar-section${title === "READ" ? " read-section" : title === "ACTIVE" ? " active-section" : ""}`}
      key={title}
    >
      <div className="section-title">
        {title}
        <span className="section-count">{count}</span>
      </div>
      {items.map((item) => {
        const notification = isNotification(item);
        const chat = notification ? item.chat : item;
        if (!publicChat(chat)) return null;
        const itemIndex = index++;
        if (!notification)
          return (
            <a
              href={chatPath(chat.id)}
              className={`notification active-notification${currentId === chat.id ? " selected" : ""}`}
              key={item.id}
              onClick={(event) => followLink(event, () => onOpen(item))}
              title={chat.title}
            >
              <span className="notification-key">
                {notificationKeys[itemIndex] ?? ""}
              </span>
              <span className="active-notification-content">
                <Badge chat={chat} />
                <span className="notification-title">
                  {chat.project && <>{chat.project.name} · </>}
                  {chat.title}
                </span>
              </span>
            </a>
          );
        return (
          <a
            href={chatPath(chat.id)}
            className={`notification${currentId === chat.id ? " selected" : ""}`}
            key={item.id}
            onClick={(event) => followLink(event, () => onOpen(item))}
          >
            <span className="notification-key">
              {notificationKeys[itemIndex] ?? ""}
            </span>
            <span className="notification-text">
              <span className="notification-head">
                <Badge chat={chat} />
                <span className="notification-time">
                  {relativeTime(
                    notification ? item.created_at : chat.activity_at,
                  )}
                </span>
              </span>
              <span className="notification-title">{chat.title}</span>
              <span className="notification-body">{item.body}</span>
            </span>
          </a>
        );
      })}
    </section>
  );
  return (
    <aside className="sidebar" aria-label="Notifications">
      <div id="notifications" className="sidebar-content" tabIndex={-1}>
        <div className="account-usage" aria-label="Codex account usage">
          <span
            className="metadata-badge"
            title="Remaining Codex usage (lowest quota window)"
          >
            <span>usage</span>
            <span className="metadata-value">
              {data.account_usage?.remaining_percent == null
                ? "—"
                : `${data.account_usage.remaining_percent}%`}
            </span>
          </span>
          {(data.account_usage?.resets ?? 0) > 0 && (
            <span className="metadata-badge" title="Earned resets remaining">
              <span>resets</span>
              <span className="metadata-value">
                {data.account_usage!.resets}
              </span>
            </span>
          )}
        </div>
        {!connected && (
          <p className="connection offline" role="status">
            Reconnecting…
          </p>
        )}
        {section("ACTIVE", data.active)}
        {section("UNREAD", data.unread)}
        {section(
          "READ",
          visibleRead(data),
          data.read_count ?? data.read.length,
        )}
      </div>
    </aside>
  );
}
