export interface Location {
  id: string;
  name: string;
  path: string;
}

export interface Project {
  id: string;
  name: string;
  description: string;
  archived_at: string | null;
}

export type ChatKind = "manager" | "bot";
export type ChatStatus =
  | "idle"
  | "queued"
  | "running"
  | "completed"
  | "interrupted"
  | "cancelled"
  | "blocked"
  | "error";

export interface ChatSummary {
  id: string;
  kind: ChatKind;
  label: string;
  bot_number: number | null;
  manager_number?: number | null;
  title: string;
  status: ChatStatus;
  active: boolean;
  codex_thread_id: string | null;
  active_turn_id: string | null;
  goal_enabled?: boolean;
  goal?: {
    objective: string;
    status:
      | "active"
      | "paused"
      | "blocked"
      | "usageLimited"
      | "budgetLimited"
      | "complete"
      | "unavailable"
      | "pending";
  } | null;
  tokens_used?: number;
  elapsed_seconds?: number;
  running_since?: string | null;
  location: Location | null;
  project: Project | null;
  activity_at: string;
  last_active_at: string | null;
  created_at: string;
}

export interface Message {
  id: string;
  kind: "system" | "user" | "assistant" | "tool" | "event" | "error";
  content: string;
  data: Record<string, unknown>;
  attachments?: Attachment[];
  created_at: string;
}

export interface Chat extends ChatSummary {
  messages: Message[];
  notice?: string | null;
  notice_message_id?: string | null;
}

export interface Notification {
  id: string;
  chat: ChatSummary;
  body: string;
  created_at: string;
  read_at: string | null;
}

export interface SidebarData {
  active: ChatSummary[];
  unread: Notification[];
  read: Notification[];
  read_count?: number;
  account_usage?: {
    remaining_percent: number | null;
    resets: number | null;
  };
}

export interface Attachment {
  path: string;
  name: string;
  size: number;
  url: string;
}

export type SidebarItem = ChatSummary | Notification;
export interface ModelSettings {
  manager_model: string | null;
  bot_model: string | null;
  title_model: string | null;
  manager_model_reasoning_effort: string | null;
  bot_model_reasoning_effort: string | null;
  title_model_reasoning_effort: string | null;
}

export type Page =
  | "home"
  | "chat"
  | "help"
  | "tutorial"
  | "search"
  | "locations"
  | "projects"
  | "project"
  | "customization";

export interface CodexModel {
  model: string;
  display_name: string;
}
