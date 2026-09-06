import type {
  Attachment,
  Chat,
  ChatKind,
  ChatSummary,
  CodexModel,
  Location,
  ModelSettings,
  Project,
  SidebarData,
} from "./types";

async function request<T>(path: string, options: RequestInit = {}): Promise<T> {
  const response = await fetch(`/api${path}`, {
    ...options,
    headers: { "Content-Type": "application/json", ...options.headers },
  });
  if (!response.ok) {
    const body = (await response.json().catch(() => null)) as {
      detail?: unknown;
    } | null;
    const detail =
      typeof body?.detail === "string"
        ? body.detail
        : JSON.stringify(body?.detail);
    throw new Error(detail || `${response.status} ${response.statusText}`);
  }
  return response.json() as Promise<T>;
}

export const api = {
  modelCatalog: () => request<CodexModel[]>("/settings/models/catalog"),
  modelSettings: () => request<ModelSettings>("/settings/models"),
  saveModelSettings: (models: ModelSettings) =>
    request<ModelSettings>("/settings/models", {
      method: "PUT",
      body: JSON.stringify(models),
    }),
  chats: ({
    query = "",
    limit = 100,
    offset = 0,
    projectId,
    unassigned = false,
  }: {
    query?: string;
    limit?: number;
    offset?: number;
    projectId?: string;
    unassigned?: boolean;
  } = {}) => {
    const params = new URLSearchParams({
      limit: String(limit),
      offset: String(offset),
    });
    if (query) params.set("query", query);
    if (projectId) params.set("project_id", projectId);
    if (unassigned) params.set("unassigned", "true");
    return request<ChatSummary[]>(`/chats?${params.toString()}`);
  },
  chat: (id: string) => request<Chat>(`/chats/${id}`),
  latestBot: (number: number) =>
    request<ChatSummary>(`/chats/by-bot/${number}`),
  setGoal: (id: string, enabled: boolean) =>
    request<Chat>(`/chats/${id}/goal`, {
      method: "PUT",
      body: JSON.stringify({ enabled }),
    }),
  createChat: (body: {
    kind: ChatKind;
    title: string;
    location_id?: string;
    project_id?: string;
  }) => request<Chat>("/chats", { method: "POST", body: JSON.stringify(body) }),
  submit: (id: string, text: string, attachments: string[]) =>
    request(`/chats/${id}/submit`, {
      method: "POST",
      body: JSON.stringify({ text, attachments }),
    }),
  locations: () => request<Location[]>("/locations"),
  projects: (archived = false) =>
    request<Project[]>(`/projects${archived ? "?archived=true" : ""}`),
  sidebar: () => request<SidebarData>("/notifications"),
  readNotification: (id: string) =>
    request(`/notifications/${id}/read`, { method: "POST" }),
  readChatNotifications: (id: string) =>
    request(`/chats/${id}/notifications/read`, { method: "POST" }),
  upload: (name: string, mediaType: string, data: string) =>
    request<Attachment>("/attachments", {
      method: "POST",
      body: JSON.stringify({ name, media_type: mediaType, data }),
    }),
};
