import type { SidebarData } from "./types";

export const notificationKeys =
  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
export const visibleRead = (data: SidebarData) =>
  data.read.slice(0, Math.max(0, 50 - data.active.length - data.unread.length));
