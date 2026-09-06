import { createContext } from "react";
import { defaultUrlTransform } from "react-markdown";

export const AiboLinkContext = createContext<(url: string) => void>(() => {});

// Allow only anchors through this exception; images retain Markdown's sanitizer.
export function markdownUrl(url: string, key: string): string {
  return key === "href" && /^aibo:/i.test(url) ? url : defaultUrlTransform(url);
}
