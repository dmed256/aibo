import { useContext, type ReactNode } from "react";
import { chatPath, parseAiboLink } from "../routing";
import { AiboLinkContext } from "../markdownLinks";

export function ChatLink({
  children,
  href,
  title,
}: {
  children?: ReactNode;
  href?: string;
  title?: string;
}) {
  const open = useContext(AiboLinkContext);
  if (!href || !/^aibo:/i.test(href))
    return (
      <a href={href} title={title} target="_blank" rel="noreferrer">
        {children}
      </a>
    );
  const target = parseAiboLink(href);
  if (!target)
    return (
      <a aria-disabled="true" title="Invalid Aibo chat link">
        {children}
      </a>
    );
  return (
    <a
      href={target.kind === "chat" ? chatPath(target.id) : "#"}
      title={title ?? href}
      onClick={(event) => {
        event.preventDefault();
        open(href);
      }}
      onAuxClick={(event) => event.preventDefault()}
    >
      {children}
    </a>
  );
}
