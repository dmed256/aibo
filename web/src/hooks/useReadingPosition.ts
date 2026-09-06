import { type RefObject, useLayoutEffect, useRef } from "react";

interface ReadingPosition {
  top: number;
  following: boolean;
  tail: number;
}

export function useReadingPosition(
  content: RefObject<HTMLElement | null>,
  view: string,
  isChat: boolean,
  ready: boolean,
) {
  const positions = useRef(new Map<string, ReadingPosition>());
  useLayoutEffect(() => {
    const element = content.current;
    // A loading placeholder must not record the chat as a non-following page.
    if (!element || !ready) return;
    const position = positions.current.get(view) ?? {
      top: 0,
      following: isChat,
      tail: 0,
    };
    positions.current.set(view, position);
    let space = 0;
    const updateSpace = () => {
      // Subtract our own padding so it cannot make a short chat look long.
      space =
        isChat && element.scrollHeight - space > element.clientHeight ? 200 : 0;
      element.style.setProperty("--chat-scroll-space", `${space}px`);
      position.tail = Math.min(position.tail, space);
    };
    updateSpace();
    let height = element.scrollHeight;
    let viewport = element.clientHeight;
    const restore = () => {
      updateSpace();
      height = element.scrollHeight;
      viewport = element.clientHeight;
      if (position.following)
        element.scrollTo({
          top: height - viewport - space + position.tail,
          behavior: "instant",
        });
      position.top = element.scrollTop;
    };
    element.scrollTo({
      top: position.following
        ? height - viewport - space + position.tail
        : position.top,
      behavior: "instant",
    });
    restore();

    const scroll = () => {
      // Layout can emit a scroll event before ResizeObserver (e.g. viewport
      // shrinking). Preserve following unless the user actually scrolls away.
      if (
        position.following &&
        (height !== element.scrollHeight || viewport !== element.clientHeight)
      ) {
        restore();
        return;
      }
      position.top = element.scrollTop;
      const end = Math.max(
        0,
        element.scrollHeight - element.clientHeight - space,
      );
      position.following = isChat && element.scrollTop >= end - 8;
      // Preserve deliberate scrolling into the extra space during updates.
      position.tail = Math.max(0, element.scrollTop - end);
    };
    const wheel = (event: WheelEvent) => {
      if (event.deltaY < 0 && element.scrollHeight > element.clientHeight)
        position.following = false;
    };
    element.addEventListener("scroll", scroll, { passive: true });
    element.addEventListener("wheel", wheel, { passive: true });
    const observer = new ResizeObserver(restore);
    if (isChat) {
      observer.observe(element);
      const messages = element.querySelector(".messages");
      if (messages) observer.observe(messages);
    }
    return () => {
      observer.disconnect();
      element.removeEventListener("scroll", scroll);
      element.removeEventListener("wheel", wheel);
      element.style.removeProperty("--chat-scroll-space");
    };
  }, [content, view, isChat, ready]);
}
