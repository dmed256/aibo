import { type ClipboardEvent, useCallback } from "react";
import { api } from "../api";
import type { Workspace } from "../state/workspace";

export function useAttachments(
  workspace: Workspace,
  report: (error: unknown) => void,
) {
  const encodeUpload = useCallback(async (blob: Blob, name: string) => {
    if (!/^image\/(png|jpeg|gif|webp)$/.test(blob.type))
      throw new Error("Only PNG, JPEG, GIF, and WebP images are supported");
    const dataUrl = await new Promise<string>((resolve, reject) => {
      const reader = new FileReader();
      reader.onload = () =>
        typeof reader.result === "string"
          ? resolve(reader.result)
          : reject(new Error("Could not encode attachment"));
      reader.onerror = () =>
        reject(reader.error ?? new Error("Could not read attachment"));
      reader.readAsDataURL(blob);
    });
    return api.upload(name, blob.type, dataUrl.split(",", 2)[1] ?? "");
  }, []);
  const upload = useCallback(
    (blob: Blob, name: string) =>
      workspace.importAttachment(() => encodeUpload(blob, name)),
    [workspace, encodeUpload],
  );
  const attachClipboard = useCallback(async () => {
    try {
      await workspace.importAttachment(async () => {
        if (!navigator.clipboard?.read)
          throw new Error(
            "Clipboard image access is unavailable; paste an image into the input",
          );
        for (const item of await navigator.clipboard.read()) {
          const type = item.types.find((candidate) =>
            /^image\/(png|jpeg|gif|webp)$/.test(candidate),
          );
          if (type)
            return encodeUpload(
              await item.getType(type),
              `clipboard-${Date.now()}.${type.split("/")[1]}`,
            );
        }
        throw new Error("Clipboard has no supported image");
      });
    } catch (error) {
      report(error);
    }
  }, [report, workspace, encodeUpload]);
  const paste = useCallback(
    (event: ClipboardEvent<HTMLTextAreaElement>) => {
      const file = [...event.clipboardData.files].find((candidate) =>
        candidate.type.startsWith("image/"),
      );
      if (file) {
        event.preventDefault();
        void upload(file, file.name || `clipboard-${Date.now()}`).catch(report);
      }
    },
    [report, upload],
  );

  return { attachClipboard, paste };
}
