import { useLayoutEffect, useRef, useState } from "react";
import { createPortal } from "react-dom";
import { FiChevronLeft, FiChevronRight, FiX } from "react-icons/fi";

export interface PreviewImage {
  url: string;
  name: string;
}

function ImageViewer({
  images,
  initial,
  onClose,
}: {
  images: PreviewImage[];
  initial: number;
  onClose: () => void;
}) {
  const [index, setIndex] = useState(initial);
  const dialog = useRef<HTMLDialogElement>(null);
  const current = images[index] ?? images[0]!;
  const move = (step: number) =>
    setIndex((index) => (index + step + images.length) % images.length);
  useLayoutEffect(() => {
    const element = dialog.current!;
    const previous = document.activeElement;
    element.showModal();
    element.focus({ preventScroll: true });
    return () => {
      element.close();
      if (previous instanceof HTMLElement && previous.isConnected)
        previous.focus({ preventScroll: true });
    };
  }, []);
  return createPortal(
    <dialog
      ref={dialog}
      className="image-viewer"
      aria-label="Image viewer"
      onCancel={(event) => {
        event.preventDefault();
        onClose();
      }}
      onClick={(event) => {
        if (event.target === event.currentTarget) onClose();
      }}
      onKeyDown={(event) => {
        event.stopPropagation();
        if (event.key === "ArrowLeft" || event.key === "ArrowRight") {
          event.preventDefault();
          move(event.key === "ArrowLeft" ? -1 : 1);
        }
      }}
    >
      <header>
        <span>{current.name}</span>
        <span className="image-count">
          {index + 1} / {images.length}
        </span>
      </header>
      <div className="image-stage">
        <button
          className="image-control image-previous"
          aria-label="Previous image"
          disabled={images.length < 2}
          onClick={() => move(-1)}
        >
          <FiChevronLeft />
        </button>
        <img src={current.url} alt={current.name} />
        <button
          className="image-control image-next"
          aria-label="Next image"
          disabled={images.length < 2}
          onClick={() => move(1)}
        >
          <FiChevronRight />
        </button>
      </div>
    </dialog>,
    document.body,
  );
}

export default function ImageGallery({
  images,
  onRemove,
}: {
  images: PreviewImage[];
  onRemove?: (index: number) => void;
}) {
  const [viewing, setViewing] = useState<string | null>(null);
  const index = images.findIndex((image) => image.url === viewing);
  if (!images.length) return null;
  return (
    <span className="image-gallery">
      {images.map((image, index) => (
        <span className="image-tile" key={`${image.url}:${index}`}>
          <button
            className="image-preview"
            aria-label={`View ${image.name}`}
            title={image.name}
            onClick={() => setViewing(image.url)}
          >
            <img src={image.url} alt={image.name} loading="lazy" />
          </button>
          {onRemove && (
            <button
              className="remove-image"
              aria-label={`Remove ${image.name}`}
              title="Remove attachment"
              onClick={() => onRemove(index)}
            >
              <FiX />
            </button>
          )}
        </span>
      ))}
      {index >= 0 && (
        <ImageViewer
          images={images}
          initial={index}
          onClose={() => setViewing(null)}
        />
      )}
    </span>
  );
}
