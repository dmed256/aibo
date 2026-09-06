;;; aibo-custom.el --- Aibo settings and faces -*- lexical-binding: t -*-

(defgroup aibo nil
  "Aibo's Emacs interface."
  :group 'applications)

(defmacro aibo:--define-keymap (name bindings)
  "Refresh NAME with BINDINGS while preserving references from live buffers."
  `(progn
     (defvar ,name (make-sparse-keymap))
     (setcdr ,name (cdr ,bindings))))

(defface aibo:echo-face
  '((t :foreground "#a9aeba" :background "#1c1e25"))
  "Transient Aibo feedback in the echo area."
  :group 'aibo)

(defcustom aibo:server-url "http://127.0.0.1:5000"
  "Base URL of aibo-server."
  :type 'string
  :group 'aibo)

(defcustom aibo:sidebar-width 35
  "Width of the notification sidebar in full mode."
  :type 'integer
  :group 'aibo)

(defface aibo:orange-face
  '((t :foreground "#e9b96e"))
  "Aibo orange foreground."
  :group 'aibo)

(defface aibo:orange-bg-face
  '((t :foreground "#f5f7fb" :background "#272119" :extend t))
  "Aibo orange background."
  :group 'aibo)

(defface aibo:blue-face
  '((t :foreground "#8cc4ff"))
  "Aibo blue foreground."
  :group 'aibo)

(defface aibo:blue-bg-face
  '((t :foreground "#f5f7fb" :background "#19232c" :extend t))
  "Aibo blue background."
  :group 'aibo)

(defface aibo:green-face
  '((t :foreground "#8ae234"))
  "Aibo green foreground."
  :group 'aibo)

(defface aibo:inactive-green-face
  '((t :foreground "#52882f"))
  "Running status in an unselected shortcut." :group 'aibo)

(defface aibo:bot-input-face
  '((t :foreground "#f5f7fb" :background "#211b29" :extend t))
  "Dark purple composer background." :group 'aibo)

(defface aibo:manager-input-face
  '((t :foreground "#f5f7fb" :background "#272119" :extend t))
  "Dark orange composer background." :group 'aibo)

(defface aibo:bot-shortcut-face
  '((t :foreground "#dfb8f5"))
  "Light purple shortcut text." :group 'aibo)

(defface aibo:manager-shortcut-face
  '((t :foreground "#f5d39a"))
  "Light orange shortcut text." :group 'aibo)

(defface aibo:selected-shortcut-face
  '((t :foreground "#ffffff" :weight bold))
  "Selected shortcut key and title." :group 'aibo)

(defface aibo:metadata-value-face
  '((t :foreground "#eeeeee" :background "#292c34"))
  "Darker value half of a header metadata badge." :group 'aibo)

(defface aibo:account-label-face
  '((t :foreground "#9295a3" :background "#25272e"))
  "Dark label half of usage and reset badges." :group 'aibo)

(defface aibo:account-value-face
  '((t :foreground "#c2c5ce" :background "#17181d"))
  "Darker value half of usage and reset badges." :group 'aibo)

(defface aibo:diff-added-face
  '((t :foreground "#a4d6a0" :background "#203025" :extend t))
  "Added patch lines." :group 'aibo)

(defface aibo:diff-removed-face
  '((t :foreground "#e6a0a6" :background "#352429" :extend t))
  "Removed patch lines." :group 'aibo)

(defface aibo:diff-hunk-face
  '((t :foreground "#a0bdd8" :background "#242b35" :extend t))
  "Patch hunk coordinates." :group 'aibo)

(defface aibo:purple-face
  '((t :foreground "#c792ea"))
  "Aibo purple foreground."
  :group 'aibo)

(defface aibo:purple-bg-face
  '((t :foreground "#f5f7fb" :background "#211b29" :extend t))
  "Aibo purple background."
  :group 'aibo)

(defface aibo:muted-face
  '((t :foreground "#8d929e"))
  "Muted Aibo text."
  :group 'aibo)

(defface aibo:selected-face
  '((t :background "#24262d" :extend t))
  "Selected sidebar item."
  :group 'aibo)

(defface aibo:key-face
  '((t :foreground "#eeeeee" :background "#343740"))
  "Key binding or input-like text."
  :group 'aibo)

(defface aibo:user-message-face
  '((t :inherit aibo:blue-bg-face))
  "User message body."
  :group 'aibo)

(defface aibo:error-message-face
  '((t :foreground "#f0b0b0" :background "#352429" :extend t))
  "Persisted error message body." :group 'aibo)

(defface aibo:error-badge-face
  '((t :foreground "#ffffff" :background "#8b3b45"))
  "Persisted error message label." :group 'aibo)

(defface aibo:manager-message-face
  '((t :inherit aibo:orange-bg-face))
  "Manager message body."
  :group 'aibo)

(defface aibo:bot-message-face
  '((t :inherit aibo:purple-bg-face))
  "Bot message body."
  :group 'aibo)

(defface aibo:hidden-message-face
  '((t :inherit aibo:muted-face))
  "Collapsed internal message groups."
  :group 'aibo)

(defface aibo:internal-badge-face
  '((t :foreground "#c2c6cf" :background "#383b44"))
  "Message type badges for system, tool, and event messages."
  :group 'aibo)

(defface aibo:internal-message-face
  '((t :foreground "#a9aeba" :background "#20232b" :extend t))
  "Expanded system, tool, and event messages."
  :group 'aibo)

(defface aibo:purple-bar-face
  '((t :foreground "#c792ea" :background "#211a2b" :extend t))
  "Bot shortcut bar." :group 'aibo)

(defface aibo:orange-bar-face
  '((t :foreground "#e9b96e" :background "#292218" :extend t))
  "Manager shortcut bar." :group 'aibo)

(defface aibo:base-face
  '((t :foreground "#d8dee9" :background "#15171c"))
  "Workspace background and ordinary text." :group 'aibo)

(defface aibo:code-face
  '((t :foreground "#d1d7e0" :background "#20232b" :extend t))
  "Fenced code in messages." :group 'aibo)

(defface aibo:selection-face
  '((t :foreground "#ffffff" :background "#48648c"))
  "Selected literal text." :group 'aibo)

(defface aibo:manager-badge-face
  '((t :foreground "#ffffff" :background "#976b32"))
  "Manager badge and selected tab." :group 'aibo)

(defface aibo:bot-badge-face
  '((t :foreground "#ffffff" :background "#754993"))
  "Bot badge and selected tab." :group 'aibo)

(defface aibo:user-badge-face
  '((t :foreground "#ffffff" :background "#527da6"))
  "Padded User badge." :group 'aibo)

(defface aibo:location-face
  '((t :foreground "#eeeeee" :background "#383b44"))
  "Location badge." :group 'aibo)

(defface aibo:error-face
  '((t :foreground "#f07886"))
  "Unsuccessful chat status." :group 'aibo)

(defface aibo:error-message-face
  '((t :inherit aibo:error-face :background "#2b1c21" :extend t))
  "Dark red error messages, separate from foreground-only status indicators."
  :group 'aibo)

(defface aibo:rule-face
  '((t :foreground "#414550" :background "#15171c"))
  "Panel separator." :group 'aibo)

(defface aibo:search-input-face
  '((t :foreground "#eeeeee" :background "#383b44" :extend t))
  "Full-width search query." :group 'aibo)

(defface aibo:echo-face
  '((t :foreground "#a9aeba" :background "#1c1e25"))
  "Workspace echo area." :group 'aibo)

(defface aibo:selected-muted-face
  '((t :foreground "#a0a4af" :background "#24262d"))
  "Secondary text in selected notifications." :group 'aibo)

(defface aibo:link-face
  '((t :foreground "#8cc4ff"))
  "Links in workspace pages and messages." :group 'aibo)

(provide 'aibo-custom)
