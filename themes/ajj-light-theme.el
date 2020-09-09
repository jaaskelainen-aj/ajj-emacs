;;; material-light-theme.el --- A Theme based on the colors of the Google Material Design

;; Copyright (C) 2014 Paulik Christoph

;; Author: AJJ, original Christoph Paulik <cpaulik@gmail.com>
;; Keywords: themes
;; URL: http://github.com/cpaulik/emacs-material-theme
;; Version: 2015
;; X-Original-Version: 0.1
;; Package-Requires: ((emacs "24.1"))

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; To use it, put the following in your Emacs configuration file:
;;
;;   (load-theme 'ajj-light t)
;;
;; Requirements: Emacs 24.

;;; Credits:

;; Thanks to Bruce Williams of the Spacegray Theme which was used as the boilerplate:
;; https://github.com/bruce/emacs-spacegray-theme

;;; Code:

(deftheme ajj-light
  "A UI Theme for Emacs, hand-made ")
(display-color-cells (selected-frame))
(let* ((class '((class color) (min-colors 89)))
       (l-red "#e63946")
       (d-red "#4f000b")
       (l-green "#70C8A2")
       (green "#04673D")
       (d-green "#003921")
       (ll-yellow "#fffea8")
       (l-yellow "#ffef78")
       (yellow "#e2cc24")
       (d-yellow "#a69411")
       (l-blue "#a8dadc")
       (blue "#457b9d")
       (d-blue "#1d3557")
       (purple "#4b296b")
       (l-purple "#ad5ef7")
       (l-gray "#c0c0c0")
       (gray "#707070")
       (d-gray "#303030")
       (m-white "#f5f5f5")
       (m-black "#101010")
       ;;
       (background "#FAFAFA")
       (far-background "#b0eaf2")
       (inactive-gray "#cfd8dc")
       (header-color "#C8E6C9")
       (subtle "#a7adba")
       (selection "#C1CDD2") ;; tab-control-dirty-tab-close-button
       (secondary-selection "#bf616a") ;; tab-control-hover-tab-close-button
       (foreground "#212121")
       (comment "#607d8b") ;; table-row
       (red "#B71C1C") ;; tab-control-hover-tab-close-button
       (orange "#FF5722") ;; darker tab-control-dirty-tab-close-butto
       (green "#6B7D14") ;; complement tab-control-dirty-tab-close-button
       (aqua "#2A72C9") ;; lighter complement tab-control-dirty-tab-close-button
       (cyan "#007979")
       (diff-b "#A6CD99")
       ) ;; complement tab-control-dirty-tab-close-button

  (custom-theme-set-faces
   'ajj-light
   `(default ((,class (:foreground ,m-black :background ,m-white))))
   `(bold ((,class (:weight bold))))
   `(bold-italic ((,class (:slant italic :weight bold))))
   `(underline ((,class (:underline t))))
   `(italic ((,class (:slant italic))))
   `(font-lock-builtin-face ((,class (:foreground ,red))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment))))
   `(font-lock-comment-face ((,class (:foreground , gray))))
   `(font-lock-constant-face ((,class (:foreground ,green))))
   `(font-lock-doc-face ((,class (:foreground "#673ab7"))))
   `(font-lock-doc-string-face ((,class (:foreground ,d-yellow))))
   `(font-lock-function-name-face ((,class (:foreground ,"#0097A7"))))
   `(font-lock-keyword-face ((,class (:foreground ,aqua))))
   `(font-lock-negation-char-face ((,class (:foreground ,d-red))))
   `(font-lock-preprocessor-face ((,class (:foreground ,d-yellow))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,l-blue))))
   `(font-lock-string-face ((,class (:foreground "#689f38"))))
   `(font-lock-type-face ((,class (:foreground "#0097A7"))))
   `(font-lock-variable-name-face ((,class (:foreground ,"#EF6C00"))))
   `(font-lock-warning-face ((,class (:weight bold :foreground ,red))))
   `(highlight-numbers-number ((,class (:foreground ,"#689f38"))))
   `(shadow ((,class (:foreground ,comment))))
   `(success ((,class (:foreground "SeaGreen2"))))
   `(error ((,class (:foreground ,red))))
   `(warning ((,class (:foreground ,orange))))
   
   ;; Emacs interface
   `(cursor ((,class (:background ,orange))))
   `(fringe ((,class (:background , "#E0E1E2"))))
   `(linum ((,class (:background ,ll-yellow :foreground ,foreground))))
   `(linum-highlight-face ((,class (:background ,l-yellow :foreground ,foreground))))
   `(border ((,class (:background ,l-yellow))))
   `(vertical-border ((,class (:background ,selection :foreground, selection))))
   `(border-glyph ((,class (nil))))
   `(highlight ((,class (:inverse-video nil :background ,l-yellow))))
   `(hl-line ((,class (:inverse-video nil :background ,l-yellow))))
   `(gui-element ((,class (:background ,ll-yellow :foreground ,foreground))))
   `(minibuffer-prompt ((,class (:foreground ,blue))))
   `(region ((,class (:background ,selection))))
   `(secondary-selection ((,class (:background ,secondary-selection))))
   `(header-line ((,class (:inherit mode-line :foreground ,purple :background nil))))
   `(mode-line ((,class (:foreground ,m-white :background ,blue :box (:line-width 2 :color ,d-blue)))))
   `(mode-line-buffer-id ((,class (:foreground ,yellow :background nil :weight bold))))
   `(mode-line-inactive ((,class (:inherit mode-line :background , d-blue))))
   `(mode-line-emphasis ((,class (:foreground ,purple :slant italic))))
   `(mode-line-highlight ((,class (:foreground ,purple :box nil))))
   `(custom-face-tag ((,class (:foreground ,gray :weight bold))))

   ;; web-mode faces
   `(web-mode-current-element-highlight-face ((t (:background "linkColor" :foreground "#ffffff"))))
   `(web-mode-html-attr-name-face ((t (:foreground "#7F591B"))))
   `(web-mode-html-attr-value-face ((t (:foreground "#697A1A"))))
   `(web-mode-html-tag-bracket-face ((t (:foreground "#4B6C94"))))
   `(web-mode-html-tag-face ((t (:foreground "#3476C7"))))

   ;; Smerge
   `(smerge-upper ((t (:background ,l-blue))))
   `(smerge-lower ((t (:background ,l-green))))
   `(smerge-markers ((t (:background "gray90" :foreground "#B34B42"))))

   `(ediff-even-diff-A ((,class (:background ,blue ))))
   `(ediff-even-diff-B ((,class (:background ,green ))))
   `(ediff-current-diff-A ((,class (:background ,l-blue))))
   `(ediff-current-diff-B ((,class (:background ,l-green))))
   `(ediff-odd-diff-A  ((,class (:background ,blue ))))
   `(ediff-odd-diff-B  ((,class (:background ,green ))))
   `(ediff-fine-diff-A ((,class (:foreground "#074270"))))
   `(ediff-fine-diff-B ((,class (:foreground "#074270"))))

   ;; highlight indentation
   `(highlight-indentation-face ((,class (:background, l-yellow))))
   `(highlight-indentation-current-column-face ((,class (:background, far-background))))

   ;; Search
   `(match ((,class (:foreground ,background :background ,green :inverse-video nil))))
   `(isearch ((,class (:foreground ,foreground :background ,l-green))))
   `(lazy-highlight ((,class (:foreground ,foreground :background ,l-green :inverse-video nil))))
   `(isearch-fail ((,class (:background ,background :inherit font-lock-warning-face :inverse-video t))))

   ;; which-function
   `(which-func ((,class (:foreground ,d-blue :background nil))))

   `(trailing-whitespace ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-trailing ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-space-after-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-space-before-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-empty ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-line ((,class (:background nil :foreground ,red))))
   `(whitespace-indentation ((,class (:background nil :foreground ,aqua))))
   `(whitespace-space ((,class (:background nil :foreground ,selection))))
   `(whitespace-newline ((,class (:background nil :foreground ,selection))))
   `(whitespace-tab ((,class (:background nil :foreground ,selection))))
   `(whitespace-hspace ((,class (:background nil :foreground ,selection))))

   ;; Parenthesis matching (built-in)
   `(show-paren-match-face ((,class (:background ,d-blue :foreground ,background))))
   `(show-paren-mismatch-face ((,class (:background "red1" :foreground "white"))))

   ;; Smartparens paren matching
   `(sp-show-pair-match-face ((,class (:foreground ,background :background ,d-blue :inherit show-paren-match))))
   `(sp-show-pair-mismatch-face ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis matching (mic-paren)
   `(paren-face-match ((,class (:foreground nil :background nil :inherit show-paren-match))))
   `(paren-face-mismatch ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))
   `(paren-face-no-match ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis dimming (parenface)
   `(paren-face ((,class (:foreground ,comment :background nil))))

   `(sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
   `(sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
   `(slime-highlight-edits-face ((,class (:weight bold))))
   `(slime-repl-input-face ((,class (:weight normal :underline nil))))
   `(slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,purple))))
   `(slime-repl-result-face ((,class (:foreground ,green))))
   `(slime-repl-output-face ((,class (:foreground ,d-blue :background ,background))))

   `(csv-separator-face ((,class (:foreground ,orange))))

   `(diff-hl-insert ((,class (:background ,green :foreground ,green))))
   `(diff-hl-change ((,class (:background ,d-blue :foreground ,blue))))
   `(diff-hl-delete ((,class (:background ,orange :foreground ,orange))))
   `(diff-context ((,class (:foreground, comment))))

   `(diff-added ((,class (:foreground ,green))))
   `(diff-changed ((,class (:foreground ,blue))))
   `(diff-removed ((,class (:foreground ,orange))))
   `(diff-header ((,class (:foreground ,aqua :background nil))))
   `(diff-hunk-header ((,class (:foreground ,purple))))
   `(diff-refine-added ((,class (:inherit diff-added :inverse-video t))))
   `(diff-refine-removed ((,class (:inherit diff-removed :inverse-video t))))

   `(eldoc-highlight-function-argument ((,class (:foreground ,green :weight bold))))

   ;; macrostep
   `(macrostep-expansion-highlight-face ((,class (:inherit highlight :foreground nil))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((,class (:foreground ,foreground))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,green :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

   ;; dired+
   `(diredp-compressed-file-suffix ((,class (:foreground ,blue))))
   `(diredp-deletion ((,class (:inherit error :inverse-video t))))
   `(diredp-deletion-file-name ((,class (:inherit error))))
   `(diredp-dir-heading ((,class (:foreground ,green :weight bold))))
   `(diredp-dir-priv ((,class (:foreground ,aqua :background nil))))
   `(diredp-exec-priv ((,class (:foreground ,blue :background nil))))
   `(diredp-executable-tag ((,class (:foreground ,red :background nil))))
   `(diredp-file-name ((,class (:foreground ,yellow))))
   `(diredp-file-suffix ((,class (:foreground ,green))))
   `(diredp-flag-mark ((,class (:foreground ,green :inverse-video t))))
   `(diredp-flag-mark-line ((,class (:background nil :inherit highlight))))
   `(diredp-ignored-file-name ((,class (:foreground ,comment))))
   `(diredp-link-priv ((,class (:background nil :foreground ,purple))))
   `(diredp-mode-line-flagged ((,class (:foreground ,red))))
   `(diredp-mode-line-marked ((,class (:foreground ,green))))
   `(diredp-no-priv ((,class (:background nil))))
   `(diredp-number ((,class (:foreground ,yellow))))
   `(diredp-other-priv ((,class (:background nil :foreground ,purple))))
   `(diredp-rare-priv ((,class (:foreground ,red :background nil))))
   `(diredp-read-priv ((,class (:foreground ,green :background nil))))
   `(diredp-symlink ((,class (:foreground ,purple))))
   `(diredp-write-priv ((,class (:foreground ,yellow :background nil))))

   ;; diredfl
   `(diredfl-compressed-file-suffix ((,class (:foreground ,blue))))
   `(diredfl-compressed-file-name ((,class (:foreground ,blue))))
   `(diredfl-ignored-file-name ((,class (:foreground ,comment))))
   `(diredfl-date-time ((,class (:foreground ,green))))
   `(diredfl-file-name ((,class (:foreground ,foreground))))
   `(diredfl-read-priv ((,class (:foreground ,green :background nil))))
   `(diredfl-write-priv ((,class (:foreground ,yellow :background nil))))
   `(diredfl-exec-priv ((,class (:foreground ,red :background nil))))
   `(diredfl-rare-priv ((,class (:foreground ,orange :background nil))))
   `(diredfl-no-priv ((,class (:background nil))))
   `(diredfl-deletion ((,class (:inherit error :inverse-video t))))
   `(diredfl-deletion-file-name ((,class (:inherit error))))
   `(diredfl-dir-heading ((,class (:foreground ,green :weight bold))))
   `(diredfl-symlink ((,class (:foreground ,purple))))
   `(diredfl-dir-priv ((,class (:foreground ,aqua :background nil))))
   `(diredfl-dir-name ((,class (:foreground ,aqua :background nil))))
   `(diredfl-number ((,class (:foreground ,yellow :background nil))))
   `(diredfl-flag-mark ((,class (:foreground ,orange :background nil))))
   `(diredfl-flag-mark-line ((,class (:foreground ,nil :background ,selection))))
   `(diredfl-file-suffix ((,class (:foreground ,aqua :background nil))))

   ;; Magit
   `(magit-branch ((,class (:foreground ,green))))
   `(magit-diff-added ((,class (:inherit diff-added))))
   `(magit-diff-added-highlight ((,class (:inherit magit-diff-added
                                          :background ,far-background))))
   `(magit-diff-removed ((,class (:inherit diff-removed))))
   `(magit-diff-removed-highlight ((,class (:inherit magit-diff-removed
                                            :background ,far-background))))
   `(magit-header ((,class (:inherit nil :weight bold))))
   `(magit-item-highlight ((,class (:inherit highlight :background nil))))
   `(magit-log-author ((,class (:foreground ,aqua))))
   `(magit-log-graph ((,class (:foreground ,comment))))
   `(magit-log-date ((,class (:foreground ,yellow))))
   `(magit-section-title ((,class (:foreground ,blue :weight bold))))
   `(magit-section-highlight           ((t (:background ,l-yellow))))
   `(magit-section-heading             ((t (:foreground ,yellow :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,l-yellow  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,background
                                            :foreground ,orange :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,header-color))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,header-color))))
   `(magit-diff-hunk-heading-selection ((t (:background ,background
                                            :foreground ,orange))))
   `(magit-diff-lines-heading          ((t (:background ,orange
                                            :foreground ,background))))
   `(magit-blame-heading          ((t (:background ,far-background
                                                   :foreground ,aqua))))
   `(magit-blame-date             ((t (:background ,far-background
                                                   :foreground ,blue))))
   `(magit-blame-summary          ((t (:background ,far-background
                                                   :foreground ,green))))
   `(magit-diff-context-highlight      ((t (:background ,far-background
                                            :foreground "grey30"))))
   `(magit-diff-context                ((t (:foreground ,comment))))
   `(magit-diffstat-added   ((t (:foreground ,green))))
   `(magit-diffstat-removed ((t (:foreground ,red))))
   `(magit-process-ok    ((t (:foreground ,green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,red    :weight bold))))
   `(magit-branch-local  ((t (:foreground ,blue   :weight bold))))
   `(magit-branch-remote ((t (:foreground ,green  :weight bold))))
   `(magit-tag           ((t (:foreground ,orange :weight bold))))
   `(magit-hash          ((t (:foreground ,comment))))
   `(magit-sequence-stop ((t (:foreground ,green))))
   `(magit-sequence-part ((t (:foreground ,yellow))))
   `(magit-sequence-head ((t (:foreground ,blue))))
   `(magit-sequence-drop ((t (:foreground ,red))))

   ;; git-gutter
   `(git-gutter:modified ((,class (:foreground ,purple :weight bold))))
   `(git-gutter:added ((,class (:foreground ,green :weight bold))))
   `(git-gutter:deleted ((,class (:foreground ,red :weight bold))))
   `(git-gutter:unchanged ((,class (:background ,yellow))))

   ;; git-gutter-fringe
   `(git-gutter-fr:modified ((,class (:foreground ,purple :weight bold))))
   `(git-gutter-fr:added ((,class (:foreground ,green :weight bold))))
   `(git-gutter-fr:deleted ((,class (:foreground ,red :weight bold))))

   `(link ((,class (:foreground nil :underline t))))
   `(widget-button ((,class (:underline t :weight bold))))
   `(widget-field ((,class (:background ,l-yellow :box (:line-width 1 :color ,foreground)))))

   ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
   `(compilation-column-number ((,class (:foreground , d-yellow))))
   `(compilation-line-number ((,class (:foreground , d-yellow))))
   `(compilation-message-face ((,class (:foreground ,blue))))
   `(compilation-mode-line-exit ((,class (:foreground ,green))))
   `(compilation-mode-line-fail ((,class (:foreground ,red))))
   `(compilation-mode-line-run ((,class (:foreground ,blue))))

   ;; Grep
   `(grep-context-face ((,class (:foreground ,comment))))
   `(grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,blue))))
   `(grep-match-face ((,class (:foreground nil :background nil :inherit match))))

   `(regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

   ;; Helm
   `(helm-header ((,class (:foreground ,foreground :background ,background))))
   `(helm-selection ((,class (:background ,l-yellow ))))
   `(helm-match ((,class (:foreground ,green ))))
   `(helm-moccur-buffer ((,class (:foreground ,blue ))))
   `(helm-ff-file ((,class (:foreground ,foreground ))))
   `(helm-ff-directory ((,class (:foreground ,d-gray ))))
   `(helm-ff-symlink ((,class (:foreground ,purple ))))
   `(helm-ff-executable ((,class (:foreground ,green ))))
   `(helm-buffer-directory ((,class (:foreground ,aqua))))
   `(helm-buffer-file ((,class (:foreground ,foreground))))
   `(helm-grep-file ((,class (:foreground ,aqua :underline t))))
   `(helm-buffer-process ((,class (:foreground ,red))))
   `(helm-buffer-not-saved ((,class (:foreground ,orange))))
   `(helm-candidate-number ((,class (:foreground ,background :background ,aqua))))
   `(helm-source-header ((,class (:background ,header-color :foreground ,"#424242" :height 1.3 :bold t ))))

   `(speedbar-directory-face ((t (:foreground "#0367B4"))))
   `(speedbar-file-face ((t (:foreground "#007979"))))
   `(speedbar-highlight-face ((t (:background "#E6A860"))))
   `(speedbar-separator-face ((t (:background "#0367B4" :foreground "white" :overline "dark gray"))))
   `(speedbar-tag-face ((t (:foreground "#6B7D14"))))
   
   ;; guide-key
   `(guide-key/key-face ((,class (:foreground ,foreground ))))
   `(guide-key/highlight-command-face ((,class (:foreground ,orange ))))
   `(guide-key/prefix-command-face ((,class (:foreground ,blue ))))

   ;; which-key
   `(which-key-key-face ((,class (:foreground ,foreground  :weight bold))))
   `(which-key-special-key-face ((,class (:foreground ,orange  :weight bold :height 1.1))))
   `(which-key-command-description-face ((,class (:foreground ,foreground ))))
   `(which-key-group-description-face ((,class (:foreground ,blue ))))
   `(which-key-separator-face ((,class (:foreground ,comment ))))

   ;; mark-multiple
   `(mm/master-face ((,class (:inherit region :foreground nil :background nil))))
   `(mm/mirror-face ((,class (:inherit region :foreground nil :background nil))))

   `(org-agenda-structure ((,class (:foreground ,aqua :bold t))))
   `(org-agenda-date ((,class (:foreground ,blue :underline nil))))
   `(org-agenda-done ((,class (:foreground ,green))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
   `(org-block ((,class (:foreground ,foreground :background ,"#EFEBE9"))))
   `(org-block-background ((,t (:background ,"#EFEBE9"))))
   `(org-code ((,class (:foreground ,foreground :background ,"#EFEBE9"))))
   `(org-column ((,class (:background ,ll-yellow))))
   `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
   `(org-date ((,class (:foreground ,"#80cbc4" :underline t))))
   `(org-document-info ((,class (:foreground ,aqua :height 1.35))))
   `(org-document-info-keyword ((,class (:foreground ,green :height 1.35))))
   `(org-document-title ((,class (:weight bold :foreground ,foreground :height 1.35))))
   `(org-done ((,class (:background ,"#c8e6c9" :bold t :foreground,"#2e7d32"))))
   `(org-ellipsis ((,class (:foreground ,comment))))
   `(org-footnote ((,class (:foreground ,aqua))))
   `(org-formula ((,class (:foreground ,red))))
   `(org-hide ((,class (:foreground ,background :background ,background))))
   `(org-link ((,class (:foreground ,red :underline t))))
   `(org-scheduled ((,class (:foreground ,green))))
   `(org-scheduled-previously ((,class (:foreground ,orange))))
   `(org-scheduled-today ((,class (:foreground ,green))))
   `(org-special-keyword ((,class (:foreground ,comment))))
   `(org-table ((,class (:foreground ,"#1565c0" :background ,"#e0f7fa"))))
   `(org-todo ((,class (:background ,"#ffcdd2" :bold t :foreground ,"#c62828"))))
   `(org-upcoming-deadline ((,class (:foreground ,orange))))
   `(org-warning ((,class (:weight bold :foreground ,red))))
   `(org-block-begin-line ((,class (:foreground ,"#4e342e" :background "#efebe9"
                                                :box (:style released-button)
                                                ))))
   `(org-block-end-line ((,class (:foreground ,"#4e342e" :background "#efebe9"
                                              :box (:style released-button)))))
   `(org-kbd ((,class (:background ,inactive-gray :foreground ,foreground
                                   :box (:line-width 1 :color nil :style pressed-button)))))

   `(org-level-1 ((,class (:inherit outline-1
                         :background ,inactive-gray
                         :weight bold
                         :box (:style released-button)
                         :height 1.3))))
   `(org-level-2 ((,class (:inherit outline-2
                                  :background ,"#C8E6C9"
                                  :box (:style released-button)
                         :height 1.2))))
   `(org-level-3 ((,class (:inherit outline-3  :height 1.1))))
   `(org-level-4 ((,class (:inherit outline-4  :height 1.0))))
   `(org-level-5 ((,class (:inherit outline-5 ))))
   `(org-level-6 ((,class (:inherit outline-6 ))))
   `(org-level-7 ((,class (:inherit outline-7 ))))
   `(org-level-8 ((,class (:inherit outline-8 ))))
   `(org-level-9 ((,class (:inherit outline-9 ))))

   `(markdown-header-face-1 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.3 ))))
   `(markdown-header-face-2 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.2 ))))
   `(markdown-header-face-3 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-4 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-5 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-6 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-7 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-8 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-9 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-delimiter-face ((,class (:inherit font-lock-function-name-face :weight bold
                                              :height 1.2))))
   `(markdown-url-face ((,class (:inherit link))))
   `(markdown-link-face ((,class (:foreground ,blue :underline t))))

   ;`(hl-sexp-face ((,class (:background ,ll-yellow))))
   `(highlight-symbol-face ((,class (:background ,selection))))
   `(highlight-80+ ((,class (:background ,ll-yellow))))

   ;; Python-specific overrides
   `(py-builtins-face ((,class (:foreground ,"#ff7043" :weight normal))))

   ;; js2-mode
   `(js2-warning ((,class (:underline ,orange))))
   `(js2-error ((,class (:foreground nil :underline ,red))))
   `(js2-external-variable ((,class (:foreground ,purple))))
   `(js2-function-param ((,class (:foreground ,blue))))
   `(js2-instance-member ((,class (:foreground ,blue))))
   `(js2-private-function-call ((,class (:foreground ,red))))

   ;; js3-mode
   `(js3-warning-face ((,class (:underline ,orange))))
   `(js3-error-face ((,class (:foreground nil :underline ,red))))
   `(js3-external-variable-face ((,class (:foreground ,purple))))
   `(js3-function-param-face ((,class (:foreground ,blue))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,orange))))
   `(js3-jsdoc-type-face ((,class (:foreground ,aqua))))
   `(js3-jsdoc-value-face ((,class (:foreground ,yellow))))
   `(js3-jsdoc-html-tag-name-face ((,class (:foreground ,blue))))
   `(js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,green))))
   `(js3-instance-member-face ((,class (:foreground ,blue))))
   `(js3-private-function-call-face ((,class (:foreground ,red))))

   ;; nxml
   `(nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))))
   `(nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
   `(nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
   `(nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
   `(nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))))
   `(rng-error-face ((,class (:underline ,red))))

   ;; Message-mode
   `(message-header-other ((,class (:foreground nil :background nil :weight normal))))
   `(message-header-subject ((,class (:inherit message-header-other :weight bold :foreground ,yellow))))
   `(message-header-to ((,class (:inherit message-header-other :weight bold :foreground ,orange))))
   `(message-header-cc ((,class (:inherit message-header-to :foreground nil))))
   `(message-header-name ((,class (:foreground ,blue :background nil))))
   `(message-header-newsgroups ((,class (:foreground ,aqua :background nil :slant normal))))
   `(message-separator ((,class (:foreground ,purple))))

   ;; Company autocomplete
   ;; `(company-echo ((,class ())))
   ;; `(company-echo-common ((,class ())))
   `(company-preview ((,class (:foreground ,comment :background ,inactive-gray))))
   `(company-preview-common ((,class (:foreground ,comment :background ,inactive-gray)))) ; same background as highlight-line
   ;; `(company-preview-search ((,class ())))
   `(company-scrollbar-bg ((,class (:background "#F0F0F0"))))
   `(company-scrollbar-fg ((,class (:background "#C0C0C0"))))
   `(company-template-field ((,class (:background ,inactive-gray))))
   `(company-tooltip ((,class (:weight bold :foreground, comment :background ,inactive-gray))))
   `(company-tooltip-annotation ((,class (:weight normal :foreground ,comment :background ,inactive-gray))))
   `(company-tooltip-annotation-selection ((,class (:weight normal :inherit company-tooltip-selection))))
   `(company-tooltip-common ((,class (:weight normal :inherit company-tooltip))))
   `(company-tooltip-common-selection ((,class (:weight normal :inherit company-tooltip-selection))))
   ;; `(company-tooltip-mouse ((,class ())))
   ;; `(company-tooltip-search ((,class ())))
   `(company-tooltip-selection ((,class (:weight bold :foreground ,foreground :background ,far-background))))

   ;; Outline
   `(outline-1 ((,class (:inherit nil :foreground ,"#424242"))))
   `(outline-2 ((,class (:inherit nil :foreground ,"#646464"))))
   `(outline-3 ((,class (:inherit nil :foreground ,"#2e7d32"))))
   `(outline-4 ((,class (:inherit nil :foreground ,"#ef6c00"))))
   `(outline-5 ((,class (:inherit nil :foreground ,"#0277bd"))))
   `(outline-6 ((,class (:inherit nil :foreground ,"#0288d1"))))
   `(outline-7 ((,class (:inherit nil :foreground ,"#689f38"))))
   `(outline-8 ((,class (:inherit nil :foreground ,purple))))
   `(outline-9 ((,class (:inherit nil :foreground ,"LightSteelBlue1"))))

   ;; auctex
   `(font-latex-bold-face                 ((t (:inherit bold :foreground ,foreground))))
   `(font-latex-doctex-documentation-face ((t (:background unspecified))))
   `(font-latex-doctex-preprocessor-face ((t (:inherit (font-latex-doctex-documentation-face
                                                        font-lock-builtin-face font-lock-preprocessor-face)))))
   `(font-latex-italic-face               ((t (:inherit italic :foreground ,foreground))))
   `(font-latex-math-face                 ((t (:foreground ,blue))))
   `(font-latex-sectioning-0-face         ((t (:inherit outline-1 :height 1.4))))
   `(font-latex-sectioning-1-face         ((t (:inherit outline-2 :height 1.35))))
   `(font-latex-sectioning-2-face         ((t (:inherit outline-3 :height 1.3))))
   `(font-latex-sectioning-3-face         ((t (:inherit outline-4 :height 1.25))))
   `(font-latex-sectioning-4-face         ((t (:inherit outline-5 :height 1.2))))
   `(font-latex-sectioning-5-face         ((t (:inherit outline-6 :height 1.1))))
   `(font-latex-sedate-face               ((t (:foreground ,green))))
   `(font-latex-slide-title-face          ((t (:inherit font-lock-type-face :weight bold :height 1.2))))
   `(font-latex-string-face               ((t (:inherit font-lock-string-face))))
   `(font-latex-subscript-face            ((t (:height 0.8))))
   `(font-latex-superscript-face          ((t (:height 0.8))))
   `(font-latex-warning-face              ((t (:inherit font-lock-warning-face))))
   )

  (custom-theme-set-variables
   'ajj-light
   `(fci-rule-color ,ll-yellow)
   `(vc-annotate-color-map
     '((20  . ,red)
       (40  . ,orange)
       (60  . ,yellow)
       (80  . ,green)
       (100 . ,aqua)
       (120 . ,blue)
       (140 . ,purple)
       (160 . ,red)
       (180 . ,orange)
       (200 . ,yellow)
       (220 . ,green)
       (240 . ,aqua)
       (260 . ,blue)
       (280 . ,purple)
       (300 . ,red)
       (320 . ,orange)
       (340 . ,yellow)
       (360 . ,green)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; highlight-sexp-mode
   `(hl-sexp-background-color ,"#efebe9")

   `(ansi-color-names-vector (vector ,foreground ,red ,green ,yellow ,blue ,purple ,aqua ,background))
   '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ajj-light)

;;; ajj-light-theme.el ends here
