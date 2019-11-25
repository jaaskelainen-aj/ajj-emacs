;;; ajj-dark-theme.el --- Personal dark theme

;; Copyright (C) 2019 Antti Jääskeläinen

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(unless (>= emacs-major-version 24)
  (error "The AJJ dark theme requires Emacs 24 or later!"))

(deftheme ajj-dark
  "The AJJ dark colour theme. Based on Monokai (by Kelvin Smith)")

(defgroup ajj-dark nil
  "Ajj-Dark theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom ajj-dark-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'ajj-dark)

(defcustom ajj-dark-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'ajj-dark)

(defcustom ajj-dark-doc-face-as-comment nil
  "Consider `font-lock-doc-face' as comment instead of a string."
  :type 'boolean
  :group 'ajj-dark
  :package-version "3.5.1")

(defcustom ajj-dark-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'ajj-dark)

(defcustom ajj-dark-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'ajj-dark)

(defcustom ajj-dark-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'ajj-dark)

(defcustom ajj-dark-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'ajj-dark)

(defcustom ajj-dark-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'ajj-dark)

;; Primary colors
(defcustom ajj-dark-yellow "#E6DB74"
  "Primary colors - yellow"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-orange "#FD971F"
  "Primary colors - orange"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-red "#F92672"
  "Primary colors - red"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-magenta "#FD5FF0"
  "Primary colors - magenta"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-blue "#66D9EF"
  "Primary colors - blue"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-green "#A6E22E"
  "Primary colors - green"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-cyan "#A1EFE4"
  "Primary colors - cyan"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-violet "#AE81FF"
  "Primary colors - violet"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-gray "#64645E"
  "Primary colors - gray"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-foreground "#F8F8F2"
  "Adaptive colors - foreground"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-background "#232516"
  "Adaptive colors - background"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-comments "#75715E"
  "Adaptive colors - comments"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-emphasis "#F8F8F0"
  "Adaptive colors - emphasis"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-line-number "#8F908A"
  "Adaptive colors - line number"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-highlight "#49483E"
  "Adaptive colors - highlight"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-highlight-alt "#3E3D31"
  "Adaptive colors - highlight"
  :type 'string
  :group 'ajj-dark)

(defcustom ajj-dark-highlight-line "#3C3D37"
  "Adaptive colors - line highlight"
  :type 'string
  :group 'ajj-dark)

(let* (;; Variable pitch
       (ajj-dark-pitch (if ajj-dark-use-variable-pitch
                          'variable-pitch
                        'default))

       ;; Definitions for guis that support 256 colors
       (ajj-dark-class '((class color) (min-colors 257)))

       ;; Functionality specific colors
       (ajj-dark-diff-blue-base      "#232438")
       (ajj-dark-diff-blue-emphasis  "#1F204E")
       (ajj-dark-diff-green-base     "#233E1E")
       (ajj-dark-diff-green-emphasis "#1F541A")
       (ajj-dark-diff-red-base       "#3D241E")
       (ajj-dark-diff-red-emphasis   "#53201A")

       ;; Darker and lighter accented colors
       (ajj-dark-yellow-d       "#BEB244")
       (ajj-dark-yellow-l       "#FFF7A8")
       (ajj-dark-orange-d       "#D47402")
       (ajj-dark-orange-l       "#FFAC4A")
       (ajj-dark-red-d          "#F70057")
       (ajj-dark-red-l          "#FA518D")
       (ajj-dark-magenta-d      "#FB35EA")
       (ajj-dark-magenta-l      "#FE8CF4")
       (ajj-dark-violet-d       "#945AFF")
       (ajj-dark-violet-l       "#C9ACFF")
       (ajj-dark-blue-d         "#40CAE4")
       (ajj-dark-blue-l         "#92E7F7")
       (ajj-dark-cyan-d         "#74DBCD")
       (ajj-dark-cyan-l         "#D3FBF6")
       (ajj-dark-green-d        "#86C30D")
       (ajj-dark-green-l        "#BBEF53")
       (ajj-dark-gray-d         "#35331D")
       (ajj-dark-gray-l         "#7B7962")
       ;; Adaptive higher/lower contrast accented colors
       (ajj-dark-foreground-hc  "#141414")
       (ajj-dark-foreground-lc  "#171A0B")
       ;; High contrast colors
       (ajj-dark-yellow-hc      "#FFFACE")
       (ajj-dark-yellow-lc      "#9A8F21")
       (ajj-dark-orange-hc      "#FFBE74")
       (ajj-dark-orange-lc      "#A75B00")
       (ajj-dark-red-hc         "#FEB0CC")
       (ajj-dark-red-lc         "#F20055")
       (ajj-dark-magenta-hc     "#FEC6F9")
       (ajj-dark-magenta-lc     "#F309DF")
       (ajj-dark-violet-hc      "#F0E7FF")
       (ajj-dark-violet-lc      "#7830FC")
       (ajj-dark-blue-hc        "#CAF5FD")
       (ajj-dark-blue-lc        "#1DB4D0")
       (ajj-dark-cyan-hc        "#D3FBF6")
       (ajj-dark-cyan-lc        "#4BBEAE")
       (ajj-dark-green-hc       "#CCF47C")
       (ajj-dark-green-lc       "#679A01")

       ;; Distinct fringe
       (ajj-dark-fringe-bg (if ajj-dark-distinct-fringe-background
                              ajj-dark-gray
                            ajj-dark-background))

       ;; Definitions for terminals that do not support 256 colors
       (ajj-dark-256-class '((class color) (min-colors 89)))

       ;; Functionality specific colors
       (ajj-dark-256-diff-blue-base      "#00005f")
       (ajj-dark-256-diff-blue-emphasis  "#000087")
       (ajj-dark-256-diff-green-base     "#005800")
       (ajj-dark-256-diff-green-emphasis "#008700")
       (ajj-dark-256-diff-red-base       "#5f0000")
       (ajj-dark-256-diff-red-emphasis   "#870000")

       ;; Primary colors
       (ajj-dark-256-yellow         "#CDC673")
       (ajj-dark-256-orange         "#FF8C00")
       (ajj-dark-256-red            "#FF1493")
       (ajj-dark-256-magenta        "#D700D7")
       (ajj-dark-256-violet         "#AF87FF")
       (ajj-dark-256-blue           "#5FD7FF")
       (ajj-dark-256-cyan           "#5FFFFF")
       (ajj-dark-256-green          "#87D700")
       (ajj-dark-256-gray           "#3D3D3D")
       ;; Darker and lighter accented colors
       (ajj-dark-256-yellow-d       "#878700")
       (ajj-dark-256-yellow-l       "#FFFF87")
       (ajj-dark-256-orange-d       "#AF5F00")
       (ajj-dark-256-orange-l       "#FFAF5F")
       (ajj-dark-256-red-d          "#870000")
       (ajj-dark-256-red-l          "#FF5F87")
       (ajj-dark-256-magenta-d      "#AF0087")
       (ajj-dark-256-magenta-l      "#FF87DF")
       (ajj-dark-256-violet-d       "#5F00AF")
       (ajj-dark-256-violet-l       "#AF87D7")
       (ajj-dark-256-blue-d         "#008787")
       (ajj-dark-256-blue-l         "#87D7FF")
       (ajj-dark-256-cyan-d         "#5FAFAF")
       (ajj-dark-256-cyan-l         "#AFFFFF")
       (ajj-dark-256-green-d        "#5F8700")
       (ajj-dark-256-green-l        "#AFD700")
       (ajj-dark-256-gray-d         "#333333")
       (ajj-dark-256-gray-l         "#707070")
       ;; Adaptive colors
       (ajj-dark-256-foreground     "#F5F5F5")
       (ajj-dark-256-background     "#1B1E1C")
       (ajj-dark-256-comments       "#8B8878")
       (ajj-dark-256-emphasis       "#FFFAFA")
       (ajj-dark-256-line-number    "#8F908A")
       (ajj-dark-256-highlight      "#474747")
       (ajj-dark-256-highlight-alt  "#3E3E3E")
       (ajj-dark-256-highlight-line "#000000")
       ;; Adaptive higher/lower contrast accented colors
       (ajj-dark-256-foreground-hc  "#171A0B")
       (ajj-dark-256-foreground-lc  "#141414")
       ;; High contrast colors
       (ajj-dark-256-yellow-hc      ajj-dark-256-yellow-d)
       (ajj-dark-256-yellow-lc      ajj-dark-256-yellow-l)
       (ajj-dark-256-orange-hc      ajj-dark-256-orange-d)
       (ajj-dark-256-orange-lc      ajj-dark-256-orange-l)
       (ajj-dark-256-red-hc         ajj-dark-256-red-d)
       (ajj-dark-256-red-lc         ajj-dark-256-red-l)
       (ajj-dark-256-magenta-hc     ajj-dark-256-magenta-d)
       (ajj-dark-256-magenta-lc     ajj-dark-256-magenta-l)
       (ajj-dark-256-violet-hc      ajj-dark-256-violet-d)
       (ajj-dark-256-violet-lc      ajj-dark-256-violet-l)
       (ajj-dark-256-blue-hc        ajj-dark-256-blue-d)
       (ajj-dark-256-blue-lc        ajj-dark-256-blue-l)
       (ajj-dark-256-cyan-hc        ajj-dark-256-cyan-d)
       (ajj-dark-256-cyan-lc        ajj-dark-256-cyan-l)
       (ajj-dark-256-green-hc       ajj-dark-256-green-d)
       (ajj-dark-256-green-lc       ajj-dark-256-green-l)

       ;; Distinct fringe
       (ajj-dark-256-fringe-bg (if ajj-dark-distinct-fringe-background
                                  ajj-dark-256-gray
                                ajj-dark-256-background)))

  ;; Define faces
  (custom-theme-set-faces
   'ajj-dark

   ;; font lock for syntax highlighting
   `(font-lock-builtin-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(font-lock-comment-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(font-lock-constant-face
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   `(font-lock-doc-face
     ((,ajj-dark-class (:foreground ,(if ajj-dark-doc-face-as-comment
                                        ajj-dark-comments
                                      ajj-dark-yellow)))
      (,ajj-dark-256-class (:foreground ,(if ajj-dark-doc-face-as-comment
                                            ajj-dark-256-comments
                                          ajj-dark-256-yellow)))))

   `(font-lock-function-name-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(font-lock-keyword-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :weight normal))))

   `(font-lock-negation-char-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight bold))))

   `(font-lock-preprocessor-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(font-lock-regexp-grouping-construct
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,ajj-dark-class (:foreground ,ajj-dark-violet
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet
                                        :weight normal))))

   `(font-lock-string-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(font-lock-type-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :italic nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :italic nil))))

   `(font-lock-variable-name-face
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(font-lock-warning-face
     ((,ajj-dark-class (:foreground ,ajj-dark-orange
                                   :weight bold
                                   :italic t
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange
                                        :weight bold
                                        :italic t
                                        :underline t))))

   `(c-annotation-face
     ((,ajj-dark-class (:inherit font-lock-constant-face))
      (,ajj-dark-256-class (:inherit font-lock-constant-face))))

   ;; general colouring
   '(button ((t (:underline t))))

   `(default
      ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                    :background ,ajj-dark-background))
       (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                         :background ,ajj-dark-256-background))))

   `(highlight
     ((,ajj-dark-class (:background ,ajj-dark-highlight))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight))))

   `(lazy-highlight
     ((,ajj-dark-class (:inherit highlight
                                :background ,ajj-dark-highlight-alt))
      (,ajj-dark-256-class (:inherit highlight
                                     :background ,ajj-dark-256-highlight-alt))))

   `(region
     ((,ajj-dark-class (:inherit highlight
                                :background ,ajj-dark-highlight))
      (,ajj-dark-256-class (:inherit highlight
                                     :background ,ajj-dark-256-highlight))))

   `(secondary-selection
     ((,ajj-dark-class (:inherit region
                                :background ,ajj-dark-highlight-alt))
      (,ajj-dark-256-class (:inherit region
                                     :background ,ajj-dark-256-highlight-alt))))

   `(shadow
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(match
     ((,ajj-dark-class (:background ,ajj-dark-green
                                   :foreground ,ajj-dark-background
                                   :weight bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-green
                                        :foreground ,ajj-dark-256-background
                                        :weight bold))))

   `(cursor
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-foreground
                                   :inverse-video t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-foreground
                                        :inverse-video t))))

   `(mouse
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-foreground
                                   :inverse-video t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-foreground
                                        :inverse-video t))))

   `(escape-glyph
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(escape-glyph-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(fringe
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :background ,ajj-dark-fringe-bg))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :background ,ajj-dark-256-fringe-bg))))

   `(link
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :underline t
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :underline t
                                        :weight bold))))

   `(link-visited
     ((,ajj-dark-class (:foreground ,ajj-dark-violet
                                   :underline t
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet
                                        :underline t
                                        :weight normal))))

   `(success
     ((,ajj-dark-class (:foreground ,ajj-dark-green ))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green ))))

   `(warning
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow ))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow ))))

   `(error
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(eval-sexp-fu-flash
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-green))))

   `(eval-sexp-fu-flash-error
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-red))))

   `(trailing-whitespace
     ((,ajj-dark-class (:background ,ajj-dark-red))
      (,ajj-dark-256-class (:background ,ajj-dark-256-red))))

   `(vertical-border
     ((,ajj-dark-class (:foreground ,ajj-dark-gray))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-gray))))

   `(menu
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :background ,ajj-dark-256-background))))

   `(minibuffer-prompt
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   ;; mode-line and powerline
   `(mode-line-buffer-id
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :weight bold))))

   `(mode-line
     ((,ajj-dark-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,ajj-dark-emphasis
                                      :background ,ajj-dark-highlight
                                      :box (:line-width 1
                                                        :color ,ajj-dark-gray
                                                        :style unspecified)))
      (,ajj-dark-256-class (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,ajj-dark-256-foreground
                                           :background ,ajj-dark-256-background
                                           :box (:line-width 1
                                                             :color ,ajj-dark-256-highlight
                                                             :style unspecified)))))

   `(powerline-active1
     ((,ajj-dark-class (:background ,ajj-dark-gray-d))
      (,ajj-dark-256-class (:background ,ajj-dark-256-gray-d))))

   `(powerline-active2
     ((,ajj-dark-class (:background ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background))))


   `(mode-line-inactive
     ((,ajj-dark-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,ajj-dark-comments
                                      :background ,ajj-dark-background
                                      :box (:line-width 1
                                                        :color ,ajj-dark-gray
                                                        :style unspecified)))
      (,ajj-dark-256-class (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,ajj-dark-256-comments
                                           :background ,ajj-dark-256-background
                                           :box (:line-width 1
                                                             :color ,ajj-dark-256-gray
                                                             :style unspecified)))))

   `(powerline-inactive1
     ((,ajj-dark-class (:background ,ajj-dark-gray-d))
      (,ajj-dark-256-class (:background ,ajj-dark-256-gray-d))))

   `(powerline-inactive2
     ((,ajj-dark-class (:background ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background))))

   ;; header-line
   `(header-line
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :background ,ajj-dark-highlight
                                   :box (:color ,ajj-dark-gray
                                                :line-width 1
                                                :style unspecified)))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :background ,ajj-dark-256-highlight
                                        :box (:color ,ajj-dark-256-gray
                                                     :line-width 1
                                                     :style unspecified)))))

   ;; cua
   `(cua-global-mark
     ((,ajj-dark-class (:background ,ajj-dark-yellow
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow
                                        :foreground ,ajj-dark-256-background))))

   `(cua-rectangle
     ((,ajj-dark-class (:inherit region))
      (,ajj-dark-256-class (:inherit region))))

   `(cua-rectangle-noselect
     ((,ajj-dark-class (:inherit secondary-selection))
      (,ajj-dark-256-class (:inherit secondary-selection))))

   ;; diary
   `(diary
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   ;; dired
   `(dired-directory
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(dired-flagged
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(dired-header
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :background ,ajj-dark-background
                                   :inherit bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :background ,ajj-dark-256-background
                                        :inherit bold))))

   `(dired-ignored
     ((,ajj-dark-class (:inherit shadow))
      (,ajj-dark-256-class (:inherit shadow))))

   `(dired-mark
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :weight bold))))

   `(dired-marked
     ((,ajj-dark-class (:foreground ,ajj-dark-violet
                                   :inherit bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet
                                        :inherit bold))))

   `(dired-perm-write
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :underline t))))

   `(dired-symlink
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan
                                   :slant italic))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan
                                        :slant italic))))

   `(dired-warning
     ((,ajj-dark-class (:foreground ,ajj-dark-orange
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange
                                        :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-blue))))

   `(dropdown-list-selection-face
     ((,ajj-dark-class (:background ,ajj-dark-green
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-green
                                        :foreground ,ajj-dark-256-background))))

   ;; ecb
   `(ecb-default-highlight-face
     ((,ajj-dark-class (:background ,ajj-dark-blue
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue
                                        :foreground ,ajj-dark-256-background))))

   `(ecb-history-bucket-node-dir-soure-path-face
     ((,ajj-dark-class (:inherit ecb-history-bucket-node-face
                                :foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:inherit ecb-history-bucket-node-face
                                     :foreground ,ajj-dark-256-yellow))))

   `(ecb-source-in-directories-buffer-face
     ((,ajj-dark-class (:inherit ecb-directories-general-face
                                :foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:inherit ecb-directories-general-face
                                     :foreground ,ajj-dark-256-foreground))))

   `(ecb-history-dead-buffer-face
     ((,ajj-dark-class (:inherit ecb-history-general-face
                                :foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:inherit ecb-history-general-face
                                     :foreground ,ajj-dark-256-comments))))

   `(ecb-directory-not-accessible-face
     ((,ajj-dark-class (:inherit ecb-directories-general-face
                                :foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:inherit ecb-directories-general-face
                                     :foreground ,ajj-dark-256-comments))))

   `(ecb-bucket-node-face
     ((,ajj-dark-class (:inherit ecb-default-general-face
                                :weight normal
                                :foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:inherit ecb-default-general-face
                                     :weight normal
                                     :foreground ,ajj-dark-256-blue))))

   `(ecb-tag-header-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line))))

   `(ecb-analyse-bucket-element-face
     ((,ajj-dark-class (:inherit ecb-analyse-general-face
                                :foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:inherit ecb-analyse-general-face
                                     :foreground ,ajj-dark-256-green))))

   `(ecb-directories-general-face
     ((,ajj-dark-class (:inherit ecb-default-general-face
                                :height 1.0))
      (,ajj-dark-256-class (:inherit ecb-default-general-face
                                     :height 1.0))))

   `(ecb-method-non-semantic-face
     ((,ajj-dark-class (:inherit ecb-methods-general-face
                                :foreground ,ajj-dark-cyan))
      (,ajj-dark-256-class (:inherit ecb-methods-general-face
                                     :foreground ,ajj-dark-256-cyan))))

   `(ecb-mode-line-prefix-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(ecb-tree-guide-line-face
     ((,ajj-dark-class (:inherit ecb-default-general-face
                                :foreground ,ajj-dark-gray
                                :height 1.0))
      (,ajj-dark-256-class (:inherit ecb-default-general-face
                                     :foreground ,ajj-dark-256-gray
                                     :height 1.0))))

   ;; ee
   `(ee-bookmarked
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis))))

   `(ee-category
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(ee-link
     ((,ajj-dark-class (:inherit link))
      (,ajj-dark-256-class (:inherit link))))

   `(ee-link-visited
     ((,ajj-dark-class (:inherit link-visited))
      (,ajj-dark-256-class (:inherit link-visited))))

   `(ee-marked
     ((,ajj-dark-class (:foreground ,ajj-dark-magenta
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-magenta
                                        :weight bold))))

   `(ee-omitted
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(ee-shadow
     ((,ajj-dark-class (:inherit shadow))
      (,ajj-dark-256-class (:inherit shadow))))

   ;; grep
   `(grep-context-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(grep-error-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :weight bold
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :weight bold
                                        :underline t))))

   `(grep-hit-face
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(grep-match-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :weight bold))))

   ;; isearch
   `(isearch
     ((,ajj-dark-class (:inherit region
                                :foreground ,ajj-dark-background
                                :background ,ajj-dark-yellow))
      (,ajj-dark-256-class (:inherit region
                                     :foreground ,ajj-dark-256-background
                                     :background ,ajj-dark-256-yellow))))

   `(isearch-fail
     ((,ajj-dark-class (:inherit isearch
                                :foreground ,ajj-dark-red
                                :background ,ajj-dark-background
                                :bold t))
      (,ajj-dark-256-class (:inherit isearch
                                     :foreground ,ajj-dark-256-red
                                     :background ,ajj-dark-256-background
                                     :bold t))))


   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,ajj-dark-class (:foreground ,ajj-dark-comments
                                   :background ,ajj-dark-background
                                   :inverse-video nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments
                                        :background ,ajj-dark-256-background
                                        :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :background ,ajj-dark-background
                                   :inverse-video nil
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :background ,ajj-dark-256-background
                                        :inverse-video nil
                                        :weight bold))))

   ;; auctex
   `(font-latex-bold-face
     ((,ajj-dark-class (:inherit bold
                                :foreground ,ajj-dark-emphasis))
      (,ajj-dark-256-class (:inherit bold
                                     :foreground ,ajj-dark-256-emphasis))))

   `(font-latex-doctex-documentation-face
     ((,ajj-dark-class (:background unspecified))
      (,ajj-dark-256-class (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,ajj-dark-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))
      (,ajj-dark-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,ajj-dark-class (:inherit italic :foreground ,ajj-dark-emphasis))
      (,ajj-dark-256-class (:inherit italic :foreground ,ajj-dark-256-emphasis))))

   `(font-latex-math-face
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   `(font-latex-sectioning-0-face
     ((,ajj-dark-class (:inherit font-latex-sectioning-1-face
                                :height ,ajj-dark-height-plus-1))
      (,ajj-dark-256-class (:inherit font-latex-sectioning-1-face
                                     :height ,ajj-dark-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,ajj-dark-class (:inherit font-latex-sectioning-2-face
                                :height ,ajj-dark-height-plus-1))
      (,ajj-dark-256-class (:inherit font-latex-sectioning-2-face
                                     :height ,ajj-dark-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,ajj-dark-class (:inherit font-latex-sectioning-3-face
                                :height ,ajj-dark-height-plus-1))
      (,ajj-dark-256-class (:inherit font-latex-sectioning-3-face
                                     :height ,ajj-dark-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,ajj-dark-class (:inherit font-latex-sectioning-4-face
                                :height ,ajj-dark-height-plus-1))
      (,ajj-dark-256-class (:inherit font-latex-sectioning-4-face
                                     :height ,ajj-dark-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,ajj-dark-class (:inherit font-latex-sectioning-5-face
                                :height ,ajj-dark-height-plus-1))
      (,ajj-dark-256-class (:inherit font-latex-sectioning-5-face
                                     :height ,ajj-dark-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :foreground ,ajj-dark-yellow
                                :weight bold))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch :
                                     foreground ,ajj-dark-256-yellow
                                     :weight bold))))

   `(font-latex-sedate-face
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis))))

   `(font-latex-slide-title-face
     ((,ajj-dark-class (:inherit (,ajj-dark-pitch font-lock-type-face)
                                :weight bold
                                :height ,ajj-dark-height-plus-3))
      (,ajj-dark-256-class (:inherit (,ajj-dark-pitch font-lock-type-face)
                                     :weight bold
                                     :height ,ajj-dark-height-plus-3))))

   `(font-latex-string-face
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan))))

   `(font-latex-subscript-face
     ((,ajj-dark-class (:height ,ajj-dark-height-minus-1))
      (,ajj-dark-256-class (:height ,ajj-dark-height-minus-1))))

   `(font-latex-superscript-face
     ((,ajj-dark-class (:height ,ajj-dark-height-minus-1))
      (,ajj-dark-256-class (:height ,ajj-dark-height-minus-1))))

   `(font-latex-verbatim-face
     ((,ajj-dark-class (:inherit fixed-pitch
                                :foreground ,ajj-dark-foreground
                                :slant italic))
      (,ajj-dark-256-class (:inherit fixed-pitch
                                     :foreground ,ajj-dark-256-foreground
                                     :slant italic))))

   `(font-latex-warning-face
     ((,ajj-dark-class (:inherit bold
                                :foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:inherit bold
                                     :foreground ,ajj-dark-256-orange))))

   ;; auto-complete
   `(ac-candidate-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-blue))))

   `(ac-selection-face
     ((,ajj-dark-class (:background ,ajj-dark-blue
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue
                                        :foreground ,ajj-dark-256-background))))

   `(ac-candidate-mouse-face
     ((,ajj-dark-class (:background ,ajj-dark-blue
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue
                                        :foreground ,ajj-dark-256-background))))

   `(ac-completion-face
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :underline t))))

   `(ac-gtags-candidate-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-blue))))

   `(ac-gtags-selection-face
     ((,ajj-dark-class (:background ,ajj-dark-blue
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue
                                        :foreground ,ajj-dark-256-background))))

   `(ac-yasnippet-candidate-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-yellow))))

   `(ac-yasnippet-selection-face
     ((,ajj-dark-class (:background ,ajj-dark-yellow
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow
                                        :foreground ,ajj-dark-256-background))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-blue))))

   `(ahs-edit-mode-face
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-highlight))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-highlight))))

   `(ahs-face
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-magenta
                                        :background unspecified))))

   `(ahs-plugin-bod-face
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-violet ))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-cyan ))))

   `(ahs-plugin-defalt-face
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-orange))))

   `(ahs-plugin-whole-buffer-face
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-green))))

   `(ahs-warning-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :weight bold))))

   ;; android mode
   `(android-mode-debug-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(android-mode-error-face
     ((,ajj-dark-class (:foreground ,ajj-dark-orange
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange
                                        :weight bold))))

   `(android-mode-info-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(android-mode-verbose-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(android-mode-warning-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,ajj-dark-class (:foreground ,ajj-dark-violet
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet
                                        :weight bold))))

   ;; bm
   `(bm-face
     ((,ajj-dark-class (:background ,ajj-dark-yellow-lc
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow-lc
                                        :foreground ,ajj-dark-256-background))))

   `(bm-fringe-face
     ((,ajj-dark-class (:background ,ajj-dark-yellow-lc
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow-lc
                                        :foreground ,ajj-dark-256-background))))

   `(bm-fringe-persistent-face
     ((,ajj-dark-class (:background ,ajj-dark-green-lc
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-green-lc
                                        :foreground ,ajj-dark-256-background))))

   `(bm-persistent-face
     ((,ajj-dark-class (:background ,ajj-dark-green-lc
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-green-lc
                                        :foreground ,ajj-dark-256-background))))

   ;; calfw
   `(cfw:face-day-title
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line))))

   `(cfw:face-annotation
     ((,ajj-dark-class (:inherit cfw:face-day-title
                                :foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:inherit cfw:face-day-title
                                     :foreground ,ajj-dark-256-yellow))))

   `(cfw:face-default-content
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(cfw:face-default-day
     ((,ajj-dark-class (:inherit cfw:face-day-title
                                :weight bold))
      (,ajj-dark-256-class (:inherit cfw:face-day-title
                                     :weight bold))))

   `(cfw:face-disable
     ((,ajj-dark-class (:inherit cfw:face-day-title
                                :foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:inherit cfw:face-day-title
                                     :foreground ,ajj-dark-256-comments))))

   `(cfw:face-grid
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(cfw:face-header
     ((,ajj-dark-class (:foreground ,ajj-dark-blue-hc
                                   :background ,ajj-dark-blue-lc
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue-hc
                                        :background ,ajj-dark-256-blue-lc
                                        :weight bold))))

   `(cfw:face-holiday
     ((,ajj-dark-class (:background nil
                                   :foreground ,ajj-dark-red
                                   :weight bold))
      (,ajj-dark-256-class (:background nil
                                        :foreground ,ajj-dark-256-red
                                        :weight bold))))

   `(cfw:face-periods
     ((,ajj-dark-class (:foreground ,ajj-dark-magenta))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-magenta))))

   `(cfw:face-select
     ((,ajj-dark-class (:background ,ajj-dark-magenta-lc
                                   :foreground ,ajj-dark-magenta-hc))
      (,ajj-dark-256-class (:background ,ajj-dark-256-magenta-lc
                                        :foreground ,ajj-dark-256-magenta-hc))))

   `(cfw:face-saturday
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan-hc
                                   :background ,ajj-dark-cyan-lc))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan-hc
                                        :background ,ajj-dark-256-cyan-lc))))

   `(cfw:face-sunday
     ((,ajj-dark-class (:foreground ,ajj-dark-red-hc
                                   :background ,ajj-dark-red-lc
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red-hc
                                        :background ,ajj-dark-256-red-lc
                                        :weight bold))))

   `(cfw:face-title
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :foreground ,ajj-dark-yellow
                                :weight bold
                                :height ,ajj-dark-height-plus-4))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :foreground ,ajj-dark-256-yellow
                                     :weight bold
                                     :height ,ajj-dark-height-plus-4))))

   `(cfw:face-today
     ((,ajj-dark-class (:weight bold
                               :background ,ajj-dark-highlight-line
                               :foreground nil))
      (,ajj-dark-256-class (:weight bold
                                    :background ,ajj-dark-256-highlight-line
                                    :foreground nil))))

   `(cfw:face-today-title
     ((,ajj-dark-class (:background ,ajj-dark-yellow-lc
                                   :foreground ,ajj-dark-yellow-hc
                                   :weight bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow-lc
                                        :foreground ,ajj-dark-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-foreground))))

   `(cfw:face-toolbar-button-off
     ((,ajj-dark-class (:background ,ajj-dark-yellow-lc
                                   :foreground ,ajj-dark-yellow-hc
                                   :weight bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow-lc
                                        :foreground ,ajj-dark-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar-button-on
     ((,ajj-dark-class (:background ,ajj-dark-yellow-hc
                                   :foreground ,ajj-dark-yellow-lc
                                   :weight bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow-hc
                                        :foreground ,ajj-dark-256-yellow-lc
                                        :weight bold))))

   ;; cider
   `(cider-enlightened
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :background nil
                                   :box (:color ,ajj-dark-yellow :line-width -1 :style nil)))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :background nil
                                        :box (:color ,ajj-dark-256-yellow :line-width -1 :style nil))) ))

   `(cider-enlightened-local
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(cider-instrumented-face
     ((,ajj-dark-class (:foreground ,ajj-dark-violet
                                   :background nil
                                   :box (:color ,ajj-dark-violet :line-width -1 :style nil)))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet
                                        :background nil
                                        :box (:color ,ajj-dark-256-violet :line-width -1 :style nil)))))

   `(cider-result-overlay-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :background nil
                                   :box (:color ,ajj-dark-blue :line-width -1 :style nil)))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :background nil
                                        :box (:color ,ajj-dark-256-blue :line-width -1 :style nil)))))

   `(cider-test-error-face
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-orange))))

   `(cider-test-failure-face
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-red))))

   `(cider-test-success-face
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-green))))

   `(cider-traced-face
     ((,ajj-dark-class :box (:color ,ajj-dark-blue :line-width -1 :style nil))
      (,ajj-dark-256-class  :box (:color ,ajj-dark-256-blue :line-width -1 :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :weight bold
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-error-face
     ((,ajj-dark-class (:foreground ,ajj-dark-orange
                                   :weight bold
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-success-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :weight bold
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :weight bold
                                        :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-emphasis))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-emphasis))))

   `(company-tooltip-selection
     ((,ajj-dark-class (:background ,ajj-dark-blue
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue
                                        :foreground ,ajj-dark-256-background))))

   `(company-tooltip-mouse
     ((,ajj-dark-class (:background ,ajj-dark-blue
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue
                                        :foreground ,ajj-dark-256-background))))

   `(company-tooltip-common
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :underline t))))

   `(company-tooltip-common-selection
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-blue
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-blue
                                        :underline t))))

   `(company-preview
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-emphasis))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-emphasis))))

   `(company-preview-common
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :underline t))))

   `(company-scrollbar-bg
     ((,ajj-dark-class (:background ,ajj-dark-gray))
      (,ajj-dark-256-class (:background ,ajj-dark-256-gray))))

   `(company-scrollbar-fg
     ((,ajj-dark-class (:background ,ajj-dark-comments))
      (,ajj-dark-256-class (:background ,ajj-dark-256-comments))))

   `(company-tooltip-annotation
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-green))))

   `(company-template-field
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-blue))))

   ;; compilation
   `(compilation-column-face
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan
                                        :underline nil))))

   `(compilation-column-number
     ((,ajj-dark-class (:inherit font-lock-doc-face
                                :foreground ,ajj-dark-cyan
                                :underline nil))
      (,ajj-dark-256-class (:inherit font-lock-doc-face
                                     :foreground ,ajj-dark-256-cyan
                                     :underline nil))))

   `(compilation-enter-directory-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :underline nil))))

   `(compilation-error
     ((,ajj-dark-class (:inherit error
                                :underline nil))
      (,ajj-dark-256-class (:inherit error
                                     :underline nil))))

   `(compilation-error-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :underline nil))))

   `(compilation-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :underline nil))))

   `(compilation-info
     ((,ajj-dark-class (:foreground ,ajj-dark-comments
                                   :underline nil
                                   :bold nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments
                                        :underline nil
                                        :bold nil))))

   `(compilation-info-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :underline nil))))

   `(compilation-leave-directory-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :underline nil))))

   `(compilation-line-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :underline nil))))

   `(compilation-line-number
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :underline nil))))

   `(compilation-warning
     ((,ajj-dark-class (:inherit warning
                                :underline nil))
      (,ajj-dark-256-class (:inherit warning
                                     :underline nil))))

   `(compilation-warning-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight normal
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(compilation-mode-line-exit
     ((,ajj-dark-class (:inherit compilation-info
                                :foreground ,ajj-dark-green
                                :weight bold))
      (,ajj-dark-256-class (:inherit compilation-info
                                     :foreground ,ajj-dark-256-green
                                     :weight bold))))

   `(compilation-mode-line-fail
     ((,ajj-dark-class (:inherit compilation-error
                                :foreground ,ajj-dark-red
                                :weight bold))
      (,ajj-dark-256-class (:inherit compilation-error
                                     :foreground ,ajj-dark-256-red
                                     :weight bold))))

   `(compilation-mode-line-run
     ((,ajj-dark-class (:foreground ,ajj-dark-orange
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange
                                        :weight bold))))

   ;; CSCOPE
   `(cscope-file-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :weight bold))))

   `(cscope-function-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(cscope-line-number-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(cscope-line-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(cscope-mouse-face
     ((,ajj-dark-class (:background ,ajj-dark-blue
                                   :foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue
                                        :foreground ,ajj-dark-256-foreground))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-emphasis
                                   :underline ,ajj-dark-emphasis
                                   :weight bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-emphasis
                                        :underline ,ajj-dark-256-emphasis
                                        :weight bold))))

   `(ctbl:face-continue-bar
     ((,ajj-dark-class (:background ,ajj-dark-gray
                                   :foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:background ,ajj-dark-256-gray
                                        :foreground ,ajj-dark-256-yellow))))

   `(ctbl:face-row-select
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-foreground
                                   :underline t))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-foreground
                                        :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight bold))))

   `(coffee-mode-function-param
     ((,ajj-dark-class (:foreground ,ajj-dark-violet
                                   :slant italic))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet
                                        :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :height ,ajj-dark-height-plus-3
                                :foreground ,ajj-dark-violet
                                :weight bold))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :height ,ajj-dark-height-plus-3
                                     :foreground ,ajj-dark-256-violet
                                     :weight bold))))

   `(custom-variable-tag
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :foreground ,ajj-dark-cyan
                                :height ,ajj-dark-height-plus-3))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :foreground ,ajj-dark-256-cyan
                                     :height ,ajj-dark-height-plus-3))))

   `(custom-comment-tag
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(custom-group-tag
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :foreground ,ajj-dark-blue
                                :height ,ajj-dark-height-plus-3))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :foreground ,ajj-dark-256-blue
                                     :height ,ajj-dark-height-plus-3))))

   `(custom-group-tag-1
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :foreground ,ajj-dark-red
                                :height ,ajj-dark-height-plus-3))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :foreground ,ajj-dark-256-red
                                     :height ,ajj-dark-height-plus-3))))

   `(custom-state
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   ;; diff
   `(diff-added
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :background ,ajj-dark-256-background))))

   `(diff-changed
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :background ,ajj-dark-256-background))))

   `(diff-removed
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :background ,ajj-dark-256-background))))

   `(diff-header
     ((,ajj-dark-class (:background ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background))))

   `(diff-file-header
     ((,ajj-dark-class (:background ,ajj-dark-background
                                   :foreground ,ajj-dark-foreground
                                   :weight bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background
                                        :foreground ,ajj-dark-256-foreground
                                        :weight bold))))

   `(diff-refine-added
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-green))))

   `(diff-refine-change
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-blue))))

   `(diff-refine-removed
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-red))))

   ;; diff-hl
   `(diff-hl-change
     ((,ajj-dark-class (:background ,ajj-dark-blue-lc
                                   :foreground ,ajj-dark-blue-hc))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue-lc
                                        :foreground ,ajj-dark-256-blue-hc))))

   `(diff-hl-delete
     ((,ajj-dark-class (:background ,ajj-dark-red-lc
                                   :foreground ,ajj-dark-red-hc))
      (,ajj-dark-256-class (:background ,ajj-dark-256-red-lc
                                        :foreground ,ajj-dark-256-red-hc))))

   `(diff-hl-insert
     ((,ajj-dark-class (:background ,ajj-dark-green-lc
                                   :foreground ,ajj-dark-green-hc))
      (,ajj-dark-256-class (:background ,ajj-dark-256-green-lc
                                        :foreground ,ajj-dark-256-green-hc))))

   `(diff-hl-unknown
     ((,ajj-dark-class (:background ,ajj-dark-violet-lc
                                   :foreground ,ajj-dark-violet-hc))
      (,ajj-dark-256-class (:background ,ajj-dark-256-violet-lc
                                        :foreground ,ajj-dark-256-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,ajj-dark-class (:background ,ajj-dark-diff-red-emphasis))
      (,ajj-dark-256-class (:background ,ajj-dark-256-diff-red-emphasis))))

   `(ediff-fine-diff-B
     ((,ajj-dark-class (:background ,ajj-dark-diff-green-emphasis))
      (,ajj-dark-256-class (:background ,ajj-dark-256-diff-green-emphasis))))

   `(ediff-fine-diff-C
     ((,ajj-dark-class (:background ,ajj-dark-diff-blue-emphasis))
      (,ajj-dark-256-class (:background ,ajj-dark-256-diff-blue-emphasis))))

   `(ediff-current-diff-A
     ((,ajj-dark-class (:background ,ajj-dark-diff-red-base))
      (,ajj-dark-256-class (:background ,ajj-dark-256-diff-red-base))))

   `(ediff-current-diff-B
     ((,ajj-dark-class (:background ,ajj-dark-diff-green-base))
      (,ajj-dark-256-class (:background ,ajj-dark-256-diff-green-base))))

   `(ediff-current-diff-C
     ((,ajj-dark-class (:background ,ajj-dark-diff-blue-base))
      (,ajj-dark-256-class (:background ,ajj-dark-256-diff-blue-base))))

   `(ediff-even-diff-A
     ((,ajj-dark-class (:background ,ajj-dark-comments
                                   :foreground ,ajj-dark-foreground-lc ))
      (,ajj-dark-256-class (:background ,ajj-dark-256-comments
                                        :foreground ,ajj-dark-256-foreground-lc ))))

   `(ediff-odd-diff-A
     ((,ajj-dark-class (:background ,ajj-dark-comments
                                   :foreground ,ajj-dark-foreground-hc ))
      (,ajj-dark-256-class (:background ,ajj-dark-256-comments
                                        :foreground ,ajj-dark-256-foreground-hc ))))

   `(ediff-even-diff-B
     ((,ajj-dark-class (:background ,ajj-dark-comments
                                   :foreground ,ajj-dark-foreground-hc ))
      (,ajj-dark-256-class (:background ,ajj-dark-256-comments
                                        :foreground ,ajj-dark-256-foreground-hc ))))

   `(ediff-odd-diff-B
     ((,ajj-dark-class (:background ,ajj-dark-comments
                                   :foreground ,ajj-dark-foreground-lc ))
      (,ajj-dark-256-class (:background ,ajj-dark-256-comments
                                        :foreground ,ajj-dark-256-foreground-lc ))))

   `(ediff-even-diff-C
     ((,ajj-dark-class (:background ,ajj-dark-comments
                                   :foreground ,ajj-dark-foreground ))
      (,ajj-dark-256-class (:background ,ajj-dark-256-comments
                                        :foreground ,ajj-dark-256-foreground ))))

   `(ediff-odd-diff-C
     ((,ajj-dark-class (:background ,ajj-dark-comments
                                   :foreground ,ajj-dark-background ))
      (,ajj-dark-256-class (:background ,ajj-dark-256-comments
                                        :foreground ,ajj-dark-256-background ))))

   ;; edts
   `(edts-face-error-line
     ((,(append '((supports :underline (:style line))) ajj-dark-class)
       (:underline (:style line :color ,ajj-dark-red)
                   :inherit unspecified))
      (,ajj-dark-class (:foreground ,ajj-dark-red-hc
                                   :background ,ajj-dark-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) ajj-dark-256-class )
       (:underline (:style line :color ,ajj-dark-256-red)
                   :inherit unspecified))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red-hc
                                        :background ,ajj-dark-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-warning-line
     ((,(append '((supports :underline (:style line))) ajj-dark-class)
       (:underline (:style line :color ,ajj-dark-yellow)
                   :inherit unspecified))
      (,ajj-dark-class (:foreground ,ajj-dark-yellow-hc
                                   :background ,ajj-dark-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) ajj-dark-256-class )
       (:underline (:style line :color ,ajj-dark-256-yellow)
                   :inherit unspecified))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow-hc
                                        :background ,ajj-dark-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-error-fringe-bitmap
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :background unspecified
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-warning-fringe-bitmap
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :background unspecified
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-error-mode-line
     ((,ajj-dark-class (:background ,ajj-dark-red
                                   :foreground unspecified))
      (,ajj-dark-256-class (:background ,ajj-dark-256-red
                                        :foreground unspecified))))

   `(edts-face-warning-mode-line
     ((,ajj-dark-class (:background ,ajj-dark-yellow
                                   :foreground unspecified))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow
                                        :foreground unspecified))))


   ;; elfeed
   `(elfeed-search-date-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(elfeed-search-feed-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(elfeed-search-tag-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(elfeed-search-title-face
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan))))

   ;; elixir
   `(elixir-attribute-face
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(elixir-atom-face
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   ;; ein
   `(ein:cell-input-area
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line))))
   `(ein:cell-input-prompt
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))
   `(ein:cell-output-prompt
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))
   `(ein:notification-tab-normal
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))
   `(ein:notification-tab-selected
     ((,ajj-dark-class (:foreground ,ajj-dark-orange :inherit bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange :inherit bold))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,ajj-dark-class (:inherit font-lock-string-face))
      (,ajj-dark-256-class (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,ajj-dark-class (:inherit font-lock-string-face))
      (,ajj-dark-256-class (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,ajj-dark-class (:inherit font-lock-string-face))
      (,ajj-dark-256-class (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,ajj-dark-class (:inherit font-lock-keyword-face))
      (,ajj-dark-256-class (:inherit font-lock-keyword-face))))

   ;; erm-syn
   `(erm-syn-errline
     ((,(append '((supports :underline (:style wave))) ajj-dark-class)
       (:underline (:style wave :color ,ajj-dark-red)
                   :inherit unspecified))
      (,ajj-dark-class (:foreground ,ajj-dark-red-hc
                                   :background ,ajj-dark-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) ajj-dark-256-class )
       (:underline (:style wave :color ,ajj-dark-256-red)
                   :inherit unspecified))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red-hc
                                        :background ,ajj-dark-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(erm-syn-warnline
     ((,(append '((supports :underline (:style wave))) ajj-dark-class)
       (:underline (:style wave :color ,ajj-dark-orange)
                   :inherit unspecified))
      (,ajj-dark-class (:foreground ,ajj-dark-orange-hc
                                   :background ,ajj-dark-orange-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) ajj-dark-256-class )
       (:underline (:style wave :color ,ajj-dark-256-orange)
                   :inherit unspecified))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange-hc
                                        :background ,ajj-dark-256-orange-lc
                                        :weight bold
                                        :underline t))))

   ;; epc
   `(epc:face-title
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :background ,ajj-dark-background
                                   :weight normal
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :background ,ajj-dark-256-background
                                        :weight normal
                                        :underline nil))))

   ;; erc
   `(erc-action-face
     ((,ajj-dark-class (:inherit erc-default-face))
      (,ajj-dark-256-class (:inherit erc-default-face))))

   `(erc-bold-face
     ((,ajj-dark-class (:weight bold))
      (,ajj-dark-256-class (:weight bold))))

   `(erc-current-nick-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :weight bold))))

   `(erc-dangerous-host-face
     ((,ajj-dark-class (:inherit font-lock-warning-face))
      (,ajj-dark-256-class (:inherit font-lock-warning-face))))

   `(erc-default-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(erc-highlight-face
     ((,ajj-dark-class (:inherit erc-default-face
                                :background ,ajj-dark-highlight))
      (,ajj-dark-256-class (:inherit erc-default-face
                                     :background ,ajj-dark-256-highlight))))

   `(erc-direct-msg-face
     ((,ajj-dark-class (:inherit erc-default-face))
      (,ajj-dark-256-class (:inherit erc-default-face))))

   `(erc-error-face
     ((,ajj-dark-class (:inherit font-lock-warning-face))
      (,ajj-dark-256-class (:inherit font-lock-warning-face))))

   `(erc-fool-face
     ((,ajj-dark-class (:inherit erc-default-face))
      (,ajj-dark-256-class (:inherit erc-default-face))))

   `(erc-input-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(erc-keyword-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :weight bold))))

   `(erc-nick-default-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight bold))))

   `(erc-my-nick-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :weight bold))))

   `(erc-nick-msg-face
     ((,ajj-dark-class (:inherit erc-default-face))
      (,ajj-dark-256-class (:inherit erc-default-face))))

   `(erc-notice-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(erc-pal-face
     ((,ajj-dark-class (:foreground ,ajj-dark-orange
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange
                                        :weight bold))))

   `(erc-prompt-face
     ((,ajj-dark-class (:foreground ,ajj-dark-orange
                                   :background ,ajj-dark-background
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange
                                        :background ,ajj-dark-256-background
                                        :weight bold))))

   `(erc-timestamp-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(erc-underline-face
     ((t (:underline t))))

   ;; eshell
   `(eshell-prompt
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :inherit bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :inherit bold))))

   `(eshell-ls-archive
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :inherit bold))))

   `(eshell-ls-backup
     ((,ajj-dark-class (:inherit font-lock-comment-face))
      (,ajj-dark-256-class (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,ajj-dark-class (:inherit font-lock-comment-face))
      (,ajj-dark-256-class (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :inherit bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :inherit bold))))

   `(eshell-ls-executable
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :inherit bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :inherit bold))))

   `(eshell-ls-unreadable
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(eshell-ls-missing
     ((,ajj-dark-class (:inherit font-lock-warning-face))
      (,ajj-dark-256-class (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,ajj-dark-class (:inherit font-lock-doc-face))
      (,ajj-dark-256-class (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :inherit bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :inherit bold))))

   `(eshell-ls-symlink
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan
                                   :inherit bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan
                                        :inherit bold))))

   ;; evil-ex-substitute
   `(evil-ex-substitute-matches
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-red-l
                                   :inherit italic))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-red-l
                                        :inherit italic))))
   `(evil-ex-substitute-replacement
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-green-l
                                   :inherit italic))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line :foreground ,ajj-dark-256-green-l :inherit italic))))

   ;; evil-search-highlight-persist
   `(evil-search-highlight-persist-highlight-face
     ((,ajj-dark-class (:inherit region))
      (,ajj-dark-256-class (:inherit region))))

   ;; fic
   `(fic-author-face
     ((,ajj-dark-class (:background ,ajj-dark-background
                                   :foreground ,ajj-dark-orange
                                   :underline t
                                   :slant italic))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background
                                        :foreground ,ajj-dark-256-orange
                                        :underline t
                                        :slant italic))))

   `(fic-face
     ((,ajj-dark-class (:background ,ajj-dark-background
                                   :foreground ,ajj-dark-orange
                                   :weight normal
                                   :slant italic))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background
                                        :foreground ,ajj-dark-256-orange
                                        :weight normal
                                        :slant italic))))

   `(font-lock-fic-face
     ((,ajj-dark-class (:background ,ajj-dark-background
                                   :foreground ,ajj-dark-orange
                                   :weight normal
                                   :slant italic))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background
                                        :foreground ,ajj-dark-256-orange
                                        :weight normal
                                        :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :weight normal
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) ajj-dark-class)
       (:underline (:style wave :color ,ajj-dark-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,ajj-dark-class (:foreground ,ajj-dark-red-hc
                                   :background ,ajj-dark-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) ajj-dark-256-class )
       (:underline (:style wave :color ,ajj-dark-256-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red-hc
                                        :background ,ajj-dark-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) ajj-dark-class)
       (:underline (:style wave :color ,ajj-dark-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,ajj-dark-class (:foreground ,ajj-dark-green-hc
                                   :background ,ajj-dark-green-lc))
      (,(append '((supports :underline (:style wave))) ajj-dark-256-class )
       (:underline (:style wave :color ,ajj-dark-256-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green-hc
                                        :background ,ajj-dark-256-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) ajj-dark-class)
       (:underline (:style wave :color ,ajj-dark-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,ajj-dark-class (:foreground ,ajj-dark-yellow-hc
                                   :background ,ajj-dark-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) ajj-dark-256-class )
       (:underline (:style wave :color ,ajj-dark-256-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow-hc
                                        :background ,ajj-dark-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style line))) ajj-dark-class)
       (:underline (:style line :color ,ajj-dark-red)))
      (,ajj-dark-class (:foreground ,ajj-dark-red
                                   :background ,ajj-dark-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) ajj-dark-256-class )
       (:underline (:style line :color ,ajj-dark-256-red)))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :background ,ajj-dark-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style line))) ajj-dark-class)
       (:underline (:style line :color ,ajj-dark-orange)))
      (,ajj-dark-class (:foreground ,ajj-dark-orange
                                   :background ,ajj-dark-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) ajj-dark-256-class )
       (:underline (:style line :color ,ajj-dark-256-orange)))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange
                                        :background ,ajj-dark-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style line))) ajj-dark-class)
       (:underline (:style line :color ,ajj-dark-blue)))
      (,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :background ,ajj-dark-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) ajj-dark-256-class )
       (:underline (:style line :color ,ajj-dark-256-blue)))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :background ,ajj-dark-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-fringe-error
     ((,ajj-dark-class (:foreground ,ajj-dark-red-l
                                   :background unspecified
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red-l
                                        :background unspecified
                                        :weight bold))))

   `(flycheck-fringe-warning
     ((,ajj-dark-class (:foreground ,ajj-dark-orange-l
                                   :background unspecified
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange-l
                                        :background unspecified
                                        :weight bold))))

   `(flycheck-fringe-info
     ((,ajj-dark-class (:foreground ,ajj-dark-blue-l
                                   :background unspecified
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue-l
                                        :background unspecified
                                        :weight bold))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) ajj-dark-class)
       (:underline (:style wave :color ,ajj-dark-yellow)
                   :inherit unspecified))
      (,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) ajj-dark-256-class )
       (:underline (:style wave :color ,ajj-dark-256-yellow)
                   :inherit unspecified))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight bold
                                        :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) ajj-dark-class)
       (:underline (:style wave :color ,ajj-dark-red)
                   :inherit unspecified))
      (,ajj-dark-class (:foreground ,ajj-dark-red
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) ajj-dark-256-class )
       (:underline (:style wave :color ,ajj-dark-256-red)
                   :inherit unspecified))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :weight bold
                                        :underline t))))


   ;; git-gutter
   `(git-gutter:added
     ((,ajj-dark-class (:background ,ajj-dark-green
                                   :foreground ,ajj-dark-background
                                   :inherit bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-green
                                        :foreground ,ajj-dark-256-background
                                        :inherit bold))))

   `(git-gutter:deleted
     ((,ajj-dark-class (:background ,ajj-dark-red
                                   :foreground ,ajj-dark-background
                                   :inherit bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-red
                                        :foreground ,ajj-dark-256-background
                                        :inherit bold))))

   `(git-gutter:modified
     ((,ajj-dark-class (:background ,ajj-dark-blue
                                   :foreground ,ajj-dark-background
                                   :inherit bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue
                                        :foreground ,ajj-dark-256-background
                                        :inherit bold))))

   `(git-gutter:unchanged
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-background
                                   :inherit bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-background
                                        :inherit bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :inherit bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :inherit bold))))

   `(git-gutter-fr:deleted
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :inherit bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :inherit bold))))

   `(git-gutter-fr:modified
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :inherit bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :inherit bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,ajj-dark-class (:background ,ajj-dark-green
                                   :foreground ,ajj-dark-background
                                   :inherit bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-green
                                        :foreground ,ajj-dark-256-background
                                        :inherit bold))))

   `(git-gutter+-deleted
     ((,ajj-dark-class (:background ,ajj-dark-red
                                   :foreground ,ajj-dark-background
                                   :inherit bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-red
                                        :foreground ,ajj-dark-256-background
                                        :inherit bold))))

   `(git-gutter+-modified
     ((,ajj-dark-class (:background ,ajj-dark-blue
                                   :foreground ,ajj-dark-background
                                   :inherit bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue
                                        :foreground ,ajj-dark-256-background
                                        :inherit bold))))

   `(git-gutter+-unchanged
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-background
                                   :inherit bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-background
                                        :inherit bold))))

   `(git-gutter-fr+-added
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :weight bold))))

   `(git-gutter-fr+-deleted
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :weight bold))))

   `(git-gutter-fr+-modified
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :weight bold))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :background ,ajj-dark-highlight-line
                                   :inherit bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-blue
                                        :background ,ajj-dark-256-highlight-line
                                        :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(guide-key/key-face
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(guide-key/prefix-command-face
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   ;; gnus
   `(gnus-group-mail-1
     ((,ajj-dark-class (:weight bold
                               :inherit gnus-group-mail-1-empty))
      (,ajj-dark-256-class (:weight bold
                                    :inherit gnus-group-mail-1-empty))))

   `(gnus-group-mail-1-empty
     ((,ajj-dark-class (:inherit gnus-group-news-1-empty))
      (,ajj-dark-256-class (:inherit gnus-group-news-1-empty))))

   `(gnus-group-mail-2
     ((,ajj-dark-class (:weight bold
                               :inherit gnus-group-mail-2-empty))
      (,ajj-dark-256-class (:weight bold
                                    :inherit gnus-group-mail-2-empty))))

   `(gnus-group-mail-2-empty
     ((,ajj-dark-class (:inherit gnus-group-news-2-empty))
      (,ajj-dark-256-class (:inherit gnus-group-news-2-empty))))

   `(gnus-group-mail-3
     ((,ajj-dark-class (:weight bold
                               :inherit gnus-group-mail-3-empty))
      (,ajj-dark-256-class (:weight bold
                                    :inherit gnus-group-mail-3-empty))))

   `(gnus-group-mail-3-empty
     ((,ajj-dark-class (:inherit gnus-group-news-3-empty))
      (,ajj-dark-256-class (:inherit gnus-group-news-3-empty))))

   `(gnus-group-mail-low
     ((,ajj-dark-class (:weight bold
                               :inherit gnus-group-mail-low-empty))
      (,ajj-dark-256-class (:weight bold
                                    :inherit gnus-group-mail-low-empty))))

   `(gnus-group-mail-low-empty
     ((,ajj-dark-class (:inherit gnus-group-news-low-empty))
      (,ajj-dark-256-class (:inherit gnus-group-news-low-empty))))

   `(gnus-group-news-1
     ((,ajj-dark-class (:weight bold
                               :inherit gnus-group-news-1-empty))
      (,ajj-dark-256-class (:weight bold
                                    :inherit gnus-group-news-1-empty))))

   `(gnus-group-news-2
     ((,ajj-dark-class (:weight bold
                               :inherit gnus-group-news-2-empty))
      (,ajj-dark-256-class (:weight bold
                                    :inherit gnus-group-news-2-empty))))

   `(gnus-group-news-3
     ((,ajj-dark-class (:weight bold
                               :inherit gnus-group-news-3-empty))
      (,ajj-dark-256-class (:weight bold
                                    :inherit gnus-group-news-3-empty))))

   `(gnus-group-news-4
     ((,ajj-dark-class (:weight bold
                               :inherit gnus-group-news-4-empty))
      (,ajj-dark-256-class (:weight bold
                                    :inherit gnus-group-news-4-empty))))

   `(gnus-group-news-5
     ((,ajj-dark-class (:weight bold
                               :inherit gnus-group-news-5-empty))
      (,ajj-dark-256-class (:weight bold
                                    :inherit gnus-group-news-5-empty))))

   `(gnus-group-news-6
     ((,ajj-dark-class (:weight bold
                               :inherit gnus-group-news-6-empty))
      (,ajj-dark-256-class (:weight bold
                                    :inherit gnus-group-news-6-empty))))

   `(gnus-group-news-low
     ((,ajj-dark-class (:weight bold
                               :inherit gnus-group-news-low-empty))
      (,ajj-dark-256-class (:weight bold
                                    :inherit gnus-group-news-low-empty))))

   `(gnus-header-content
     ((,ajj-dark-class (:inherit message-header-other))
      (,ajj-dark-256-class (:inherit message-header-other))))

   `(gnus-header-from
     ((,ajj-dark-class (:inherit message-header-other))
      (,ajj-dark-256-class (:inherit message-header-other))))

   `(gnus-header-name
     ((,ajj-dark-class (:inherit message-header-name))
      (,ajj-dark-256-class (:inherit message-header-name))))

   `(gnus-header-newsgroups
     ((,ajj-dark-class (:inherit message-header-other))
      (,ajj-dark-256-class (:inherit message-header-other))))

   `(gnus-header-subject
     ((,ajj-dark-class (:inherit message-header-subject))
      (,ajj-dark-256-class (:inherit message-header-subject))))

   `(gnus-summary-cancelled
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(gnus-summary-high-ancient
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :weight bold))))

   `(gnus-summary-high-read
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :weight bold))))

   `(gnus-summary-high-ticked
     ((,ajj-dark-class (:foreground ,ajj-dark-orange
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange
                                        :weight bold))))

   `(gnus-summary-high-unread
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :weight bold))))

   `(gnus-summary-low-ancient
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(gnus-summary-low-read
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(gnus-summary-low-ticked
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(gnus-summary-low-unread
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(gnus-summary-normal-ancient
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(gnus-summary-normal-read
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(gnus-summary-normal-ticked
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(gnus-summary-normal-unread
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(gnus-summary-selected
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight bold))))

   `(gnus-cite-1
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(gnus-cite-2
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(gnus-cite-3
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(gnus-cite-4
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(gnus-cite-5
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(gnus-cite-6
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(gnus-cite-7
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(gnus-cite-8
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(gnus-cite-9
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(gnus-cite-10
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(gnus-cite-11
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(gnus-group-news-1-empty
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(gnus-group-news-2-empty
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(gnus-group-news-3-empty
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(gnus-group-news-4-empty
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(gnus-group-news-5-empty
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(gnus-group-news-6-empty
     ((,ajj-dark-class (:foreground ,ajj-dark-blue-lc))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue-lc))))

   `(gnus-group-news-low-empty
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(gnus-signature
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(gnus-x-face
     ((,ajj-dark-class (:background ,ajj-dark-foreground
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-foreground
                                        :foreground ,ajj-dark-256-background))))


   ;; helm
   `(helm-apt-deinstalled
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(helm-apt-installed
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(helm-bookmark-directory
     ((,ajj-dark-class (:inherit helm-ff-directory))
      (,ajj-dark-256-class (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(helm-bookmark-gnus
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan))))

   `(helm-bookmark-info
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(helm-bookmark-man
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   `(helm-bookmark-w3m
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(helm-bookmarks-su
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(helm-buffer-file
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(helm-buffer-directory
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(helm-buffer-process
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(helm-buffer-saved-out
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :background ,ajj-dark-background
                                   :inverse-video t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :background ,ajj-dark-256-background
                                        :inverse-video t))))

   `(helm-buffer-size
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(helm-candidate-number
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-emphasis
                                   :bold t))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-emphasis
                                        :bold t))))

   `(helm-ff-directory
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(helm-ff-executable
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(helm-ff-file
     ((,ajj-dark-class (:background ,ajj-dark-background
                                   :foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background
                                        :foreground ,ajj-dark-256-foreground))))

   `(helm-ff-invalid-symlink
     ((,ajj-dark-class (:background ,ajj-dark-background
                                   :foreground ,ajj-dark-orange
                                   :slant italic))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background
                                        :foreground ,ajj-dark-256-orange
                                        :slant italic))))

   `(helm-ff-prefix
     ((,ajj-dark-class (:background ,ajj-dark-green
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-green
                                        :foreground ,ajj-dark-256-background))))

   `(helm-ff-symlink
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan))))

   `(helm-grep-file
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan
                                        :underline t))))

   `(helm-grep-finish
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(helm-grep-lineno
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(helm-grep-match
     ((,ajj-dark-class (:inherit helm-match)))
     ((,ajj-dark-256-class (:inherit helm-match))))

   `(helm-grep-running
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(helm-header
     ((,ajj-dark-class (:inherit header-line))
      (,ajj-dark-256-class (:inherit terminal-header-line))))

   `(helm-lisp-completion-info
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(helm-lisp-show-completion
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :background ,ajj-dark-highlight-line
                                   :bold t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :background ,ajj-dark-256-highlight-line
                                        :bold t))))

   `(helm-M-x-key
     ((,ajj-dark-class (:foreground ,ajj-dark-orange
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange
                                        :underline t))))

   `(helm-moccur-buffer
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan
                                        :underline t))))

   `(helm-match
     ((,ajj-dark-class (:foreground ,ajj-dark-green :inherit bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green :inherit bold))))

   `(helm-match-item
     ((,ajj-dark-class (:inherit helm-match))
      (,ajj-dark-256-class (:inherit helm-match))))

   `(helm-selection
     ((,ajj-dark-class (:background ,ajj-dark-highlight
                                   :inherit bold
                                   :underline nil))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight
                                        :inherit bold
                                        :underline nil))))

   `(helm-selection-line
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-emphasis
                                   :underline nil))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-emphasis
                                        :underline nil))))

   `(helm-separator
     ((,ajj-dark-class (:foreground ,ajj-dark-gray))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-gray))))

   `(helm-source-header
     ((,ajj-dark-class (:background ,ajj-dark-violet-l
                                   :foreground ,ajj-dark-background
                                   :underline nil))
      (,ajj-dark-256-class (:background ,ajj-dark-256-violet-l
                                        :foreground ,ajj-dark-256-background
                                        :underline nil))))

   `(helm-swoop-target-line-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line))))

   `(helm-swoop-target-line-block-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line))))

   `(helm-swoop-target-word-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(helm-time-zone-current
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(helm-time-zone-home
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(helm-visible-mark
     ((,ajj-dark-class (:background ,ajj-dark-background
                                   :foreground ,ajj-dark-magenta :bold t))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background
                                        :foreground ,ajj-dark-256-magenta :bold t))))

   ;; helm-ls-git
   `(helm-ls-git-modified-not-staged-face
     ((,ajj-dark-class :foreground ,ajj-dark-blue)
      (,ajj-dark-256-class  :foreground ,ajj-dark-256-blue)))

   `(helm-ls-git-modified-and-staged-face
     ((,ajj-dark-class :foreground ,ajj-dark-blue-l)
      (,ajj-dark-256-class  :foreground ,ajj-dark-256-blue-l)))

   `(helm-ls-git-renamed-modified-face
     ((,ajj-dark-class :foreground ,ajj-dark-blue-l)
      (,ajj-dark-256-class  :foreground ,ajj-dark-256-blue-l)))

   `(helm-ls-git-untracked-face
     ((,ajj-dark-class :foreground ,ajj-dark-orange)
      (,ajj-dark-256-class  :foreground ,ajj-dark-256-orange)))

   `(helm-ls-git-added-copied-face
     ((,ajj-dark-class :foreground ,ajj-dark-green)
      (,ajj-dark-256-class  :foreground ,ajj-dark-256-green)))

   `(helm-ls-git-added-modified-face
     ((,ajj-dark-class :foreground ,ajj-dark-green-l)
      (,ajj-dark-256-class  :foreground ,ajj-dark-256-green-l)))

   `(helm-ls-git-deleted-not-staged-face
     ((,ajj-dark-class :foreground ,ajj-dark-red)
      (,ajj-dark-256-class  :foreground ,ajj-dark-256-red)))

   `(helm-ls-git-deleted-and-staged-face
     ((,ajj-dark-class :foreground ,ajj-dark-red-l)
      (,ajj-dark-256-class  :foreground ,ajj-dark-256-red-l)))

   `(helm-ls-git-conflict-face
     ((,ajj-dark-class :foreground ,ajj-dark-yellow)
      (,ajj-dark-256-class  :foreground ,ajj-dark-256-yellow)))

   ;; hi-lock-mode
   `(hi-yellow
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow-lc
                                   :background ,ajj-dark-yellow-hc))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow-lc
                                        :background ,ajj-dark-256-yellow-hc))))

   `(hi-pink
     ((,ajj-dark-class (:foreground ,ajj-dark-magenta-lc
                                   :background ,ajj-dark-magenta-hc))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-magenta-lc
                                        :background ,ajj-dark-256-magenta-hc))))

   `(hi-green
     ((,ajj-dark-class (:foreground ,ajj-dark-green-lc
                                   :background ,ajj-dark-green-hc))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green-lc
                                        :background ,ajj-dark-256-green-hc))))

   `(hi-blue
     ((,ajj-dark-class (:foreground ,ajj-dark-blue-lc
                                   :background ,ajj-dark-blue-hc))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue-lc
                                        :background ,ajj-dark-256-blue-hc))))

   `(hi-black-b
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :background ,ajj-dark-background
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :background ,ajj-dark-256-background
                                        :weight bold))))

   `(hi-blue-b
     ((,ajj-dark-class (:foreground ,ajj-dark-blue-lc
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue-lc
                                        :weight bold))))

   `(hi-green-b
     ((,ajj-dark-class (:foreground ,ajj-dark-green-lc
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green-lc
                                        :weight bold))))

   `(hi-red-b
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :weight bold))))

   `(hi-black-hb
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :background ,ajj-dark-background
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :background ,ajj-dark-256-background
                                        :weight bold))))

   ;; highlight-changes
   `(highlight-changes
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(highlight-changes-delete
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,ajj-dark-class (:background ,ajj-dark-gray))
      (,ajj-dark-256-class (:background ,ajj-dark-256-gray))))

   `(highlight-indentation-current-column-face
     ((,ajj-dark-class (:background ,ajj-dark-gray))
      (,ajj-dark-256-class (:background ,ajj-dark-256-gray))))

   ;; highlight-symbol
   `(highlight-symbol-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight))))

   ;; hl-line-mode
   `(hl-line
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line))))

   `(hl-line-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line))))

   ;; ido-mode
   `(ido-first-match
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight normal))))

   `(ido-only-match
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-yellow
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-yellow
                                        :weight normal))))

   `(ido-subdir
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(ido-incomplete-regexp
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :weight bold ))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :weight bold ))))

   `(ido-indicator
     ((,ajj-dark-class (:background ,ajj-dark-red
                                   :foreground ,ajj-dark-background
                                   :width condensed))
      (,ajj-dark-256-class (:background ,ajj-dark-256-red
                                        :foreground ,ajj-dark-256-background
                                        :width condensed))))

   `(ido-virtual
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan))))

   ;; info
   `(info-header-xref
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :inherit bold
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :inherit bold
                                        :underline t))))

   `(info-menu
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(info-node
     ((,ajj-dark-class (:foreground ,ajj-dark-violet
                                   :inherit bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet
                                        :inherit bold))))

   `(info-quoted-name
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(info-reference-item
     ((,ajj-dark-class (:background nil
                                   :underline t
                                   :inherit bold))
      (,ajj-dark-256-class (:background nil
                                        :underline t
                                        :inherit bold))))

   `(info-string
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(info-title-1
     ((,ajj-dark-class (:height ,ajj-dark-height-plus-4))
      (,ajj-dark-256-class (:height ,ajj-dark-height-plus-4))))

   `(info-title-2
     ((,ajj-dark-class (:height ,ajj-dark-height-plus-3))
      (,ajj-dark-256-class (:height ,ajj-dark-height-plus-3))))

   `(info-title-3
     ((,ajj-dark-class (:height ,ajj-dark-height-plus-2))
      (,ajj-dark-256-class (:height ,ajj-dark-height-plus-2))))

   `(info-title-4
     ((,ajj-dark-class (:height ,ajj-dark-height-plus-1))
      (,ajj-dark-256-class (:height ,ajj-dark-height-plus-1))))

   ;; ivy
   `(ivy-current-match
     ((,ajj-dark-class (:background ,ajj-dark-gray :inherit bold))
      (,ajj-dark-256-class (:background ,ajj-dark-gray-l :inherit bold))))

   `(ivy-minibuffer-match-face-1
     ((,ajj-dark-class (:inherit bold))
      (,ajj-dark-256-class (:inherit bold))))

   `(ivy-minibuffer-match-face-2
     ((,ajj-dark-class (:foreground ,ajj-dark-violet
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet
                                        :underline t))))

   `(ivy-minibuffer-match-face-3
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :underline t))))

   `(ivy-minibuffer-match-face-4
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :underline t))))

   `(ivy-remote
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(swiper-line-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))))

   `(swiper-match-face-1
     ((,ajj-dark-class (:background ,ajj-dark-gray-d))))

   `(swiper-match-face-2
     ((,ajj-dark-class (:background ,ajj-dark-green))))

   `(swiper-match-face-3
     ((,ajj-dark-class (:background ,ajj-dark-orange))))

   `(swiper-match-face-4
     ((,ajj-dark-class (:background ,ajj-dark-magenta))))

   ;; jabber
   `(jabber-activity-face
     ((,ajj-dark-class (:weight bold
                               :foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:weight bold
                                    :foreground ,ajj-dark-256-red))))

   `(jabber-activity-personal-face
     ((,ajj-dark-class (:weight bold
                               :foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:weight bold
                                    :foreground ,ajj-dark-256-blue))))

   `(jabber-chat-error
     ((,ajj-dark-class (:weight bold
                               :foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:weight bold
                                    :foreground ,ajj-dark-256-red))))

   `(jabber-chat-prompt-foreign
     ((,ajj-dark-class (:weight bold
                               :foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:weight bold
                                    :foreground ,ajj-dark-256-red))))

   `(jabber-chat-prompt-local
     ((,ajj-dark-class (:weight bold
                               :foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:weight bold
                                    :foreground ,ajj-dark-256-blue))))

   `(jabber-chat-prompt-system
     ((,ajj-dark-class (:weight bold
                               :foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:weight bold
                                    :foreground ,ajj-dark-256-green))))

   `(jabber-chat-text-foreign
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(jabber-chat-text-local
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(jabber-chat-rare-time-face
     ((,ajj-dark-class (:underline t
                                  :foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:underline t
                                       :foreground ,ajj-dark-256-green))))

   `(jabber-roster-user-away
     ((,ajj-dark-class (:slant italic
                              :foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:slant italic
                                   :foreground ,ajj-dark-256-green))))

   `(jabber-roster-user-chatty
     ((,ajj-dark-class (:weight bold
                               :foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:weight bold
                                    :foreground ,ajj-dark-256-orange))))

   `(jabber-roster-user-dnd
     ((,ajj-dark-class (:slant italic
                              :foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:slant italic
                                   :foreground ,ajj-dark-256-red))))

   `(jabber-roster-user-error
     ((,ajj-dark-class (:weight light
                               :slant italic
                               :foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:weight light
                                    :slant italic
                                    :foreground ,ajj-dark-256-red))))

   `(jabber-roster-user-offline
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(jabber-roster-user-online
     ((,ajj-dark-class (:weight bold
                               :foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:weight bold
                                    :foreground ,ajj-dark-256-blue))))

   `(jabber-roster-user-xa
     ((,ajj-dark-class (:slant italic
                              :foreground ,ajj-dark-magenta))
      (,ajj-dark-256-class (:slant italic
                                   :foreground ,ajj-dark-256-magenta))))

   ;; js2-mode colors
   `(js2-error
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(js2-external-variable
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(js2-function-call
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(js2-function-param
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(js2-instance-member
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   `(js2-jsdoc-html-tag-delimiter
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(js2-jsdoc-html-tag-name
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(js2-jsdoc-tag
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   `(js2-jsdoc-type
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(js2-jsdoc-value
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(js2-magic-paren
     ((,ajj-dark-class (:underline t))
      (,ajj-dark-256-class (:underline t))))

   `(js2-object-property
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(js2-private-function-call
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   `(js2-private-member
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(js2-warning
     ((,ajj-dark-class (:underline ,ajj-dark-orange))
      (,ajj-dark-256-class (:underline ,ajj-dark-256-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,ajj-dark-class (:inherit bold))
      (,ajj-dark-256-class (:inherit bold))))

   ;; linum-mode
   `(linum
     ((,ajj-dark-class (:foreground ,ajj-dark-line-number
                                   :background ,ajj-dark-fringe-bg
                                   :inherit default
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-line-number
                                        :background ,ajj-dark-256-fringe-bg
                                        :inherit default
                                        :underline nil))))

   ;; line-number (>= Emacs26)
   `(line-number
     ((,ajj-dark-class (:foreground ,ajj-dark-line-number
                                   :background ,ajj-dark-fringe-bg
                                   :inherit default
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-line-number
                                        :background ,ajj-dark-256-fringe-bg
                                        :inherit default
                                        :underline nil))))
   `(line-number-current-line
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :background ,ajj-dark-fringe-bg
                                   :inherit default
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :background ,ajj-dark-256-fringe-bg
                                        :inherit default
                                        :underline nil))))

   ;; linum-relative-current-face
   `(linum-relative-current-face
     ((,ajj-dark-class (:foreground ,ajj-dark-line-number
                                   :background ,ajj-dark-highlight-line
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-line-number
                                        :background ,ajj-dark-256-highlight-line
                                        :underline nil))))

   ;; lsp-mode
   `(lsp-ui-doc-header
     ((,ajj-dark-class (:inherit org-document-title))
      (,ajj-dark-256-class (:inherit org-document-title))))

   `(lsp-ui-doc-background
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:background ,ajj-dark-highlight-line))))

   ;; lusty-explorer
   `(lusty-directory-face
     ((,ajj-dark-class (:inherit diajj-dark-red-directory))
      (,ajj-dark-256-class (:inherit diajj-dark-red-directory))))

   `(lusty-file-face
     ((,ajj-dark-class nil)
      (,ajj-dark-256-class  nil)))

   `(lusty-match-face
     ((,ajj-dark-class (:inherit ido-first-match))
      (,ajj-dark-256-class (:inherit ido-first-match))))

   `(lusty-slash-face
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan
                                        :weight bold))))

   ;; magit
   ;;
   ;; TODO: Add supports for all magit faces
   ;; https://github.com/magit/magit/search?utf8=%E2%9C%93&q=face
   ;;
   `(magit-diff-added
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :background ,ajj-dark-256-background))))

   `(magit-diff-added-highlight
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :background ,ajj-dark-256-highlight-line))))

   `(magit-diff-removed
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :background ,ajj-dark-256-background))))

   `(magit-diff-removed-highlight
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :background ,ajj-dark-256-highlight-line))))

   `(magit-section-title
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight bold))))

   `(magit-branch
     ((,ajj-dark-class (:foreground ,ajj-dark-orange
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange
                                        :weight bold))))

   `(magit-item-highlight
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :weight unspecified))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :weight unspecified))))

   `(magit-log-author
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan))))

   `(magit-log-graph
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(magit-log-head-label-bisect-bad
     ((,ajj-dark-class (:background ,ajj-dark-red-hc
                                   :foreground ,ajj-dark-red-lc
                                   :box 1))
      (,ajj-dark-256-class (:background ,ajj-dark-256-red-hc
                                        :foreground ,ajj-dark-256-red-lc
                                        :box 1))))

   `(magit-log-head-label-bisect-good
     ((,ajj-dark-class (:background ,ajj-dark-green-hc
                                   :foreground ,ajj-dark-green-lc
                                   :box 1))
      (,ajj-dark-256-class (:background ,ajj-dark-256-green-hc
                                        :foreground ,ajj-dark-256-green-lc
                                        :box 1))))

   `(magit-log-head-label-default
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :box 1))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :box 1))))

   `(magit-log-head-label-local
     ((,ajj-dark-class (:background ,ajj-dark-blue-lc
                                   :foreground ,ajj-dark-blue-hc
                                   :box 1))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue-lc
                                        :foreground ,ajj-dark-256-blue-hc
                                        :box 1))))

   `(magit-log-head-label-patches
     ((,ajj-dark-class (:background ,ajj-dark-red-lc
                                   :foreground ,ajj-dark-red-hc
                                   :box 1))
      (,ajj-dark-256-class (:background ,ajj-dark-256-red-lc
                                        :foreground ,ajj-dark-256-red-hc
                                        :box 1))))

   `(magit-log-head-label-remote
     ((,ajj-dark-class (:background ,ajj-dark-green-lc
                                   :foreground ,ajj-dark-green-hc
                                   :box 1))
      (,ajj-dark-256-class (:background ,ajj-dark-256-green-lc
                                        :foreground ,ajj-dark-256-green-hc
                                        :box 1))))

   `(magit-log-head-label-tags
     ((,ajj-dark-class (:background ,ajj-dark-yellow-lc
                                   :foreground ,ajj-dark-yellow-hc
                                   :box 1))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow-lc
                                        :foreground ,ajj-dark-256-yellow-hc
                                        :box 1))))

   `(magit-log-sha1
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   ;; man
   `(Man-overstrike
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :weight bold))))

   `(Man-reverse
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(Man-underline
     ((,ajj-dark-class (:foreground ,ajj-dark-green :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green :underline t))))

   ;; monky
   `(monky-section-title
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight bold))))

   `(monky-diff-add
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(monky-diff-del
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(markdown-header-face-1
     ((,ajj-dark-class (:inherit markdown-header-face
                                :height ,ajj-dark-height-plus-4))
      (,ajj-dark-256-class (:inherit markdown-header-face
                                     :height ,ajj-dark-height-plus-4))))

   `(markdown-header-face-2
     ((,ajj-dark-class (:inherit markdown-header-face
                                :height ,ajj-dark-height-plus-3))
      (,ajj-dark-256-class (:inherit markdown-header-face
                                     :height ,ajj-dark-height-plus-3))))

   `(markdown-header-face-3
     ((,ajj-dark-class (:inherit markdown-header-face
                                :height ,ajj-dark-height-plus-2))
      (,ajj-dark-256-class (:inherit markdown-header-face
                                     :height ,ajj-dark-height-plus-2))))

   `(markdown-header-face-4
     ((,ajj-dark-class (:inherit markdown-header-face
                                :height ,ajj-dark-height-plus-1))
      (,ajj-dark-256-class (:inherit markdown-header-face
                                     :height ,ajj-dark-height-plus-1))))

   `(markdown-header-face-5
     ((,ajj-dark-class (:inherit markdown-header-face))
      (,ajj-dark-256-class (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,ajj-dark-class (:inherit markdown-header-face))
      (,ajj-dark-256-class (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(message-header-name
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(message-header-other
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :weight normal))))

   `(message-header-to
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :weight normal))))

   `(message-header-cc
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :weight normal))))

   `(message-header-newsgroups
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight bold))))

   `(message-header-subject
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan
                                        :weight normal))))

   `(message-header-xheader
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan))))

   `(message-mml
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight bold))))

   `(message-separator
     ((,ajj-dark-class (:foreground ,ajj-dark-comments
                                   :slant italic))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments
                                        :slant italic))))

   ;; mew
   `(mew-face-header-subject
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(mew-face-header-from
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(mew-face-header-date
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(mew-face-header-to
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(mew-face-header-key
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(mew-face-header-private
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(mew-face-header-important
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(mew-face-header-marginal
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :weight bold))))

   `(mew-face-header-warning
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(mew-face-header-xmew
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(mew-face-header-xmew-bad
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(mew-face-body-url
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(mew-face-body-comment
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :slant italic))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :slant italic))))

   `(mew-face-body-cite1
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(mew-face-body-cite2
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(mew-face-body-cite3
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(mew-face-body-cite4
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(mew-face-body-cite5
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(mew-face-mark-review
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(mew-face-mark-escape
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(mew-face-mark-delete
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(mew-face-mark-unlink
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(mew-face-mark-refile
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(mew-face-mark-unread
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(mew-face-eof-message
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(mew-face-eof-part
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   ;; mingus
   `(mingus-directory-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(mingus-pausing-face
     ((,ajj-dark-class (:foreground ,ajj-dark-magenta))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-magenta))))

   `(mingus-playing-face
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan))))

   `(mingus-playlist-face
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan ))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan ))))

   `(mingus-song-file-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(mingus-stopped-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   ;; mmm
   `(mmm-init-submode-face
     ((,ajj-dark-class (:background ,ajj-dark-violet-d))
      (,ajj-dark-256-class (:background ,ajj-dark-256-violet-d))))

   `(mmm-cleanup-submode-face
     ((,ajj-dark-class (:background ,ajj-dark-orange-d))
      (,ajj-dark-256-class (:background ,ajj-dark-256-orange-d))))

   `(mmm-declaration-submode-face
     ((,ajj-dark-class (:background ,ajj-dark-cyan-d))
      (,ajj-dark-256-class (:background ,ajj-dark-256-cyan-d))))

   `(mmm-comment-submode-face
     ((,ajj-dark-class (:background ,ajj-dark-blue-d))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue-d))))

   `(mmm-output-submode-face
     ((,ajj-dark-class (:background ,ajj-dark-red-d))
      (,ajj-dark-256-class (:background ,ajj-dark-256-red-d))))

   `(mmm-special-submode-face
     ((,ajj-dark-class (:background ,ajj-dark-green-d))
      (,ajj-dark-256-class (:background ,ajj-dark-256-green-d))))

   `(mmm-code-submode-face
     ((,ajj-dark-class (:background ,ajj-dark-gray))
      (,ajj-dark-256-class (:background ,ajj-dark-256-gray))))

   `(mmm-default-submode-face
     ((,ajj-dark-class (:background ,ajj-dark-gray-d))
      (,ajj-dark-256-class (:background ,ajj-dark-256-gray-d))))

   ;; moccur
   `(moccur-current-line-face
     ((,ajj-dark-class (:underline t))
      (,ajj-dark-256-class (:underline t))))

   `(moccur-edit-done-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments
                                   :background ,ajj-dark-background
                                   :slant italic))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments
                                        :background ,ajj-dark-256-background
                                        :slant italic))))

   `(moccur-edit-face
     ((,ajj-dark-class (:background ,ajj-dark-yellow
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow
                                        :foreground ,ajj-dark-256-background))))

   `(moccur-edit-file-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line))))

   `(moccur-edit-reject-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(moccur-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-emphasis
                                   :weight bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-emphasis
                                        :weight bold))))

   `(search-buffers-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-emphasis
                                   :weight bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-emphasis
                                        :weight bold))))

   `(search-buffers-header-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-yellow
                                   :weight bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-yellow
                                        :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :slant italic
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-2-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :slant italic
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-3-face
     ((,ajj-dark-class (:foreground ,ajj-dark-orange
                                   :slant italic
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-4-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :slant italic
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-5-face
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan
                                   :slant italic
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-6-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :slant italic
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-7-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :slant italic
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-flagged-face
     ((,ajj-dark-class (:foreground ,ajj-dark-magenta
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-magenta
                                        :weight bold))))

   `(mu4e-view-url-number-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight normal))))

   `(mu4e-warning-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :slant normal
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :slant normal
                                        :weight bold))))

   `(mu4e-header-highlight-face
     ((,ajj-dark-class (:inherit unspecified
                                :foreground unspecified
                                :background ,ajj-dark-highlight-line
                                :underline ,ajj-dark-emphasis
                                :weight normal))
      (,ajj-dark-256-class (:inherit unspecified
                                     :foreground unspecified
                                     :background ,ajj-dark-256-highlight-line
                                     :underline ,ajj-dark-256-emphasis
                                     :weight normal))))


   `(mu4e-draft-face
     ((,ajj-dark-class (:inherit font-lock-string-face))
      (,ajj-dark-256-class (:inherit font-lock-string-face))))

   `(mu4e-footer-face
     ((,ajj-dark-class (:inherit font-lock-comment-face))
      (,ajj-dark-256-class (:inherit font-lock-comment-face))))

   `(mu4e-forwarded-face
     ((,ajj-dark-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,ajj-dark-256-class (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-header-face
     ((,ajj-dark-class (:inherit default))
      (,ajj-dark-256-class (:inherit default))))

   `(mu4e-header-marks-face
     ((,ajj-dark-class (:inherit font-lock-preprocessor-face))
      (,ajj-dark-256-class (:inherit font-lock-preprocessor-face))))

   `(mu4e-header-title-face
     ((,ajj-dark-class (:inherit font-lock-type-face))
      (,ajj-dark-256-class (:inherit font-lock-type-face))))

   `(mu4e-highlight-face
     ((,ajj-dark-class (:inherit font-lock-pseudo-keyword-face
                                :weight bold))
      (,ajj-dark-256-class (:inherit font-lock-pseudo-keyword-face
                                     :weight bold))))

   `(mu4e-moved-face
     ((,ajj-dark-class (:inherit font-lock-comment-face
                                :slant italic))
      (,ajj-dark-256-class (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-ok-face
     ((,ajj-dark-class (:inherit font-lock-comment-face
                                :slant normal
                                :weight bold))
      (,ajj-dark-256-class (:inherit font-lock-comment-face
                                     :slant normal
                                     :weight bold))))

   `(mu4e-replied-face
     ((,ajj-dark-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,ajj-dark-256-class (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-system-face
     ((,ajj-dark-class (:inherit font-lock-comment-face
                                :slant italic))
      (,ajj-dark-256-class (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-title-face
     ((,ajj-dark-class (:inherit font-lock-type-face
                                :weight bold))
      (,ajj-dark-256-class (:inherit font-lock-type-face
                                     :weight bold))))

   `(mu4e-trashed-face
     ((,ajj-dark-class (:inherit font-lock-comment-face
                                :strike-through t))
      (,ajj-dark-256-class (:inherit font-lock-comment-face
                                     :strike-through t))))

   `(mu4e-unread-face
     ((,ajj-dark-class (:inherit font-lock-keyword-face
                                :weight bold))
      (,ajj-dark-256-class (:inherit font-lock-keyword-face
                                     :weight bold))))

   `(mu4e-view-attach-number-face
     ((,ajj-dark-class (:inherit font-lock-variable-name-face
                                :weight bold))
      (,ajj-dark-256-class (:inherit font-lock-variable-name-face
                                     :weight bold))))

   `(mu4e-view-contact-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :weight normal))))

   `(mu4e-view-header-key-face
     ((,ajj-dark-class (:inherit message-header-name
                                :weight normal))
      (,ajj-dark-256-class (:inherit message-header-name
                                     :weight normal))))

   `(mu4e-view-header-value-face
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan
                                   :weight normal
                                   :slant normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan
                                        :weight normal
                                        :slant normal))))

   `(mu4e-view-link-face
     ((,ajj-dark-class (:inherit link))
      (,ajj-dark-256-class (:inherit link))))

   `(mu4e-view-special-header-value-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :weight normal
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line))))

   ;; nav
   `(nav-face-heading
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(nav-face-button-num
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan))))

   `(nav-face-dir
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(nav-face-hdir
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(nav-face-file
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(nav-face-hfile
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line))))

   ;; neo-tree
   `(neo-banner-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :background ,ajj-dark-background
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :background ,ajj-dark-256-background
                                        :weight bold))))


   `(neo-header-face
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :background ,ajj-dark-256-background))))

   `(neo-root-dir-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :background ,ajj-dark-256-background))))

   `(neo-dir-link-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :background ,ajj-dark-256-background))))

   `(neo-file-link-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(neo-button-face
     ((,ajj-dark-class (:underline nil))
      (,ajj-dark-256-class (:underline nil))))

   `(neo-expand-btn-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(neo-vc-default-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(neo-vc-user-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :slant italic))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :slant italic))))

   `(neo-vc-up-to-date-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(neo-vc-edited-face
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(neo-vc-needs-update-face
     ((,ajj-dark-class (:underline t))
      (,ajj-dark-256-class (:underline t))))

   `(neo-vc-needs-merge-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(neo-vc-unlocked-changes-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :background ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :background ,ajj-dark-256-comments))))

   `(neo-vc-added-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(neo-vc-removed-face
     ((,ajj-dark-class (:strike-through t))
      (,ajj-dark-256-class (:strike-through t))))

   `(neo-vc-conflict-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(neo-vc-missing-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(neo-vc-ignored-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   ;; adoc-mode / markup
   `(markup-meta-face
     ((,ajj-dark-class (:foreground ,ajj-dark-gray-l))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-gray-l))))

   `(markup-table-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue-hc
                                   :background ,ajj-dark-blue-lc))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue-hc
                                        :background ,ajj-dark-256-blue-lc))))

   `(markup-verbatim-face
     ((,ajj-dark-class (:background ,ajj-dark-orange-lc))
      (,ajj-dark-256-class (:background ,ajj-dark-256-orange-lc))))

   `(markup-list-face
     ((,ajj-dark-class (:foreground ,ajj-dark-violet-hc
                                   :background ,ajj-dark-violet-lc))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet-hc
                                        :background ,ajj-dark-256-violet-lc))))

   `(markup-replacement-face
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   `(markup-complex-replacement-face
     ((,ajj-dark-class (:foreground ,ajj-dark-violet-hc
                                   :background ,ajj-dark-violet-lc))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet-hc
                                        :background ,ajj-dark-256-violet-lc))))

   `(markup-gen-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(markup-secondary-text-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   ;; org-mode
   `(org-agenda-structure
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :background ,ajj-dark-highlight-line
                                   :weight bold
                                   :slant normal
                                   :inverse-video nil
                                   :height ,ajj-dark-height-plus-1
                                   :underline nil
                                   :box (:line-width 2 :color ,ajj-dark-background)))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :background ,ajj-dark-256-highlight-line
                                        :weight bold
                                        :slant normal
                                        :inverse-video nil
                                        :height ,ajj-dark-height-plus-1
                                        :underline nil
                                        :box (:line-width 2 :color ,ajj-dark-256-background)))))

   `(org-agenda-calendar-event
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis))))

   `(org-agenda-calendar-sexp
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :slant italic))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :slant italic))))

   `(org-agenda-date
     ((,ajj-dark-class (:foreground ,ajj-dark-comments
                                   :background ,ajj-dark-background
                                   :weight normal
                                   :inverse-video nil
                                   :overline nil
                                   :slant normal
                                   :height 1.0
                                   :box (:line-width 2 :color ,ajj-dark-background)))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments
                                        :background ,ajj-dark-256-background
                                        :weight normal
                                        :inverse-video nil
                                        :overline nil
                                        :slant normal
                                        :height 1.0
                                        :box (:line-width 2 :color ,ajj-dark-256-background)))) t)

   `(org-agenda-date-weekend
     ((,ajj-dark-class (:inherit org-agenda-date
                                :inverse-video nil
                                :background unspecified
                                :foreground ,ajj-dark-comments
                                :weight unspecified
                                :underline t
                                :overline nil
                                :box unspecified))
      (,ajj-dark-256-class (:inherit org-agenda-date
                                     :inverse-video nil
                                     :background unspecified
                                     :foreground ,ajj-dark-256-comments
                                     :weight unspecified
                                     :underline t
                                     :overline nil
                                     :box unspecified))) t)

   `(org-agenda-date-today
     ((,ajj-dark-class (:inherit org-agenda-date
                                :inverse-video t
                                :weight bold
                                :underline unspecified
                                :overline nil
                                :box unspecified
                                :foreground ,ajj-dark-blue
                                :background ,ajj-dark-background))
      (,ajj-dark-256-class (:inherit org-agenda-date
                                     :inverse-video t
                                     :weight bold
                                     :underline unspecified
                                     :overline nil
                                     :box unspecified
                                     :foreground ,ajj-dark-256-blue
                                     :background ,ajj-dark-256-background))) t)

   `(org-agenda-done
     ((,ajj-dark-class (:foreground ,ajj-dark-comments
                                   :slant italic))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments
                                        :slant italic))) t)

   `(org-archived
     ((,ajj-dark-class (:foreground ,ajj-dark-comments
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments
                                        :weight normal))))

   `(org-block
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :background ,ajj-dark-highlight-alt))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :background ,ajj-dark-256-highlight-alt))))

   `(org-block-background
     ((,ajj-dark-class (:background ,ajj-dark-highlight-alt))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-alt))))

   `(org-block-begin-line
     ((,ajj-dark-class (:foreground ,ajj-dark-comments
                                   :background ,ajj-dark-gray-d
                                   :slant italic))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments
                                        :background ,ajj-dark-256-gray-d
                                        :slant italic))))

   `(org-block-end-line
     ((,ajj-dark-class (:foreground ,ajj-dark-comments
                                   :background ,ajj-dark-gray-d
                                   :slant italic))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments
                                        :background ,ajj-dark-256-gray-d
                                        :slant italic))))

   `(org-checkbox
     ((,ajj-dark-class (:background ,ajj-dark-background
                                   :foreground ,ajj-dark-foreground
                                   :box (:line-width 1 :style released-button)))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background
                                        :foreground ,ajj-dark-256-foreground
                                        :box (:line-width 1 :style released-button)))))

   `(org-code
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(org-date
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :underline t))))

   `(org-done
     ((,ajj-dark-class (:weight bold
                               :foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:weight bold
                                    :foreground ,ajj-dark-256-green))))

   `(org-ellipsis
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(org-formula
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(org-headline-done
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(org-hide
     ((,ajj-dark-class (:foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background))))

   `(org-level-1
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :height ,ajj-dark-height-plus-4
                                :foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :height ,ajj-dark-height-plus-4
                                     :foreground ,ajj-dark-256-orange))))

   `(org-level-2
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :height ,ajj-dark-height-plus-3
                                :foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :height ,ajj-dark-height-plus-3
                                     :foreground ,ajj-dark-256-green))))

   `(org-level-3
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :height ,ajj-dark-height-plus-2
                                :foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :height ,ajj-dark-height-plus-2
                                     :foreground ,ajj-dark-256-blue))))

   `(org-level-4
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :height ,ajj-dark-height-plus-1
                                :foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :height ,ajj-dark-height-plus-1
                                     :foreground ,ajj-dark-256-yellow))))

   `(org-level-5
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :foreground ,ajj-dark-cyan))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :foreground ,ajj-dark-256-cyan))))

   `(org-level-6
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :foreground ,ajj-dark-256-green))))

   `(org-level-7
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :foreground ,ajj-dark-256-red))))

   `(org-level-8
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :foreground ,ajj-dark-256-blue))))

   `(org-link
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :underline t))))

   `(org-sexp-date
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   `(org-scheduled
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(org-scheduled-previously
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan))))

   `(org-scheduled-today
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :weight normal))))

   `(org-special-keyword
     ((,ajj-dark-class (:foreground ,ajj-dark-comments
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments
                                        :weight bold))))

   `(org-table
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(org-tag
     ((,ajj-dark-class (:weight bold))
      (,ajj-dark-256-class (:weight bold))))

   `(org-time-grid
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(org-todo
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :weight bold)))
     ((,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :weight bold))))

   `(org-upcoming-deadline
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight normal
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(org-warning
     ((,ajj-dark-class (:foreground ,ajj-dark-orange
                                   :weight normal
                                   :underline nil))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange
                                        :weight normal
                                        :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,ajj-dark-class (:background ,ajj-dark-blue-lc
                                   :foreground ,ajj-dark-blue-hc))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue-lc
                                        :foreground ,ajj-dark-256-blue-hc))))

   `(org-habit-clear-future-face
     ((,ajj-dark-class (:background ,ajj-dark-blue-lc))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue-lc))))

   `(org-habit-ready-face
     ((,ajj-dark-class (:background ,ajj-dark-green-lc
                                   :foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:background ,ajj-dark-256-green-lc
                                        :foreground ,ajj-dark-256-green))))

   `(org-habit-ready-future-face
     ((,ajj-dark-class (:background ,ajj-dark-green-lc))
      (,ajj-dark-256-class (:background ,ajj-dark-256-green-lc))))

   `(org-habit-alert-face
     ((,ajj-dark-class (:background ,ajj-dark-yellow
                                   :foreground ,ajj-dark-yellow-lc))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow
                                        :foreground ,ajj-dark-256-yellow-lc))))

   `(org-habit-alert-future-face
     ((,ajj-dark-class (:background ,ajj-dark-yellow-lc))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow-lc))))

   `(org-habit-overdue-face
     ((,ajj-dark-class (:background ,ajj-dark-red
                                   :foreground ,ajj-dark-red-lc))
      (,ajj-dark-256-class (:background ,ajj-dark-256-red
                                        :foreground ,ajj-dark-256-red-lc))))

   `(org-habit-overdue-future-face
     ((,ajj-dark-class (:background ,ajj-dark-red-lc))
      (,ajj-dark-256-class (:background ,ajj-dark-256-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(org-agenda-restriction-lock
     ((,ajj-dark-class (:background ,ajj-dark-yellow))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow))))

   `(org-clock-overlay
     ((,ajj-dark-class (:background ,ajj-dark-yellow))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow))))

   `(org-column
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :strike-through nil
                                   :underline nil
                                   :slant normal
                                   :weight normal
                                   :inherit default))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :strike-through nil
                                        :underline nil
                                        :slant normal
                                        :weight normal
                                        :inherit default))))

   `(org-column-title
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :underline t
                                   :weight bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :underline t
                                        :weight bold))))

   `(org-date-selected
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :inverse-video t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :inverse-video t))))

   `(org-document-info
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(org-document-title
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :weight bold
                                   :height ,ajj-dark-height-plus-4))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :weight bold
                                        :height ,ajj-dark-height-plus-4))))

   `(org-drawer
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan))))

   `(org-footnote
     ((,ajj-dark-class (:foreground ,ajj-dark-magenta
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-magenta
                                        :underline t))))

   `(org-latex-and-export-specials
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(org-mode-line-clock-overrun
     ((,ajj-dark-class (:inherit mode-line))
      (,ajj-dark-256-class (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,ajj-dark-class (:inherit org-level-1))
      (,ajj-dark-256-class (:inherit org-level-1))))

   `(outline-2
     ((,ajj-dark-class (:inherit org-level-2))
      (,ajj-dark-256-class (:inherit org-level-2))))

   `(outline-3
     ((,ajj-dark-class (:inherit org-level-3))
      (,ajj-dark-256-class (:inherit org-level-3))))

   `(outline-4
     ((,ajj-dark-class (:inherit org-level-4))
      (,ajj-dark-256-class (:inherit org-level-4))))

   `(outline-5
     ((,ajj-dark-class (:inherit org-level-5))
      (,ajj-dark-256-class (:inherit org-level-5))))

   `(outline-6
     ((,ajj-dark-class (:inherit org-level-6))
      (,ajj-dark-256-class (:inherit org-level-6))))

   `(outline-7
     ((,ajj-dark-class (:inherit org-level-7))
      (,ajj-dark-256-class (:inherit org-level-7))))

   `(outline-8
     ((,ajj-dark-class (:inherit org-level-8))
      (,ajj-dark-256-class (:inherit org-level-8))))

   ;; parenface
   `(paren-face
     ((,ajj-dark-256-class (:foreground ,ajj-dark-comments))))

   ;; perspective
   `(persp-selected-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :weight bold))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight normal))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight normal))))

   ;; popup
   `(popup-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-foreground))))

   `(popup-isearch-match
     ((,ajj-dark-class (:background ,ajj-dark-green))
      (,ajj-dark-256-class (:background ,ajj-dark-256-green))))

   `(popup-menu-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-foreground))))

   `(popup-menu-mouse-face
     ((,ajj-dark-class (:background ,ajj-dark-blue
                                   :foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue
                                        :foreground ,ajj-dark-256-foreground))))

   `(popup-menu-selection-face
     ((,ajj-dark-class (:background ,ajj-dark-magenta
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-magenta
                                        :foreground ,ajj-dark-256-background))))

   `(popup-scroll-bar-background-face
     ((,ajj-dark-class (:background ,ajj-dark-comments))
      (,ajj-dark-256-class (:background ,ajj-dark-256-comments))))

   `(popup-scroll-bar-foreground-face
     ((,ajj-dark-class (:background ,ajj-dark-emphasis))
      (,ajj-dark-256-class (:background ,ajj-dark-256-emphasis))))

   `(popup-tip-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-foreground))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   `(rainbow-delimiters-depth-2-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(rainbow-delimiters-depth-3-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(rainbow-delimiters-depth-4-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(rainbow-delimiters-depth-5-face
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(rainbow-delimiters-depth-6-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(rainbow-delimiters-depth-7-face
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   `(rainbow-delimiters-depth-8-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(rainbow-delimiters-depth-9-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(rainbow-delimiters-depth-10-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(rainbow-delimiters-depth-11-face
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(rainbow-delimiters-depth-12-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(rainbow-delimiters-unmatched-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :background ,ajj-dark-background
                                   :inverse-video t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :background ,ajj-dark-256-background
                                        :inverse-video t))))

   ;; realgud
   `(realgud-overlay-arrow1
     ((,ajj-dark-class (:foreground ,ajj-dark-green-d))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green-d))))

   `(realgud-overlay-arrow2
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow-d))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow-d))))

   `(realgud-overlay-arrow3
     ((,ajj-dark-class (:foreground ,ajj-dark-orange-d))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange-d))))

   `(realgud-bp-enabled-face
     ((,ajj-dark-class (:inherit error)))
     ((,ajj-dark-256-class (:inherit error))))

   `(realgud-bp-disabled-face
     ((,ajj-dark-class (:inherit secondary-selection)))
     ((,ajj-dark-256-class (:inherit secondary-selection))))

   `(realgud-bp-line-enabled-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red-d)))
     ((,ajj-dark-256-class (:foreground ,ajj-dark-256-red-d))))

   `(realgud-bp-line-disabled-face
     ((,ajj-dark-class (:inherit secondary-selection)))
     ((,ajj-dark-256-class (:inherit secondary-selection))))

   `(realgud-line-number
     ((,ajj-dark-class (:inerhit ajj-dark-line-number)))
     ((,ajj-dark-256-class (:inerhit ajj-dark-line-number))))

   `(realgud-backtrace-number
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow-d
                                   :weight bold)))
     ((,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                       :weight bold))))

   ;; rhtm-mode
   `(erb-face
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :background ,ajj-dark-256-background))))

   `(erb-delim-face
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan
                                        :background ,ajj-dark-256-background))))

   `(erb-exec-face
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :background ,ajj-dark-256-background))))

   `(erb-exec-delim-face
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan
                                        :background ,ajj-dark-256-background))))

   `(erb-out-face
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :background ,ajj-dark-256-background))))

   `(erb-out-delim-face
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan
                                        :background ,ajj-dark-256-background))))

   `(erb-comment-face
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :background ,ajj-dark-256-background))))

   `(erb-comment-delim-face
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan
                                        :background ,ajj-dark-256-background))))

   ;; rst-mode
   `(rst-level-1-face
     ((,ajj-dark-class (:background ,ajj-dark-yellow
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow
                                        :foreground ,ajj-dark-256-background))))

   `(rst-level-2-face
     ((,ajj-dark-class (:background ,ajj-dark-cyan
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-cyan
                                        :foreground ,ajj-dark-256-background))))

   `(rst-level-3-face
     ((,ajj-dark-class (:background ,ajj-dark-blue
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue
                                        :foreground ,ajj-dark-256-background))))

   `(rst-level-4-face
     ((,ajj-dark-class (:background ,ajj-dark-violet
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-violet
                                        :foreground ,ajj-dark-256-background))))

   `(rst-level-5-face
     ((,ajj-dark-class (:background ,ajj-dark-magenta
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-magenta
                                        :foreground ,ajj-dark-256-background))))

   `(rst-level-6-face
     ((,ajj-dark-class (:background ,ajj-dark-red
                                   :foreground ,ajj-dark-background))
      (,ajj-dark-256-class (:background ,ajj-dark-256-red
                                        :foreground ,ajj-dark-256-background))))

   ;; rpm-mode
   `(rpm-spec-dir-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(rpm-spec-doc-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(rpm-spec-ghost-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(rpm-spec-macro-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(rpm-spec-obsolete-tag-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(rpm-spec-package-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(rpm-spec-section-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(rpm-spec-tag-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(rpm-spec-var-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,ajj-dark-class (:foreground ,ajj-dark-violet
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet
                                        :weight bold))))

   `(sh-escaped-newline
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight bold))))

   `(sh-heredoc
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :weight bold))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line))))

   `(sp-wrap-overlay-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line))))

   `(sp-wrap-tag-overlay-face
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line))))

   `(sp-show-pair-enclosing
     ((,ajj-dark-class (:inherit highlight))
      (,ajj-dark-256-class (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :background ,ajj-dark-background
                                   :weight normal
                                   :inverse-video t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :background ,ajj-dark-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :background ,ajj-dark-background
                                   :weight normal
                                   :inverse-video t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :background ,ajj-dark-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :background ,ajj-dark-background
                                   :weight normal
                                   :inverse-video t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :background ,ajj-dark-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(show-paren-mismatch
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :background ,ajj-dark-background
                                   :weight normal
                                   :inverse-video t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :background ,ajj-dark-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :background ,ajj-dark-background
                                   :weight normal
                                   :inverse-video t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :background ,ajj-dark-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-mismatch
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :background ,ajj-dark-background
                                   :weight normal
                                   :inverse-video t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :background ,ajj-dark-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-no-match
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :background ,ajj-dark-background
                                   :weight normal
                                   :inverse-video t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :background ,ajj-dark-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   ;; smerge
   `(smerge-base
      ((,ajj-dark-class (:background ,ajj-dark-diff-blue-base))
        (,ajj-dark-256-class (:background ,ajj-dark-256-diff-blue-base))))
   `(smerge-upper
      ((,ajj-dark-class (:background ,ajj-dark-diff-red-base))
        (,ajj-dark-256-class (:background ,ajj-dark-256-diff-red-base))))
   `(smerge-lower
      ((,ajj-dark-class (:background ,ajj-dark-diff-green-base))
        (,ajj-dark-256-class (:background ,ajj-dark-256-diff-green-base))))
   ;; WARNING: defining this face will overwrite the next two when displaying a
   ;; smerge diff in a file.
   ;; `(smerge-refined-changed
   ;;    ((,ajj-dark-class (:background ,ajj-dark-diff-blue-emphasis))
   ;;      (,ajj-dark-256-class (:background ,ajj-dark-256-diff-blue-emphasis))))
   `(smerge-refined-added
      ((,ajj-dark-class (:background ,ajj-dark-diff-green-emphasis))
        (,ajj-dark-256-class (:background ,ajj-dark-256-diff-green-emphasis))))
   `(smerge-refined-removed
      ((,ajj-dark-class (:background ,ajj-dark-diff-red-emphasis))
        (,ajj-dark-256-class (:background ,ajj-dark-256-diff-red-emphasis))))

   ;; speedbar
   `(speedbar-button-face
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :foreground ,ajj-dark-256-comments))))

   `(speedbar-directory-face
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :foreground ,ajj-dark-256-blue))))

   `(speedbar-file-face
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :foreground ,ajj-dark-256-foreground))))

   `(speedbar-highlight-face
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :background ,ajj-dark-256-highlight-line))))

   `(speedbar-selected-face
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :foreground ,ajj-dark-yellow
                                :underline t))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :foreground ,ajj-dark-256-yellow
                                     :underline t))))

   `(speedbar-separator-face
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :background ,ajj-dark-blue
                                :foreground ,ajj-dark-background
                                :overline ,ajj-dark-cyan-lc))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :background ,ajj-dark-256-blue
                                     :foreground ,ajj-dark-256-background
                                     :overline ,ajj-dark-256-cyan-lc))))

   `(speedbar-tag-face
     ((,ajj-dark-class (:inherit ,ajj-dark-pitch
                                :foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:inherit ,ajj-dark-pitch
                                     :foreground ,ajj-dark-256-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,ajj-dark-class (:background ,ajj-dark-blue
                                   :foreground ,ajj-dark-background
                                   :height ,ajj-dark-height-plus-1
                                   :weight bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-blue
                                        :foreground ,ajj-dark-256-background
                                        :height ,ajj-dark-height-plus-1
                                        :weight bold))))

   `(sr-editing-path-face
     ((,ajj-dark-class (:background ,ajj-dark-yellow
                                   :foreground ,ajj-dark-background
                                   :weight bold
                                   :height ,ajj-dark-height-plus-1))
      (,ajj-dark-256-class (:background ,ajj-dark-256-yellow
                                        :foreground ,ajj-dark-256-background
                                        :weight bold
                                        :height ,ajj-dark-height-plus-1))))

   `(sr-highlight-path-face
     ((,ajj-dark-class (:background ,ajj-dark-green
                                   :foreground ,ajj-dark-background
                                   :weight bold
                                   :height ,ajj-dark-height-plus-1))
      (,ajj-dark-256-class (:background ,ajj-dark-256-green
                                        :foreground ,ajj-dark-256-background
                                        :weight bold
                                        :height ,ajj-dark-height-plus-1))))

   `(sr-passive-path-face
     ((,ajj-dark-class (:background ,ajj-dark-comments
                                   :foreground ,ajj-dark-background
                                   :weight bold
                                   :height ,ajj-dark-height-plus-1))
      (,ajj-dark-256-class (:background ,ajj-dark-256-comments
                                        :foreground ,ajj-dark-256-background
                                        :weight bold
                                        :height ,ajj-dark-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,ajj-dark-class (:inherit diajj-dark-red-marked))
      (,ajj-dark-256-class (:inherit diajj-dark-red-marked))))

   `(sr-marked-file-face
     ((,ajj-dark-class (:inherit diajj-dark-red-marked))
      (,ajj-dark-256-class (:inherit diajj-dark-red-marked))))

   `(sr-alt-marked-dir-face
     ((,ajj-dark-class (:background ,ajj-dark-magenta
                                   :foreground ,ajj-dark-background
                                   :weight bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-magenta
                                        :foreground ,ajj-dark-256-background
                                        :weight bold))))

   `(sr-alt-marked-file-face
     ((,ajj-dark-class (:background ,ajj-dark-magenta
                                   :foreground ,ajj-dark-background
                                   :weight bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-magenta
                                        :foreground ,ajj-dark-256-background
                                        :weight bold))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,ajj-dark-class (:inherit diajj-dark-red-directory
                                :weight normal))
      (,ajj-dark-256-class (:inherit diajj-dark-red-directory
                                     :weight normal))))

   `(sr-symlink-directory-face
     ((,ajj-dark-class (:inherit diajj-dark-red-directory
                                :slant italic
                                :weight normal))
      (,ajj-dark-256-class (:inherit diajj-dark-red-directory
                                     :slant italic
                                     :weight normal))))

   `(sr-symlink-face
     ((,ajj-dark-class (:inherit diajj-dark-red-symlink
                                :slant italic
                                :weight normal))
      (,ajj-dark-256-class (:inherit diajj-dark-red-symlink
                                     :slant italic
                                     :weight normal))))

   `(sr-broken-link-face
     ((,ajj-dark-class (:inherit diajj-dark-red-warning
                                :slant italic
                                :weight normal))
      (,ajj-dark-256-class (:inherit diajj-dark-red-warning
                                     :slant italic
                                     :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(sr-encrypted-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(sr-log-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(sr-packaged-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(sr-html-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(sr-xml-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,ajj-dark-class (:background ,ajj-dark-red
                                   :foreground ,ajj-dark-background
                                   :weight bold))
      (,ajj-dark-256-class (:background ,ajj-dark-256-red
                                        :foreground ,ajj-dark-256-background
                                        :weight bold))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,ajj-dark-class (:background unspecified
                                   :foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:background unspecified
                                        :foreground ,ajj-dark-256-yellow))))

   `(syslog-hour-face
     ((,ajj-dark-class (:background unspecified
                                   :foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:background unspecified
                                        :foreground ,ajj-dark-256-green))))

   `(syslog-error-face
     ((,ajj-dark-class (:background unspecified
                                   :foreground ,ajj-dark-red
                                   :weight bold))
      (,ajj-dark-256-class (:background unspecified
                                        :foreground ,ajj-dark-256-red
                                        :weight bold))))

   `(syslog-warn-face
     ((,ajj-dark-class (:background unspecified
                                   :foreground ,ajj-dark-orange
                                   :weight bold))
      (,ajj-dark-256-class (:background unspecified
                                        :foreground ,ajj-dark-256-orange
                                        :weight bold))))

   `(syslog-info-face
     ((,ajj-dark-class (:background unspecified
                                   :foreground ,ajj-dark-blue
                                   :weight bold))
      (,ajj-dark-256-class (:background unspecified
                                        :foreground ,ajj-dark-256-blue
                                        :weight bold))))

   `(syslog-debug-face
     ((,ajj-dark-class (:background unspecified
                                   :foreground ,ajj-dark-cyan
                                   :weight bold))
      (,ajj-dark-256-class (:background unspecified
                                        :foreground ,ajj-dark-256-cyan
                                        :weight bold))))

   `(syslog-su-face
     ((,ajj-dark-class (:background unspecified
                                   :foreground ,ajj-dark-magenta))
      (,ajj-dark-256-class (:background unspecified
                                        :foreground ,ajj-dark-256-magenta))))

   ;; table
   `(table-cell
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :background ,ajj-dark-256-highlight-line))))

   ;; term
   `(term-color-black
     ((,ajj-dark-class (:foreground ,ajj-dark-background
                                   :background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-background
                                        :background ,ajj-dark-256-highlight-line))))

   `(term-color-red
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :background ,ajj-dark-red-d))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :background ,ajj-dark-256-red-d))))

   `(term-color-green
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :background ,ajj-dark-green-d))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :background ,ajj-dark-256-green-d))))

   `(term-color-yellow
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :background ,ajj-dark-yellow-d))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :background ,ajj-dark-256-yellow-d))))

   `(term-color-blue
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :background ,ajj-dark-blue-d))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :background ,ajj-dark-256-blue-d))))

   `(term-color-magenta
     ((,ajj-dark-class (:foreground ,ajj-dark-magenta
                                   :background ,ajj-dark-magenta-d))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-magenta
                                        :background ,ajj-dark-256-magenta-d))))

   `(term-color-cyan
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan
                                   :background ,ajj-dark-cyan-d))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan
                                        :background ,ajj-dark-256-cyan-d))))

   `(term-color-white
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :background ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :background ,ajj-dark-256-foreground))))

   `(term-default-fg-color
     ((,ajj-dark-class (:inherit term-color-white))
      (,ajj-dark-256-class (:inherit term-color-white))))

   `(term-default-bg-color
     ((,ajj-dark-class (:inherit term-color-black))
      (,ajj-dark-256-class (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,ajj-dark-class (:background ,ajj-dark-yellow-hc
                                   :foreground ,ajj-dark-background
                                   :inherit ,ajj-dark-pitch))))

   ;; treemacs
   `(treemacs-directory-face
      ((,ajj-dark-class (:foreground ,ajj-dark-violet
                         :background ,ajj-dark-background
                         :weight bold))
        (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet
                              :background ,ajj-dark-256-background
                              :weight bold))))

   `(treemacs-header-face
      ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                         :background ,ajj-dark-background
                         :underline t
                         :weight bold))
        (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                              :background ,ajj-dark-256-background
                              :underline t
                              :weight bold))))

   `(treemacs-git-modified-face
      ((,ajj-dark-class (:foreground ,ajj-dark-green
                         :background ,ajj-dark-background))
        (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                              :background ,ajj-dark-256-background))))

   `(treemacs-git-renamed-face
      ((,ajj-dark-class (:foreground ,ajj-dark-red
                         :background ,ajj-dark-background))
        (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                              :background ,ajj-dark-256-background))))

   `(treemacs-git-ignored-face
      ((,ajj-dark-class (:foreground ,ajj-dark-gray-l
                         :background ,ajj-dark-background))
        (,ajj-dark-256-class (:foreground ,ajj-dark-256-gray-l
                              :background ,ajj-dark-256-background))))

   `(treemacs-git-untracked-face
      ((,ajj-dark-class (:foreground ,ajj-dark-red
                         :background ,ajj-dark-background))
        (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                              :background ,ajj-dark-256-background))))

   `(treemacs-git-added-face
      ((,ajj-dark-class (:foreground ,ajj-dark-green
                         :background ,ajj-dark-background))
        (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                              :background ,ajj-dark-256-background))))

   `(treemacs-git-conflict-face
      ((,ajj-dark-class (:foreground ,ajj-dark-orange
                         :background ,ajj-dark-background))
        (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange
                              :background ,ajj-dark-256-background))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,ajj-dark-class (:foreground ,ajj-dark-magenta
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-magenta
                                        :weight bold))))

   `(tuareg-font-lock-multistage-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :background ,ajj-dark-highlight-line
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :background ,ajj-dark-256-highlight-line
                                        :weight bold))))

   `(tuareg-font-lock-operator-face
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis))))

   `(tuareg-font-lock-error-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :background ,ajj-dark-red
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :background ,ajj-dark-256-red
                                        :weight bold))))

   `(tuareg-font-lock-interactive-output-face
     ((,ajj-dark-class (:foreground ,ajj-dark-cyan))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-cyan))))

   `(tuareg-font-lock-interactive-error-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments
                                   :background ,ajj-dark-background))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments
                                        :background ,ajj-dark-256-background))))

   `(undo-tree-visualizer-unmodified-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(undo-tree-visualizer-current-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue
                                   :inverse-video t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue
                                        :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :background ,ajj-dark-background
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :background ,ajj-dark-256-background
                                        :weight bold))))

   `(undo-tree-visualizer-register-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   ;; volatile highlights
   `(vhl/default-face
      ((,ajj-dark-class (:background ,ajj-dark-highlight-alt))
        (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-alt))))

   ;; w3m
   `(w3m-anchor
     ((,ajj-dark-class (:inherit link))
      (,ajj-dark-256-class (:inherit link))))

   `(w3m-arrived-anchor
     ((,ajj-dark-class (:inherit link-visited))
      (,ajj-dark-256-class (:inherit link-visited))))

   `(w3m-form
     ((,ajj-dark-class (:background ,ajj-dark-background
                                   :foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background
                                        :foreground ,ajj-dark-256-foreground))))

   `(w3m-header-line-location-title
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-yellow))))

   `(w3m-header-line-location-content

     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-foreground))))

   `(w3m-bold
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :weight bold))))

   `(w3m-image-anchor
     ((,ajj-dark-class (:background ,ajj-dark-background
                                   :foreground ,ajj-dark-cyan
                                   :inherit link))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background
                                        :foreground ,ajj-dark-256-cyan
                                        :inherit link))))

   `(w3m-image
     ((,ajj-dark-class (:background ,ajj-dark-background
                                   :foreground ,ajj-dark-cyan))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background
                                        :foreground ,ajj-dark-256-cyan))))

   `(w3m-lnum-minibuffer-prompt
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis))))

   `(w3m-lnum-match
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line))))

   `(w3m-lnum
     ((,ajj-dark-class (:underline nil
                                  :bold nil
                                  :foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:underline nil
                                       :bold nil
                                       :foreground ,ajj-dark-256-red))))

   `(w3m-session-select
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(w3m-session-selected
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :bold t
                                   :underline t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :bold t
                                        :underline t))))

   `(w3m-tab-background
     ((,ajj-dark-class (:background ,ajj-dark-background
                                   :foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background
                                        :foreground ,ajj-dark-256-foreground))))

   `(w3m-tab-selected-background
     ((,ajj-dark-class (:background ,ajj-dark-background
                                   :foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:background ,ajj-dark-256-background
                                        :foreground ,ajj-dark-256-foreground))))

   `(w3m-tab-mouse
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-yellow))))

   `(w3m-tab-selected
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-emphasis
                                   :bold t))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-emphasis
                                        :bold t))))

   `(w3m-tab-unselected
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-foreground))))

   `(w3m-tab-selected-retrieving
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-red))))

   `(w3m-tab-unselected-retrieving
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-orange))))

   `(w3m-tab-unselected-unseen
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :foreground ,ajj-dark-256-violet))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(web-mode-comment-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(web-mode-constant-face
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   `(web-mode-current-element-highlight-face
     ((,ajj-dark-class (:underline unspecified
                                  :weight unspecified
                                  :background ,ajj-dark-highlight-line))
      (,ajj-dark-256-class (:underline unspecified
                                       :weight unspecified
                                       :background ,ajj-dark-256-highlight-line))))

   `(web-mode-doctype-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments
                                   :slant italic
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments
                                        :slant italic
                                        :weight bold))))

   `(web-mode-folded-face
     ((,ajj-dark-class (:underline t))
      (,ajj-dark-256-class (:underline t))))

   `(web-mode-function-name-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(web-mode-html-attr-name-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(web-mode-html-attr-custom-face
     ((,ajj-dark-class (:inherit web-mode-html-attr-name-face))
      (,ajj-dark-256-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-engine-face
     ((,ajj-dark-class (:inherit web-mode-block-delimiter-face))
      (,ajj-dark-256-class (:inherit web-mode-block-delimiter-face))))

   `(web-mode-html-attr-equal-face
     ((,ajj-dark-class (:inherit web-mode-html-attr-name-face))
      (,ajj-dark-256-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-value-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(web-mode-html-tag-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(web-mode-keyword-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(web-mode-preprocessor-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow
                                   :slant normal
                                   :weight unspecified))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow
                                        :slant normal
                                        :weight unspecified))))

   `(web-mode-string-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(web-mode-type-face
     ((,ajj-dark-class (:inherit font-lock-type-face))
      (,ajj-dark-256-class (:inherit font-lock-type-face))))

   `(web-mode-variable-name-face
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(web-mode-warning-face
     ((,ajj-dark-class (:inherit font-lock-warning-face))
      (,ajj-dark-256-class (:inherit font-lock-warning-face))))

   `(web-mode-block-face
     ((,ajj-dark-class (:background unspecified))
      (,ajj-dark-256-class (:background unspecified))))

   `(web-mode-block-delimiter-face
     ((,ajj-dark-class (:inherit font-lock-preprocessor-face))
      (,ajj-dark-256-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-comment-face
     ((,ajj-dark-class (:inherit web-mode-comment-face))
      (,ajj-dark-256-class (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,ajj-dark-class (:inherit font-lock-preprocessor-face))
      (,ajj-dark-256-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-string-face
     ((,ajj-dark-class (:inherit web-mode-string-face))
      (,ajj-dark-256-class (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,ajj-dark-class (:box 1 :weight bold))
      (,ajj-dark-256-class (:box 1 :weight bold))))

   `(web-mode-css-at-rule-face
     ((,ajj-dark-class (:inherit font-lock-constant-face))
      (,ajj-dark-256-class (:inherit font-lock-constant-face))))

   `(web-mode-css-pseudo-class-face
     ((,ajj-dark-class (:inherit font-lock-builtin-face))
      (,ajj-dark-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-color-face
     ((,ajj-dark-class (:inherit font-lock-builtin-face))
      (,ajj-dark-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-filter-face
     ((,ajj-dark-class (:inherit font-lock-function-name-face))
      (,ajj-dark-256-class (:inherit font-lock-function-name-face))))

   `(web-mode-css-function-face
     ((,ajj-dark-class (:inherit font-lock-builtin-face))
      (,ajj-dark-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-call-face
     ((,ajj-dark-class (:inherit font-lock-function-name-face))
      (,ajj-dark-256-class (:inherit font-lock-function-name-face))))

   `(web-mode-css-priority-face
     ((,ajj-dark-class (:inherit font-lock-builtin-face))
      (,ajj-dark-256-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,ajj-dark-class (:inherit font-lock-variable-name-face))
      (,ajj-dark-256-class (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,ajj-dark-class (:inherit font-lock-keyword-face))
      (,ajj-dark-256-class (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,ajj-dark-class (:inherit web-mode-string-face))
      (,ajj-dark-256-class (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,ajj-dark-class (:inherit web-mode-string-face))
      (,ajj-dark-256-class (:inherit web-mode-string-face))))

   `(web-mode-json-comment-face
     ((,ajj-dark-class (:inherit web-mode-comment-face))
      (,ajj-dark-256-class (:inherit web-mode-comment-face))))

   `(web-mode-json-context-face
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   `(web-mode-json-key-face
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   `(web-mode-json-string-face
     ((,ajj-dark-class (:inherit web-mode-string-face))
      (,ajj-dark-256-class (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(web-mode-part-comment-face
     ((,ajj-dark-class (:inherit web-mode-comment-face))
      (,ajj-dark-256-class (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,ajj-dark-class (:inherit web-mode-block-face))
      (,ajj-dark-256-class (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,ajj-dark-class (:inherit web-mode-string-face))
      (,ajj-dark-256-class (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,ajj-dark-class (:foreground ,ajj-dark-violet))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-violet))))

   `(web-mode-whitespace-face
     ((,ajj-dark-class (:background ,ajj-dark-red))
      (,ajj-dark-256-class (:background ,ajj-dark-256-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,ajj-dark-class (:background unspecified
                                   :foreground ,ajj-dark-comments
                                   :inverse-video unspecified
                                   :slant italic))
      (,ajj-dark-256-class (:background unspecified
                                        :foreground ,ajj-dark-256-comments
                                        :inverse-video unspecified
                                        :slant italic))))

   `(whitespace-hspace
     ((,ajj-dark-class (:background unspecified
                                   :foreground ,ajj-dark-emphasis
                                   :inverse-video unspecified))
      (,ajj-dark-256-class (:background unspecified
                                        :foreground ,ajj-dark-256-emphasis
                                        :inverse-video unspecified))))

   `(whitespace-tab
     ((,ajj-dark-class (:background unspecified
                                   :foreground ,ajj-dark-red
                                   :inverse-video unspecified
                                   :weight bold))
      (,ajj-dark-256-class (:background unspecified
                                        :foreground ,ajj-dark-256-red
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-newline
     ((,ajj-dark-class(:background unspecified
                                  :foreground ,ajj-dark-comments
                                  :inverse-video unspecified))
      (,ajj-dark-256-class (:background unspecified
                                       :foreground ,ajj-dark-256-comments
                                       :inverse-video unspecified))))

   `(whitespace-trailing
     ((,ajj-dark-class (:background unspecified
                                   :foreground ,ajj-dark-orange-lc
                                   :inverse-video t))
      (,ajj-dark-256-class (:background unspecified
                                        :foreground ,ajj-dark-256-orange-lc
                                        :inverse-video t))))

   `(whitespace-line
     ((,ajj-dark-class (:background unspecified
                                   :foreground ,ajj-dark-magenta
                                   :inverse-video unspecified))
      (,ajj-dark-256-class (:background unspecified
                                        :foreground ,ajj-dark-256-magenta
                                        :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,ajj-dark-class (:background ,ajj-dark-red-lc
                                   :foreground unspecified
                                   :inverse-video unspecified))
      (,ajj-dark-256-class (:background ,ajj-dark-256-red-lc
                                        :foreground unspecified
                                        :inverse-video unspecified))))

   `(whitespace-indentation
     ((,ajj-dark-class (:background unspecified
                                   :foreground ,ajj-dark-yellow
                                   :inverse-video unspecified
                                   :weight bold))
      (,ajj-dark-256-class (:background unspecified
                                        :foreground ,ajj-dark-256-yellow
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-empty
     ((,ajj-dark-class (:background unspecified
                                   :foreground ,ajj-dark-red-lc
                                   :inverse-video t))
      (,ajj-dark-256-class (:background unspecified
                                        :foreground ,ajj-dark-256-red-lc
                                        :inverse-video t))))

   `(whitespace-space-after-tab
     ((,ajj-dark-class (:background unspecified
                                   :foreground ,ajj-dark-orange
                                   :inverse-video t
                                   :weight bold))
      (,ajj-dark-256-class (:background unspecified
                                        :foreground ,ajj-dark-256-orange
                                        :inverse-video t
                                        :weight bold))))

   ;; wanderlust
   `(wl-highlight-folder-few-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(wl-highlight-folder-many-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(wl-highlight-folder-path-face
     ((,ajj-dark-class (:foreground ,ajj-dark-orange))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-orange))))

   `(wl-highlight-folder-unread-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(wl-highlight-folder-zero-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(wl-highlight-folder-unknown-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(wl-highlight-message-citation-header
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(wl-highlight-message-cited-text-1
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(wl-highlight-message-cited-text-2
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(wl-highlight-message-cited-text-3
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(wl-highlight-message-cited-text-4
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(wl-highlight-message-header-contents-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(wl-highlight-message-headers-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red))))

   `(wl-highlight-message-important-header-contents
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(wl-highlight-message-header-contents
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(wl-highlight-message-important-header-contents2
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(wl-highlight-message-signature
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   `(wl-highlight-message-unimportant-header-contents
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(wl-highlight-summary-answeajj-dark-red-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(wl-highlight-summary-disposed-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground
                                   :slant italic))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground
                                        :slant italic))))

   `(wl-highlight-summary-new-face
     ((,ajj-dark-class (:foreground ,ajj-dark-blue))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-blue))))

   `(wl-highlight-summary-normal-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(wl-highlight-summary-thread-top-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow))))

   `(wl-highlight-thread-indent-face
     ((,ajj-dark-class (:foreground ,ajj-dark-magenta))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-magenta))))

   `(wl-highlight-summary-refiled-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(wl-highlight-summary-displaying-face
     ((,ajj-dark-class (:underline t
                                  :weight bold))
      (,ajj-dark-256-class (:underline t
                                       :weight bold))))

   ;; weechat
   `(weechat-error-face
     ((,ajj-dark-class (:inherit error))
      (,ajj-dark-256-class (:inherit error))))

   `(weechat-highlight-face
     ((,ajj-dark-class (:foreground ,ajj-dark-emphasis
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-emphasis
                                        :weight bold))))

   `(weechat-nick-self-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :weight unspecified
                                   :inverse-video t))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :weight unspecified
                                        :inverse-video t))))

   `(weechat-prompt-face
     ((,ajj-dark-class (:inherit minibuffer-prompt))
      (,ajj-dark-256-class (:inherit minibuffer-prompt))))

   `(weechat-time-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   ;; which-func-mode
   `(which-func
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   ;; which-key
   `(which-key-key-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green
                                        :weight bold))))

   `(which-key-separator-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(which-key-note-face
     ((,ajj-dark-class (:foreground ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments))))

   `(which-key-command-description-face
     ((,ajj-dark-class (:foreground ,ajj-dark-foreground))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-foreground))))

   `(which-key-local-map-description-face
     ((,ajj-dark-class (:foreground ,ajj-dark-yellow-hc))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-yellow-hc))))

   `(which-key-group-description-face
     ((,ajj-dark-class (:foreground ,ajj-dark-red
                                   :weight bold))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-red
                                        :weight bold))))
   ;; window-number-mode
   `(window-number-face
     ((,ajj-dark-class (:foreground ,ajj-dark-green))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,ajj-dark-class (:foreground ,ajj-dark-comments
                                   :background ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments
                                        :background ,ajj-dark-256-comments))))

   `(yascroll:thumb-fringe
     ((,ajj-dark-class (:foreground ,ajj-dark-comments
                                   :background ,ajj-dark-comments))
      (,ajj-dark-256-class (:foreground ,ajj-dark-256-comments
                                        :background ,ajj-dark-256-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,ajj-dark-class (:background ,ajj-dark-highlight-line
                                   :box ,ajj-dark-emphasis))
      (,ajj-dark-256-class (:background ,ajj-dark-256-highlight-line
                                        :box ,ajj-dark-256-emphasis)))))

  (custom-theme-set-variables
   'ajj-dark
   `(ansi-color-names-vector [,ajj-dark-background ,ajj-dark-red ,ajj-dark-green ,ajj-dark-yellow
                                                  ,ajj-dark-blue ,ajj-dark-magenta ,ajj-dark-cyan ,ajj-dark-foreground])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,ajj-dark-highlight-line)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors '(,ajj-dark-magenta ,ajj-dark-violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,ajj-dark-highlight-line . 0)
       (,ajj-dark-green-lc . 20)
       (,ajj-dark-cyan-lc . 30)
       (,ajj-dark-blue-lc . 50)
       (,ajj-dark-yellow-lc . 60)
       (,ajj-dark-orange-lc . 70)
       (,ajj-dark-magenta-lc . 85)
       (,ajj-dark-highlight-line . 100)))

   ;; pos-tip
   `(pos-tip-foreground-color ,ajj-dark-background)
   `(pos-tip-background-color ,ajj-dark-yellow-hc)

   ;; vc
   `(vc-annotate-color-map
     '((20 . ,ajj-dark-red)
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . ,ajj-dark-yellow)
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . ,ajj-dark-green)
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . ,ajj-dark-cyan)
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . ,ajj-dark-blue)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; weechat
   `(weechat-color-list
     '(unspecified ,ajj-dark-background ,ajj-dark-highlight-line
                  ,ajj-dark-red-d ,ajj-dark-red
                  ,ajj-dark-green-d ,ajj-dark-green
                  ,ajj-dark-yellow-d ,ajj-dark-yellow
                  ,ajj-dark-blue-d ,ajj-dark-blue
                  ,ajj-dark-magenta-d ,ajj-dark-magenta
                  ,ajj-dark-cyan-d ,ajj-dark-cyan
                  ,ajj-dark-foreground ,ajj-dark-emphasis))))

(provide-theme 'ajj-dark)

;; Local Variables:
;; no-byte-compile: t
;; fill-column: 95
;; End:

;;; ajj-dark-theme.el ends here
