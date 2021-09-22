
;; uncomment this to debug problems - look at *Backtrace* buffer for clues
(setq debug-on-error t)



;; package management --------------------------------------------


;; workaround for bug https://debbugs.gnu.org/34341 (It should be fixed in Emacs 26.3+)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")


(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
 ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  ;; (when (< emacs-major-version 24)
  ;;   ;; For important compatibility libraries like cl-lib
  ;;   (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
  )
(package-initialize)
;; Bootstrap 'use-package'
;; N.B. package initialization should happen before you require Org and set any Org options. 


(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

;;}}}


;; workaround for emacs bug fixed in version 26.3
;; where attempts to retrieve packages from elpa/melpa result in 'bad request':
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")



;; week starts on a Monday
(setq calendar-week-start-day 1)


(require 'sr-speedbar)

;; editor behavior ---------------------------------------------

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; show cursor position within line
(column-number-mode 1)

;; make cursor movement stop in between camelCase words.
(global-subword-mode 1)

;; turn on bracket match highlight
(show-paren-mode 1)

(setq-default indent-tabs-mode nil)

;; backups ------------------------------------------------------

; stop emacs's backup changing the file's creation date of the original file
(setq backup-by-copying t)

(defun make-backup-file-name (FILE)                                             
  (let ((dirname (concat "~/tmp/emacs_backups/"                                    
                         (format-time-string "%Y/%m/%d/"))))                    
    (if (not (file-exists-p dirname))                                           
        (make-directory dirname t))                                             
    (concat dirname (file-name-nondirectory FILE))))



;; files --------------------------------------------------------

;; keep a list of recently opened files
(require 'recentf)
(recentf-mode 1)

;; save/restore opened files
(desktop-save-mode 1)



;; Python  
(elpy-enable)



;; yaml

;; use
;;    highlight-indention-mode
;;    smart-shift
;;    flycheck
;;    



;; org configuration --------------------------------------------
;;(require 'org-tempo)
;;File is missing: Cannot open load file, No such file or directory, org-tempo

;(setq org--check-org-structure-template-alist '() )

;; (with-eval-after-load "org"
;;   (add-to-list 'org-structure-template-alist
;;                    '("S" . "src emacs-lisp")
;;                  '("S" "#+begin_src emacs-lisp\n?\n#+END_SRC" "<src lang=\"emacs-lisp\">\n\n</src>")))


;; org-mode timers
(setq org-clock-sound "~/.emacs.d/Temple_Bell-SoundBible.com-756181215.wav")


;; when opening a org file, don't collapse headings
(setq org-startup-folded nil)

;; wrap long lines. don't let it disappear to the right
(setq org-startup-truncated nil)

;; when in a url link, enter key should open it
(setq org-return-follows-link t)

;; make org-mode” syntax color embedded source code
(setq org-src-fontify-natively t)


(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "ditaa"))  
  (not (string= lang "python")) 
  )
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)


;; Reveal.js + Org mode for presentations
(require 'ox-reveal)
(setq org-reveal-root "file:////home/jdonald/.local/lib/javascript/reveal.js-master")            ;; CDN version:    https://cdn.jsdelivr.net/npm/reveal.js
(setq org-reveal-title-slide nil)


;; Impress.js + Org mode for super-sexy 3D presentations
;; see docs at https://github.com/nullpofy/org-impress-js.el
(require 'ox-impress-js)

(require 'ob-http)

(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

(setq org-todo-keywords
      '((sequence "TODO" "IN_PROGRESS" "BLOCKED" "DEFERRED" "VERIFY" "|" "DONE" )))

(require 'ox-tufte)


;; PlantUML!
(setq org-plantuml-jar-path
      (expand-file-name "~/.local/lib/java/plantuml.jar"))
(setq plantuml-default-exec-mode 'jar);;  org-babel configuration


;;; org-babel: execute languages in code blocks(!)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ditaa . t) ; this line activates ditaa
   (http . t)  ; ob-http
   (latex . t)   ;;org-edit-latex
   (shell . t)
   (python . t)
   (http . t)
   (plantuml . t)))

(push '("plantuml" . plantuml) org-src-lang-modes)



(eval-after-load 'autoinsert
  '(define-auto-insert '(org-mode . "org skeleton")
     '("org-mode boilerplate " \n
       "#+title: " > _  \n
       "#+HTML_HEAD: \<link rel=\"stylesheet\" type=\"text/css\" href=\"/home/jdonald/.css/org-tufte.css\" \/\> \<link rel=\"stylesheet\" type=\"text/css\" href=\"https://www.treeblossom.com/style/org-tufte.css\" \/\>" \n
       "#+options: ^:nil toc:2 num:2\n#+startup: hidestars\n#+startup: overview\n#+LANGUAGE: en" \n
       "* Overview\n    + \n*\n\n* References and Resources\n    + \n")))
(auto-insert-mode)

;; UI --------------------------------------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)

;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))

(add-to-list 'default-frame-alist '(alpha . (85 . 50)))


(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)




;; buffer menu ------------------------------------------------------
;; map C-x C-b to (buffer-menu-other-window) instead of (list-buffers)
;; to save having to make the extra step of focusing the list of buffers
(global-set-key (kbd "C-x C-b") (function buffer-menu-other-window))




;; be the system editor  --------------------------------------------
(server-start)

;; Tramp
;; tramp gives me all kinds of problems....
(customize-set-variable 'tramp-syntax 'simplified)


;; Haskell --------------------------------------------------------
;; (use-package lsp-haskell
;;  :ensure t
;;  :config
;;  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
;;  ;; Comment/uncomment this line to see interactions between lsp client/server.
;;  ;;(setq lsp-log-io t)
;; )

;; Javascript
;; (require 'indium)
;; (add-hook 'js-mode-hook #'indium-interaction-mode)




;; lsp-mode   - language server
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode 1)
  (global-set-key (kbd "C-\<tab\>") 'company-complete))

;; Flycheck - modern on-the-fly syntax checking extension; a modern alternative to Flymake
;; docs at https://www.flycheck.org/en/latest
(use-package flycheck)
(global-flycheck-mode)


;; docs at https://github.com/abingham/flycheck-vale
(require 'flycheck-vale)
(flycheck-vale-setup)


;; org-roam v2
;; (use-package org-roam  ;;; this must run before org-roam loads
;;   :ensure t
;;   :init
;;   (seq org-roam-v2-ack t)   ;; prevent the org-roam v2 warning message from appearing every single time
;;   :custom
;;   (org-roam-directory "~/roamfiles")   ;; must be separate from the rest of your .org files
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n l" . org-roam-node-insert))
;;          :config
;;          (org-roam-setup))

;; Yasnippets
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)


;; Swagger Mode integrates Swagger Codegen into Emacs as a Minor Mode
;; docs are at https://github.com/Nooby/swagger-mode
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/swagger-mode.el"))   ;;  can't seem to get this to load this way
;;(require 'swagger-mode)




;; eclim - use eclipse as a Java language server - cool idea....too bad it requires Eclipse Photon
;; http://www.goldsborough.me/emacs,/java/2016/02/24/22-54-16-setting_up_emacs_for_java_development/

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-insert-mode t)
 '(custom-safe-themes
   '("3a78cae35163bb71df460ebcfdebf811fd7bc74eaa15428c7e0bccfd4f858d30" "9685cefcb4efd32520b899a34925c476e7920725c8d1f660e7336f37d6d95764" "86704574d397606ee1433af037c46611fb0a2787e8b6fd1d6c96361575be72d2" "201b95ea8c96dd0859ca72ba84ac32e696c4a52b1236c540cd3d36d2828735b8" "3cd4f09a44fe31e6dd65af9eb1f10dc00d5c2f1db31a427713a1784d7db7fdfc" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b6269b0356ed8d9ed55b0dcea10b4e13227b89fd2af4452eee19ac88297b0f99" "fb83a50c80de36f23aea5919e50e1bccd565ca5bb646af95729dc8c5f926cbf3" "3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "2679db166117d5b26b22a8f12a940f5ac415d76b004de03fcd34483505705f62" "ade241807d5b43f335f0a7394a1608911f272ea00c81bc0d8448801719d9da0a" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "2dc4191c0bb7406a2fe078e642e23a54bf169709eb3cc3f14ce07bbe430a9301" "d94a55a07623ee474ddb4a0a5dca9a250ea4dcebe554249ce305560c3340ec57" "ace21d57cd9a22c96c021acfd2938411e3374626fe8d91afb9bb969b5269ea75" "bbd478fcc11adf3e314bfb4d326197e9492095a58739c2cce6cad97884b186e5" "1dd7b369ab51f00e91b6a990634017916e7bdeb64002b4dda0d7a618785725ac" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "6ca5f925de5c119694dbe47e2bc95f8bad16b46d154b3e2e0ae246fec4100ec5" "13a654e817774e669cc17ee0705a3e1dfc62aedb01005a8abe2f8930a1d16d2e" "d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" "c221703cc604312f6f72349704f7329f80ccc6a261af769332ec80171b728cc0" "2642a1b7f53b9bb34c7f1e032d2098c852811ec2881eec2dc8cc07be004e45a0" "3a5f04a517096b08b08ef39db6d12bd55c04ed3d43b344cf8bd855bde6d3a1ae" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "454c1c9ce70f7d807c51c890910365fd3c64a9e63f596511e9ff57dd97bbeea8" "2593436c53c59d650c8e3b5337a45f0e1542b1ba46ce8956861316e860b145a0" "88049c35e4a6cedd4437ff6b093230b687d8a1fb65408ef17bfcf9b7338734f6" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "8a97050c9dd0af1cd8c3290b061f4b6032ccf2044ddc4d3c2c39e516239b2463" "bd51a329aa9b8e29c6cf2c8a8cf136e0d2960947dfa5c1f82b29c9178ad89a27" "abe5ee8858cd1fbe36304a8c3b2315d3e0a4ef7c8588fcc45d1c23eafb725bb6" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "3ae46713c18018fcc81f3ac191080083525089954080090e542af0d5c8070d76" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "7824eb15543c5c57c232c131ca64c4f25bfeeeda6744f71b999787a9172fa74e" "8dc7f4a05c53572d03f161d82158728618fb306636ddeec4cce204578432a06d" "174502267725776b47bdd2d220f035cae2c00c818765b138fea376b2cdc15eb6" "57290e991bf30a375509b74ea7ecfdb5de5150e0a14130c925582726f003c919" "3903f1967cdce48bcf4e9914cdc4f16cd0cd2f09560fe7f2429a8b9f1dfb9c99" "a77ced882e25028e994d168a612c763a4feb8c4ab67c5ff48688654d0264370c" "5a2772c3ba633ab530cf1c648c5828d2e061ca7d454c2d67c88d044fdd848fa7" "d4131a682c4436bb5a61103d9a850bf788cbf793f3fd8897de520d20583aeb58" "4b19d61c560a93ef90767abe513c11f236caec2864617d718aa366618133704c" "3acd6c080ef00f41997222d10253cb1eefc6f5229a63ccf0a7515fb98b09e88a" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "8d8423e863b3fbc6346758d726bae66b3d2bacac526067d7d8f2d710203066eb" "1b2ad37ab42d520aecc4a17c806381273176ab9f463288536f1df304b68e54d1" "713f898dd8c881c139b62cf05b7ac476d05735825d49006255c0a31f9a4f46ab" "d9aa334b2011d57c8ce279e076d6884c951e82ebc347adbe8b7ac03c4b2f3d72" "fde75446f309059ce742bd3650b6222c73acadab5b31c572dd366141d2abd453" "5ac259a7a0a0d2b541199480c58510b4f9f244e810da999d3f22d5e3bb0ad208" "0c46a9128995ad772ecbfc5a5193cb253a9a0200bcddf4d6895370e0a92545b4" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "24fc62afe2e5f0609e436aa2427b396adf9a958a8fa660edbaab5fb13c08aae6" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "1068ae7acf99967cc322831589497fee6fb430490147ca12ca7dd3e38d9b552a" "cd7ffd461946d2a644af8013d529870ea0761dccec33ac5c51a7aaeadec861c2" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default))
 '(flycheck-checker-error-threshold 999)
 '(flycheck-posframe-border-width 5)
 '(font-use-system-font t)
 '(hydra-posframe-show-params
   '(:poshandler posframe-poshandler-frame-bottom-center :internal-border-width 15 :internal-border-color "#3f4242" :background-color "#3f4242") t)
 '(ivy-posframe-border-width 15)
 '(ivy-posframe-style 'frame-bottom-window-center)
 '(mode-line-buffer-identification (propertized-buffer-identification "%b") t)
 '(org-fontify-whole-block-delimiter-line t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   '(all-the-icons acme-theme atom-one-dark-theme autumn-light-theme ayu-theme berrys-theme borland-blue-theme boron-theme busybee-theme caroline-theme challenger-deep-theme cherry-blossom-theme chyla-theme clues-theme creamsody-theme cyberpunk-2019-theme cyberpunk-theme cycle-themes dakrone-light-theme darcula-theme display-theme doom-themes dream-theme eink-theme elegant-agenda-mode enlightened-theme espresso-theme fantom-theme farmhouse-theme firecode-theme flatfluc-theme flucui-themes gandalf-theme github-modern-theme github-theme goose-theme grayscale-theme green-screen-theme greymatters-theme gruvbox-theme helm-themes hemisu-theme heroku-theme horizon-theme hybrid-reverse-theme iceberg-theme immaterial-theme inkpot-theme intellij-theme inverse-acme-theme ipcalc ir-black-theme jbeans-theme labburn-theme laguna-theme mandm-theme material-theme mellow-theme minsk-theme moe-theme molokai-theme mustang-theme mustard-theme naquadah-theme naysayer-theme night-owl-theme nofrils-acme-theme nordless-theme nothing-theme oceanic-theme panda-theme peacock-theme plan9-theme poet-theme professional-theme purple-haze-theme qtcreator-theme railscasts-reloaded-theme rebecca-theme sculpture-themes select-themes shades-of-purple-theme slime-theme smyx-theme snazzy-theme solo-jazz-theme sorcery-theme soria-theme sourcerer-theme spacegray-theme stimmung-themes subatomic-theme sunburn-theme sunny-day-theme suscolors-theme theme-looper tommyh-theme tron-legacy-theme ujelly-theme undersea-theme unobtrusive-magit-theme vscode-dark-plus-theme weyland-yutani-theme zenburn-theme php-mode org-edit-latex org-runbook org-scrum org-sidebar smart-shift sr-speedbar adoc-mode flycheck-pyflakes flycheck-vale json-mode org-babel-eval-in-repl ob-http restclient restclient-test ox-html5slide ox-impress-js ox-ioslide ox-pandoc ox-pukiwiki ox-report ox-slack ox-ssh ox-tiddly ox-timeline humanoid-themes ox-reveal docbook python-mode company-lsp elpy scala-mode ox-minutes ox-asciidoc ox-tufte indium use-package lsp-haskell lsp-java flymake-rust company-web eshell-z flycheck-haskell helm-flycheck prettier-js web-mode web-mode-edit-element haskell-mode hasklig-mode eglot lsp-mode sublime-themes tango-2-theme tango-plus-theme tangotango-theme tao-theme underwater-theme vs-dark-theme vs-light-theme vscdark-theme waher-theme white-sand-theme white-theme xresources-theme yoshi-theme markup markup-faces organic-green-theme atom-dark-theme badger-theme badwolf-theme birds-of-paradise-plus-theme calmer-forest-theme chocolate-theme constant-theme dakrone-theme dark-krystal-theme dark-mint-theme dark-souls darkburn-theme darkmine-theme darkokai-theme darktooth-theme distinguished-theme exotica-theme eziam-theme flatui-dark-theme flatui-theme forest-blue-theme gotham-theme grandshell-theme gruber-darker-theme hamburg-theme klere-theme liso-theme lush-theme madhat2r-theme majapahit-theme minimal-theme monochrome-theme monokai-alt-theme monokai-pro-theme monokai-theme monotropic-theme mood-one-theme nimbus-theme noctilux-theme northcode-theme nova-theme nubox nyx-theme omtose-phellack-theme overcast-theme paganini-theme planet-theme reykjavik-theme rimero-theme seti-theme sexy-monochrome-theme soft-charcoal-theme soft-morning-theme soft-stone-theme soothe-theme spacemacs-theme srcery-theme toxi-theme warm-night-theme ensime solarized-theme alect-themes hc-zenburn-theme zen-and-art-theme zeno-theme zerodark-theme zweilight-theme afternoon-theme flycheck flycheck-demjsonlint ample-theme ample-zen-theme arc-dark-theme arjen-grey-theme lua-mode yaml-imenu yaml-mode htmlize org-bullets org-mind-map org-reverse-datetree org-shoplist org color-theme-buffer-local color-theme-modern highlight-parentheses idea-darkula-theme jazz-theme kooten-theme))
 '(tramp-syntax 'simplified nil (tramp))
 '(window-divider-default-right-width 1)
 '(window-divider-mode t))
(require 'company)
(global-company-mode t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))







;; Java -------------------------------------------------------------

;;(require 'lsp-java)

;(add-hook 'java-mode-hook #'lsp)

;; (use-package lsp-java    ;; not recognized, for some reason
;;   :init
;;   (defun jmi/java-mode-config ()
;;     (setq-local tab-width 4
;;                 c-basic-offset 4)
;;     (toggle-truncate-lines 1)
;;     (setq-local tab-width 4)
;;     (setq-local c-basic-offset 4)
;;     (lsp))

;;   :config
;;   ;; Enable dap-java
;;   (require 'dap-java)

;;   ;; Support Lombok in our projects, among other things
;;   (setq lsp-java-vmargs
;;         (list "-noverify"
;;               "-Xmx2G"
;;               "-XX:+UseG1GC"
;;               "-XX:+UseStringDeduplication"
;;               (concat "-javaagent:" jmi/lombok-jar)
;;               (concat "-Xbootclasspath/a:" jmi/lombok-jar))
;;         lsp-file-watch-ignored
;;         '(".idea" ".ensime_cache" ".eunit" "node_modules"
;;           ".git" ".hg" ".fslckout" "_FOSSIL_"
;;           ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
;;           "build")

;;         lsp-java-import-order '["" "java" "javax" "#"]
;;         ;; Don't organize imports on save
;;         lsp-java-save-action-organize-imports nil

;;         ;; Formatter profile
;;         lsp-java-format-settings-url
;;         (concat "file://" jmi/java-format-settings-file))

;;   :hook (java-mode   . jmi/java-mode-config)

;;   :demand t
;;   :after (lsp lsp-mode dap-mode jmi-init-platform-paths))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
