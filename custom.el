
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#2d2d2d" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#6699cc" "#d3d0c8"])
 '(ansi-term-color-vector
   [unspecified "#2d2d2d" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#6699cc" "#d3d0c8"])
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cursor-type (quote bar))
 '(custom-safe-themes
   (quote
    ("274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "579e9950513524d8739e08eae289419cfcb64ed9b7cc910dd2e66151c77975c4" "7f9dc0c7bc8e5b4a1b9904359ee449cac91fd89dde6aca7a45e4ed2e4985664c" "47ec21abaa6642fefec1b7ace282221574c2dd7ef7715c099af5629926eb4fd7" "7f89ec3c988c398b88f7304a75ed225eaac64efa8df3638c815acc563dfd3b55" "595617a3c537447aa7e76ce05c8d43146a995296ea083211225e7efc069c598f" "fd3c7bd752f48dcb7efa5f852ef858c425b1c397b73851ff8816c0580eab92f1" "3da031b25828b115c6b50bb92a117f5c0bbd3d9d0e9ba5af3cd2cb9db80db1c2" "a622aaf6377fe1cd14e4298497b7b2cae2efc9e0ce362dade3a58c16c89e089c" "2a9039b093df61e4517302f40ebaf2d3e95215cb2f9684c8c1a446659ee226b9" "e2fd81495089dc09d14a88f29dfdff7645f213e2c03650ac2dd275de52a513de" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "a5b1d671532f27c524264b433ad58df329297b7bb21966eddda1d385e7a9b055" "c9f102cf31165896631747fd20a0ca0b9c64ecae019ce5c2786713a5b7d6315e" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "5ed25f51c2ed06fc63ada02d3af8ed860d62707e96efc826f4a88fd511f45a1d" "abdacb77b5eba6abbb3505ad297b2b296645e8578d1335f08fea0ff641f3895b" "28bf1b0a72e3a1e08242d776c5befc44ba67a36ced0e55df27cfc7ae6be6c24d" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "0329c772ed96053a73b9ddddf96c1183e23c267955bbdf78e7933057ce9da04b" "e8825f26af32403c5ad8bc983f8610a4a4786eb55e3a363fa9acb48e0677fe7e" "cdd26fa6a8c6706c9009db659d2dffd7f4b0350f9cc94e5df657fa295fffec71" "5a21604c4b1f2df98e67cda2347b8f42dc9ce471a48164fcb8d3d52c3a0d10be" "3a5f04a517096b08b08ef39db6d12bd55c04ed3d43b344cf8bd855bde6d3a1ae" "72085337718a3a9b4a7d8857079aa1144ea42d07a4a7696f86627e46ac52f50b" "ace21d57cd9a22c96c021acfd2938411e3374626fe8d91afb9bb969b5269ea75" "1b6f7535c9526a5dbf9fb7e3604d0280feb7a07b970caf21ebd276ddc93ef07a" "d74fe1508cff43708fa2f97c4bf58d19f0e002b2e0c92bf958bf483113b7d89d" "36c86cb6c648b9a15d849026c90bd6a4ae76e4d482f7bcd138dedd4707ff26a5" "819d24b9aba8fcb446aecfb59f87d1817a6d3eb07de7fdec67743ef32194438b" "840db7f67ce92c39deb38f38fbc5a990b8f89b0f47b77b96d98e4bf400ee590a" "8b4d8679804cdca97f35d1b6ba48627e4d733531c64f7324f764036071af6534" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "0d456bc74e0ffa4bf5b69b0b54dac5104512c324199e96fc9f3a1db10dfa31f3" "8d8423e863b3fbc6346758d726bae66b3d2bacac526067d7d8f2d710203066eb" "e9740103f6ae2cbc97fef889b85b1c51b4d4a2d95c2b398b57a1842d14d96304" "0e8bac1e87493f6954faf5a62e1356ec9365bd5c33398af3e83cfdf662ad955f" "3e160974b9e3e1b53270d1fb5bbaf56f0c689017e177972f72584bf096efc4cc" "7023f8768081cd1275f7fd1cd567277e44402c65adfe4dc10a3a908055ed634d" "565aa482e486e2bdb9c3cf5bfb14d1a07c4a42cfc0dc9d6a14069e53b6435b56" "5d75f9080a171ccf5508ce033e31dbf5cc8aa19292a7e0ce8071f024c6bcad2a" "bd82c92996136fdacbb4ae672785506b8d1d1d511df90a502674a51808ecc89f" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" default)))
 '(doom-neotree-folder-size 1.0)
 '(fci-always-use-textual-rule nil)
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "light blue")
 '(fci-rule-use-dashes nil)
 '(fci-rule-width 1)
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
 '(frame-brackground-mode (quote dark))
 '(fringe-mode 6 nil (fringe))
 '(global-linum-mode nil)
 '(highlight-changes-colors (quote ("#EF5350" "#7E57C2")))
 '(highlight-indent-guides-character 124)
 '(highlight-tail-colors
   (quote
    (("#010F1D" . 0)
     ("#B44322" . 20)
     ("#34A18C" . 30)
     ("#3172FC" . 50)
     ("#B49C34" . 60)
     ("#B44322" . 70)
     ("#8C46BC" . 85)
     ("#010F1D" . 100))))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f")
     ("???" . "#dc752f"))))
 '(jdee-db-active-breakpoint-face-colors (cons "#000000" "#fd971f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#000000" "#b6e63e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#000000" "#525254"))
 '(js2-missing-semi-one-line-override nil)
 '(linum-format " %3i ")
 '(magit-diff-use-overlays nil)
 '(neo-theme (quote ascii))
 '(neo-window-fixed-size nil)
 '(neo-window-position (quote right))
 '(neo-window-width 35)
 '(nrepl-message-colors
   (quote
    ("#ee11dd" "#8584ae" "#b4f5fe" "#4c406d" "#ffe000" "#ffa500" "#ffa500" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (gruvbox-theme night-owl-theme dark-krystal-theme farmhouse-theme majapahit-theme twilight-anti-bright-theme hamburg-theme ubuntu-theme reverse-theme challenger-deep-theme distinguished-theme kooten-theme zweilight-theme snazzy-theme firecode-theme purple-haze-theme klere-theme avk-emacs-themes noctilux-theme color-theme-emacs-revert-theme gruber-darker-theme color-theme-gruber-darker jujube-theme brutalist-theme seti-theme auto-complete merlin lsp-ocaml import-js graphene-meta-theme flatui-dark-theme goto-chg restclient tern-auto-complete xref-js2 company-tern aggressive-indent multi-term emojify eslint-fix dart-mode dracula-theme ace-jump-buffer ace-jump-mode badger-theme atom-dark-theme atom-one-dark-theme bubbleberry-theme ujelly-theme docker-compose-mode nodejs-repl indium eglot easy-kill-extras easy-kill doom-modeline zoom zerodark-theme yaml-mode winum use-package tagedit spacemacs-theme smooth-scrolling smex smartparens rjsx-mode rainbow-delimiters projectile php-mode paredit page-break-lines neotree move-dup markdown-mode json-mode js2-refactor jbeans-theme highlight-symbol highlight-indent-guides guide-key go-guru go-autocomplete focus fill-column-indicator exec-path-from-shell ergoemacs-status ergoemacs-mode dumb-jump doom-themes diminish counsel clojure-mode-extra-font-locking cider change-inner browse-kill-ring base16-theme alchemist)))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e")))
 '(pos-tip-background-color "#303030")
 '(pos-tip-foreground-color "#011627")
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "mediumspringgreen")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "goldenrod")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "mediumspringgreen")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "goldenrod")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "mediumspringgreen"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#011627" "#010F1D" "#DC2E29" "#EF5350" "#D76443" "#F78C6C" "#D8C15E" "#FFEB95" "#5B8FFF" "#82AAFF" "#AB69D7" "#C792EA" "#AFEFE2" "#7FDBCA" "#D6DEEB" "#FFFFFF")))
 '(when
      (or
       (not
        (boundp
         (quote ansi-term-color-vector)))
       (not
        (facep
         (aref ansi-term-color-vector 0)))))
 '(xterm-color-names
   ["#303030" "#D66F84" "#D79887" "#D49A8A" "#94B1A3" "#A8938C" "#989584" "#BAB2A9"])
 '(xterm-color-names-bright
   ["#3A3A3A" "#E47386" "#CC816B" "#769188" "#7D6F6A" "#9C8772" "#BAB2A9"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
