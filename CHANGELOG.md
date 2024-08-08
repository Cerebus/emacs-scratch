# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## 0.2.0 - 2024-08-08
#### Bug Fixes
- **(email)** find cited region in both directions - (fbdea67) - Timothy J. Miller
- **(email)** force mime-types loading - (90906cc) - Timothy J. Miller
- **(email)** refile action now unflags email - (207f700) - Timothy J. Miller
- **(embark)** delete-pair region - (0fbaf8d) - Timothy J. Miller
- **(ide)** use format-all instead of LSP for formatting - (3399059) - Timothy J. Miller
- **(ide)** keep eglot out of echo area - (c362399) - Timothy J. Miller
- **(ide)** limit eldoc echo area to 1 line - (212779b) - Timothy J. Miller
- **(ide)** make switch-to-scratch function only for direct creation of tab/frame. - (24f5f0a) - Timothy J. Miller
- **(ide)** switch to frame before switching buffer on create - (81f2361) - Timothy J. Miller
- **(ide)** better eglot integration with polymode - (872de87) - Timothy J. Miller
- **(ide)** auto-revert mode - (de7f3f7) - Timothy J. Miller
- **(init)** unneeded package config - (65d9717) - Timothy Miller
- **(init)** finally resolve that annoying display error message - (cd976a4) - Timothy J. Miller
- **(init)** remove server start and exit - (2f76257) - Timothy J. Miller
- **(init)** server-start instead of mode - (e8f54d9) - Timothy J. Miller
- **(mail)** remove gnus-calendar - (edc4930) - Timothy J. Miller
- **(mail)** limit cited region target to message-mode derivatives - (16b4e86) - Timothy J. Miller
- **(markdown)** mark mermaid target region before compile - (8088221) - Timothy J. Miller
- **(markdown)** add visual and adaptive wrap - (f34b0ce) - Timothy J. Miller
- **(org)** enable glossary mode by default - (7255a03) - Timothy J. Miller
- **(org)** add capt-of to mitretr - (3ab7488) - Timothy J. Miller
- **(org)** startup with code folded and images inlined - (8690e83) - Timothy J. Miller
- **(org)** remove < from eletric-pair - (c065a08) - Timothy J. Miller
- **(org)** add adaptive and visual wrap - (4ef64f5) - Timothy J. Miller
- **(pdf)** disable line numbers minor mode in pdf-view-mode - (d484173) - Timothy J. Miller
- **(pdf)** swap pdf-tools for docview, at least for email - (23dc47e) - Timothy J. Miller
- **(plantuml)** add additional org-babel languages - (e9c8537) - Timothy J. Miller
- **(plantuml)** fix mode list regex - (b869980) - Timothy J. Miller
- **(plantuml)** add mermaid-mode for ob-mermaid - (bf61cbf) - Timothy J. Miller
- **(python)** set fill column - (027d6a4) - Timothy J. Miller
- **(python)** tweak plist syntax - (8270466) - Timothy J. Miller
- **(python)** configure eglot for flake8 - (e1ffaec) - Timothy J. Miller
- **(python)** remove format-all-formatters - (30e307c) - Timothy J. Miller
- **(python)** remove extra formatters - (acf1bf6) - Timothy J. Miller
- **(python)** add org-babel support - (f433fc6) - Timothy J. Miller
- **(python)** format on save with black and isort - (026f405) - Timothy J. Miller
- **(python)** move formatting to eglot and enable python start w/ C-c C-z - (c51e79d) - Timothy J. Miller
- **(yaml)** update yamlls init options - (04f5fdb) - Timothy J. Miller
- **(yaml)** yaml-language-server syntax fix - (fef0731) - Timothy J. Miller
- **(yaml)** remove yaml-ts-mode from auto-mode-alist - (cfce045) - Timothy J. Miller
- **(yaml)** better yaml/jinja2 file detection - (2953b0f) - Timothy J. Miller
- **(zettel)** update reference capture template - (b65cb88) - Timothy J. Miller
- **(zettel)** set global bibliography correctly - (30fd40e) - Timothy J. Miller
- **(zettel)** update dblocks on save - (4fc0b98) - Timothy J. Miller
- **(zettel)** enable denote dired mode - (cca411a) - Timothy J. Miller
- **(zettle)** correct urldate format - (cab092c) - Timothy J. Miller
- ignore cache dir - (cdb443e) - Timothy J. Miller
- new mac, new homebrew path - (445bb5a) - Timothy J. Miller
- use eglot format only with modes that have an lsp - (c402dcd) - Timothy J. Miller
- add early-init for undecorated frame - (fca0c29) - Timothy Miller
- ignore request cache - (3432799) - Timothy J. Miller
- org-babel redisplay images - (5c3dc0a) - Timothy J. Miller
#### Documentation
- **(mail)** docstring fix - (d6b261e) - Timothy J. Miller
- **(zettel)** update sorter docstring - (888bb08) - Timothy J. Miller
- **(zettle)** todos - (d747f0b) - Timothy J. Miller
#### Features
- **(consult)** add flyspell and eglot - (7aa7886) - Timothy J. Miller
- **(email)** embark target for citation regions - (f0fae98) - Timothy J. Miller
- **(email)** add refile and unflag action - (b94282e) - Timothy J. Miller
- **(global)** add flyspell to text-modes - (3d66967) - Timothy J. Miller
- **(ide)** add snippet support to eglot - (9ff2f88) - Timothy J. Miller
- **(ide)** eglot format and flyspell added to prog-modes - (70a7e11) - Timothy J. Miller
- **(ide)** switch to scratch on new frame - (6364126) - Timothy J. Miller
- **(init)** new key: s-i completion-at-point - (2839e6e) - Timothy J. Miller
- **(init)** emacs server mode - (5422903) - Timothy J. Miller
- **(init)** enable auto-insert-mode - (7a20a11) - Timothy J. Miller
- **(mail)** use mailcaps for microsoft crapola - (c25df40) - Timothy J. Miller
- **(markdown)** add embark region target for markdown fenced code blocks - (6f6cf09) - Timothy J. Miller
- **(org)** mitretr export support - (f693c0f) - Timothy J. Miller
- **(org)** add glossary support - (83da69b) - Timothy J. Miller
- **(org)** add fragtog auto-preview LaTeX fragments - (30c9b83) - Timothy J. Miller
- **(pdf-tools)** initial-commit - (0f918ed) - Timothy J. Miller
- **(rego)** opa rego-mode initial commit - (d55854f) - Timothy J. Miller
- **(zettel)** add citar support - (bf2502c) - Timothy J. Miller
- **(zettel)** add sequence sorting to dired - (4ab5e6b) - Timothy J. Miller
- **(zettel)** add sorting by signature - (2a26f63) - Timothy J. Miller
- **(zettel)** bind biblio-lookup - (a89c218) - Timothy J. Miller
- **(zettel)** initial commit - (2df2999) - Timothy J. Miller
- add mailcaps for mu4e - (bcbc5e0) - Timothy J. Miller
- json mode - (071e9fd) - Timothy J. Miller
- add ob-mermaid - (a70bf2a) - Timothy J. Miller
- dockerfile mode added - (9d3b59f) - Timothy J. Miller
#### Miscellaneous Chores
- **(python)** update package deps comment - (aad6d22) - Timothy J. Miller
- **(version)** 0.1.0 - (6d1306c) - Timothy J. Miller
- formatting - (977ee9f) - Timothy J. Miller
- update gitignore - (9c56953) - Timothy J. Miller
- update gitignore - (58e729d) - Timothy J. Miller
#### Refactoring
- **(zettel)** simplify my-zettelkasten-sequence-sort - (3cedee6) - Timothy J. Miller
- **(zettel)** use proper keymap - (e357f80) - Timothy J. Miller
- **(zettel)** proper customization and cleanup - (8c2655b) - Timothy J. Miller
#### Revert
- **(ide)** go back to eglot formatting - (7247e1a) - Timothy J. Miller
#### Style
- **(zettel)** move function to correct block - (b80cc10) - Timothy J. Miller

- - -

## 0.1.0 - 2023-09-28
#### Bug Fixes
- **(ide)** auto-revert mode - (de7f3f7) - Timothy J. Miller
- **(markdown)** add visual and adaptive wrap - (f34b0ce) - Timothy J. Miller
- **(org)** add adaptive and visual wrap - (4ef64f5) - Timothy J. Miller
- org-babel redisplay images - (5c3dc0a) - Timothy J. Miller
#### Features
- **(email)** add refile and unflag action - (b94282e) - Timothy J. Miller
- json mode - (071e9fd) - Timothy J. Miller
- add ob-mermaid - (a70bf2a) - Timothy J. Miller
- dockerfile mode added - (9d3b59f) - Timothy J. Miller

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).