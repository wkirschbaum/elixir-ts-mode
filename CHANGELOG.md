# Changelog for elixir-ts-mode ( MELPA )

## v1.5

This version will change some font features to only be applied for
level 4, and not for the default level 3.  If you want the old
behaviour back you can set `treesit-font-lock-level` to 4. For
example:

    (setq treesit-font-lock-level 4)


in your init.el will highlight function calls, operators and numbers.

### 1. Enhancements
  * [font] Properly organize font-lock levels.

### 2. Bug fixes
  * [font] Match spec definition for function as a function-name. 
  * [font] Do not mark builtins as comments on lower font-lock levels.
  * [font] Fix dot for function variables.
  * [font] Properly match unary operator for lists.
  * [font] Do not mark any identifier for lower font-lock levels.

### 3. Soft deprecations (no warnings emitted)

  Custom font faces ending with -face are being fazed out in Emacs, so
  renaming them to follow modern conventions.

  * [font] `elixir-ts-comment-doc-identifier-face` marked as obsolete
    to favor `elixir-ts-comment-doc-identifier`.
  * [font] `elixir-ts-comment-doc-attribute-face` marked as obsolete
    to favor `elixir-ts-comment-doc-attribute`.
  * [font] `elixir-ts-sigil-name-face` marked as obsolete
    to favor `elixir-ts-sigil-name`.
  * [font] `elixir-ts-atom-face` marked as obsolete
    to favor `elixir-ts-atom`.
  * [font] `elixir-ts-keyword-key-face` marked as obsolete
    to favor `elixir-ts-atom`.
  * [font] `elixir-ts-attribute-face` marked as obsolete
    to favor `elixir-ts-attribute`.
