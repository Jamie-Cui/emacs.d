---
name: emacs
description: 'Use this skill proactively for any Emacs-related task: exploring Emacs configuration, debugging issues, testing packages, or interacting with a running Emacs instance. Capabilities: list/describe elisp functions, evaluate expressions, simulate keystrokes, inspect buffer/minibuffer state via emacsclient.'
tools: Bash
---

# Emacs Operations

The user has an Emacs server running. **All** Emacs operations must go through `emacsclient`, never `emacs` or `emacs --batch`. This includes both user-requested actions and agent-initiated operations like byte compilation, syntax checking, or running tests.

Interact with the running Emacs instance using `emacsclient --eval`. Supports six operations:

- **List functions**: Return interactive command names matching a prefix.
- **Describe function**: Return the arglist and docstring of a function.
- **Eval expression**: Evaluate an arbitrary elisp expression and return the result.
- **Execute keys**: Simulate keystrokes as if typed by the user.
- **Minibuffer prompt**: Read the current minibuffer prompt and contents (useful for seeing what Emacs is asking).
- **Current buffer state**: Return the name, major mode, and excerpt of the focused buffer.

## Usage

Locate `agent-skills-emacs.el` which lives alongside this skill file, then call the appropriate function.

### List all elisp functions matching a prefix

```sh
emacsclient --eval '
(progn
  (load "/path/to/skills/emacs/agent-skills-emacs.el" nil t)
  (agent-skills/list-functions "PREFIX"))'
```

### Describe an elisp function

```sh
emacsclient --eval '
(progn
  (load "/path/to/skills/emacs/agent-skills-emacs.el" nil t)
  (agent-skills/describe-function "FUNCTION-NAME"))'
```

### Evaluate an elisp expression

```sh
emacsclient --eval '
(progn
  (load "/path/to/skills/emacs/agent-skills-emacs.el" nil t)
  (agent-skills/eval-expression "EXPRESSION"))'
```

### Execute keystrokes

```sh
emacsclient --eval '
(progn
  (load "/path/to/skills/emacs/agent-skills-emacs.el" nil t)
  (agent-skills/execute-keys "KEYS"))'
```

KEYS uses `kbd` format (e.g. `C-x C-s` to save, `S c c` for magit stage-all then commit).

### Read minibuffer prompt

```sh
emacsclient --eval '
(progn
  (load "/path/to/skills/emacs/agent-skills-emacs.el" nil t)
  (agent-skills/minibuffer-prompt))'
```

### Read current buffer state

```sh
emacsclient --eval '
(progn
  (load "/path/to/skills/emacs/agent-skills-emacs.el" nil t)
  (agent-skills/current-buffer-state))'
```

## Rules

- Locate `agent-skills-emacs.el` relative to this skill file's directory.
- Determine the user's intent:
  - To find functions → call `agent-skills/list-functions` with a prefix string.
  - To inspect a function → call `agent-skills/describe-function` with the function name.
  - To evaluate elisp → call `agent-skills/eval-expression` with the expression string.
  - To simulate keystrokes → call `agent-skills/execute-keys` with a `kbd`-format key string.
  - To check what Emacs is prompting for → call `agent-skills/minibuffer-prompt`.
  - To see the focused buffer → call `agent-skills/current-buffer-state`.
- When driving interactive commands, use `minibuffer-prompt` and `current-buffer-state` to observe Emacs state between `execute-keys` calls.
- Note that `execute-keys` runs synchronously — if a command is async (e.g. magit refresh), a subsequent `execute-keys` call may need to be a separate `emacsclient` invocation.
- If the user's request is ambiguous, ask for clarification.
- Run the `emacsclient --eval` command via the Bash tool.
- Present the result to the user in a readable format.
