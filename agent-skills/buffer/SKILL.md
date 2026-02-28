---
name: buffer
description: 'This skill should be used when the user invokes "/buffer" to list all open Emacs buffer names or read the content of a specific buffer via emacsclient.'
tools: Bash
disable-model-invocation: true
---

# Emacs Buffer Operations

Interact with Emacs buffers using `emacsclient --eval`. Supports two operations:

- **List buffers**: Return all open buffer names.
- **Read buffer**: Return the content of a specific buffer.

## Usage

Locate `agent-skills-buffer.el` which lives alongside this skill file, then call the appropriate function.

### List all buffers

```sh
emacsclient --eval '
(progn
  (load "/path/to/skills/buffer/agent-skills-buffer.el" nil t)
  (agent-skills/list-buffers))'
```

### Read a specific buffer

```sh
emacsclient --eval '
(progn
  (load "/path/to/skills/buffer/agent-skills-buffer.el" nil t)
  (agent-skills/read-buffer "BUFFER-NAME"))'
```

## Rules

- Locate `agent-skills-buffer.el` relative to this skill file's directory.
- Determine the user's intent:
  - To list buffers → call `agent-skills/list-buffers`.
  - To read a buffer → call `agent-skills/read-buffer` with the buffer name.
- If the user wants to read a buffer but does not specify which one, ask the user for the buffer name.
- Run the `emacsclient --eval` command via the Bash tool.
- Present the result to the user in a readable format.
