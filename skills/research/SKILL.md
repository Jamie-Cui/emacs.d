---
name: research
description: Document-aware research assistant. Reads all document files (.tex, .org, .md, .txt) in the current directory and helps with research tasks — finding related work, analyzing gaps, drafting sections, etc. Works for LaTeX papers, org documents, plain text, or any mix.
---

# Research Assistant Skill

## Trigger
`/research [optional task description]`

## Step 1: Document Discovery

Use the Glob tool to recursively find all document files in the current working directory.

Search for these patterns (run all in parallel):
- `**/*.tex`
- `**/*.org`
- `**/*.md`
- `**/*.txt`

From the results, exclude any paths containing these segments:
- `.git/`
- `_build/`
- `auto/`
- `node_modules/`
- `.elpa/`
- `elpa/`

If no files remain after filtering: output the following and stop:
> "No document files found in the current directory. Please confirm your working directory contains .tex, .org, .md, or .txt files."

## Step 2: Read All Documents

Use the Read tool to read every file discovered in Step 1. Read them in parallel where possible.

Do not summarize yet — load the full content into context.

## Step 3: Understand the Document

From the loaded content, extract and hold in mind:
- Title or topic of the document/project
- Core research question or thesis (if present)
- Main sections or structure
- Key terminology, methods, and concepts

No need to output this yet — it is internal context for the following steps.

## Step 4: Mode Branch

### If the user provided a task argument:

Execute the task immediately using the document content as context. Be specific and grounded in the actual paper — reference sections, quote relevant passages, and tailor your response to the document's topic and stage.

Example tasks you should handle well:
- `find related work on [topic]` → search web + synthesize relevant literature in context of the paper
- `analyze research gaps` → identify what the paper leaves unaddressed based on its own framing
- `draft an introduction` → write a LaTeX/org/markdown introduction grounded in the paper's content
- `suggest experiments` → propose experiments consistent with the paper's methodology
- `improve the abstract` → rewrite or critique the existing abstract

### If no argument was provided:

Output a brief document overview in this format:

```
## Document Overview

**Topic:** [inferred title or topic]
**Type:** [e.g., research paper, technical report, notes]
**Structure:** [list of main sections/files, one per line]
**Core question:** [one sentence summary of the research question or main argument]

## What can I help with?

- Find and summarize related work
- Identify research gaps or missing citations
- Draft or improve a specific section
- Suggest experiments or evaluation strategies
- Critique the argument or methodology
- Search for references on a specific topic

What would you like to work on?
```

Then wait for the user's input. All subsequent conversation in this session uses the document content as context.

## Output Rules

- Default: respond conversationally in the terminal
- Write to file only when the user explicitly requests it (e.g., "save this to notes.md" or "add this to the paper")
- When writing to file, confirm the path with the user before writing
