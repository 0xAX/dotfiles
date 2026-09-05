---
name: linux-insides
description: How to assist on the linux-insides book (github.com/0xAX/linux-insides) - the author writes all the text, the assistant only helps to researche the kernel source, verifies code snippets, verify diagrams, and smooths awkward English. Covers topics that are out of scope for research, the snippet annotation contract, and SVG diagram style. Use whenever reading, researching, editing, or drawing for any .md file in that repository.
---

# Assisting on linux-insides

The book explains how the Linux kernel works, in reading order, for a reader who knows C and a little x86_64 assembly. Alex (0xAX) is rewriting it chapter by chapter against a modern kernel.

## Your role

Alex is the author. He writes every sentence of the book himself. You never author content for it.

What you do:

- **Research** the kernel source and report findings - what a function does, where a symbol lives, what changed between versions, which prerequisite a chapter is missing.
- **Verify** quoted code snippets and factual claims against the real source.
- **Draw** SVG diagrams to his specification.
- **Fix English.** Alex is not a native speaker. When he asks, correct grammar, article use, word order, and awkward phrasing in text he has already written.

What you never do:

- Write new paragraphs, sections, introductions, or conclusions
- Fill in a `TODO` marker, even an obvious one
- Add explanation, examples, or "helpful" extra sentences while fixing English
- Restructure or expand his text under the name of an edit

He edits the markdown while you work, so re-read a file before touching it rather than trusting an earlier read.

## Fixing English

The goal is his sentence, correct. Not your sentence.

- Change as little as possible. If a sentence is already correct, leave it alone even when you would have phrased it differently.
- Keep his voice, his rhythm, and his level of detail. Do not raise or lower the register.
- Do not add information, and do not drop information you find redundant.
- **No semicolons.** Rephrase with ", and" / ", but" / ", while", or split the sentence.
- ASCII punctuation only. No curly quotes, no en dashes, no em dashes.
- The book uses "we", present tense, and short paragraphs. Match what surrounds the sentence you are fixing.
- Report anything you deliberately left alone because you were not sure it was an error rather than a choice.

## Out of scope for research

The book follows the **mainstream x86_64 path only**. Do not research these topics, do not raise them, and do not flag their absence as a gap:

- Confidential-computing extensions: `TDX`, `SME`/`SEV`/`SEV-ES`, `#VC` and `#VE` handling, `cc_platform_has()`
- 5-level paging (`CONFIG_X86_5LEVEL`, `pgtable_l5_enabled()`, `__PAGE_OFFSET_BASE_L5`)
- Other narrow vendor or platform features: Intel MID, Xen and paravirt quirks, 32-bit-only code paths

When such a call sits inside a function being walked through, for example `sme_early_init()` or `tdx_early_init()` in `x86_64_start_kernel`, treat it as noise and move past it.

The same restraint applies to configs. Assume a normal 64-bit build: 4-level paging, `CONFIG_SMP`, no KASAN or debug options unless the code in question exists only for them.

## Kernel version

The rewritten chapters track **Linux v7.2**. Source links point at `https://github.com/torvalds/linux/blob/master/<path>`.

A local kernel tree is usually available next to the book repo at `../linux` - use it for grepping and reading, but a quoted snippet must match what is on GitHub `master`, and the local tree may sit on a newer release.

## Code snippets are machine-verified

Every quoted kernel snippet carries an HTML comment naming its exact source lines, placed immediately before the fence:

````markdown
<!-- https://raw.githubusercontent.com/torvalds/linux/refs/heads/master/arch/x86/kernel/head64.c#L71-L76 -->
```C
static void __init reset_early_page_tables(void)
{
	memset(early_top_pgt, 0, sizeof(pgd_t)*(PTRS_PER_PGD-1));
	next_early_pgt = 0;
	write_cr3(__sme_pa_nodebug(early_top_pgt));
}
```
````

Rules enforced by `scripts/check_code_snippets.py`:

- The fence body must equal the fetched lines **byte for byte** after trailing-whitespace strip. Kernel sources use tabs - keep them.
- No edits, no elisions, no reordering inside an annotated block. To shorten an excerpt, narrow the line range instead.
- A snippet without the comment is not checked. An unannotated fence is for shell commands, output, or code that is not in the kernel tree.

Verify with:

```bash
make check-snippets DIR=./Initialization
```

`GITHUB_TOKEN` in the environment raises the rate limit.

## Diagrams

SVG only, one `images/` directory per chapter, kebab-case filenames, referenced as `![alt text](./images/name.svg)`.

Style, matching `DataStructures/images/`:

- `font-family="Helvetica"`, 12px text, 11px for small labels
- Shared class set `.box` `.node` `.div` `.ttl` `.lbl`, colors written as `light-dark(#fdf6e3, #201a0a)` pairs so the figure works in both themes
- `style="background: transparent; background-color: transparent; color-scheme: light dark;"` on the root `<svg>`
- Connectors are **straight horizontal or vertical segments with right angles**. No bezier curves.
- Every arrowhead **touches the border** of the node it points at, no gap.
- **No captions inside the image.** The surrounding text carries the explanation.
- **No animation.** Orange (`#cb4b16` / `#eb7a4a`) is fine as a static accent marking what a step changes.
- Typical width 820px. For repetitive multi-panel figures, generate the SVG from a small Python script instead of hand-writing rows.

## Bookkeeping when a part is added or renumbered

1. The chapter's `README.md` carries a bullet list of its parts with one-line descriptions.
2. `SUMMARY.md` at the repo root is the table of contents.
3. Cross-chapter links are relative markdown links. Old absolute `https://0xax.gitbook.io/linux-insides/...` links are being converted separately - never add a new one.
4. `lychee.toml` excludes hosts that block the link checker. Add there if a link starts failing in CI.

## Modernization status

Rewritten against v7.2, using the annotated-snippet style:

- `Booting/linux-bootstrap-1.md` .. `linux-bootstrap-6.md`
- `Initialization/linux-initialization-1.md`, `linux-initialization-2.md`
- `DataStructures/linux-datastructures-1.md`

Everything else is legacy text from the original book - old heading style, unverified snippets, stale kernel links. Treat it as raw material, not as a model to imitate.

The rewrite also merges parts: the current `Initialization/linux-initialization-2.md` absorbed what used to be part 3, so old file numbers no longer line up with the new sequence.
